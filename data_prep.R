# ==============================================================================
# data_prep.R  --  ETL pipeline for the Clinical KPI & Quality Dashboard
# ------------------------------------------------------------------------------
# Input : raw Synthea CSV exports in data/raw/
#           patients.csv, encounters.csv, conditions.csv,
#           organizations.csv, payers.csv, providers.csv
# Output: analysis-ready marts in data/processed/
#           app_data.rds   (named list, loaded by the Shiny app)
#           *.parquet       (optional flat exports, if `arrow` is installed)
#
# Run with:  Rscript data_prep.R
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
})

t0 <- Sys.time()
root      <- normalizePath(".")
raw_dir   <- file.path(root, "data", "raw")
proc_dir  <- file.path(root, "data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

source(file.path(root, "R", "mappings.R"))

msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n")

# ---- analysis window ---------------------------------------------------------
ANALYSIS_START <- as.Date("2015-01-01")
ANALYSIS_END   <- as.Date("2026-09-06")   # pinned "today" for a stable demo
READMIT_WINDOW <- 30                      # days

# ------------------------------------------------------------------------------
# 1. LOAD RAW
# ------------------------------------------------------------------------------
msg("Loading raw Synthea CSVs from", raw_dir)
stopifnot(dir.exists(raw_dir))

# raw tables ship gzipped; fall back to plain .csv if present
read_raw <- function(name) {
  gz  <- file.path(raw_dir, paste0(name, ".csv.gz"))
  csv <- file.path(raw_dir, paste0(name, ".csv"))
  path <- if (file.exists(gz)) gz else csv
  if (!file.exists(path)) stop("Missing raw table: ", name, " (looked for .csv.gz / .csv)")
  fread(path, showProgress = FALSE)
}

patients_raw   <- read_raw("patients")
encounters_raw <- read_raw("encounters")
conditions_raw <- read_raw("conditions")
orgs_raw       <- read_raw("organizations")
payers_raw     <- read_raw("payers")

msg(sprintf("  patients=%d  encounters=%d  conditions=%d  orgs=%d",
            nrow(patients_raw), nrow(encounters_raw),
            nrow(conditions_raw), nrow(orgs_raw)))

# ------------------------------------------------------------------------------
# 2. DIMENSION: PATIENTS
# ------------------------------------------------------------------------------
patients <- patients_raw %>%
  transmute(
    patient_id  = Id,
    birthdate   = as.Date(BIRTHDATE),
    deathdate   = as.Date(DEATHDATE),
    gender      = fifelse(GENDER == "M", "Male", "Female"),
    race        = str_to_title(RACE),
    ethnicity   = str_to_title(ETHNICITY),
    county      = COUNTY,
    zip         = as.character(ZIP)
  )

# ------------------------------------------------------------------------------
# 3. DIMENSION: FACILITIES  (synthetic acute-care roster -- see R/mappings.R)
# ------------------------------------------------------------------------------
facilities <- facility_roster %>%
  transmute(facility, region, beds)

# patient -> home facility
patient_facility <- patients %>%
  transmute(patient_id, facility = assign_facility(patient_id)) %>%
  left_join(facilities, by = "facility")

payer_lookup <- payers_raw %>%
  transmute(payer_id = Id, payer_name = NAME)

# ------------------------------------------------------------------------------
# 4. CONDITIONS -> primary diagnosis per encounter
# ------------------------------------------------------------------------------
# The condition whose START matches the encounter is treated as the primary
# reason for that visit. Fall back to the encounter's own REASONDESCRIPTION.
conditions <- conditions_raw %>%
  transmute(
    patient_id  = PATIENT,
    encounter_id = ENCOUNTER,
    dx_code     = as.character(CODE),
    dx_desc     = DESCRIPTION,
    dx_start    = as.Date(START)
  )

primary_dx <- conditions %>%
  filter(!is.na(encounter_id), encounter_id != "") %>%
  group_by(encounter_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(encounter_id, dx_code, dx_desc)

# ------------------------------------------------------------------------------
# 5. FACT: ENCOUNTERS
# ------------------------------------------------------------------------------
msg("Building encounter fact table")

enc <- encounters_raw %>%
  transmute(
    encounter_id    = Id,
    patient_id      = PATIENT,
    facility_id     = ORGANIZATION,
    payer_id        = PAYER,
    encounter_class = ENCOUNTERCLASS,
    enc_code        = as.character(CODE),
    enc_desc        = DESCRIPTION,
    reason_desc     = REASONDESCRIPTION,
    start_ts        = ymd_hms(START, quiet = TRUE),
    stop_ts         = ymd_hms(STOP,  quiet = TRUE),
    total_cost      = as.numeric(TOTAL_CLAIM_COST),
    payer_coverage  = as.numeric(PAYER_COVERAGE)
  ) %>%
  left_join(primary_dx, by = "encounter_id") %>%
  left_join(patients,   by = "patient_id") %>%
  left_join(patient_facility %>% select(patient_id, facility, region), by = "patient_id") %>%
  left_join(payer_lookup, by = "payer_id") %>%
  mutate(
    start_date   = as_date(start_ts),
    stop_date    = as_date(stop_ts),
    # best available clinical label for the visit
    dx_label     = coalesce(na_if(reason_desc, ""), dx_desc, enc_desc),
    dx_label     = str_squish(str_remove(dx_label, "\\s*\\((disorder|finding|situation|procedure)\\)$")),
    service_line = assign_service_line(coalesce(na_if(reason_desc, ""), dx_desc)),
    payer_type   = map_payer_type(payer_name),
    age_at_enc   = as.numeric(difftime(start_date, birthdate, units = "days")) / 365.25,
    age_band     = age_band(age_at_enc),
    los_days     = as.numeric(difftime(stop_ts, start_ts, units = "days")),
    is_acute_stay = encounter_class %in% c("inpatient", "snf"),
    los_days     = fifelse(is_acute_stay, pmax(los_days, 0.05), NA_real_),
    encounter_type = recode(encounter_class,
      ambulatory = "Ambulatory", wellness = "Wellness", outpatient = "Outpatient",
      urgentcare = "Urgent Care", emergency = "Emergency", inpatient = "Inpatient",
      home = "Home Health", virtual = "Virtual", hospice = "Hospice", snf = "Skilled Nursing",
      .default = str_to_title(encounter_class)),
    year  = year(start_date),
    month = floor_date(start_date, "month"),
    dow   = factor(wday(start_date, label = TRUE, abbr = FALSE),
                   levels = c("Monday","Tuesday","Wednesday","Thursday",
                              "Friday","Saturday","Sunday"))
  ) %>%
  filter(!is.na(start_date),
         start_date >= ANALYSIS_START, start_date <= ANALYSIS_END,
         !is.na(facility))

msg(sprintf("  kept %d encounters in window %s .. %s",
            nrow(enc), ANALYSIS_START, ANALYSIS_END))

# ------------------------------------------------------------------------------
# 6. FACT: ACUTE ADMISSIONS  (readmissions, LOS O/E, mortality, complications)
# ------------------------------------------------------------------------------
msg("Deriving acute admissions, 30-day readmissions and outcome proxies")

adm <- enc %>%
  filter(encounter_class == "inpatient") %>%
  arrange(patient_id, start_ts) %>%
  select(encounter_id, patient_id, facility, region,
         service_line, payer_type, dx_code, dx_label,
         start_ts, stop_ts, start_date, stop_date, los_days,
         age_at_enc, age_band, gender, race, deathdate, total_cost, month, year, dow)

setDT(adm)
setorder(adm, patient_id, start_ts)

# next inpatient admission for the same patient
adm[, next_admit_ts   := shift(start_ts, type = "lead"), by = patient_id]
adm[, days_to_readmit := as.numeric(difftime(next_admit_ts, stop_ts, units = "days"))]

# --- hospice / SNF transfer + death lookups (exclusions & disposition) --------
hospice_ts <- enc %>% filter(encounter_class == "hospice") %>%
  transmute(patient_id, hospice_ts = start_ts) %>% as.data.table()
snf_ts <- enc %>% filter(encounter_class == "snf") %>%
  transmute(patient_id, snf_ts = start_ts) %>% as.data.table()
home_ts <- enc %>% filter(encounter_class == "home") %>%
  transmute(patient_id, home_ts = start_ts) %>% as.data.table()

near_after <- function(dt_events, ts_col, ref, patient) {
  # TRUE if patient has an event of this type within (-1, 3] days of discharge ref
  if (nrow(dt_events) == 0) return(rep(FALSE, length(ref)))
  ev <- split(dt_events[[ts_col]], dt_events$patient_id)
  vapply(seq_along(ref), function(i) {
    e <- ev[[ patient[i] ]]
    if (is.null(e)) return(FALSE)
    any(e >= (ref[i] - lubridate::days(1)) & e <= (ref[i] + lubridate::days(3)))
  }, logical(1))
}

adm[, to_hospice := near_after(hospice_ts, "hospice_ts", stop_ts, patient_id)]
adm[, to_snf     := near_after(snf_ts,     "snf_ts",     stop_ts, patient_id)]
adm[, to_home_hh := near_after(home_ts,    "home_ts",    stop_ts, patient_id)]

adm[, died_in_hospital := !is.na(deathdate) & deathdate >= start_date & deathdate <= (stop_date + 1)]
adm[, died_30d         := !is.na(deathdate) & deathdate >  stop_date & deathdate <= (stop_date + 30)]

# --- discharge disposition (synthesised, documented in README) ----------------
adm[, discharge_disposition := fcase(
  died_in_hospital,               "Expired",
  to_hospice,                     "Hospice",
  to_snf,                         "Skilled Nursing Facility",
  to_home_hh,                     "Home with Home Health",
  default =                       "Home / Self-Care"
)]

# --- 30-day all-cause readmission --------------------------------------------
# Index admission = a live discharge that is not itself a transfer to hospice.
# Readmission = the same patient has another inpatient admission whose start is
# 0-30 days after the index discharge.
adm[, is_index := !died_in_hospital & discharge_disposition != "Hospice"]
adm[, readmit_30d := is_index & !is.na(days_to_readmit) &
      days_to_readmit >= 0 & days_to_readmit <= READMIT_WINDOW]

# --- expected LOS (O/E) ------------------------------------------------------
# Expected LOS = risk-adjusted-ish benchmark: mean LOS for the same service line
# and age band across the full analysis window (Winsorised to tame outliers).
adm[, los_w := pmin(los_days, quantile(los_days, 0.98, na.rm = TRUE))]
adm[, expected_los := mean(los_w, na.rm = TRUE), by = .(service_line, age_band)]
adm[is.na(expected_los) | expected_los <= 0,
    expected_los := mean(adm$los_w, na.rm = TRUE)]
adm[, oe_los := los_days / expected_los]

# --- complication proxy -----------------------------------------------------
comp_enc <- conditions %>%
  inner_join(adm %>% as_tibble() %>% select(patient_id, encounter_id, start_date, stop_date),
             by = c("patient_id", "encounter_id" = "encounter_id")) %>%
  mutate(is_comp = str_detect(tolower(dx_desc), complication_pattern)) %>%
  group_by(encounter_id) %>%
  summarise(complication_flag = any(is_comp), .groups = "drop")

# also catch complications recorded within +0..+14 days of discharge
comp_followup <- conditions %>%
  select(patient_id, dx_desc, dx_start) %>%
  inner_join(adm %>% as_tibble() %>%
               select(patient_id, idx_stop = stop_date, idx_encounter_id = encounter_id),
             by = "patient_id", relationship = "many-to-many") %>%
  filter(str_detect(tolower(dx_desc), complication_pattern),
         dx_start > idx_stop, dx_start <= idx_stop + 14) %>%
  distinct(encounter_id = idx_encounter_id) %>%
  mutate(comp_followup = TRUE)

adm <- adm %>%
  as_tibble() %>%
  left_join(comp_enc, by = "encounter_id") %>%
  left_join(comp_followup, by = "encounter_id") %>%
  mutate(
    complication_flag = coalesce(complication_flag, FALSE) | coalesce(comp_followup, FALSE)
  ) %>%
  select(-comp_followup, -los_w, -next_admit_ts,
         -to_hospice, -to_snf, -to_home_hh)

msg(sprintf("  admissions=%d  index=%d  30d-readmits=%d (%.1f%%)  30d-mortality=%.1f%%",
            nrow(adm), sum(adm$is_index),
            sum(adm$readmit_30d), 100 * sum(adm$readmit_30d) / sum(adm$is_index),
            100 * sum(adm$died_30d) / nrow(adm)))

# ------------------------------------------------------------------------------
# 7. AGGREGATE MARTS  (what the app charts read)
# ------------------------------------------------------------------------------
msg("Building aggregated marts")

# common grouping keys kept on every mart so the app can filter consistently
grp <- c("month", "year", "facility", "region", "service_line", "payer_type")

# --- 7a. encounter volume ---------------------------------------------------
agg_volume <- enc %>%
  group_by(month, year, facility, region, service_line, payer_type,
           encounter_type, age_band, gender) %>%
  summarise(encounters = n(),
            patients   = n_distinct(patient_id),
            total_cost = sum(total_cost, na.rm = TRUE),
            .groups = "drop")

# --- 7b. length of stay ---------------------------------------------------
agg_los <- adm %>%
  group_by(month, year, facility, region, service_line, payer_type, age_band) %>%
  summarise(admissions   = n(),
            los_mean     = mean(los_days, na.rm = TRUE),
            los_median   = median(los_days, na.rm = TRUE),
            los_total    = sum(los_days, na.rm = TRUE),
            expected_los = mean(expected_los, na.rm = TRUE),
            oe_ratio     = sum(los_days, na.rm = TRUE) / sum(expected_los, na.rm = TRUE),
            .groups = "drop")

# --- 7c. readmissions ---------------------------------------------------
agg_readmit <- adm %>%
  group_by(month, year, facility, region, service_line, payer_type, age_band) %>%
  summarise(index_admissions = sum(is_index),
            readmissions     = sum(readmit_30d),
            readmit_rate     = sum(readmit_30d) / pmax(sum(is_index), 1),
            .groups = "drop")

# by primary diagnosis (condition-level) -- for the clinical cohort view
agg_readmit_dx <- adm %>%
  filter(is_index) %>%
  group_by(service_line, dx_label) %>%
  summarise(index_admissions = n(),
            readmissions     = sum(readmit_30d),
            readmit_rate     = mean(readmit_30d),
            los_mean         = mean(los_days, na.rm = TRUE),
            oe_los           = mean(oe_los, na.rm = TRUE),
            mortality_30d    = mean(died_30d),
            complication_rate = mean(complication_flag),
            .groups = "drop") %>%
  filter(index_admissions >= 10) %>%
  arrange(desc(index_admissions))

# --- 7d. outcomes (mortality / complication proxies) ----------------------
agg_outcomes <- adm %>%
  group_by(month, year, facility, region, service_line, payer_type) %>%
  summarise(admissions        = n(),
            deaths_30d        = sum(died_30d),
            mortality_30d     = mean(died_30d),
            complications     = sum(complication_flag),
            complication_rate = mean(complication_flag),
            .groups = "drop")

# --- 7e. discharge disposition ------------------------------------------
agg_disposition <- adm %>%
  group_by(month, year, facility, region, service_line,
           discharge_disposition) %>%
  summarise(admissions = n(), .groups = "drop")

# --- 7f. day-of-week / seasonality -------------------------------------
agg_dow <- enc %>%
  group_by(facility, region, service_line, encounter_type, dow) %>%
  summarise(encounters = n(), .groups = "drop")

agg_admit_dow <- adm %>%
  mutate(discharge_dow = factor(wday(stop_date, label = TRUE, abbr = FALSE),
           levels = levels(enc$dow))) %>%
  group_by(facility, region, service_line) %>%
  summarise(
    admit_mon = sum(dow == "Monday"), admit_fri = sum(dow == "Friday"),
    weekend_admits = mean(dow %in% c("Saturday", "Sunday")),
    weekend_discharges = mean(discharge_dow %in% c("Saturday", "Sunday")),
    admissions = n(), .groups = "drop")

# --- 7g. occupancy / census (operational) ---------------------------------
# Approximate mid-month census: count admissions overlapping the 15th of a month.
census_days <- seq(ANALYSIS_START, ANALYSIS_END, by = "month") + 14
occ <- adm %>%
  select(facility, region, service_line, start_date, stop_date) %>%
  as.data.table()
agg_census <- rbindlist(lapply(census_days, function(d) {
  occ[start_date <= d & stop_date >= d,
      .(census = .N), by = .(facility, region, service_line)][
        , snapshot := as.Date(d)][]
}))

# --- 7h. top conditions driving admissions -----------------------------
agg_top_conditions <- adm %>%
  filter(is_index) %>%
  group_by(service_line, dx_label) %>%
  summarise(admissions = n(),
            los_mean = mean(los_days, na.rm = TRUE),
            readmit_rate = mean(readmit_30d),
            .groups = "drop") %>%
  arrange(desc(admissions)) %>%
  slice_head(n = 40)

# --- 7i. demographics -------------------------------------------------
agg_demographics <- enc %>%
  distinct(patient_id, .keep_all = TRUE) %>%
  group_by(age_band, gender, race, ethnicity, payer_type) %>%
  summarise(patients = n_distinct(patient_id), .groups = "drop")

# ------------------------------------------------------------------------------
# 8. ENCOUNTER-LEVEL DETAIL (for drill-down tables) -- trimmed columns only
# ------------------------------------------------------------------------------
detail_encounters <- enc %>%
  transmute(
    encounter_id, patient_id, start_date, stop_date, month, year,
    facility, region, service_line, encounter_type, payer_type,
    age_band, gender, dx_label, los_days,
    total_cost = round(total_cost, 0)
  )

detail_admissions <- adm %>%
  transmute(
    encounter_id, patient_id, admit_date = start_date, discharge_date = stop_date,
    month, year, facility, region, service_line, payer_type,
    age_band, gender, dx_label,
    los_days = round(los_days, 1),
    expected_los = round(expected_los, 1),
    oe_los = round(oe_los, 2),
    is_index, readmit_30d,
    days_to_readmit = round(days_to_readmit, 0),
    discharge_disposition, died_30d, complication_flag
  )

# ------------------------------------------------------------------------------
# 9. KPI HEADLINES + METADATA
# ------------------------------------------------------------------------------
kpi_overall <- list(
  n_patients          = n_distinct(enc$patient_id),
  n_encounters        = nrow(enc),
  n_admissions        = nrow(adm),
  readmit_rate_30d    = sum(adm$readmit_30d) / sum(adm$is_index),
  los_mean            = mean(adm$los_days, na.rm = TRUE),
  oe_los              = sum(adm$los_days, na.rm = TRUE) / sum(adm$expected_los, na.rm = TRUE),
  mortality_30d       = mean(adm$died_30d),
  complication_rate   = mean(adm$complication_flag)
)

meta <- list(
  generated_at   = Sys.time(),
  analysis_start = ANALYSIS_START,
  analysis_end   = ANALYSIS_END,
  readmit_window = READMIT_WINDOW,
  source         = "Synthea synthetic patient generator (Massachusetts, seed 20240101, ~2,000 living patients)",
  facilities     = sort(unique(enc$facility)),
  facility_beds  = setNames(facility_roster$beds, facility_roster$facility),
  regions        = sort(unique(enc$region)),
  service_lines  = sort(unique(enc$service_line)),
  payer_types    = sort(unique(enc$payer_type)),
  date_min       = min(enc$start_date),
  date_max       = max(enc$start_date)
)

# ------------------------------------------------------------------------------
# 10. WRITE OUTPUTS
# ------------------------------------------------------------------------------
app_data <- list(
  meta               = meta,
  kpi_overall        = kpi_overall,
  agg_volume         = agg_volume,
  agg_los            = agg_los,
  agg_readmit        = agg_readmit,
  agg_readmit_dx     = agg_readmit_dx,
  agg_outcomes       = agg_outcomes,
  agg_disposition    = agg_disposition,
  agg_dow            = agg_dow,
  agg_admit_dow      = agg_admit_dow,
  agg_census         = agg_census,
  agg_top_conditions = agg_top_conditions,
  agg_demographics   = agg_demographics,
  detail_encounters  = detail_encounters,
  detail_admissions  = detail_admissions
)

saveRDS(app_data, file.path(proc_dir, "app_data.rds"), compress = "xz")
msg("Wrote", file.path(proc_dir, "app_data.rds"),
    sprintf("(%.1f MB)", file.size(file.path(proc_dir, "app_data.rds")) / 1e6))

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(detail_encounters, file.path(proc_dir, "detail_encounters.parquet"))
  arrow::write_parquet(detail_admissions, file.path(proc_dir, "detail_admissions.parquet"))
  msg("Wrote parquet detail exports")
} else {
  saveRDS(detail_encounters, file.path(proc_dir, "detail_encounters.rds"))
  saveRDS(detail_admissions, file.path(proc_dir, "detail_admissions.rds"))
  msg("arrow not installed -- wrote .rds detail exports instead of parquet")
}

# quick data-quality report ---------------------------------------------------
dq <- tibble::tibble(
  check = c("encounters with facility", "encounters with service line",
            "admissions with LOS", "index admissions", "readmit rate in 3-12% band",
            "mortality rate < 15%"),
  pass  = c(
    all(!is.na(enc$facility)),
    all(!is.na(enc$service_line)),
    all(!is.na(adm$los_days)),
    sum(adm$is_index) > 0,
    dplyr::between(kpi_overall$readmit_rate_30d, 0.03, 0.20),
    kpi_overall$mortality_30d < 0.15
  )
)
print(dq)
saveRDS(dq, file.path(proc_dir, "dq_report.rds"))

msg(sprintf("ETL complete in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
