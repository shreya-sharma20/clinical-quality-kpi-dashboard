# ------------------------------------------------------------------------------
# mappings.R
# Lookup tables and keyword rules used by the ETL to turn raw Synthea fields
# into analytics-friendly dimensions (service line, age band, payer type, etc.)
# ------------------------------------------------------------------------------

# Ordered list: the FIRST pattern that matches a diagnosis / reason description
# wins. Patterns are matched case-insensitively against the text.
service_line_rules <- tibble::tribble(
  ~service_line,          ~pattern,
  "Cardiology",           "heart failure|myocardial|cardiac|coronary|atrial fibrillation|angina|hypertensive heart",
  "Pulmonary",            "pneumonia|copd|chronic obstructive|asthma|respiratory failure|pulmonary|bronchitis|emphysema",
  "Infectious Disease",   "sepsis|septic|bacteremia|cellulitis|infection|covid|influenza|abscess",
  "Neurology",            "stroke|cerebral|seizure|epilep|transient ischemic|concussion|migraine|parkinson|alzheimer|dementia",
  "Orthopedics",          "fracture|osteoarthritis|joint|hip replacement|knee replacement|spinal|back pain|dislocation|tendon|rupture of",
  "Gastroenterology",     "appendicitis|cholecystitis|pancreatitis|gastroenteritis|bowel|gastrointestinal|hepatitis|cirrhosis|diverticulitis|ulcer",
  "Oncology",             "cancer|carcinoma|malignant|neoplasm|leukemia|lymphoma|tumor",
  "Renal",                "kidney|renal failure|renal disease|nephr|dialysis|urinary tract",
  "Obstetrics",           "pregnan|labor|delivery|childbirth|prenatal|miscarriage|postpartum|preeclampsia|fetal",
  "Behavioral Health",    "depress|anxiety|bipolar|schizophren|substance|alcohol|opioid|overdose|suicidal|psychiatric|major depressive|drug abuse|drug dependence|misuse of drugs|addiction",
  "Endocrinology",        "diabet|hyperglycemia|hypoglycemia|thyroid|ketoacidosis",
  "Trauma / Injury",      "injury|trauma|laceration|burn|wound|poisoning|contusion|foreign body|fall",
  "General Surgery",      "hernia|appendectomy|surgical|postoperative|post-operative|sterilization|cholecystectomy",
)

default_service_line <- "General Medicine"

# ------------------------------------------------------------------------------
# Synthetic acute-care facility roster.
# Synthea assigns encounters to hundreds of provider organisations (many of them
# outpatient clinics), which makes a facility filter unusable and the trends
# noisy. For this portfolio demo we deterministically assign each PATIENT to one
# "home" hospital via a hash of their id, so a patient's admissions stay at one
# facility and monthly volumes are stable and interpretable.
facility_roster <- tibble::tribble(
  ~facility,                       ~region,   ~weight, ~beds,
  "Bay State General Hospital",     "Metro",    0.26,   420,
  "Harbor View Medical Center",     "Metro",    0.21,   340,
  "Riverside Regional Hospital",    "North",    0.18,   260,
  "Summit Community Hospital",      "West",     0.15,   180,
  "Lakeside Memorial Hospital",     "South",    0.12,   210,
  "Pinecrest University Hospital",  "Metro",    0.08,   510
)

#' Deterministically map ids -> a facility from the roster (weighted).
assign_facility <- function(ids) {
  h <- vapply(as.character(ids), function(x) {
    r <- strtoi(substr(rlang::hash(x), 1, 6), base = 16L)
    r / (16^6)
  }, numeric(1))
  cw <- cumsum(facility_roster$weight) / sum(facility_roster$weight)
  idx <- findInterval(h, cw, left.open = TRUE) + 1L
  idx <- pmin(idx, nrow(facility_roster))
  facility_roster$facility[idx]
}

#' Assign a service line from a free-text clinical description
#' @param x character vector of diagnosis / reason descriptions
assign_service_line <- function(x) {
  x_l <- tolower(ifelse(is.na(x), "", x))
  out <- rep(default_service_line, length(x_l))
  assigned <- rep(FALSE, length(x_l))
  for (i in seq_len(nrow(service_line_rules))) {
    hit <- !assigned & grepl(service_line_rules$pattern[i], x_l, perl = TRUE)
    out[hit] <- service_line_rules$service_line[i]
    assigned[hit] <- TRUE
  }
  out
}

#' Collapse Synthea payer names into payer categories
map_payer_type <- function(payer_name) {
  pn <- tolower(ifelse(is.na(payer_name), "", payer_name))
  dplyr::case_when(
    grepl("medicare", pn)                      ~ "Medicare",
    grepl("medicaid", pn)                      ~ "Medicaid",
    grepl("dual", pn)                          ~ "Dual Eligible",
    grepl("no insurance|self", pn) | pn == ""  ~ "Self-Pay / Uninsured",
    grepl("va |veteran|tricare|military", pn)  ~ "Government (Other)",
    TRUE                                       ~ "Commercial"
  )
}

#' Age -> age band
age_band <- function(age) {
  cut(
    age,
    breaks = c(-Inf, 0.999, 17, 34, 49, 64, 74, 84, Inf),
    labels = c("<1", "1-17", "18-34", "35-49", "50-64", "65-74", "75-84", "85+"),
    right = TRUE
  )
}

# Keyword rules for a lightweight "hospital-acquired complication" proxy.
complication_pattern <- paste(
  "sepsis|septic|clostridium|c. difficile|pressure ulcer|pressure injury",
  "catheter-associated|surgical site infection|postoperative infection",
  "pulmonary embolism|deep vein thrombosis|hospital acquired|healthcare associated",
  "acute respiratory failure|acute kidney injury|aspiration pneumonia",
  sep = "|"
)
