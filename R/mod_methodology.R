# ------------------------------------------------------------------------------
# mod_methodology.R -- static "Methodology & Data" tab
# ------------------------------------------------------------------------------

methodologyUI <- function(id, meta) {
  ns <- NS(id)
  card(
    card_body(
      class = "methodology",
      div(
        class = "alert alert-warning",
        strong("Synthetic data notice. "),
        "This dashboard is a portfolio demonstration built entirely on ",
        tags$b("synthetic data"), " generated with the ",
        tags$a(href = "https://github.com/synthetichealth/synthea", target = "_blank",
               "Synthea"), " open-source patient simulator. ",
        "No real patients, providers, or facilities are represented. ",
        "Facility names, regions and bed counts are fictional and were assigned by the ETL. ",
        "Nothing here should be used for clinical or operational decision-making."
      ),
      h4("Data source"),
      tags$ul(
        tags$li(sprintf("Generator: %s", meta$source)),
        tags$li(sprintf("Raw tables used: patients, encounters, conditions, organizations, payers")),
        tags$li(sprintf("Analysis window: %s to %s (pinned for a stable demo)",
                        meta$analysis_start, meta$analysis_end)),
        tags$li(sprintf("Pipeline: data_prep.R reads the raw Synthea CSVs and writes aggregated marts to data/processed/app_data.rds; the app never touches patient-level raw files."))
      ),

      h4("Dimensions"),
      tags$dl(
        tags$dt("Facility"),
        tags$dd("Each patient is deterministically assigned a single 'home' hospital (hash of patient id, weighted). This replaces Synthea's hundreds of provider organisations so filters and trends are usable. Six fictional hospitals across four regions."),
        tags$dt("Service line"),
        tags$dd("Derived by keyword-matching the encounter reason / primary condition description to 13 clinical service lines (Cardiology, Pulmonary, Oncology, ...). Unmatched encounters fall back to 'General Medicine'. Rules live in R/mappings.R."),
        tags$dt("Payer type"),
        tags$dd("Synthea payer names collapsed to Medicare / Medicaid / Dual Eligible / Commercial / Self-Pay / Government (Other)."),
        tags$dt("Age band"),
        tags$dd("Age at encounter, bucketed <1 / 1-17 / 18-34 / 35-49 / 50-64 / 65-74 / 75-84 / 85+.")
      ),

      h4("KPI definitions"),
      tags$dl(
        tags$dt("Encounter volume"),
        tags$dd("Count of encounters with start date in range, by encounter type (Ambulatory, Wellness, Outpatient, Urgent Care, Emergency, Inpatient, Home Health, Virtual, Hospice, Skilled Nursing)."),

        tags$dt("Acute admission"),
        tags$dd("An encounter with class = inpatient. Length of stay (LOS) = discharge timestamp - admission timestamp, in days (floored at ~1 hour)."),

        tags$dt("Average LOS"),
        tags$dd("Total patient-days / number of admissions for the selected filter and period. Trend shown quarterly."),

        tags$dt("Expected LOS and O/E ratio"),
        tags$dd("Expected LOS = mean Winsorised (98th percentile cap) LOS for the same service line x age band across the full analysis window - a simple benchmark, not a validated risk model. O/E ratio = observed patient-days / expected patient-days. Above 1.0 means longer stays than the benchmark."),

        tags$dt("30-day all-cause readmission rate"),
        tags$dd(tags$ul(
          tags$li("Denominator (index admissions): live inpatient discharges that were not discharged to hospice."),
          tags$li("Numerator: index admissions where the same patient has another inpatient admission starting 0-30 days after the index discharge (any cause)."),
          tags$li("Rate = readmissions / index admissions. Reported overall, by service line, by facility, by condition and as a quarterly trend."),
          tags$li("Simplifications vs CMS methodology: no planned-readmission exclusions, no 3-day post-acute grouping, no clinical risk adjustment.")
        )),

        tags$dt("30-day mortality proxy"),
        tags$dd("Share of admissions where the patient's death date falls between admission and 30 days after discharge. A crude proxy - Synthea deaths are model-generated."),

        tags$dt("Complication proxy"),
        tags$dd("Share of admissions with a condition matching a hospital-acquired-complication keyword list (sepsis, C. difficile, pressure injury, catheter-associated infection, DVT/PE, acute kidney injury, ...) recorded during the stay or within 14 days of discharge. Not a validated PSI measure."),

        tags$dt("Discharge disposition"),
        tags$dd("Synthesised from downstream events: Expired (death in stay), Hospice, Skilled Nursing Facility, Home with Home Health, or Home / Self-Care - based on the next encounter within ~3 days of discharge."),

        tags$dt("Bed occupancy"),
        tags$dd("Mid-month census (admissions overlapping the 15th) divided by the facility's fictional staffed-bed count, averaged over the selected period."),

        tags$dt("Trend indicator on KPI tiles"),
        tags$dd("The selected period is split in half; the tile compares the recent half to the earlier half. Arrow colour reflects whether the movement is favourable for that metric (e.g. a falling readmission rate is green).")
      ),

      h4("Architecture"),
      tags$pre(
"data/raw/*.csv  (Synthea export)
      |
      v
 data_prep.R  --  data.table / dplyr ETL
      |            - build patient / facility / payer dimensions
      |            - derive service line, LOS, readmissions, outcome proxies
      |            - aggregate to monthly / quarterly marts
      v
data/processed/app_data.rds   (+ parquet detail exports)
      |
      v
 app.R  --  bslib + plotly + reactable Shiny app
            Operational Leader view  |  Clinical Leader view  |  Methodology"
      ),

      h4("Reproducing"),
      tags$ol(
        tags$li("Install dependencies: Rscript install.R"),
        tags$li("(Optional) regenerate Synthea data: see scripts/generate_synthea.sh"),
        tags$li("Run the ETL: Rscript data_prep.R"),
        tags$li("Launch the app: Rscript -e \"shiny::runApp(port = 8080)\"")
      ),
      p(class = "text-muted",
        sprintf("Marts generated %s.", format(meta$generated_at, "%Y-%m-%d %H:%M")))
    )
  )
}
