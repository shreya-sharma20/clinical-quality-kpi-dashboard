# Clinical KPI & Quality Dashboard

An interactive R Shiny dashboard that turns raw synthetic EHR data into the
operational and clinical-quality KPIs a hospital leadership team actually looks
at: **encounter volume, length of stay, 30-day readmissions, and outcome
proxies** — sliced by facility, service line, payer and time.

> **Synthetic data only.** Every patient, encounter, provider and facility in
> this project is fictional, generated with [Synthea](https://github.com/synthetichealth/synthea).
> This is a portfolio demonstration and must not be used for clinical or
> operational decision-making. See the in-app **Methodology** tab for full
> KPI definitions and caveats.

---

## Highlights

- **Two persona views** toggled from the top navigation:
  - **Operational Leader** — throughput and capacity: encounter volume by month
    and type, LOS trend vs expected, mid-month bed occupancy, arrivals by day of
    week, discharge-disposition mix.
  - **Clinical Leader** — quality and outcomes: 30-day all-cause readmission
    rate (trend + benchmark), observed/expected LOS by service line, mortality
    and complication proxies, condition-cohort bubble chart.
- **Shared filters** (date range, facility, service line, payer) drive every
  chart and tile on both views.
- **KPI tiles with trend indicators** — each tile compares the recent half of
  the selected period to the earlier half and colours the delta by whether the
  movement is favourable for that metric, with an inline sparkline.
- **Click-through drill-downs** — clicking a service-line bar filters a detail
  table beneath it (encounter-level admissions on the operational view,
  condition-level cohorts on the clinical view).
- **Fast load** — the app reads a single pre-aggregated `.rds`; it never touches
  patient-level raw files at runtime.
- Modern **bslib** theme, custom CSS, accessible palette, `plotly` charts,
  `reactable` tables.

## Screenshots

_Add images to `docs/img/` and link them here._

| Operational Leader | Clinical Leader | Methodology |
|---|---|---|
| `docs/img/operational.png` _(placeholder)_ | `docs/img/clinical.png` _(placeholder)_ | `docs/img/methodology.png` _(placeholder)_ |

---

## Architecture

```
data/raw/*.csv.gz            Synthea CSV export (patients, encounters,
      |                      conditions, organizations, payers) — gzipped
      v
 data_prep.R                 ETL (data.table + dplyr)
      |                      - patient / facility / payer / date dimensions
      |                      - service line via keyword rules (R/mappings.R)
      |                      - LOS, expected LOS (O/E), 30-day readmissions
      |                      - mortality / complication / disposition proxies
      |                      - aggregate to monthly & quarterly marts
      v
data/processed/app_data.rds  one named list of ~13 aggregated marts +
      |                      trimmed encounter/admission detail tables
      |                      (also written as .parquet)
      v
 app.R  +  R/                bslib + plotly + reactable Shiny app
   R/theme.R                 bs_theme, palette, plotly layout helpers
   R/helpers.R               formatting, KPI trend logic, sparkline, filters
   R/mappings.R              service-line / payer / age-band / facility rules
   R/mod_operational.R       Operational Leader module (UI + server)
   R/mod_clinical.R          Clinical Leader module (UI + server)
   R/mod_methodology.R       Methodology tab
```

## Project layout

```
clinical_KPI_dashboard/
├── app.R                     # Shiny entry point
├── data_prep.R               # re-runnable ETL pipeline
├── install.R                 # dependency installer
├── DESCRIPTION               # dependency manifest
├── R/                        # helpers + Shiny modules
├── data/
│   ├── raw/                  # gzipped Synthea sample (committed, ~7 MB)
│   └── processed/            # app_data.rds, *.parquet, dq_report.rds
├── scripts/
│   ├── generate_synthea.sh   # optional: regenerate raw data (needs Java 17+)
│   └── smoke_test.R          # headless test: renders every output in both views
└── www/styles.css            # custom styling on top of the bslib theme
```

---

## Setup & run

### 1. Install R package dependencies

```bash
Rscript install.R
```

Core packages: `shiny`, `bslib`, `plotly`, `reactable`, `shinyWidgets`,
`data.table`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `arrow`, `R.utils`.
Requires R >= 4.1.

### 2. Build the analytics marts

The repo already ships `data/raw/*.csv.gz` and a pre-built
`data/processed/app_data.rds`, so you can skip straight to step 3. To rebuild:

```bash
Rscript data_prep.R
```

Runs in ~10 seconds and prints a small data-quality report.

### 3. Launch the app

```bash
Rscript -e "shiny::runApp('.', port = 8080, launch.browser = TRUE)"
```

or from an R session:

```r
shiny::runApp()
```

### 4. (Optional) Regenerate the synthetic population

Needs **Java 17+** (Synthea master builds target class-file version 61).

```bash
bash scripts/generate_synthea.sh 2000 Massachusetts 20240101
#                                 ^pop  ^state        ^seed
Rscript data_prep.R
```

---

## Data dictionary

### Dimensions

| Field | Values | Notes |
|---|---|---|
| `facility` | 6 fictional hospitals | Each patient is deterministically assigned one "home" hospital (hash of patient id, weighted). Replaces Synthea's hundreds of provider orgs. |
| `region` | Metro / North / South / West | Attached to each facility. |
| `service_line` | 13 lines (Cardiology, Pulmonary, Oncology, Behavioral Health, …) | Keyword match on the encounter reason / primary condition; unmatched → `General Medicine`. Rules in `R/mappings.R`. |
| `payer_type` | Medicare / Medicaid / Dual Eligible / Commercial / Self-Pay / Government (Other) | Collapsed from Synthea payer names. |
| `age_band` | `<1`, `1-17`, `18-34`, `35-49`, `50-64`, `65-74`, `75-84`, `85+` | Age at encounter. |
| `encounter_type` | Ambulatory, Wellness, Outpatient, Urgent Care, Emergency, Inpatient, Home Health, Virtual, Hospice, Skilled Nursing | From Synthea `ENCOUNTERCLASS`. |

### KPIs

| KPI | Definition |
|---|---|
| **Encounter volume** | Count of encounters with start date in range, by type. |
| **Acute admission** | Encounter with class `inpatient`. |
| **Length of stay (LOS)** | Discharge − admission timestamp, in days. |
| **Average LOS** | Total patient-days ÷ admissions for the filter/period. |
| **Expected LOS / O/E ratio** | Expected = mean Winsorised LOS for the same *service line × age band* over the full window. O/E = observed patient-days ÷ expected patient-days. A simple benchmark, **not** a validated risk model. |
| **30-day all-cause readmission rate** | Denominator = live inpatient discharges not sent to hospice (index admissions). Numerator = index admissions with another inpatient admission 0–30 days after discharge, any cause. Rate = numerator ÷ denominator. No planned-readmission exclusions or clinical risk adjustment (differs from CMS). |
| **30-day mortality proxy** | Share of admissions where the patient's (model-generated) death date is between admission and 30 days post-discharge. |
| **Complication proxy** | Share of admissions with a condition matching a hospital-acquired-complication keyword list (sepsis, C. difficile, pressure injury, catheter-associated infection, DVT/PE, AKI, …) during the stay or within 14 days. Not a validated PSI. |
| **Discharge disposition** | Synthesised from the next encounter within ~3 days of discharge: Expired / Hospice / Skilled Nursing Facility / Home with Home Health / Home – Self-Care. |
| **Bed occupancy** | Mid-month census (admissions overlapping the 15th) ÷ the facility's fictional staffed-bed count, averaged over the period. |
| **Tile trend indicator** | Selected period split in half; recent half vs earlier half; colour = favourable/unfavourable for that metric. |

---

## Testing

```bash
Rscript scripts/smoke_test.R
```

Uses `shiny::testServer` to render every output in both persona modules across
three filter scenarios (all data, a single facility + service line, and a narrow
recent window) and fails if any output errors.

## Trade-offs & decisions

- **Synthetic facility roster.** Synthea assigns encounters to ~790 provider
  organisations, most of them tiny outpatient clinics, which makes a facility
  filter meaningless. Patients are re-assigned to 6 fictional hospitals so
  volumes and trends are interpretable. Documented in-app.
- **Keyword-based service lines and complication flags** instead of a full
  SNOMED value-set hierarchy — transparent, easy to audit, good enough for a
  demo; the rules live in one file.
- **Simple expected-LOS benchmark** (service line × age band mean) rather than a
  regression model — keeps the O/E metric explainable.
- **Quarterly** trend rollups for readmissions and LOS (monthly for volume)
  because the synthetic acute population is ~1,000 admissions over 11 years and
  monthly rates would be too noisy.
- **Pre-aggregated `.rds`** as the app's data source so first paint is instant
  and no patient-level data is shipped to the browser.

## License

MIT — see `LICENSE`. Synthea is licensed separately (Apache 2.0).
