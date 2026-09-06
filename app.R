# ==============================================================================
# Clinical KPI & Quality Dashboard  --  Shiny app entry point
#
#   Run:  shiny::runApp()   (or Rscript -e "shiny::runApp(port = 8080)")
#   Data: data/processed/app_data.rds  (build with:  Rscript data_prep.R)
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(reactable)
library(shinyWidgets)

# ---- load helpers & modules -------------------------------------------------
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- load pre-aggregated data ---------------------------------------------
DATA_PATH <- "data/processed/app_data.rds"
if (!file.exists(DATA_PATH)) {
  stop("data/processed/app_data.rds not found. Run  Rscript data_prep.R  first.")
}
APP <- readRDS(DATA_PATH)
META <- APP$meta

def_start <- max(META$date_min, seq(META$date_max, length.out = 2, by = "-3 years")[2])

# ---- sidebar (shared filters) --------------------------------------------
app_sidebar <- sidebar(
  width = 300, class = "app-sidebar",
  div(class = "sidebar-section-title", "Filters"),
  dateRangeInput(
    "dates", "Date range",
    start = def_start, end = META$date_max,
    min = META$date_min, max = META$date_max,
    format = "M yyyy", startview = "year"
  ),
  pickerInput(
    "facilities", "Facility",
    choices = META$facilities, multiple = TRUE,
    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                            noneSelectedText = "All facilities",
                            selectedTextFormat = "count > 1")
  ),
  pickerInput(
    "service_lines", "Service line",
    choices = META$service_lines, multiple = TRUE,
    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                            noneSelectedText = "All service lines",
                            selectedTextFormat = "count > 2")
  ),
  pickerInput(
    "payers", "Payer type",
    choices = META$payer_types, multiple = TRUE,
    options = pickerOptions(actionsBox = TRUE,
                            noneSelectedText = "All payers",
                            selectedTextFormat = "count > 2")
  ),
  actionButton("reset", "Reset filters", class = "btn-sm btn-outline-secondary",
               icon = icon("rotate-left")),
  hr(),
  div(class = "sidebar-note",
      icon("circle-info"),
      HTML("Synthetic <b>Synthea</b> data - portfolio demo, not real patients. ",
           "See the <b>Methodology</b> tab.")),
  div(class = "sidebar-note text-muted",
      sprintf("%s patients | %s encounters | %s admissions",
              format(APP$kpi_overall$n_patients, big.mark = ","),
              format(APP$kpi_overall$n_encounters, big.mark = ","),
              format(APP$kpi_overall$n_admissions, big.mark = ",")))
)

# ---- UI --------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(class = "brand",
                    tags$span(class = "brand-mark", "+"),
                    "Clinical KPI & Quality Dashboard"),
  theme = app_theme(),
  fillable = FALSE,
  sidebar = app_sidebar,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com")
  ),
  id = "persona",

  nav_panel(
    title = "Operational Leader",
    value = "operational",
    div(class = "persona-intro",
        tags$b("Operational Leader view "),
        "- what is happening and where: encounter volume, length of stay, capacity and discharge flow."),
    operationalUI("op")
  ),
  nav_panel(
    title = "Clinical Leader",
    value = "clinical",
    div(class = "persona-intro",
        tags$b("Clinical Leader view "),
        "- why, and is care effective: 30-day readmissions, LOS vs expected, outcome proxies and condition cohorts."),
    clinicalUI("cl")
  ),
  nav_panel(
    title = "Methodology",
    value = "methodology",
    methodologyUI("mth", META)
  ),
  nav_spacer(),
  nav_item(tags$a(icon("github"), "Source",
                  href = "https://github.com/", target = "_blank", class = "nav-ext"))
)

# ---- server --------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start = def_start, end = META$date_max)
    updatePickerInput(session, "facilities", selected = character(0))
    updatePickerInput(session, "service_lines", selected = character(0))
    updatePickerInput(session, "payers", selected = character(0))
  })

  filters <- list(
    dates         = reactive(req(input$dates)),
    facilities    = reactive(input$facilities),
    service_lines = reactive(input$service_lines),
    payers        = reactive(input$payers)
  )

  operationalServer("op", APP, filters)
  clinicalServer("cl", APP, filters)
}

shinyApp(ui, server)
