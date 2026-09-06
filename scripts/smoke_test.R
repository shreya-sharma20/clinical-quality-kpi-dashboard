# Headless smoke test -- run from the project root:  Rscript scripts/smoke_test.R
# Exercises both persona modules with shiny::testServer and renders every output.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(plotly)
  library(reactable); library(shinyWidgets)
})

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
APP  <- readRDS("data/processed/app_data.rds")
META <- APP$meta

scenarios <- list(
  "default (all facilities, last ~2.5y)" = list(
    d = c(META$date_max - 900, META$date_max), f = NULL, s = NULL, p = NULL),
  "single facility + service line"       = list(
    d = c(META$date_min, META$date_max),
    f = META$facilities[1], s = META$service_lines[1:2], p = "Medicare"),
  "narrow window"                        = list(
    d = c(META$date_max - 120, META$date_max), f = NULL, s = NULL, p = NULL)
)

out_ids <- list(
  operational = c("kpi_encounters", "kpi_admissions", "kpi_los", "kpi_occupancy",
                  "vol_trend", "vol_mix", "los_trend", "occupancy", "dow",
                  "disposition", "adm_by_sl", "drill_title", "drill_tbl"),
  clinical = c("kpi_readmit", "kpi_oe", "kpi_mortality", "kpi_complication",
               "readmit_trend", "oe_by_sl", "readmit_by_sl", "cohort_title",
               "cohort_tbl", "cond_scatter", "outcomes_bar")
)

ok <- TRUE
for (sc_name in names(scenarios)) {
  sc <- scenarios[[sc_name]]
  filters <- list(
    dates = reactive(sc$d), facilities = reactive(sc$f),
    service_lines = reactive(sc$s), payers = reactive(sc$p)
  )
  cat("\n== scenario:", sc_name, "==\n")
  for (mod in c("operational", "clinical")) {
    srv <- get(paste0(mod, "Server"))
    testServer(srv, args = list(id = "t", data = APP, filters = filters), {
      for (o in out_ids[[mod]]) {
        res <- try(output[[o]], silent = TRUE)
        if (inherits(res, "try-error")) {
          cat(sprintf("  [FAIL] %s$%s : %s\n", mod, o,
                      conditionMessage(attr(res, "condition"))))
          ok <<- FALSE
        } else {
          cat(sprintf("  [ok]   %s$%s\n", mod, o))
        }
      }
    })
  }
}
# ---- drill-down interaction -------------------------------------------------
cat("\n== drill-down: simulated plotly_click ==\n")
filters <- list(
  dates = reactive(c(META$date_min, META$date_max)),
  facilities = reactive(NULL), service_lines = reactive(NULL), payers = reactive(NULL)
)
click_key <- ".clientValue-plotly_click-t-adm_sl"
testServer(operationalServer, args = list(id = "t", data = APP, filters = filters), {
  force(output$drill_tbl)                       # render with no selection
  args <- list(jsonlite::toJSON(
    data.frame(curveNumber = 0, pointNumber = 0, x = 100, y = 0,
               customdata = "Cardiology"), auto_unbox = TRUE))
  names(args) <- click_key
  do.call(session$setInputs, args)
  t1 <- try(output$drill_title, silent = TRUE)
  t2 <- try(output$drill_tbl,   silent = TRUE)
  if (inherits(t1, "try-error") || inherits(t2, "try-error")) {
    cat("  [FAIL] drill-down after click\n"); ok <<- FALSE
  } else {
    cat("  [ok]   drill title + table re-render after a bar click\n")
  }
})

cat(if (ok) "\nSMOKE TEST PASSED\n" else "\nSMOKE TEST FAILED\n")
if (!ok) quit(status = 1)
