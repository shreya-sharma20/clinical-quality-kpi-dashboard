# ------------------------------------------------------------------------------
# mod_clinical.R -- "Clinical Leader" persona view
# Focus: quality & outcomes -- 30-day readmissions, LOS vs expected,
# mortality / complication proxies, condition-level cohorts.
# ------------------------------------------------------------------------------

clinicalUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      uiOutput(ns("kpi_readmit")),
      uiOutput(ns("kpi_oe")),
      uiOutput(ns("kpi_mortality")),
      uiOutput(ns("kpi_complication"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("30-day all-cause readmission rate",
                    span(class = "card-hint", "Quarterly, with portfolio benchmark")),
        plotlyOutput(ns("readmit_trend"), height = 300)
      ),
      card(
        card_header("Observed / expected LOS by service line",
                    span(class = "card-hint", "> 1.0 = longer than expected")),
        plotlyOutput(ns("oe_by_sl"), height = 300)
      )
    ),
    card(
      card_header(
        span("Readmission rate by service line"),
        span(class = "card-hint", "Click a bar to load the condition-level cohort below")
      ),
      layout_columns(
        col_widths = c(5, 7),
        plotlyOutput(ns("readmit_by_sl"), height = 340),
        div(
          uiOutput(ns("cohort_title")),
          reactable::reactableOutput(ns("cohort_tbl"))
        )
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Condition cohorts: volume vs readmission risk",
                    span(class = "card-hint", "Bubble size = average LOS")),
        plotlyOutput(ns("cond_scatter"), height = 330)
      ),
      card(
        card_header("Outcome proxies by service line"),
        plotlyOutput(ns("outcomes_bar"), height = 330)
      )
    )
  )
}

clinicalServer <- function(id, data, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rd <- reactive({
      apply_filters(data$agg_readmit, filters$dates(), filters$facilities(),
                    filters$service_lines(), filters$payers())
    })
    oc <- reactive({
      apply_filters(data$agg_outcomes, filters$dates(), filters$facilities(),
                    filters$service_lines(), filters$payers())
    })
    los <- reactive({
      apply_filters(data$agg_los, filters$dates(), filters$facilities(),
                    filters$service_lines(), filters$payers())
    })
    adm_detail <- reactive({
      d <- data$detail_admissions
      d <- d[d$admit_date >= as.Date(filters$dates()[1]) &
             d$admit_date <= as.Date(filters$dates()[2]), ]
      if (length(filters$facilities()))    d <- d[d$facility %in% filters$facilities(), ]
      if (length(filters$service_lines())) d <- d[d$service_line %in% filters$service_lines(), ]
      if (length(filters$payers()))        d <- d[d$payer_type %in% filters$payers(), ]
      d
    })

    BENCH <- 0.134  # portfolio benchmark line (overall pooled rate)

    # ---- KPI tiles -------------------------------------------------------
    output$kpi_readmit <- renderUI({
      d <- rd()
      val <- if (nrow(d)) sum(d$readmissions) / sum(d$index_admissions) else NA
      tr <- period_trend(d, function(x) sum(x$readmissions) / sum(x$index_admissions),
                         lower_is_better = TRUE)
      kpi_box("30-day readmission", val, tr, fmt = function(x) pct(x, 1),
              lower_is_better = TRUE)
    })
    output$kpi_oe <- renderUI({
      d <- los()
      val <- if (nrow(d)) sum(d$los_total) / sum(d$expected_los * d$admissions) else NA
      tr <- period_trend(d, function(x) sum(x$los_total) / sum(x$expected_los * x$admissions),
                         lower_is_better = TRUE)
      kpi_box("O/E length of stay", val, tr, fmt = function(x) dec(x, 2),
              lower_is_better = TRUE)
    })
    output$kpi_mortality <- renderUI({
      d <- oc()
      val <- if (nrow(d)) sum(d$deaths_30d) / sum(d$admissions) else NA
      tr <- period_trend(d, function(x) sum(x$deaths_30d) / sum(x$admissions),
                         lower_is_better = TRUE)
      kpi_box("30-day mortality proxy", val, tr, fmt = function(x) pct(x, 1),
              lower_is_better = TRUE)
    })
    output$kpi_complication <- renderUI({
      d <- oc()
      val <- if (nrow(d)) sum(d$complications) / sum(d$admissions) else NA
      tr <- period_trend(d, function(x) sum(x$complications) / sum(x$admissions),
                         lower_is_better = TRUE)
      kpi_box("Complication proxy", val, tr, fmt = function(x) pct(x, 1),
              lower_is_better = TRUE)
    })

    # ---- readmission trend --------------------------------------------
    output$readmit_trend <- renderPlotly({
      d <- rd()
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>%
        mutate(qtr = lubridate::floor_date(month, "quarter")) %>%
        group_by(qtr) %>%
        summarise(rate = sum(readmissions) / sum(index_admissions),
                  n = sum(index_admissions), .groups = "drop") %>%
        mutate(bench = BENCH)
      plot_ly(d, x = ~qtr) %>%
        add_bars(y = ~n, name = "Index admissions", yaxis = "y2",
                 marker = list(color = PAL$grid), hoverinfo = "skip") %>%
        add_lines(y = ~bench, name = "Benchmark",
                  line = list(color = PAL$amber, width = 2, dash = "dash"),
                  hoverinfo = "skip") %>%
        add_lines(y = ~rate, name = "Readmission rate",
                  line = list(color = PAL$navy, width = 3),
                  hovertemplate = "%{y:.1%}<extra>Readmit rate</extra>") %>%
        plotly_layout() %>%
        layout(yaxis = list(title = "Rate", tickformat = ".0%", rangemode = "tozero"),
               yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
                             title = "Index admits"),
               legend = list(orientation = "h", y = -0.2)) %>%
        no_plotly_bar()
    })

    # ---- O/E by service line ----------------------------------------
    output$oe_by_sl <- renderPlotly({
      d <- los()
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% group_by(service_line) %>%
        summarise(oe = sum(los_total) / sum(expected_los * admissions),
                  n = sum(admissions), .groups = "drop") %>%
        filter(n >= 5) %>% arrange(oe)
      d$col <- ifelse(d$oe > 1.05, PAL$red, ifelse(d$oe < 0.95, PAL$green, PAL$slate))
      plot_ly(d, y = ~reorder(service_line, oe), x = ~oe - 1, type = "bar",
              orientation = "h", marker = list(color = ~col),
              hovertemplate = "O/E %{customdata:.2f}<extra>%{y}</extra>",
              customdata = ~oe) %>%
        plotly_layout() %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "O/E - 1.0", tickformat = "+.0%",
                            zeroline = TRUE, zerolinecolor = PAL$slate)) %>%
        no_plotly_bar()
    })

    # ---- drill-down: readmission by service line -------------------
    output$readmit_by_sl <- renderPlotly({
      d <- adm_detail() %>% filter(is_index)
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% group_by(service_line) %>%
        summarise(rate = mean(readmit_30d), n = n(), .groups = "drop") %>%
        filter(n >= 5) %>% arrange(rate)
      plot_ly(d, y = ~reorder(service_line, rate), x = ~rate, type = "bar",
              orientation = "h", marker = list(color = PAL$navy),
              source = ns("rd_sl"), customdata = ~service_line,
              hovertemplate = "%{x:.1%} (n=%{text})<extra>%{y}</extra>",
              text = ~n) %>%
        plotly_layout() %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "30-day readmission rate", tickformat = ".0%")) %>%
        no_plotly_bar()
    })

    sel_sl <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", source = ns("rd_sl")), {
      cd <- event_data("plotly_click", source = ns("rd_sl"))$customdata
      sel_sl(if (identical(cd, sel_sl())) NULL else cd)
    })
    observeEvent(list(filters$dates(), filters$facilities(),
                      filters$service_lines(), filters$payers()), sel_sl(NULL))
    observeEvent(input$clear_sel, sel_sl(NULL))

    output$cohort_title <- renderUI({
      if (is.null(sel_sl()))
        span(class = "card-hint", "Showing all condition cohorts - click a bar to filter")
      else
        span(strong(sel_sl()), " cohorts - ",
             tags$a(href = "#",
                    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("clear_sel")),
                    "clear"))
    })

    output$cohort_tbl <- reactable::renderReactable({
      d <- adm_detail() %>% filter(is_index)
      if (!is.null(sel_sl())) d <- d[d$service_line == sel_sl(), ]
      validate(need(nrow(d) > 0, "No admissions"))
      tbl <- d %>%
        group_by(`Service line` = service_line, `Primary reason` = dx_label) %>%
        summarise(`Index admits` = n(),
                  `Readmit rate` = mean(readmit_30d),
                  `Avg LOS` = mean(los_days, na.rm = TRUE),
                  `O/E LOS` = mean(oe_los, na.rm = TRUE),
                  `Mortality` = mean(died_30d),
                  `Complication` = mean(complication_flag),
                  .groups = "drop") %>%
        filter(`Index admits` >= 3) %>%
        arrange(desc(`Index admits`))
      reactable::reactable(
        tbl, compact = TRUE, striped = TRUE, highlight = TRUE,
        defaultPageSize = 8, searchable = TRUE, theme = reactable_theme(),
        columns = list(
          `Readmit rate` = reactable::colDef(
            format = reactable::colFormat(percent = TRUE, digits = 1), maxWidth = 110,
            style = function(v) if (!is.na(v) && v > 0.15) list(color = PAL$red, fontWeight = 600) else NULL),
          `Avg LOS` = reactable::colDef(format = reactable::colFormat(digits = 1), maxWidth = 90),
          `O/E LOS` = reactable::colDef(format = reactable::colFormat(digits = 2), maxWidth = 90),
          `Mortality` = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1), maxWidth = 100),
          `Complication` = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1), maxWidth = 110)
        )
      )
    })

    # ---- condition scatter -----------------------------------------
    output$cond_scatter <- renderPlotly({
      d <- adm_detail() %>% filter(is_index)
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% group_by(service_line, dx_label) %>%
        summarise(n = n(), rate = mean(readmit_30d),
                  los = mean(los_days, na.rm = TRUE), .groups = "drop") %>%
        filter(n >= 8)
      validate(need(nrow(d) > 0, "Not enough cohort volume at this filter level"))
      d$msize <- scales::rescale(sqrt(pmax(d$los, 0.1)), to = c(10, 34))
      plot_ly(d, x = ~n, y = ~rate, color = ~service_line, colors = PAL_SEQ,
              type = "scatter", mode = "markers",
              marker = list(opacity = 0.65, size = ~msize),
              hovertemplate = paste0("<b>%{text}</b><br>%{x} admits<br>",
                                     "%{y:.1%} readmit<br>%{customdata:.1f}d LOS<extra></extra>"),
              text = ~dx_label, customdata = ~los) %>%
        plotly_layout() %>%
        layout(xaxis = list(title = "Index admissions"),
               yaxis = list(title = "30-day readmission rate", tickformat = ".0%")) %>%
        no_plotly_bar()
    })

    # ---- outcomes bar --------------------------------------------
    output$outcomes_bar <- renderPlotly({
      d <- oc()
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% group_by(service_line) %>%
        summarise(Mortality = sum(deaths_30d) / sum(admissions),
                  Complication = sum(complications) / sum(admissions),
                  n = sum(admissions), .groups = "drop") %>%
        filter(n >= 5) %>%
        tidyr::pivot_longer(c(Mortality, Complication), names_to = "metric", values_to = "rate")
      plot_ly(d, x = ~rate, y = ~service_line, color = ~metric,
              colors = c(Mortality = PAL$red, Complication = PAL$amber),
              type = "bar", orientation = "h",
              hovertemplate = "%{x:.1%} %{fullData.name}<extra>%{y}</extra>") %>%
        plotly_layout() %>%
        layout(barmode = "group", yaxis = list(title = ""),
               xaxis = list(title = "Rate", tickformat = ".0%")) %>%
        no_plotly_bar()
    })
  })
}
