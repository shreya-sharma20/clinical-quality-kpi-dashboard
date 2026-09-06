# ------------------------------------------------------------------------------
# mod_operational.R -- "Operational Leader" persona view
# Focus: throughput, capacity, volume, LOS trend, discharge patterns.
# ------------------------------------------------------------------------------

operationalUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      uiOutput(ns("kpi_encounters")),
      uiOutput(ns("kpi_admissions")),
      uiOutput(ns("kpi_los")),
      uiOutput(ns("kpi_occupancy"))
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Encounter volume by month",
                    span(class = "card-hint", "Stacked by encounter type")),
        plotlyOutput(ns("vol_trend"), height = 300)
      ),
      card(
        card_header("Volume mix by encounter type"),
        plotlyOutput(ns("vol_mix"), height = 300)
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Average length of stay",
                    span(class = "card-hint", "Quarterly, acute inpatient")),
        plotlyOutput(ns("los_trend"), height = 290)
      ),
      card(
        card_header("Mid-month inpatient census vs staffed beds"),
        plotlyOutput(ns("occupancy"), height = 290)
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Arrivals by day of week"),
        plotlyOutput(ns("dow"), height = 290)
      ),
      card(
        card_header("Discharge disposition mix"),
        plotlyOutput(ns("disposition"), height = 290)
      )
    ),
    card(
      card_header(
        span("Admissions by service line"),
        span(class = "card-hint", "Click a bar to drill into the encounter list below")
      ),
      layout_columns(
        col_widths = c(5, 7),
        plotlyOutput(ns("adm_by_sl"), height = 320),
        div(
          uiOutput(ns("drill_title")),
          reactable::reactableOutput(ns("drill_tbl"))
        )
      )
    )
  )
}

operationalServer <- function(id, data, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vol <- reactive({
      apply_filters(data$agg_volume, filters$dates(), filters$facilities(),
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
      if (length(filters$facilities()))     d <- d[d$facility %in% filters$facilities(), ]
      if (length(filters$service_lines()))  d <- d[d$service_line %in% filters$service_lines(), ]
      if (length(filters$payers()))         d <- d[d$payer_type %in% filters$payers(), ]
      d
    })

    # ---- KPI tiles ----------------------------------------------------------
    output$kpi_encounters <- renderUI({
      v <- vol(); tot <- sum(v$encounters)
      tr <- period_trend(v, function(d) sum(d$encounters))
      kpi_box("Total encounters", tot, tr, fmt = num)
    })
    output$kpi_admissions <- renderUI({
      d <- adm_detail(); tot <- nrow(d)
      tr <- period_trend(d %>% mutate(month = month), function(x) nrow(x))
      kpi_box("Acute admissions", tot, tr, fmt = num)
    })
    output$kpi_los <- renderUI({
      l <- los()
      val <- if (nrow(l)) sum(l$los_total) / sum(l$admissions) else NA
      tr <- period_trend(l, function(d) sum(d$los_total) / sum(d$admissions),
                         lower_is_better = TRUE)
      kpi_box("Average LOS (days)", val, tr, fmt = function(x) dec(x, 1),
              lower_is_better = TRUE)
    })
    output$kpi_occupancy <- renderUI({
      cen <- data$agg_census
      cen <- cen[cen$snapshot >= as.Date(filters$dates()[1]) &
                 cen$snapshot <= as.Date(filters$dates()[2]), ]
      if (length(filters$facilities()))    cen <- cen[cen$facility %in% filters$facilities(), ]
      if (length(filters$service_lines())) cen <- cen[cen$service_line %in% filters$service_lines(), ]
      beds <- data$meta$facility_beds
      fac  <- if (length(filters$facilities())) filters$facilities() else names(beds)
      bed_total <- sum(beds[fac])
      by_snap <- cen %>% group_by(snapshot) %>% summarise(c = sum(census), .groups = "drop")
      val <- if (nrow(by_snap)) mean(by_snap$c) / bed_total else NA
      tr <- period_trend(
        by_snap %>% mutate(month = snapshot),
        function(d) mean(d$c) / bed_total, lower_is_better = FALSE
      )
      kpi_box("Avg bed occupancy", val, tr, fmt = function(x) pct(x, 0))
    })

    # ---- volume trend -----------------------------------------------------
    output$vol_trend <- renderPlotly({
      v <- vol()
      validate(need(nrow(v) > 0, "No data for the current filter selection"))
      d <- v %>% group_by(month, encounter_type) %>%
        summarise(encounters = sum(encounters), .groups = "drop")
      plot_ly(d, x = ~month, y = ~encounters, color = ~encounter_type,
              colors = PAL_SEQ, type = "scatter", mode = "none",
              stackgroup = "one", hovertemplate = "%{y} %{fullData.name}<extra></extra>") %>%
        plotly_layout() %>% no_plotly_bar()
    })

    output$vol_mix <- renderPlotly({
      v <- vol()
      validate(need(nrow(v) > 0, " "))
      d <- v %>% group_by(encounter_type) %>%
        summarise(encounters = sum(encounters), .groups = "drop") %>%
        arrange(encounters)
      plot_ly(d, y = ~reorder(encounter_type, encounters), x = ~encounters,
              type = "bar", orientation = "h", marker = list(color = PAL$blue),
              hovertemplate = "%{x} encounters<extra></extra>") %>%
        plotly_layout() %>%
        layout(yaxis = list(title = ""), xaxis = list(title = "Encounters")) %>%
        no_plotly_bar()
    })

    # ---- LOS trend ------------------------------------------------------
    output$los_trend <- renderPlotly({
      l <- los()
      validate(need(nrow(l) > 0, "No data for the current filter selection"))
      d <- l %>%
        mutate(qtr = lubridate::floor_date(month, "quarter")) %>%
        group_by(qtr) %>%
        summarise(los = sum(los_total) / sum(admissions),
                  expected = sum(expected_los * admissions) / sum(admissions),
                  .groups = "drop")
      plot_ly(d, x = ~qtr) %>%
        add_lines(y = ~los, name = "Actual LOS", line = list(color = PAL$navy, width = 3),
                  hovertemplate = "%{y:.1f} days<extra>Actual</extra>") %>%
        add_lines(y = ~expected, name = "Expected LOS",
                  line = list(color = PAL$slate, width = 2, dash = "dot"),
                  hovertemplate = "%{y:.1f} days<extra>Expected</extra>") %>%
        plotly_layout() %>%
        layout(yaxis = list(title = "Days", rangemode = "tozero")) %>%
        no_plotly_bar()
    })

    # ---- occupancy ----------------------------------------------------
    output$occupancy <- renderPlotly({
      cen <- data$agg_census
      cen <- cen[cen$snapshot >= as.Date(filters$dates()[1]) &
                 cen$snapshot <= as.Date(filters$dates()[2]), ]
      if (length(filters$facilities()))    cen <- cen[cen$facility %in% filters$facilities(), ]
      if (length(filters$service_lines())) cen <- cen[cen$service_line %in% filters$service_lines(), ]
      validate(need(nrow(cen) > 0, "No data for the current filter selection"))
      beds <- data$meta$facility_beds
      d <- cen %>% group_by(facility) %>%
        summarise(census = mean(census), .groups = "drop") %>%
        mutate(beds = beds[facility],
               occ = census / beds) %>%
        arrange(census)
      plot_ly(d, y = ~reorder(facility, census)) %>%
        add_bars(x = ~beds, name = "Staffed beds",
                 marker = list(color = PAL$grid), hoverinfo = "skip") %>%
        add_bars(x = ~census, name = "Avg census",
                 marker = list(color = PAL$teal),
                 hovertemplate = "%{x:.0f} patients (%{customdata:.0%})<extra></extra>",
                 customdata = ~occ) %>%
        plotly_layout() %>%
        layout(barmode = "overlay", yaxis = list(title = ""),
               xaxis = list(title = "Beds / patients")) %>%
        no_plotly_bar()
    })

    # ---- day of week --------------------------------------------------
    output$dow <- renderPlotly({
      d <- data$agg_dow
      if (length(filters$facilities()))    d <- d[d$facility %in% filters$facilities(), ]
      if (length(filters$service_lines())) d <- d[d$service_line %in% filters$service_lines(), ]
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% group_by(dow) %>% summarise(encounters = sum(encounters), .groups = "drop")
      plot_ly(d, x = ~dow, y = ~encounters, type = "bar",
              marker = list(color = PAL$blue),
              hovertemplate = "%{y} encounters<extra>%{x}</extra>") %>%
        plotly_layout() %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Encounters")) %>%
        no_plotly_bar()
    })

    # ---- discharge disposition --------------------------------------
    output$disposition <- renderPlotly({
      d <- apply_filters(data$agg_disposition, filters$dates(), filters$facilities(),
                         filters$service_lines(), NULL)
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>%
        mutate(qtr = lubridate::floor_date(month, "quarter")) %>%
        group_by(qtr, discharge_disposition) %>%
        summarise(n = sum(admissions), .groups = "drop") %>%
        group_by(qtr) %>% mutate(share = n / sum(n)) %>% ungroup()
      plot_ly(d, x = ~qtr, y = ~share, color = ~discharge_disposition, colors = PAL_SEQ,
              type = "bar",
              hovertemplate = "%{y:.0%} %{fullData.name}<extra></extra>") %>%
        plotly_layout() %>%
        layout(barmode = "stack", yaxis = list(title = "", tickformat = ".0%"),
               xaxis = list(title = "")) %>%
        no_plotly_bar()
    })

    # ---- drill-down: admissions by service line ---------------------
    output$adm_by_sl <- renderPlotly({
      d <- adm_detail()
      validate(need(nrow(d) > 0, "No data for the current filter selection"))
      d <- d %>% count(service_line, name = "admissions") %>% arrange(admissions)
      plot_ly(d, y = ~reorder(service_line, admissions), x = ~admissions,
              type = "bar", orientation = "h",
              marker = list(color = PAL$navy),
              source = ns("adm_sl"), customdata = ~service_line,
              hovertemplate = "%{x} admissions<extra>%{y}</extra>") %>%
        plotly_layout() %>%
        layout(yaxis = list(title = ""), xaxis = list(title = "Admissions")) %>%
        no_plotly_bar()
    })

    sel_sl <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", source = ns("adm_sl")), {
      cd <- event_data("plotly_click", source = ns("adm_sl"))$customdata
      sel_sl(if (identical(cd, sel_sl())) NULL else cd)
    })
    observeEvent(list(filters$dates(), filters$facilities(),
                      filters$service_lines(), filters$payers()), sel_sl(NULL))

    output$drill_title <- renderUI({
      if (is.null(sel_sl()))
        span(class = "card-hint", "Showing all service lines - click a bar to filter")
      else
        span(strong(sel_sl()), " - ", tags$a(href = "#", id = ns("clear"),
             onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("clear_sel")),
             "clear"))
    })
    observeEvent(input$clear_sel, sel_sl(NULL))

    output$drill_tbl <- reactable::renderReactable({
      d <- adm_detail()
      if (!is.null(sel_sl())) d <- d[d$service_line == sel_sl(), ]
      validate(need(nrow(d) > 0, "No admissions"))
      d <- d %>%
        transmute(`Admit` = admit_date, `Discharge` = discharge_date,
                  Facility = facility, `Service line` = service_line,
                  `Primary reason` = dx_label, Payer = payer_type,
                  `Age band` = age_band, `LOS` = los_days,
                  `O/E LOS` = oe_los, Disposition = discharge_disposition,
                  `30d readmit` = readmit_30d) %>%
        arrange(desc(Admit))
      reactable::reactable(
        d, compact = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 8,
        searchable = TRUE, theme = reactable_theme(),
        columns = list(
          `30d readmit` = reactable::colDef(
            cell = function(v) if (isTRUE(v)) "Yes" else "No",
            style = function(v) if (isTRUE(v)) list(color = PAL$red, fontWeight = 600) else NULL,
            maxWidth = 90),
          LOS = reactable::colDef(format = reactable::colFormat(digits = 1), maxWidth = 70),
          `O/E LOS` = reactable::colDef(
            maxWidth = 80,
            style = function(v) if (!is.na(v) && v > 1.1) list(color = PAL$red)
                               else if (!is.na(v) && v < 0.9) list(color = PAL$green) else NULL)
        )
      )
    })
  })
}
