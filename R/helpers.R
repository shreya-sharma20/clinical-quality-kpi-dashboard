# ------------------------------------------------------------------------------
# helpers.R -- formatting + KPI trend utilities shared by both persona modules
# ------------------------------------------------------------------------------

pct  <- function(x, digits = 1) ifelse(is.na(x), "--", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
num  <- function(x, digits = 0) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits, big.mark = ","))
dec  <- function(x, digits = 1) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
usd  <- function(x) ifelse(is.na(x), "--", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))

#' Split a filtered data frame into "current" vs "prior" period of equal length
#' and return a named list used to draw a trend indicator on a value box.
#'
#' @param df       filtered data (must contain `month`)
#' @param value_fn function(data) -> single numeric summarising the metric
#' @param lower_is_better logical; flips the colour of the delta
period_trend <- function(df, value_fn, lower_is_better = FALSE) {
  if (nrow(df) == 0 || all(is.na(df$month))) {
    return(list(current = NA_real_, prior = NA_real_, delta = NA_real_,
                dir = "flat", favourable = NA, spark = NULL))
  }
  rng   <- range(df$month, na.rm = TRUE)
  mid   <- rng[1] + (rng[2] - rng[1]) / 2
  cur   <- value_fn(df[df$month >  mid, , drop = FALSE])
  prior <- value_fn(df[df$month <= mid, , drop = FALSE])
  delta <- cur - prior
  dir   <- if (is.na(delta) || abs(delta) < 1e-9) "flat" else if (delta > 0) "up" else "down"
  favourable <- if (is.na(delta)) NA else if (lower_is_better) delta < 0 else delta > 0

  spark <- df |>
    dplyr::group_by(month) |>
    dplyr::group_modify(~ tibble::tibble(v = value_fn(.x))) |>
    dplyr::ungroup() |>
    dplyr::arrange(month) |>
    dplyr::filter(is.finite(v))

  list(current = cur, prior = prior, delta = delta, dir = dir,
       favourable = favourable, spark = spark)
}

#' Build a bslib value_box with an embedded trend line + delta caption.
kpi_box <- function(title, value, trend, fmt = num, unit = "",
                    lower_is_better = FALSE, theme_col = NULL) {
  arrow <- switch(trend$dir, up = "▲", down = "▼", "▬")
  col   <- if (isTRUE(trend$favourable)) PAL$green else if (isFALSE(trend$favourable)) PAL$red else PAL$slate
  delta_txt <- if (is.na(trend$delta)) "no prior-period comparison" else
    sprintf("%s %s vs prior period", arrow, fmt(abs(trend$delta)))

  spark_svg <- if (!is.null(trend$spark) && nrow(trend$spark) > 1)
    sparkline_plot(trend$spark, lower_is_better) else NULL

  bslib::value_box(
    title = title,
    value = paste0(fmt(value), unit),
    tags$p(class = "kpi-delta",
           tags$span(delta_txt, style = sprintf("color:%s;font-weight:600;", col))),
    showcase = if (!is.null(spark_svg)) div(class = "kpi-spark", spark_svg) else NULL,
    showcase_layout = if (!is.null(spark_svg)) "bottom" else "left center",
    full_screen = FALSE,
    theme = bslib::value_box_theme(bg = "white", fg = PAL$navy),
    class = "kpi-box"
  )
}

#' Lightweight inline-SVG sparkline (no plotly overhead on the KPI tiles).
sparkline_plot <- function(spark, lower_is_better = FALSE) {
  v <- spark$v
  v <- v[is.finite(v)]
  if (length(v) < 2) return(NULL)
  w <- 150; h <- 42; pad <- 2
  rng <- range(v)
  if (diff(rng) == 0) rng <- rng + c(-1, 1)
  xs <- seq(pad, w - pad, length.out = length(v))
  ys <- h - pad - (v - rng[1]) / diff(rng) * (h - 2 * pad)
  pts <- paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")
  area <- sprintf("%s %.1f,%.1f %.1f,%.1f", pts, xs[length(xs)], h - pad, xs[1], h - pad)
  last_up <- v[length(v)] >= v[1]
  stroke <- if (isTRUE(last_up) != isTRUE(lower_is_better)) PAL$green else PAL$red
  htmltools::HTML(sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" height="%d" preserveAspectRatio="none" class="spark">
       <polygon points="%s" fill="%s" opacity="0.12"/>
       <polyline points="%s" fill="none" stroke="%s" stroke-width="1.8"
                 stroke-linejoin="round" stroke-linecap="round"/>
       <circle cx="%.1f" cy="%.1f" r="2.4" fill="%s"/>
     </svg>',
    w, h, h, area, stroke, pts, stroke,
    xs[length(xs)], ys[length(ys)], stroke
  ))
}

#' Apply the shared sidebar filters to any mart that carries the standard keys.
apply_filters <- function(df, dates, facilities, service_lines, payers = NULL) {
  if (!is.null(df[["month"]]) && !is.null(dates)) {
    df <- df[df$month >= lubridate::floor_date(as.Date(dates[1]), "month") &
             df$month <= as.Date(dates[2]), , drop = FALSE]
  }
  if (!is.null(facilities) && length(facilities) && !is.null(df[["facility"]]))
    df <- df[df$facility %in% facilities, , drop = FALSE]
  if (!is.null(service_lines) && length(service_lines) && !is.null(df[["service_line"]]))
    df <- df[df$service_line %in% service_lines, , drop = FALSE]
  if (!is.null(payers) && length(payers) && !is.null(df[["payer_type"]]))
    df <- df[df$payer_type %in% payers, , drop = FALSE]
  df
}

empty_plot <- function(msg = "No data for the current filter selection") {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = msg, showarrow = FALSE,
                         font = list(size = 14, color = PAL$slate)),
      paper_bgcolor = "white", plot_bgcolor = "white"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

reactable_theme <- function() {
  reactable::reactableTheme(
    borderColor = PAL$grid,
    headerStyle = list(background = "#F3F5F7", color = PAL$navy,
                       fontWeight = 600, borderColor = PAL$grid),
    stripedColor = "#F7F9FA",
    highlightColor = "#EEF4F7",
    cellPadding = "8px 10px",
    style = list(fontFamily = "Inter, sans-serif", fontSize = "13px")
  )
}
