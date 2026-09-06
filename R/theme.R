# ------------------------------------------------------------------------------
# theme.R -- visual identity for the dashboard (bslib theme + palette + helpers)
# ------------------------------------------------------------------------------

# Accessible, healthcare-analytics palette (checked for contrast on white).
PAL <- list(
  navy    = "#0B3C5D",  # primary / headers
  teal    = "#1D7874",  # positive / operational accent
  blue    = "#2E86AB",  # neutral series
  amber   = "#E8A33D",  # caution / watch
  red     = "#C4453C",  # adverse / above benchmark
  green   = "#2F8F5B",  # favourable / below benchmark
  slate   = "#5A6B7B",  # secondary text
  grid    = "#E6EAEE"
)

# categorical sequence for multi-series charts
PAL_SEQ <- unname(c(PAL$navy, PAL$teal, PAL$amber, PAL$blue,
                    PAL$red, PAL$green, PAL$slate, "#8E6C8A", "#A6761D", "#4C9F70"))

app_theme <- function() {
  bslib::bs_theme(
    version      = 5,
    bg           = "#FBFCFD",
    fg           = "#1C2A38",
    primary      = PAL$navy,
    secondary    = PAL$slate,
    success      = PAL$green,
    info         = PAL$blue,
    warning      = PAL$amber,
    danger       = PAL$red,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
    "navbar-bg"  = PAL$navy,
    "border-radius" = "0.6rem",
    "card-border-color" = PAL$grid
  )
}

# consistent plotly layout
plotly_layout <- function(p, title = NULL, ...) {
  plotly::layout(
    p,
    title = if (!is.null(title)) list(text = title, font = list(size = 14, color = PAL$navy),
                                      x = 0, xanchor = "left") else NULL,
    font = list(family = "Inter, sans-serif", color = "#1C2A38", size = 12),
    xaxis = list(gridcolor = PAL$grid, zerolinecolor = PAL$grid, title = list(standoff = 8)),
    yaxis = list(gridcolor = PAL$grid, zerolinecolor = PAL$grid),
    paper_bgcolor = "white", plot_bgcolor = "white",
    margin = list(l = 55, r = 20, t = if (is.null(title)) 20 else 44, b = 45),
    legend = list(orientation = "h", y = -0.2, x = 0),
    hoverlabel = list(font = list(family = "Inter, sans-serif")),
    ...
  )
}

no_plotly_bar <- function(p) {
  plotly::config(p, displaylogo = FALSE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                            "hoverClosestCartesian", "hoverCompareCartesian"))
}
