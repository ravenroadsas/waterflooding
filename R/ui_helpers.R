# Formatting, theme and plot helpers -----------------------------------------

pal <- list(
  bg = "#0b1220", panel = "#111a2b", line = "#1f2b40", text = "#d6e0ee", muted = "#8595ad",
  oil = "#34d399", water = "#60a5fa", inj = "#a78bfa", gas = "#fbbf24", warn = "#fbbf24",
  crit = "#f87171", ok = "#2dd4bf", accent = "#22d3ee"
)
sev_colors <- c(critical = "#f87171", warning = "#fbbf24", opportunity = "#2dd4bf", info = "#8595ad")

fmt_num <- function(x, d = 0) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, big.mark = ","))
fmt_pct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(formatC(100 * x, format = "f", digits = d), "%"))
fmt_mm <- function(x, d = 2) ifelse(is.na(x), "–", paste0(formatC(x / 1e6, format = "f", digits = d), " MM"))
fmt_month <- function(d) format(d, "%b %Y")

metric_catalog <- list(
  rf = list(label = "Recovery factor", fmt = "pct"),
  rf_wf = list(label = "Waterflood RF (incremental)", fmt = "pct"),
  hcpvi = list(label = "HCPV injected", fmt = "num2"),
  ev = list(label = "Apparent volumetric sweep Ev", fmt = "num2"),
  mi = list(label = "Maturity index (Np / movable oil)", fmt = "pct"),
  wc = list(label = "Water cut", fmt = "pct"),
  vrr = list(label = "VRR (month)", fmt = "num2"),
  cum_vrr = list(label = "VRR cumulative", fmt = "num2"),
  vrr_wf = list(label = "VRR since flood start", fmt = "num2"),
  qo = list(label = "Oil rate (stb/d)", fmt = "num0"),
  qwi = list(label = "Injection rate (bwpd)", fmt = "num0"),
  wor = list(label = "WOR", fmt = "num2"),
  remaining_mov = list(label = "Remaining movable oil (stb)", fmt = "mm"),
  wuf = list(label = "Water utilisation (bbl inj / bbl WF oil)", fmt = "num1"),
  inj_eff = list(label = "Injection efficiency (stb oil / bbl inj)", fmt = "num3")
)
# Colour scales tuned for the dark theme: low values dark, high values bright.
seq_scale <- list(list(0, "#0f1b33"), list(0.3, "#1e40af"), list(0.6, "#0ea5e9"), list(0.85, "#5eead4"), list(1, "#fde047"))
good_bad_scale <- list(list(0, "#b91c1c"), list(0.5, "#fbbf24"), list(1, "#22c55e"))
bad_high_scale <- list(list(0, "#0f1b33"), list(0.4, "#0e7490"), list(0.75, "#fbbf24"), list(1, "#ef4444"))
cscale <- function(key) {
  if (key %in% c("ev", "inj_eff")) good_bad_scale
  else if (key %in% c("wc", "wor", "wuf")) bad_high_scale
  else seq_scale
}

metric_choices <- function(keys) stats::setNames(keys, vapply(keys, function(k) metric_catalog[[k]]$label, ""))

fmt_metric <- function(x, key) {
  switch(metric_catalog[[key]]$fmt, pct = fmt_pct(x), num2 = fmt_num(x, 2), num1 = fmt_num(x, 1),
         num3 = fmt_num(x, 3), mm = fmt_mm(x), fmt_num(x, 0))
}

# Map numeric values onto a hex colour ramp.
ramp_colors <- function(x, cols = c("#1e3a8a", "#22d3ee", "#fde047", "#f87171"), lim = range(x, na.rm = TRUE)) {
  if (!any(is.finite(x))) return(rep("#334155", length(x)))
  if (diff(lim) == 0) lim <- lim + c(-1, 1)
  f <- grDevices::colorRamp(cols)
  z <- pmin(pmax((x - lim[1]) / diff(lim), 0), 1)
  out <- rep("#334155", length(x))
  ok <- is.finite(z)
  m <- f(z[ok])
  out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  out
}

axis_style <- function(title = NULL, ...) {
  utils::modifyList(list(title = list(text = title, font = list(size = 11, color = pal$muted)),
                         gridcolor = pal$line, zerolinecolor = pal$line, linecolor = pal$line,
                         tickfont = list(size = 10, color = pal$muted)), list(...))
}

pl_theme <- function(p, x = NULL, y = NULL, legend = TRUE, ...) {
  p <- plotly::layout(p,
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = pal$text, family = "Inter, 'Segoe UI', system-ui, sans-serif", size = 11),
    xaxis = axis_style(x), yaxis = axis_style(y),
    legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 10), bgcolor = "rgba(0,0,0,0)"),
    showlegend = legend,
    margin = list(l = 55, r = 20, t = 30, b = 45),
    hoverlabel = list(bgcolor = pal$panel, bordercolor = pal$line, font = list(color = pal$text)),
    ...
  )
  plotly::config(p, displaylogo = FALSE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverCompareCartesian", "toggleSpikelines"))
}

empty_plot <- function(msg = "No data for this selection") {
  p <- plotly::plot_ly(type = "scatter", mode = "markers")
  p <- plotly::layout(p, annotations = list(list(text = msg, showarrow = FALSE, font = list(color = pal$muted, size = 13),
                                                 xref = "paper", yref = "paper", x = 0.5, y = 0.5)),
                      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  pl_theme(p, legend = FALSE)
}

stage_badge <- function(stage) {
  col <- stage_colors[as.character(stage)]
  htmltools::span(class = "wf-badge", style = sprintf("--c:%s", col), as.character(stage))
}

sev_badge_html <- function(sev) {
  sprintf('<span class="wf-sev" style="--c:%s">%s</span>', sev_colors[sev], sev)
}

kpi <- function(label, value, sub = NULL, tone = "neutral", icon = NULL) {
  htmltools::div(class = paste("wf-kpi", paste0("tone-", tone)),
    htmltools::div(class = "wf-kpi-label", label),
    htmltools::div(class = "wf-kpi-value", value),
    if (!is.null(sub)) htmltools::div(class = "wf-kpi-sub", sub)
  )
}

card_title <- function(title, hint = NULL, ...) {
  bslib::card_header(class = "wf-card-head",
    htmltools::div(htmltools::span(class = "wf-card-title", title),
                   if (!is.null(hint)) htmltools::span(class = "wf-card-hint", hint)),
    htmltools::div(class = "wf-card-tools", ...)
  )
}

dt_dark <- function(df, ..., pageLength = 10, escape = TRUE) {
  DT::datatable(df, rownames = FALSE, escape = escape, selection = "single", class = "compact wf-dt",
                options = list(pageLength = pageLength, dom = "ftip", scrollX = TRUE, ...))
}

# Convex hull polygon for each pattern from its wells' coordinates.
pattern_polygons <- function(h) {
  if (is.null(h) || !all(c("x", "y") %in% names(h))) return(NULL)
  hh <- h[is.finite(x) & is.finite(y)]
  if (!nrow(hh)) return(NULL)
  hh[, {
    pts <- unique(data.table::data.table(x = x, y = y))
    if (nrow(pts) >= 3) {
      idx <- grDevices::chull(pts$x, pts$y)
      .(px = c(pts$x[idx], pts$x[idx[1]]), py = c(pts$y[idx], pts$y[idx[1]]), cx = mean(pts$x), cy = mean(pts$y))
    } else .(px = pts$x, py = pts$y, cx = mean(pts$x), cy = mean(pts$y))
  }, by = pattern]
}

`%||%` <- function(a, b) if (is.null(a)) b else a
