# Formatting, theme and plot helpers ----------------------------------------------------

pal <- list(bg = "#0b1220", panel = "#111a2b", line = "#1f2b40", text = "#d6e0ee", muted = "#8595ad",
            oil = "#34d399", water = "#60a5fa", inj = "#a78bfa", warn = "#fbbf24", crit = "#f87171",
            ok = "#2dd4bf", accent = "#22d3ee", proto = "#e2e8f0")

fmt_num <- function(x, d = 0) ifelse(is.na(x) | !is.finite(x), "–", formatC(x, format = "f", digits = d, big.mark = ","))
fmt_int <- function(x) fmt_num(x, 0)
fmt_pct <- function(x, d = 1) ifelse(is.na(x) | !is.finite(x), "–", paste0(formatC(100 * x, format = "f", digits = d), "%"))
fmt_mm <- function(x, d = 2) ifelse(is.na(x) | !is.finite(x), "–", paste0(formatC(x / 1e6, format = "f", digits = d), " MM"))
fmt_month <- function(d) format(d, "%b %Y")

metric_catalog <- list(
  dwi = list(label = "DWI", fmt = function(x) fmt_num(x, 2)),
  sec_rf = list(label = "Secondary RF", fmt = function(x) fmt_pct(x)),
  rf = list(label = "Total RF (HCPV)", fmt = function(x) fmt_pct(x)),
  opr = list(label = "OPR", fmt = function(x) fmt_num(x, 2)),
  wpr = list(label = "WPR", fmt = function(x) fmt_num(x, 2)),
  util = list(label = "Utilization", fmt = function(x) fmt_num(x, 1)),
  tp = list(label = "Inj TP (month)", fmt = function(x) paste0(fmt_num(x, 1), "%")),
  tp12 = list(label = "Inj TP 12 m", fmt = function(x) paste0(fmt_num(x, 1), "%")),
  prod_tp12 = list(label = "Prod TP 12 m", fmt = function(x) paste0(fmt_num(x, 1), "%")),
  iwr = list(label = "IWR (month)", fmt = function(x) fmt_num(x, 2)),
  iwr12 = list(label = "IWR 12 m", fmt = function(x) fmt_num(x, 2)),
  wc6 = list(label = "Water cut", fmt = function(x) fmt_pct(x)),
  wor6 = list(label = "WOR", fmt = function(x) fmt_num(x, 1)),
  loss = list(label = "Loss (DWI - DTP)", fmt = function(x) fmt_num(x, 2)),
  qo = list(label = "Oil rate", fmt = function(x) fmt_num(x)),
  qwi = list(label = "Injection rate", fmt = function(x) fmt_num(x))
)
metric_choices <- function(keys) stats::setNames(keys, vapply(keys, function(k) metric_catalog[[k]]$label, ""))

axis_style <- function(title = NULL, ...) {
  utils::modifyList(list(title = list(text = title, font = list(size = 11, color = pal$muted)),
                         gridcolor = pal$line, zerolinecolor = pal$line, linecolor = pal$line,
                         tickfont = list(size = 10, color = pal$muted)), list(...))
}

pl_theme <- function(p, x = NULL, y = NULL, legend = TRUE, ...) {
  p <- plotly::layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = pal$text, family = "Inter, 'Segoe UI', system-ui, sans-serif", size = 11),
    xaxis = axis_style(x), yaxis = axis_style(y),
    legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10), bgcolor = "rgba(0,0,0,0)"),
    showlegend = legend, margin = list(l = 55, r = 20, t = 30, b = 45),
    hoverlabel = list(bgcolor = pal$panel, bordercolor = pal$line, font = list(color = pal$text)), ...)
  plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverCompareCartesian", "toggleSpikelines"))
}

empty_plot <- function(msg = "No data for this selection") {
  p <- plotly::plot_ly(type = "scatter", mode = "markers")
  p <- plotly::layout(p, annotations = list(list(text = msg, showarrow = FALSE, font = list(color = pal$muted, size = 13),
                      xref = "paper", yref = "paper", x = 0.5, y = 0.5)), xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  pl_theme(p, legend = FALSE)
}

badge <- function(text, col) htmltools::span(class = "wf-badge", style = sprintf("--c:%s", col), text)
badge_html <- function(text, col) sprintf('<span class="wf-badge" style="--c:%s">%s</span>', col, text)

kpi <- function(label, value, sub = NULL, tone = "neutral") {
  htmltools::div(class = paste("wf-kpi", paste0("tone-", tone)),
    htmltools::div(class = "wf-kpi-label", label), htmltools::div(class = "wf-kpi-value", value),
    if (!is.null(sub)) htmltools::div(class = "wf-kpi-sub", sub))
}

tag_chip <- function(kind) htmltools::span(class = paste("wf-tag", kind), toupper(kind))

card_title <- function(title, hint = NULL, ..., tag = NULL) {
  bslib::card_header(class = "wf-card-head",
    htmltools::div(htmltools::span(class = "wf-card-title", title), if (!is.null(hint)) htmltools::span(class = "wf-card-hint", hint)),
    htmltools::div(class = "wf-card-tools", ..., if (!is.null(tag)) tag_chip(tag)))
}

dt_dark <- function(df, ..., pageLength = 10, escape = TRUE, dom = "ftip") {
  DT::datatable(df, rownames = FALSE, escape = escape, selection = "single", class = "compact wf-dt",
                options = list(pageLength = pageLength, dom = dom, scrollX = TRUE, ...))
}

# Continuous colour scale for scatter encodings (good -> bad when `bad_high`).
num_scale <- function(bad_high = TRUE) {
  s <- list(list(0, "#2dd4bf"), list(0.35, "#a3e635"), list(0.65, "#fbbf24"), list(1, "#f87171"))
  if (!bad_high) s <- lapply(seq_along(s), function(i) list(s[[i]][[1]], s[[length(s) + 1 - i]][[2]]))
  s
}

period_of <- function(d) {
  y <- as.integer(format(d, "%Y"))
  ifelse(is.na(y), "No flood", ifelse(y < 2005, "before 2005", ifelse(y < 2016, "2005-2015", "2016 on")))
}
period_symbols <- c("before 2005" = "square", "2005-2015" = "circle", "2016 on" = "triangle-up", "No flood" = "x")

# Pattern scatter with the methodology encodings. `color` is a column name, or
# "cluster" (uses sn$cluster_name) or "area".
pattern_scatter <- function(sn, x, y, color = "util", size = "iwr12", shape = TRUE, hl = NULL, source = "wf",
                            xlab = x, ylab = y, p = plotly::plot_ly(source = source), ytick = NULL) {
  d <- data.table::copy(sn)[is.finite(get(x)) & is.finite(get(y))]
  if (!nrow(d)) return(p)
  d[, sz := if (!is.null(size) && size %in% names(d)) 9 + 9 * pmin(pmax(data.table::fcoalesce(get(size), 1), 0), 2) else 12]
  d[, sym := if (shape) period_symbols[period_of(wf_start)] else "circle"]
  lab_fmt <- function(v, k) if (!is.null(metric_catalog[[k]])) metric_catalog[[k]]$fmt(v) else fmt_num(v, 2)
  d[, tip := sprintf("<b>%s</b> %s<br>%s %s · %s %s<br>OPR %s · Util %s · IWR %s · TP %s%%",
                     entity, data.table::fcoalesce(as.character(area), ""), metric_catalog[[x]]$label %||% x, lab_fmt(get(x), x),
                     metric_catalog[[y]]$label %||% y, lab_fmt(get(y), y), fmt_num(opr, 2), fmt_num(util, 1), fmt_num(iwr12, 2), fmt_num(tp12, 1))]
  categorical <- color %in% c("cluster", "area", "stage")
  if (categorical) {
    cc <- switch(color, cluster = "cluster_name", area = "area", stage = "stage")
    if (!cc %in% names(d)) { d[, (cc) := "n/a"] }
    lv <- sort(unique(as.character(d[[cc]])))
    cols <- if (color == "stage") stage_colors[lv] else stats::setNames(rep(cluster_palette, length.out = length(lv)), lv)
    for (g in lv) {
      dd <- d[as.character(get(cc)) == g]
      p <- plotly::add_markers(p, data = dd, x = dd[[x]], y = dd[[y]], name = g, customdata = dd$entity, text = dd$tip, hoverinfo = "text",
        marker = list(size = dd$sz, symbol = dd$sym, color = unname(cols[g]), opacity = 0.88, line = list(color = "#0b1220", width = 1)))
    }
  } else {
    cv <- if (color %in% names(d)) d[[color]] else rep(NA_real_, nrow(d))
    p <- plotly::add_markers(p, data = d, x = d[[x]], y = d[[y]], name = "Patterns", customdata = d$entity, text = d$tip, hoverinfo = "text",
      marker = list(size = d$sz, symbol = d$sym, color = cv, colorscale = num_scale(color != "opr"), showscale = TRUE, opacity = 0.9,
                    colorbar = list(title = list(text = metric_catalog[[color]]$label %||% color, font = list(size = 10, color = pal$muted)),
                                    thickness = 9, len = 0.7, tickfont = list(color = pal$muted, size = 9)),
                    line = list(color = "#0b1220", width = 1)), showlegend = FALSE)
  }
  p <- plotly::add_text(p, data = d, x = d[[x]], y = d[[y]], text = d$entity, textposition = "top center",
                        textfont = list(size = 9, color = pal$muted), hoverinfo = "skip", showlegend = FALSE)
  if (length(hl)) {
    h <- d[entity %in% hl]
    if (nrow(h)) p <- plotly::add_markers(p, x = h[[x]], y = h[[y]], name = "selected", hoverinfo = "skip", showlegend = FALSE,
      marker = list(size = h$sz + 10, color = "rgba(0,0,0,0)", line = list(color = "#ffffff", width = 2)))
  }
  p
}

# Curve of the prototype most used in the current selection.
main_proto <- function(res, sn) {
  k <- if ("proto_key" %in% names(sn)) names(sort(table(sn$proto_key), decreasing = TRUE))[1] else NULL
  if (is.null(k) || is.na(k)) k <- unique(res$protos$pkey)[1]
  res$protos[pkey == k]
}

add_proto <- function(p, pr, var, xmax, band = 0.2, log = FALSE, name = NULL) {
  pr <- pr[is.finite(get(var)) & dwi <= xmax]
  if (nrow(pr) < 2) return(p)
  nm <- name %||% paste("Prototype", pr$pkey[1])
  if (band > 0) {
    p <- plotly::add_ribbons(p, x = pr$dwi, ymin = pr[[var]] * (if (log) 0.5 else 1 - band), ymax = pr[[var]] * (if (log) 2 else 1 + band),
                             fillcolor = "rgba(226,232,240,0.07)", line = list(width = 0), name = "band", hoverinfo = "skip", showlegend = FALSE)
  }
  plotly::add_lines(p, x = pr$dwi, y = pr[[var]], name = nm, line = list(color = pal$proto, width = 1.8), hoverinfo = "skip")
}

vline_shape <- function(x, col = pal$accent) list(type = "line", x0 = x, x1 = x, yref = "paper", y0 = 0, y1 = 1, line = list(color = col, width = 1, dash = "dot"))
hline_shape <- function(y, col = pal$muted, dash = "dash", yref = "y") list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y, yref = yref, line = list(color = col, width = 1, dash = dash))
rect_shape <- function(x0, x1, y0, y1, fill) list(type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1, fillcolor = fill, line = list(width = 0), layer = "below")

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

ramp_colors <- function(x, cols, lim = range(x, na.rm = TRUE)) {
  if (!any(is.finite(x))) return(rep("#334155", length(x)))
  if (diff(lim) == 0) lim <- lim + c(-1, 1)
  f <- grDevices::colorRamp(cols)
  z <- pmin(pmax((x - lim[1]) / diff(lim), 0), 1)
  out <- rep("#334155", length(x)); ok <- is.finite(z)
  m <- f(z[ok]); out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
  out
}
dwi_ramp <- c("#16a34a", "#22c55e", "#22d3ee", "#3b82f6", "#1e3a8a")

# Map: pattern polygons coloured by a value, optional injector bubbles.
pattern_map_plot <- function(res, vals, title, lim = NULL, bubbles = NULL, source = "wf", ramp = dwi_ramp, hl = NULL) {
  polys <- pattern_polygons(res$wells)
  if (is.null(polys)) return(empty_plot("Add well coordinates (Hierarchy x / y) to enable maps"))
  polys <- merge(polys, vals, by = "pattern", all.x = TRUE)
  v <- unique(polys[, .(pattern, value)])
  lim <- lim %||% range(v$value, na.rm = TRUE)
  v[, col := ramp_colors(value, ramp, lim)]
  p <- plotly::plot_ly(source = source)
  for (pt in v$pattern) {
    g <- polys[pattern == pt]
    p <- plotly::add_polygons(p, x = g$px, y = g$py, fillcolor = paste0(v[pattern == pt, col], "dd"),
      line = list(color = if (identical(pt, hl)) "#ffffff" else "#0b1220", width = if (identical(pt, hl)) 3 else 1.5),
      customdata = rep(pt, nrow(g)), hoveron = "fills", text = sprintf("<b>%s</b><br>%s: %s", pt, title, fmt_num(g$value[1], 2)),
      hoverinfo = "text", showlegend = FALSE)
  }
  cen <- unique(polys[, .(pattern, cx, cy)])
  p <- plotly::add_text(p, data = cen, x = ~cx, y = ~cy, text = ~pattern, textposition = "top center",
                        textfont = list(color = "#ffffff", size = 11), hoverinfo = "skip", showlegend = FALSE)
  w <- unique(res$wells[is.finite(x)], by = "well")
  p <- plotly::add_markers(p, data = w[well_type == "PRODUCER"], x = ~x, y = ~y, name = "Producer", text = ~well, hoverinfo = "text",
    marker = list(size = 6, color = "#0b1220", line = list(color = "#e2e8f0", width = 1.1)))
  if (!is.null(bubbles) && nrow(bubbles)) {
    b <- merge(bubbles, w[, .(well, x, y)], by = "well")
    p <- plotly::add_markers(p, data = b, x = ~x, y = ~y, name = "Injector (bubble = unit TP)", customdata = b$pattern,
      text = sprintf("%s<br>TP %s%%/yr", b$well, fmt_num(b$tp, 1)), hoverinfo = "text",
      marker = list(size = 6 + 3 * sqrt(pmax(b$tp, 0)), color = ifelse(b$tp >= 30, "#ef4444", ifelse(b$tp >= 11.5, "#f97316", ifelse(b$tp >= 5, "#facc15", "#f1f5f9"))),
                    line = list(color = "#0b1220", width = 1)))
  } else {
    p <- plotly::add_markers(p, data = w[well_type == "INJECTOR"], x = ~x, y = ~y, name = "Injector", text = ~well, hoverinfo = "text",
      marker = list(symbol = "triangle-down", size = 9, color = pal$water, line = list(color = "#0b1220", width = 1)))
  }
  p <- plotly::add_markers(p, x = mean(w$x), y = mean(w$y), hoverinfo = "skip", showlegend = FALSE,
    marker = list(size = 0.1, opacity = 0, color = lim, cmin = lim[1], cmax = lim[2], showscale = TRUE,
                  colorscale = lapply(seq_along(ramp), function(i) list((i - 1) / (length(ramp) - 1), ramp[i])),
                  colorbar = list(title = list(text = title, font = list(size = 10, color = pal$muted)), thickness = 9, len = 0.75,
                                  tickfont = list(color = pal$muted, size = 9))))
  p <- pl_theme(p)
  p <- plotly::layout(p, xaxis = axis_style(NULL, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = axis_style(NULL, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, scaleanchor = "x"),
                      margin = list(l = 10, r = 10, t = 30, b = 10))
  plotly::event_register(p, "plotly_click")
}
