# Reconciliation against uploaded derived tables --------------------------------------------
# Your Patterns / Patterns_Vel / Patterns_Mat / InjSand_calc are compared with the
# app's own results so windows, baselines and prototype versions can be agreed.

reconcile <- function(res, series, st = res$settings, tol = 0.02) {
  ds <- res$ds; out <- list(); gaps <- list()
  cmp <- function(tbl, their, ours, label, rel = TRUE, abs_tol = NA) {
    x <- merge(tbl[, .(entity = pattern, date, theirs = get(their))], series[, .(entity, date, ours = get(ours))], by = c("entity", "date"))
    x <- x[is.finite(theirs) & is.finite(ours)]
    if (!nrow(x)) return(NULL)
    x[, diff := if (rel) abs(ours - theirs) / pmax(abs(theirs), 1e-9) else abs(ours - theirs)]
    ok <- if (rel) x$diff <= tol else x$diff <= abs_tol
    w <- x[which.max(diff)]
    gaps[[length(gaps) + 1]] <<- x[order(-diff)][1:min(5, .N)][, variable := label]
    data.table::data.table(table = deparse(substitute(tbl)), variable = label, rows = nrow(x), within = mean(ok),
                           largest = sprintf("%s %s: yours %.3g, app %.3g", w$entity, format(w$date, "%b %Y"), w$theirs, w$ours))
  }
  if (!is.null(ds$patterns_mat)) {
    pm <- ds$patterns_mat
    out <- c(out, list(cmp(pm, "np", "cum_oil", "Np"), cmp(pm, "nw", "cum_water", "Nw"), cmp(pm, "nwi", "cum_winj", "Nwi"),
                       cmp(pm, "iwrcum", "iwr_cum", "IWRCUM", FALSE, 0.02), cmp(pm, "opr", "opr", "OPR", FALSE, 0.05),
                       cmp(pm, "wpr", "wpr", "WPR", FALSE, 0.05)))
  }
  if (!is.null(ds$patterns_vel)) {
    pv <- ds$patterns_vel
    out <- c(out, list(cmp(pv, "tp", "tp", "TP (month)", FALSE, 0.2), cmp(pv, "wor", "wor", "WOR"), cmp(pv, "iwr", "iwr", "IWR", FALSE, 0.05)))
    # utilization: find which window matches best
    best <- NULL
    for (w in c(3, 6, 12)) {
      r <- cmp(pv, "util", paste0("util", w), sprintf("Util (%d m)", w))
      if (!is.null(r) && (is.null(best) || r$within > best$within)) best <- r
    }
    out <- c(out, list(best))
  }
  if (!is.null(ds[["patterns"]])) {
    p <- ds[["patterns"]]
    out <- c(out, list(cmp(p, "bopd", "qo", "BOPD"), cmp(p, "bwipd", "qwi", "BWIPD")))
  }
  list(summary = data.table::rbindlist(Filter(Negate(is.null), out)), gaps = data.table::rbindlist(gaps, fill = TRUE))
}
