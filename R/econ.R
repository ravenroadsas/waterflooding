# Economics hook ---------------------------------------------------------------------------
#
# Ranking uses volumes only. A private economic function can be plugged in later without
# changing the app: point WF_ECON_FILE to an R file that defines
#   wf_econ(summary, forecasts)  ->  data.table(key, <value columns>, econ_rank = <optional>)
# `summary` is the opportunity table; `forecasts` the monthly profiles (well, sand, interval_id,
# scenario, month, qo, qw, qf). Returned columns are joined to the opportunities and shown.

econ_fun <- local({
  f <- NULL; loaded <- FALSE
  function() {
    if (!loaded) {
      loaded <<- TRUE
      path <- Sys.getenv("WF_ECON_FILE")
      if (nzchar(path) && file.exists(path)) {
        e <- new.env(); tryCatch({ sys.source(path, envir = e); if (is.function(e$wf_econ)) f <<- e$wf_econ },
                                 error = function(err) warning("WF_ECON_FILE: ", conditionMessage(err)))
      }
    }
    f
  }
})
econ_available <- function() !is.null(econ_fun())

econ_apply <- function(summary, res) {
  f <- econ_fun()
  if (is.null(f) || !nrow(summary)) return(summary)
  v <- tryCatch(data.table::as.data.table(f(data.table::copy(summary), res$ds$profiles)), error = function(e) { warning("wf_econ: ", conditionMessage(e)); NULL })
  if (is.null(v) || !"key" %in% names(v)) return(summary)
  merge(summary, v, by = "key", all.x = TRUE, suffixes = c("", ".econ"))
}
