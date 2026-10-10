# Opportunity store (SQLite) ---------------------------------------------------------------
#
# Keeps what engineers decide: status changes, validation checks, notes, logged
# interventions, recorded outcomes, analog prototypes and AI drafts. One file
# (default data/floodpulse.sqlite, override with WF_DB).

store_path <- function() Sys.getenv("WF_DB", "data/floodpulse.sqlite")

store_open <- function(path = store_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS opp_state (key TEXT PRIMARY KEY, status TEXT, checks TEXT, notes TEXT, updated TEXT, user TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS opp_history (key TEXT, ts TEXT, from_status TEXT, to_status TEXT, user TEXT, comment TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS interventions (id INTEGER PRIMARY KEY AUTOINCREMENT, well TEXT, pattern TEXT, sand TEXT, date TEXT, type TEXT, status TEXT, notes TEXT, opp_key TEXT, created TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS outcomes (key TEXT, ts TEXT, verdict TEXT, expected TEXT, actual TEXT, notes TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS prototypes_user (prototype TEXT, version TEXT, dwi REAL, sec_rf REAL, dwp REAL, util REAL, wor REAL, source TEXT, created TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ai_drafts (key TEXT, ts TEXT, model TEXT, text TEXT)")
  # forecast used for the decision, frozen when the opportunity is validated (post-job comparison)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS forecasts_frozen (key TEXT, ts TEXT, scenario TEXT, month INTEGER, qo REAL, qw REAL, qf REAL)")
  # v3: interventions carry the interval and a job id (several opportunities in one rig visit)
  cols <- DBI::dbListFields(con, "interventions")
  if (!"interval_id" %in% cols) DBI::dbExecute(con, "ALTER TABLE interventions ADD COLUMN interval_id TEXT")
  if (!"job" %in% cols) DBI::dbExecute(con, "ALTER TABLE interventions ADD COLUMN job TEXT")
  con
}

now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

store_states <- function(con) data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM opp_state"))

store_get_state <- function(con, key) {
  x <- DBI::dbGetQuery(con, "SELECT * FROM opp_state WHERE key = ?", params = list(key))
  if (nrow(x)) x else NULL
}

store_set_state <- function(con, key, status = NULL, checks = NULL, notes = NULL, user = Sys.getenv("USER", "engineer"), comment = "") {
  old <- store_get_state(con, key)
  st <- status %||% old$status %||% NA_character_
  ck <- if (!is.null(checks)) paste(checks, collapse = "\u001f") else old$checks %||% ""
  nt <- notes %||% old$notes %||% ""
  DBI::dbExecute(con, "INSERT OR REPLACE INTO opp_state (key, status, checks, notes, updated, user) VALUES (?, ?, ?, ?, ?, ?)",
                 params = list(key, st, ck, nt, now_txt(), user))
  if (!is.null(status) && !identical(status, old$status)) {
    DBI::dbExecute(con, "INSERT INTO opp_history VALUES (?, ?, ?, ?, ?, ?)",
                   params = list(key, now_txt(), old$status %||% NA_character_, status, user, comment))
  }
  invisible(TRUE)
}

store_checks <- function(con, key) {
  x <- store_get_state(con, key)
  if (is.null(x) || is.na(x$checks) || !nzchar(x$checks)) character() else strsplit(x$checks, "\u001f", fixed = TRUE)[[1]]
}

store_history <- function(con, key) data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM opp_history WHERE key = ? ORDER BY ts", params = list(key)))

store_add_intervention <- function(con, well, pattern, sand, date, type, status, notes, opp_key, interval_id = NA_character_, job = NA_character_) {
  na <- function(x) if (is.null(x) || !length(x) || is.na(x) || identical(x, "")) NA_character_ else as.character(x)
  DBI::dbExecute(con, "INSERT INTO interventions (well, pattern, sand, date, type, status, notes, opp_key, created, interval_id, job) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                 params = list(well, na(pattern), na(sand), format(as.Date(date)), toupper(type), toupper(status), notes, opp_key, now_txt(), na(interval_id), na(job)))
}

store_freeze_forecast <- function(con, key, prof) {
  DBI::dbExecute(con, "DELETE FROM forecasts_frozen WHERE key = ?", params = list(key))
  if (is.null(prof) || !nrow(prof)) return(invisible(0))
  x <- data.frame(key = key, ts = now_txt(), scenario = prof$scenario, month = as.integer(prof$month), qo = prof$qo, qw = prof$qw, qf = prof$qf)
  DBI::dbWriteTable(con, "forecasts_frozen", x, append = TRUE)
}
store_frozen <- function(con, key) {
  x <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM forecasts_frozen WHERE key = ? ORDER BY scenario, month", params = list(key)))
  if (nrow(x)) x else NULL
}

store_interventions <- function(con) {
  x <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM interventions ORDER BY date"))
  if (nrow(x)) x[, date := as.Date(date)]
  x
}

store_add_outcome <- function(con, key, verdict, expected, actual, notes) {
  DBI::dbExecute(con, "INSERT INTO outcomes VALUES (?, ?, ?, ?, ?, ?)", params = list(key, now_txt(), verdict, expected, actual, notes))
}
store_outcomes <- function(con) data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM outcomes ORDER BY ts"))

store_save_prototype <- function(con, proto) {
  p <- data.table::copy(proto)[, .(prototype, version, dwi, sec_rf, dwp, util, wor, source = "analog (app)", created = now_txt())]
  DBI::dbExecute(con, "DELETE FROM prototypes_user WHERE prototype = ? AND version = ?", params = list(p$prototype[1], p$version[1]))
  DBI::dbWriteTable(con, "prototypes_user", as.data.frame(p), append = TRUE)
}
store_prototypes <- function(con) {
  x <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT prototype, version, dwi, sec_rf, dwp, util, wor, source FROM prototypes_user"))
  if (nrow(x)) x else NULL
}

store_save_ai <- function(con, key, model, text) DBI::dbExecute(con, "INSERT INTO ai_drafts VALUES (?, ?, ?, ?)", params = list(key, now_txt(), model, text))
store_ai <- function(con, key) {
  x <- DBI::dbGetQuery(con, "SELECT * FROM ai_drafts WHERE key = ? ORDER BY ts DESC LIMIT 1", params = list(key))
  if (nrow(x)) x else NULL
}
