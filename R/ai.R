# Optional AI drafting of decision records ----------------------------------------------
#
# Sends the evidence the app has already computed (never raw production data)
# to Claude and asks for a decision-record draft that follows the methodology's
# evidence contract. An engineer edits and approves it; drafts are stored.
# Enabled only when ANTHROPIC_API_KEY is set. Model: WF_AI_MODEL (default claude-opus-5).

ai_available <- function() nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
ai_model <- function() Sys.getenv("WF_AI_MODEL", "claude-opus-5")

ai_system_prompt <- paste(
  "You are a reservoir engineer reviewing waterflood surveillance evidence with the dimensionless methodology",
  "(DWI, Sec RF, DWP, DTP, injection/production throughput, IWR, utilization, OPR, WPR).",
  "Treat surveillance as evidence aggregation: a single anomalous plot is a screening signal, not a recommendation.",
  "Using only the evidence provided, draft a decision record with these sections, in this order:",
  "1. Assessment (state the evidence status: screening_only, candidate or validated_candidate, and why);",
  "2. Maturity evidence; 3. Process-velocity / balance evidence; 4. Vertical and spatial context;",
  "5. Most plausible physical mechanism; 6. Alternative explanations; 7. Data gaps;",
  "8. Validation required before execution; 9. Proposed action; 10. Expected observable response and time window.",
  "Quote numbers from the evidence with units. Say explicitly when evidence is missing instead of assuming it.",
  "Thresholds labelled LCI are field references, not universal limits. Be concise: short sentences, no preamble."
)

ai_payload <- function(rec, summ_row, pattern_row, settings) {
  list(
    opportunity = list(type = paste(rec$type, opp_types[[rec$type]]), pattern = rec$pattern, unit = rec$sand,
                       well = rec$well, evidence_families = names(rec$fam)[rec$fam], status = as.character(summ_row$status),
                       indicative_gain_bopd = round(rec$gain, 1)),
    evidence_rows = lapply(seq_len(nrow(rec$evidence)), function(i) as.list(rec$evidence[i])),
    engine_text = rec$text,
    pattern_snapshot = as.list(pattern_row[, intersect(c("entity", "area", "dwi", "sec_rf", "exp_sec_rf", "opr", "wpr", "util",
                                                          "exp_util", "util_cum", "tp12", "tp12_ago", "prod_tp12", "iwr12", "wc6",
                                                          "wor6", "loss", "evr", "ve", "stage"), names(pattern_row)), with = FALSE]),
    settings = settings[c("target_tp", "iwr_low", "iwr_high", "util_window", "judge_dwi")]
  )
}

ai_draft <- function(payload) {
  body <- list(
    model = ai_model(), max_tokens = 16000,
    fallbacks = "default",
    system = ai_system_prompt,
    messages = list(list(role = "user", content = paste(
      "Draft the decision record for this opportunity. Evidence (JSON):\n\n",
      jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 4))))
  )
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(`x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"), `anthropic-version` = "2023-06-01",
                       `anthropic-beta` = "server-side-fallback-2026-07-01", `content-type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  j <- httr2::resp_body_json(resp)
  if (httr2::resp_status(resp) >= 400) stop(sprintf("API error %s: %s", httr2::resp_status(resp), j$error$message %||% "unknown"))
  if (identical(j$stop_reason, "refusal")) stop("The model declined this request.")
  txt <- vapply(Filter(function(b) identical(b$type, "text"), j$content), function(b) b$text, "")
  list(text = paste(txt, collapse = "\n"), model = j$model %||% ai_model())
}
