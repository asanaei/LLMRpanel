# batch.R -------------------------------------------------------------------
# Batch administration through LLMR's asynchronous provider APIs. Submission
# returns a handle, and fetching matches results to the panel grid by request id.

# A panel batch job: the LLMR job plus the grid metadata needed to parse results
# back into a panel_responses. It deliberately stores no LLM config of its own
# (the LLMR job holds the reference it needs), and the metadata carries no
# secrets -- only the persona/item layout and the request ids.
.panel_batch_job <- function(llmr_job, meta, panel, instrument) {
  structure(
    list(job = llmr_job, meta = meta, panel = panel, instrument = instrument,
         submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    class = "panel_batch_job")
}

#' Administer a panel asynchronously through the batch API
#'
#' Submits one request per persona and item to a provider's batch API and returns
#' a job handle. Use [panel_batch_status()] to inspect the job and
#' [panel_batch_fetch()] to retrieve completed results. Provider services
#' determine prices and completion times.
#'
#' All personas are administered under one `config` (one model). The handle and
#' its optional `state_path` file carry the survey prompts and the rendered
#' persona text, but no API key value.
#'
#' @param panel A [panel_from_margins()] / [panel_from_data()] /
#'   [panel_from_personas()] result.
#' @param instrument A [panel_instrument()].
#' @param config An `LLMR::llm_config()` for a generative model on a provider with
#'   a supported batch API (OpenAI, Groq, Anthropic, Gemini).
#' @param state_path Optional path; when given the job is also saved there as RDS
#'   so it can be fetched from another session.
#' @param max_calls Integer. If the run would make more than this many calls, it
#'   stops unless `confirm = TRUE`. Default 5000.
#' @param confirm Logical. Set `TRUE` to proceed past `max_calls`.
#' @return A `panel_batch_job` handle.
#' @seealso [panel_batch_fetch()], [panel_batch_status()],
#'   [panel_administer()].
#' @examples
#' \dontrun{
#' panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 200)
#' instrument <- panel_instrument(item_likert("wk4", "A four-day work week helps."))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' job <- panel_batch_submit(panel, instrument, cfg, state_path = "panel_job.rds")
#' # ... later:
#' resp <- panel_batch_fetch(job)
#' }
#' @export
panel_batch_submit <- function(panel, instrument, config, state_path = NULL,
                               max_calls = 5000L, confirm = FALSE) {
  stopifnot(inherits(panel, "silicon_panel"),
            inherits(instrument, "panel_instrument"))
  if (!inherits(config, "llm_config")) {
    abort("`config` must be an LLMR::llm_config().")
  }
  exps <- .panel_build_grid(panel, instrument, config)
  .panel_preflight(nrow(exps), max_calls, confirm)
  cli::cli_inform("Submitting {nrow(exps)} call(s) to the batch API.")

  llmr_job <- LLMR::llm_batch_submit(config, exps$messages, state_path = NULL)

  # Grid metadata for the id-keyed join at fetch time. The request_id matches the
  # LLMR custom_id ('llmr-%06d' in submit order); we never rely on row order.
  meta_cols <- c("request_id", "persona_id", "item_id", "type",
                 "item_position", "option_order", "model", "provider",
                 intersect("profiles", names(exps)))
  meta <- exps[, meta_cols]
  job <- .panel_batch_job(llmr_job, meta, panel, instrument)
  if (!is.null(state_path)) saveRDS(job, state_path)
  job
}

#' Check the status of a panel batch job
#'
#' @param job A [panel_batch_submit()] handle (or a `state_path` to one).
#' @return The LLMR batch status (a one-row tibble).
#' @seealso [panel_batch_submit()], [panel_batch_fetch()].
#' @export
panel_batch_status <- function(job) {
  if (is.character(job) && length(job) == 1L) job <- readRDS(job)
  stopifnot(inherits(job, "panel_batch_job"))
  LLMR::llm_batch_status(job$job)
}

#' Fetch and parse a completed panel batch job
#'
#' Retrieves the batch results and parses them into a `panel_responses`,
#' identical in shape to a synchronous [panel_administer()] run. Responses are
#' joined to the grid by request id, so the order the provider returns them in
#' does not matter.
#'
#' @param job A [panel_batch_submit()] handle (or a `state_path` to one).
#' @return A `panel_responses` tibble.
#' @seealso [panel_batch_submit()], [panel_batch_status()].
#' @export
panel_batch_fetch <- function(job) {
  if (is.character(job) && length(job) == 1L) job <- readRDS(job)
  stopifnot(inherits(job, "panel_batch_job"))

  fetched <- LLMR::llm_batch_fetch(job$job)
  if (!("custom_id" %in% names(fetched))) {
    abort("Batch results lack `custom_id`; cannot align to the panel grid.")
  }
  if (anyDuplicated(fetched$custom_id)) {
    abort("Batch results contain duplicate `custom_id` values.")
  }
  fetched$model <- NULL
  fetched$provider <- NULL
  # Join responses to the grid metadata BY ID (never by row order).
  meta <- job$meta
  missing_ids <- setdiff(meta$request_id, fetched$custom_id)
  if (length(missing_ids)) {
    abort("Batch results omit submitted request ids; no responses were parsed.")
  }
  res <- merge(meta, fetched, by.x = "request_id", by.y = "custom_id",
               all.x = TRUE, sort = FALSE)
  # merge() makes no row-order promise: restore the grid (submission) order so
  # the batch result is row-identical to a synchronous run of the same grid.
  res <- res[match(meta$request_id, res$request_id), , drop = FALSE]
  if (!("response_text" %in% names(res))) res$response_text <- NA_character_
  .panel_parse_responses(res, job$instrument, job$panel)
}

#' @export
print.panel_batch_job <- function(x, ...) {
  cat(sprintf("<panel_batch_job | %d call(s) | provider: %s | submitted %s>\n",
              nrow(x$meta), x$job$provider %||% "?", x$submitted_at))
  cat("  fetch with panel_batch_fetch(); check with panel_batch_status().\n")
  invisible(x)
}
