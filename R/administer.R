# administer.R --------------------------------------------------------------------
# Administration: every persona answers every item. Order randomization is
# applied per response and recorded -- with LLMs, the order in which options
# are listed is a treatment, and pretending otherwise is how silicon
# "findings" get manufactured.

#' Administer an instrument to a panel
#'
#' One call per persona x item, through LLMR's parallel engine by default.
#' The persona is the system message; the item and its (possibly reordered)
#' options are the user message; replies are matched to the offered
#' options, with failures kept as `NA` -- a refusal or an essay instead of
#' an option is data about the instrument.
#'
#' @details A logprob mode -- reading the full response-option distribution
#'   from one forward pass via `LLMR::llm_logprobs()`, replacing ~30
#'   sampled replicates with one call -- is planned for 0.2 as an
#'   additional administration path, not yet an argument.
#'
#' @param panel A [panel_from_margins()] result.
#' @param instr An [instrument()].
#' @param config An `LLMR::llm_config()` for a generative model.
#' @param .runner Internal seam for tests: `function(experiments, ...)`
#'   returning the experiments with a `response_text` column. Default
#'   `LLMR::call_llm_par()`.
#' @param ... Passed to the runner (e.g. `tries`, `progress`).
#' @return A `panel_responses` tibble: `persona_id`, `item_id`, `type`,
#'   `option_order` (what this respondent saw, `|`-separated), `response`
#'   (matched option or `NA`; verbatim text for open items), `score`
#'   (1-based scale position for Likert items). Carries the panel and
#'   instrument as attributes and an empty calibration slot: every print
#'   shows the **UNCALIBRATED** banner until [calibrate()] fills it.
#' @examples
#' # offline: a simulated respondent through the .runner seam
#' set.seed(110)
#' panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 6)
#' instr <- instrument(item_likert("wk4",
#'   "A four-day work week would benefit society."))
#' fake <- function(experiments, ...) {
#'   experiments$response_text <-
#'     ifelse(grepl("right", vapply(experiments$messages, `[[`, "", "system")),
#'            "agree", "disagree")
#'   experiments
#' }
#' cfg <- LLMR::llm_config("groq", "any-model")
#' resp <- administer(panel, instr, cfg, .runner = fake)
#' resp                       # UNCALIBRATED banner, by design
#' @export
administer <- function(panel, instr, config, .runner = NULL, ...) {
  stopifnot(inherits(panel, "silicon_panel"),
            inherits(instr, "panel_instrument"))
  if (!inherits(config, "llm_config")) {
    abort("`config` must be an LLMR::llm_config().")
  }
  runner <- .runner %||% function(experiments, ...) {
    LLMR::call_llm_par(experiments, ...)
  }

  rows <- list()
  for (p in seq_len(nrow(panel))) {
    items <- instr$items
    if ("item_order" %in% instr$randomize) items <- sample(items)
    for (it in items) {
      opts <- it$options
      if (!is.null(opts) && "option_order" %in% instr$randomize) {
        opts <- sample(opts)
      }
      sys <- paste(
        "You are answering a survey strictly in character as the following",
        "person, with their views, not yours.",
        sprintf("PERSONA: %s", panel$persona[p]),
        if (!is.null(opts))
          "Reply with exactly one of the listed options, nothing else."
        else "Answer briefly, in character.",
        sep = "\n")
      usr <- if (is.null(opts)) it$text else
        paste0(it$text, "\nOptions: ", paste(opts, collapse = " | "))
      rows[[length(rows) + 1L]] <- tibble::tibble(
        persona_id = panel$persona_id[p],
        item_id = it$id, type = it$type,
        option_order = if (is.null(opts)) NA_character_
                       else paste(opts, collapse = "|"),
        config = list(config),
        messages = list(c(system = sys, user = usr)))
    }
  }
  exps <- do.call(rbind, rows)
  res <- runner(exps, ...)
  stopifnot(is.data.frame(res), "response_text" %in% names(res))

  item_index <- stats::setNames(instr$items,
                                vapply(instr$items, `[[`, "", "id"))
  res$response <- vapply(seq_len(nrow(res)), function(i) {
    it <- item_index[[res$item_id[i]]]
    raw <- res$response_text[i] %||% NA_character_
    if (identical(it$type, "open")) return(trimws(as.character(raw)))
    .match_option(raw, it$options)
  }, character(1))
  res$score <- vapply(seq_len(nrow(res)), function(i) {
    it <- item_index[[res$item_id[i]]]
    if (!identical(it$type, "likert")) return(NA_real_)
    as.numeric(match(res$response[i], it$options))
  }, numeric(1))

  out <- res[, c("persona_id", "item_id", "type", "option_order",
                 "response", "score")]
  out <- tibble::as_tibble(out)
  attr(out, "panel") <- panel
  attr(out, "instrument") <- instr
  attr(out, "calibration") <- NULL
  class(out) <- c("panel_responses", class(out))
  out
}

#' @export
print.panel_responses <- function(x, ...) {
  cal <- attr(x, "calibration")
  cat(sprintf("<panel_responses | %d persona(s) x %d item(s) | %d parse failure(s)>\n",
              length(unique(x$persona_id)), length(unique(x$item_id)),
              sum(is.na(x$response) & x$type != "open")))
  if (is.null(cal)) {
    cat(cli::format_inline(paste(
      "  {.strong UNCALIBRATED}: no benchmark comparison has been run.",
      "Read these as design-stage measurements of the model under these",
      "personas, not as estimates of any human population. See calibrate().")),
      "\n")
  } else if (cal$items_covered < cal$items_total) {
    cat(cli::format_inline(paste0(
      "  {.strong PARTIALLY CALIBRATED} (", cal$items_covered, "/",
      cal$items_total, " items, vs '", cal$benchmark_name,
      "'): mean abs. deviation ", sprintf("%.3f", cal$mad),
      " on covered items; the rest remain design-stage readings.")), "\n")
  } else {
    cat(sprintf("  calibrated against '%s' (%d/%d items): mean abs. deviation %.3f (max %.3f)\n",
                cal$benchmark_name, cal$items_covered, cal$items_total,
                cal$mad, cal$max_dev))
  }
  invisible(x)
}
