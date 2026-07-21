# administer.R --------------------------------------------------------------------
# Administration: each persona answers each item. Item and option order can be
# randomized per response and are recorded with the result.

#' Administer an instrument to a panel
#'
#' Creates one request for each combination of persona and item. Persona text
#' is placed in the system message, and item text and options in the user
#' message. Closed-item replies are matched to offered options; unmatched
#' replies are recorded as `NA`. Open-item replies are returned as text.
#'
#' @param panel A [panel_from_margins()], [panel_from_data()], or
#'   [panel_from_personas()] result.
#' @param instrument A [panel_instrument()].
#' @param config An `LLMR::llm_config()` for a generative model.
#' @param max_calls Integer. If the run would make more than this many calls
#'   (personas times items), it stops unless `confirm = TRUE`, so a large panel
#'   cannot fire thousands of calls by accident. Default 5000.
#' @param confirm Logical. Set `TRUE` to proceed past `max_calls`.
#' @param price_table,tokens_per_call Optional. When both are supplied, the
#'   preflight reports a cost figure computed from your own `price_table` (the
#'   [LLMR::llm_usage()] format: columns `model`, `input`, `output`, prices per
#'   million tokens) and your `tokens_per_call` assumption -- either one number
#'   (total tokens per call, priced as a range from all-input to all-output) or
#'   two, `c(input, output)` (priced exactly). The package itself ships no
#'   prices and estimates no token counts.
#' @param .runner Optional runner for offline or deterministic testing: a
#'   `function(experiments, ...)` that receives a data frame with `config` and
#'   `messages` list-columns and returns those rows with at least a
#'   `response_text` column. Defaults to a live LLM call via
#'   `LLMR::call_llm_par()`.
#' @param ... Passed to the runner (e.g. `tries`, `progress`).
#' @return A `panel_responses` tibble: `persona_id`, `item_id`, `type`,
#'   `item_position` (the 1-based position at which this respondent saw the
#'   item), `option_order` (what this respondent saw, `|`-separated), `response`
#'   (matched option or `NA`; verbatim text for open items), `score`
#'   (1-based scale position for Likert items). `score` is the position of the
#'   chosen response in the item's canonical scale, the order in which the levels
#'   were defined, not the position in the shuffled order this respondent saw
#'   (`option_order`); randomizing the display therefore does not change the
#'   score. `response_text`, `response_id`, `success`, `error_message`,
#'   `finish_reason`, `model`, and `provider` retain execution provenance as
#'   ordinary columns. Conjoint administrations also include a `profiles`
#'   list-column; each element is the task profile table that respondent saw.
#'   The panel and instrument are attached as attributes. The benchmark
#'   attribute is `NULL` until [panel_benchmark()] is called.
#' @examples
#' set.seed(110)   # the panel draw is local; the model call is not
#' panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 6)
#' instrument <- panel_instrument(
#'   item_likert("wk4", "A four-day work week would benefit society."),
#'   randomize = character(0))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' \dontrun{
#' resp <- panel_administer(panel, instrument, cfg)
#' resp
#' }
#'
#' # The `.runner` seam answers without a provider, for tests or for a
#' # deterministic or external respondent:
#' deterministic <- function(experiments, ...) {
#'   experiments$response_text <- "agree"
#'   experiments
#' }
#' panel_administer(panel, instrument, cfg, .runner = deterministic)
#' @export
panel_administer <- function(panel, instrument, config, max_calls = 5000L,
                             confirm = FALSE, price_table = NULL,
                             tokens_per_call = NULL, .runner = NULL, ...) {
  stopifnot(inherits(panel, "silicon_panel"),
            inherits(instrument, "panel_instrument"))
  if (!inherits(config, "llm_config")) {
    abort("`config` must be an LLMR::llm_config().")
  }
  runner <- .runner %||% function(experiments, ...) {
    LLMR::call_llm_par(experiments, ...)
  }

  exps <- .panel_build_grid(panel, instrument, config)
  .panel_preflight(nrow(exps), max_calls, confirm, price_table, tokens_per_call,
                   model = config$model)

  res <- runner(exps, ...)
  stopifnot(is.data.frame(res), "response_text" %in% names(res))
  # The parallel runner returns the input columns alongside response_text, so the
  # grid metadata is already aligned by row; parse directly.
  .panel_parse_responses(res, instrument, panel)
}

# --- internal: grid build, response parse, and preflight (shared) ----------
# Build the persona x item experiment grid. Each row carries the metadata needed
# to parse a response back (persona_id, item_id, type, option_order) plus a stable
# `request_id` used to realign asynchronous (batch) results by id, never by order.
.panel_build_grid <- function(panel, instrument, config) {
  rows <- list()
  has_conjoint <- any(vapply(instrument$items, function(item) {
    !is.null(item$conjoint)
  }, logical(1)))
  for (p in seq_len(nrow(panel))) {
    items <- instrument$items
    if ("item_order" %in% instrument$randomize) items <- sample(items)
    for (j in seq_along(items)) {
      it <- items[[j]]
      profiles <- NULL
      item_text <- it$text
      if (!is.null(it$conjoint)) {
        spec <- it$conjoint
        profiles <- .draw_conjoint_task(
          spec$attributes, spec$task, spec$profiles)
        attr_cols <- names(spec$attributes)
        lines <- vapply(seq_len(nrow(profiles)), function(i) {
          vals <- vapply(attr_cols, function(a) {
            as.character(profiles[[a]][i])
          }, character(1))
          sprintf("Profile %s: %s.", profiles$profile[i],
                  paste(sprintf("%s: %s", attr_cols, vals), collapse = "; "))
        }, character(1))
        item_text <- paste0(item_text, "\n\n", paste(lines, collapse = "\n"))
      }
      opts <- it$options
      if (!is.null(opts) && "option_order" %in% instrument$randomize) {
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
      usr <- if (is.null(opts)) item_text else
        paste0(item_text, "\nOptions: ", paste(opts, collapse = " | "))
      row <- tibble::tibble(
        persona_id = panel$persona_id[p],
        item_id = it$id, type = it$type,
        item_position = j,
        option_order = if (is.null(opts)) NA_character_
                       else paste(opts, collapse = "|"),
        model = as.character(config$model),
        provider = as.character(config$provider),
        config = list(config),
        messages = list(c(system = sys, user = usr)))
      if (has_conjoint) row$profiles <- list(profiles)
      rows[[length(rows) + 1L]] <- row
    }
  }
  exps <- do.call(rbind, rows)
  exps$request_id <- sprintf("llmr-%06d", seq_len(nrow(exps)))
  exps
}

# Parse a results frame (`res`, carrying the grid metadata columns + a
# `response_text` column) into a `panel_responses`. Shared by the parallel and
# batch paths so they produce byte-identical output. Token columns present on
# `res` are retained as a usage attribute.
.panel_parse_responses <- function(res, instrument, panel) {
  stopifnot(is.data.frame(res), "response_text" %in% names(res))
  res$response_text <- as.character(res$response_text)
  if (!"response_id" %in% names(res)) res$response_id <- NA_character_
  if (!"success" %in% names(res)) res$success <- NA
  if (!"error_message" %in% names(res)) res$error_message <- NA_character_
  if (!"finish_reason" %in% names(res)) res$finish_reason <- NA_character_
  if (!"model" %in% names(res)) res$model <- NA_character_
  if (!"provider" %in% names(res)) res$provider <- NA_character_
  res$response_id <- as.character(res$response_id)
  res$success <- as.logical(res$success)
  res$error_message <- as.character(res$error_message)
  res$finish_reason <- as.character(res$finish_reason)
  res$model <- as.character(res$model)
  res$provider <- as.character(res$provider)

  item_index <- stats::setNames(instrument$items,
                                vapply(instrument$items, `[[`, "", "id"))
  res$response <- vapply(seq_len(nrow(res)), function(i) {
    it <- item_index[[res$item_id[i]]]
    raw <- res$response_text[i] %||% NA_character_
    if (identical(res$success[i], FALSE)) return(NA_character_)
    if (identical(it$type, "open")) return(trimws(as.character(raw)))
    .match_option(raw, it$options)
  }, character(1))
  res$score <- vapply(seq_len(nrow(res)), function(i) {
    it <- item_index[[res$item_id[i]]]
    if (!identical(it$type, "likert")) return(NA_real_)
    as.numeric(match(res$response[i], it$options))
  }, numeric(1))

  keep <- c("persona_id", "item_id", "type", "item_position",
            "option_order", intersect("profiles", names(res)),
            "response_text", "response_id", "success", "error_message",
            "finish_reason", "model", "provider",
            "response", "score")
  out <- res[, keep]
  out <- tibble::as_tibble(out)

  token_cols <- intersect(
    c("sent_tokens", "rec_tokens", "total_tokens", "reasoning_tokens",
      "cached_tokens"), names(res))
  if (length(token_cols)) {
    keep <- intersect(
      c("persona_id", "item_id", "response_text", "response_id", "success",
        "error_message", "finish_reason", "model", "provider", token_cols),
      names(res))
    u <- res[, keep, drop = FALSE]
    attr(out, "usage") <- tibble::as_tibble(u)
  }

  attr(out, "panel") <- panel
  attr(out, "instrument") <- instrument
  attr(out, "benchmark") <- NULL
  class(out) <- c("panel_responses", class(out))
  out
}

# Report the call count and, optionally, gate a large run. Returns invisibly.
.panel_preflight <- function(n_calls, max_calls, confirm, price_table = NULL,
                             tokens_per_call = NULL, model = NULL) {
  msg <- sprintf("%d call(s)", n_calls)
  priced <- !is.null(price_table) && !is.null(tokens_per_call)
  if (priced) {
    # caller-supplied arithmetic only; the package invents no prices.
    msg <- paste0(msg,
                  .panel_cost_estimate(n_calls, price_table, tokens_per_call,
                                       model))
  }
  # Announce non-trivial runs, and any run whose caller asked for the cost.
  if (n_calls >= 100L || priced) cli::cli_inform("Administering: {msg}.")
  if (n_calls > max_calls && !isTRUE(confirm)) {
    abort(sprintf(paste0(
      "This run would make %d calls, above max_calls = %d. ",
      "Pass confirm = TRUE to proceed, or raise max_calls."), n_calls, max_calls))
  }
  invisible(n_calls)
}

# Cost arithmetic for the preflight, entirely from caller-supplied numbers.
# `price_table` follows the LLMR::llm_usage() format (columns model, input,
# output; prices per million tokens); `tokens_per_call` is either one number
# (total tokens per call, priced as a range from all-input to all-output) or two
# numbers c(input, output) per call (priced exactly). Returns a string to append
# to the preflight message, or "" when the model has no row in the table.
.panel_cost_estimate <- function(n_calls, price_table, tokens_per_call, model) {
  if (!is.data.frame(price_table) ||
      !all(c("model", "input", "output") %in% names(price_table))) {
    abort(paste("`price_table` must be a data frame with columns model, input,",
                "output (prices per million tokens), the LLMR::llm_usage() format."))
  }
  if (!is.numeric(tokens_per_call) || anyNA(tokens_per_call) ||
      any(tokens_per_call < 0) || !(length(tokens_per_call) %in% 1:2)) {
    abort(paste("`tokens_per_call` must be one nonnegative number (total tokens",
                "per call) or two, c(input, output)."))
  }
  idx <- if (!is.null(model)) match(model, price_table$model) else NA_integer_
  if (is.na(idx) && nrow(price_table) == 1L) idx <- 1L
  if (is.na(idx)) {
    cli::cli_warn(paste(
      "Model {.val {model}} has no row in `price_table`;",
      "no cost figure is reported."))
    return("")
  }
  p_in <- as.numeric(price_table$input[idx])
  p_out <- as.numeric(price_table$output[idx])
  if (length(tokens_per_call) == 2L) {
    tk <- tokens_per_call
    if (!is.null(names(tk)) && all(c("input", "output") %in% names(tk))) {
      tk <- tk[c("input", "output")]
    }
    cost <- n_calls * (tk[[1]] * p_in + tk[[2]] * p_out) / 1e6
    sprintf(" (~%s tokens/call; est. cost %.4g by your price_table)",
            format(sum(tk)), cost)
  } else {
    # one total-token figure: the input/output split is unknown, so report the
    # range from all-input to all-output pricing.
    total <- n_calls * tokens_per_call / 1e6
    lo <- total * min(p_in, p_out)
    hi <- total * max(p_in, p_out)
    sprintf(" (~%s tokens/call; est. cost %.4g-%.4g by your price_table)",
            format(tokens_per_call), lo, hi)
  }
}

#' @export
print.panel_responses <- function(x, ...) {
  benchmark <- attr(x, "benchmark")
  execution_failures <- sum(x$success %in% FALSE)
  parse_failures <- sum(is.na(x$response) & x$type != "open" &
                          !(x$success %in% FALSE))
  cat(sprintf(paste0(
    "<panel_responses | %d persona(s) x %d item(s) | ",
    "%d execution failure(s), %d parse failure(s)>\n"),
              length(unique(x$persona_id)), length(unique(x$item_id)),
              execution_failures, parse_failures))
  if (is.null(benchmark)) {
    cat(cli::format_inline(paste(
      "  {.strong NOT BENCHMARKED}: no benchmark comparison has been run.",
      "Read these as design-stage measurements of the model under these",
      "personas, not as estimates of any human population. See panel_benchmark().")),
      "\n")
  } else if (benchmark$items_covered < benchmark$items_total) {
    cat(cli::format_inline(paste0(
      "  {.strong PARTIALLY BENCHMARKED} (", benchmark$items_covered, "/",
      benchmark$items_total, "): vs '", benchmark$benchmark_name,
      "', mean abs. deviation ", sprintf("%.3f", benchmark$mean_abs_dev),
      " on covered items; the rest remain design-stage readings.")), "\n")
  } else {
    cat(sprintf("  BENCHMARKED against '%s' (%d/%d items): mean abs. deviation %.3f (max %.3f)\n",
                benchmark$benchmark_name, benchmark$items_covered,
                benchmark$items_total, benchmark$mean_abs_dev,
                benchmark$max_dev))
  }
  invisible(x)
}

#' @export
`[.panel_responses` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod("[")
  attr(out, "panel") <- NULL
  attr(out, "instrument") <- NULL
  attr(out, "benchmark") <- NULL
  attr(out, "usage") <- NULL
  class(out) <- setdiff(class(out), "panel_responses")
  out
}

#' @exportS3Method tibble::as_tibble
as_tibble.panel_responses <- function(x, ...) {
  attr(x, "panel") <- NULL
  attr(x, "instrument") <- NULL
  attr(x, "benchmark") <- NULL
  class(x) <- setdiff(class(x), "panel_responses")
  tibble::as_tibble(x, ...)
}

#' Token usage for an administered panel
#'
#' Summarizes token and outcome diagnostics recorded by [panel_administer()] or
#' [panel_batch_fetch()]. The diagnostics are stored in the `usage`
#' attribute of a `panel_responses` object and summarized by [LLMR::llm_usage()].
#' Model and provider remain in the returned frame. A supplied `price_table`
#' adds a cost column. The package contains no price table.
#'
#' @param responses A [panel_administer()] result.
#' @param price_table Optional price table passed to [LLMR::llm_usage()].
#' @return A one-row usage tibble, or a typed empty tibble when the runner
#'   returned no token columns.
#' @seealso [panel_administer()], [LLMR::llm_usage()].
#' @export
panel_usage <- function(responses, price_table = NULL) {
  stopifnot(inherits(responses, "panel_responses"))
  u <- attr(responses, "usage")
  if (is.null(u) || !nrow(u)) {
    out <- tibble::tibble(
      model = character(), provider = character(), n = integer(),
      n_ok = integer(), n_failed = integer(), ok_rate = numeric(),
      n_truncated = integer(), n_filtered = integer(), sent_tokens = integer(),
      rec_tokens = integer(), total_tokens = integer(),
      reasoning_tokens = integer(), cached_tokens = integer(),
      n_unknown_tokens = integer(), duration_s = numeric(),
      rowpack_calls = integer(), rows_per_rowpack = numeric())
    if (!is.null(price_table)) out$cost_estimate <- numeric()
    return(out)
  }
  out <- LLMR::llm_usage(u, price_table = price_table)
  one_value <- function(x) {
    value <- unique(stats::na.omit(as.character(x)))
    if (length(value)) value[[1]] else NA_character_
  }
  tibble::add_column(
    out, model = one_value(u$model), provider = one_value(u$provider),
    .before = 1)
}
