# 00_package.R ----------------------------------------------------------------

#' Survey and experiment design with language model persona panels
#'
#' LLMRpanel administers survey and experimental instruments to panels of
#' language model personas. [panel_from_margins()] draws attributes from
#' supplied margins. [panel_from_data()] samples microdata rows, and
#' [panel_from_personas()] uses a prepared persona data frame, such as the
#' bundled [anes_2024_personas].
#'
#' [panel_instrument()] combines survey items into an instrument.
#' [conjoint_design()] and [conjoint_instrument()] create forced-choice
#' conjoint tasks.
#'
#' [panel_administer()] sends an independent request for each persona-item
#' pair. It records the item's fixed instrument position and the displayed
#' option order. Choice options can be permuted, and Likert scales can be
#' displayed in their original or reversed order.
#' [panel_batch_submit()] submits administrations through a provider's
#' batch API.
#'
#' [panel_benchmark()] compares response shares with a supplied benchmark.
#' [panel_bias_audit()] counts parse failures and tests first-option
#' sensitivity. [conjoint_amce()] estimates conjoint effects from the
#' recorded respondent-level profile assignments.
#'
#' @keywords internal
#' @importFrom rlang %||% abort
"_PACKAGE"

#' Shared generic methods
#'
#' LLMRpanel provides `LLMR::diagnostics()`, `LLMR::report()`, and `plot()`
#' methods for `panel_responses` objects. It provides `tibble::as_tibble()`
#' methods for `panel_responses` and `silicon_panel` objects.
#'
#' @return `diagnostics()` returns the [panel_bias_audit()] tibble, one row per
#'   item, with four columns that repeat on every row: `benchmark_state`
#'   (`"NOT BENCHMARKED"`, `"PARTIALLY BENCHMARKED"`, or `"BENCHMARKED"`),
#'   `items_covered` and `items_total`, the numbers of closed items covered by
#'   the benchmark and administered, and `mean_abs_dev`, the benchmark's mean
#'   absolute deviation, or `NA` when there is no benchmark.
#'
#'   `report()` returns a `panel_report` object: a character vector that
#'   prints one line per element. The lines give the benchmark state, the
#'   panel's source and demographic fields, the response and failure counts,
#'   and each item's first-option sensitivity.
#'
#'   `as_tibble()` returns a tibble. For `panel_responses` it is the `data`
#'   field, one row per persona-item pair. For a `silicon_panel` it is the
#'   persona table without the `silicon_panel` class and without the
#'   `margins`, `source`, and `source_rows` attributes.
#'
#' @name panel_generics
#' @aliases diagnostics.panel_responses report.panel_responses
#'   as_tibble.silicon_panel as_tibble.panel_responses
NULL

utils::globalVariables(c("item_id", "response", "share", "share_human",
                         "share_silicon", "response_key", "series", "run",
                         "call", "duration"))

# A NULL binding for base::requireNamespace so the test suite can substitute it
# (testthat's documented route for mocking base functions: the namespace is
# locked under R CMD check, so the binding must exist beforehand). At run time
# R's function-call lookup skips the NULL and finds the base function.
requireNamespace <- NULL

# Internal: literal placeholder substitution ({var} -> value), brace-safe and
# single-pass, so a substituted value that itself contains "{something}" is
# never re-substituted.
.fill <- function(template, values) {
  hits <- gregexpr("\\{[^{}]+\\}", template)
  tags <- regmatches(template, hits)[[1]]
  if (!length(tags)) return(template)
  keys <- substring(tags, 2L, nchar(tags) - 1L)
  replacements <- vapply(keys, function(key) {
    if (key %in% names(values)) as.character(values[[key]])
    else paste0("{", key, "}")
  }, character(1))
  regmatches(template, hits) <- list(unname(replacements))
  template
}

# Internal: normalize a reply to one of the offered options. The first pass
# matches the trimmed reply exactly, then case-insensitively. A second pass
# strips surrounding quotation marks and trailing sentence punctuation
# ("Agree.", '"agree"') and retries; it never runs when the first pass
# succeeds, so options that themselves carry punctuation still match verbatim.
.match_option <- function(x, options) {
  x <- trimws(as.character(x))
  if (x %in% options) return(x)
  hit <- match(tolower(x), tolower(options))
  if (!is.na(hit)) return(options[hit])
  quo <- "[\"'`\u2018\u2019\u201c\u201d]+"
  y <- gsub(paste0("^", quo, "|", quo, "$"), "", x)
  y <- trimws(sub("[.!,]+$", "", y))
  if (nzchar(y) && !identical(y, x)) {
    if (y %in% options) return(y)
    hit <- match(tolower(y), tolower(options))
    if (!is.na(hit)) return(options[hit])
  }
  NA_character_
}
