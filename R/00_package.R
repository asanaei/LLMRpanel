# 00_package.R ----------------------------------------------------------------

#' Survey and experiment design with language model persona panels
#'
#' LLMRpanel administers survey and experimental instruments to panels of
#' language model personas. [panel_from_margins()] draws persona attributes from
#' supplied margins. [panel_from_data()] samples microdata rows, and
#' [panel_from_personas()] uses a prepared persona data frame.
#'
#' Build instruments with [panel_instrument()] and the item constructors.
#' [conjoint_design()] and [conjoint_instrument()] create forced-choice conjoint
#' tasks.
#' [panel_administer()] records the item and option order used for each response.
#' [panel_batch_submit()] submits larger administrations through a provider's
#' batch API.
#'
#' [panel_benchmark()] compares response shares with a supplied benchmark.
#' [panel_bias_audit()] counts parse failures and tests first-option sensitivity.
#' [conjoint_amce()] estimates conjoint effects. [panel_power()] calculates
#' two-arm sample sizes from pilot dispersion. [LLMR::report()] summarizes an
#' administration.
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

# Internal: literal placeholder substitution ({var} -> value), brace-safe.
.fill <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    out <- gsub(paste0("{", nm, "}"), as.character(values[[nm]]), out,
                fixed = TRUE)
  }
  out
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
