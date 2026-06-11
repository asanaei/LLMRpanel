# 00_package.R ----------------------------------------------------------------

#' LLMRpanel: calibrated silicon samples for survey and experiment design
#'
#' Quantitative silicon sampling with the methodological stance baked into
#' the objects:
#'
#' - [panel_from_margins()]: persona panels drawn from population margins
#'   you supply (ACS/ANES-style), with the persona text rendered from a
#'   template.
#' - [panel_instrument()] with [item_likert()], [item_choice()], [item_open()];
#'   [vignette_design()] and [conjoint_design()] for factorial stimuli.
#' - [panel_from_data()]: the joint-distribution counterpart, drawing
#'   personas from microdata rows.
#' - [panel_administer()]: every persona answers every item, with item- and
#'   option-order randomization recorded per response.
#' - [panel_calibrate()]: compare silicon marginals to human benchmarks; until it
#'   runs, every print method shows an **UNCALIBRATED** banner.
#' - [panel_bias_audit()]: option-order effects and refusal/parse rates -- the
#'   response-style artifacts silicon respondents are known for.
#' - [conjoint_instrument()] and [amce()]: forced-choice conjoint items and
#'   their average marginal component effects, with respondent-clustered
#'   standard errors.
#' - [panel_power()]: analytic two-arm power for the planned human study,
#'   priced from the silicon pilot.
#' - [panel_report()]: the design-stage report, banner included.
#'
#' The stance, printed rather than preached: silicon panels are instruments
#' for the design stage -- pretesting questionnaires, piloting vignette and
#' conjoint designs, stress-testing instruments -- and for measuring model
#' behavior. They estimate human population quantities only to the extent
#' that calibration against human data earns that reading, case by case.
#'
#' @keywords internal
#' @importFrom rlang %||% abort
"_PACKAGE"

#' Shared generic methods
#'
#' LLMRpanel registers methods for `LLMR::diagnostics()`, `LLMR::report()`,
#' and `tibble::as_tibble()` on panel objects.
#'
#' @name panel_generics
#' @aliases diagnostics.panel_responses report.panel_responses
#'   as_tibble.silicon_panel as_tibble.panel_responses
NULL

utils::globalVariables(c("item_id", "response"))

# Internal: literal placeholder substitution ({var} -> value), brace-safe.
.fill <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    out <- gsub(paste0("{", nm, "}"), as.character(values[[nm]]), out,
                fixed = TRUE)
  }
  out
}

# Internal: normalize a reply to one of the offered options.
.match_option <- function(x, options) {
  x <- trimws(as.character(x))
  if (x %in% options) return(x)
  hit <- match(tolower(x), tolower(options))
  if (!is.na(hit)) return(options[hit])
  NA_character_
}
