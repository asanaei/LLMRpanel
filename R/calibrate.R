# calibrate.R --------------------------------------------------------------------
# Calibration and bias audits: the part of silicon sampling that is usually
# skipped, here made the precondition for reading results as anything more
# than model behavior.

#' Calibrate silicon responses against a human benchmark
#'
#' Compares the panel's response marginals, item by item, to human
#' benchmark marginals you supply (from ANES, GSS, Pew, your own fielded
#' study). Returns the responses with the calibration slot filled -- which
#' is what turns off the UNCALIBRATED banner -- plus the comparison table.
#' Deviation is reported, never hidden: calibration here means *measured
#' against*, not *adjusted until pretty*.
#'
#' @param responses An [administer()] result.
#' @param benchmark A data frame with columns `item_id`, `response`, and
#'   `share` (human marginal proportions; shares within an item should sum
#'   to 1).
#' @param benchmark_name How the source should be cited in reports (e.g.
#'   `"ANES 2024 pilot"`).
#' @return `responses` with the calibration attribute set; the comparison
#'   table is in `attr(x, "calibration")$table` (columns: `item_id`,
#'   `response`, `share_silicon`, `share_human`, `deviation`).
#' @export
calibrate <- function(responses, benchmark, benchmark_name = "benchmark") {
  stopifnot(inherits(responses, "panel_responses"), is.data.frame(benchmark))
  need <- c("item_id", "response", "share")
  if (!all(need %in% names(benchmark))) {
    abort("`benchmark` needs columns item_id, response, share.")
  }
  closed <- responses[responses$type != "open" & !is.na(responses$response), ]
  if (!nrow(closed)) abort("No closed-item responses to calibrate.")
  sil <- stats::aggregate(persona_id ~ item_id + response, data = closed,
                          FUN = length)
  names(sil)[names(sil) == "persona_id"] <- "n"
  totals <- stats::aggregate(n ~ item_id, data = sil, FUN = sum)
  names(totals)[names(totals) == "n"] <- "n_total"
  sil <- merge(sil, totals, by = "item_id")
  sil$share_silicon <- sil$n / sil$n_total

  cmp <- merge(sil[, c("item_id", "response", "share_silicon")],
               stats::setNames(benchmark[, need],
                               c("item_id", "response", "share_human")),
               by = c("item_id", "response"), all = TRUE)
  cmp$share_silicon[is.na(cmp$share_silicon)] <- 0
  cmp$share_human[is.na(cmp$share_human)] <- 0
  cmp$deviation <- cmp$share_silicon - cmp$share_human

  attr(responses, "calibration") <- list(
    benchmark_name = benchmark_name,
    table = tibble::as_tibble(cmp),
    mad = mean(abs(cmp$deviation)),
    max_dev = max(abs(cmp$deviation)),
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  responses
}

#' Audit silicon response style
#'
#' The artifacts silicon respondents are known for, measured from the
#' responses themselves:
#'
#' - **Option-order effects**: for items administered with randomized
#'   option order, a chi-squared test of response against the order seen.
#'   With LLMs this is routinely significant; a result that survives
#'   [LLMRvalid](https://github.com/asanaei/LLMRvalid)-style scrutiny
#'   should not depend on it.
#' - **Non-response**: parse failures and refusals per item.
#'
#' Acquiescence scoring (reverse-keyed item pairs) arrives in 0.2 alongside
#' instrument metadata for keying.
#'
#' @param responses An [administer()] result.
#' @return A tibble: `item_id`, `n`, `parse_failures`, `order_effect_p`
#'   (NA when order was not randomized or cells are too sparse).
#' @export
bias_audit <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  items <- split(responses, responses$item_id)
  out <- lapply(items, function(ri) {
    closed <- ri$type[1] != "open"
    pf <- if (closed) sum(is.na(ri$response)) else 0L
    p <- NA_real_
    if (closed && length(unique(stats::na.omit(ri$option_order))) > 1L) {
      first_seen <- vapply(strsplit(ri$option_order, "|", fixed = TRUE),
                           `[[`, "", 1L)
      ok <- !is.na(ri$response)
      if (sum(ok) >= 4L && length(unique(ri$response[ok])) > 1L) {
        tab <- table(ri$response[ok], first_seen[ok])
        if (all(dim(tab) >= 2L)) {
          p <- tryCatch(
            suppressWarnings(stats::chisq.test(tab)$p.value),
            error = function(e) NA_real_)
        }
      }
    }
    tibble::tibble(item_id = ri$item_id[1], n = nrow(ri),
                   parse_failures = pf, order_effect_p = p)
  })
  do.call(rbind, out)
}

#' Power analysis with silicon-informed priors (arrives in 0.2)
#'
#' Uses pilot silicon responses to put realistic priors on effect sizes and
#' design effects, then simulates power for the *human* study being planned
#' -- the use of silicon panels with the clearest payoff per dollar.
#'
#' @param responses An [administer()] result (the silicon pilot).
#' @param effect The design and effect specification (under design).
#' @return Will return simulated power curves.
#' @section Status: Design contract, arrives in 0.2.
#' @export
silicon_power <- function(responses, effect) {
  stopifnot(inherits(responses, "panel_responses"))
  abort("silicon_power() arrives in LLMRpanel 0.2; the signature is the contract.")
}

#' AMCEs from conjoint administrations (arrives in 0.2)
#'
#' Average marginal component effects for [conjoint_design()] runs,
#' delegating estimation to the established conjoint stack
#' (cregg/marginaleffects) rather than reimplementing it.
#'
#' @param responses An [administer()] result from a conjoint instrument.
#' @return Will return tidy AMCE estimates.
#' @section Status: Design contract, arrives in 0.2.
#' @export
amce <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  abort("amce() arrives in LLMRpanel 0.2, delegating to cregg/marginaleffects.")
}

#' The design-stage report
#'
#' Panel composition, response and parse rates, the bias audit, and --
#' first, in capitals, when absent -- the calibration status.
#'
#' @param responses An [administer()] result.
#' @return Character lines of class `panel_report`, with a print method.
#' @export
panel_report <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  panel <- attr(responses, "panel")
  cal <- attr(responses, "calibration")
  ba <- bias_audit(responses)
  lines <- c(
    if (is.null(cal))
      "UNCALIBRATED. No benchmark comparison was run; readings below describe the model under these personas, not any human population."
    else
      sprintf("CALIBRATION. Against '%s': mean absolute deviation %.3f, max %.3f (full table in attr(x,'calibration')$table).",
              cal$benchmark_name, cal$mad, cal$max_dev),
    sprintf("PANEL. %d persona(s) drawn from margins over: %s.",
            nrow(panel), paste(names(attr(panel, "margins")), collapse = ", ")),
    sprintf("RESPONSES. %d total; %d parse failure(s).",
            nrow(responses), sum(ba$parse_failures)),
    "ORDER EFFECTS (chi-squared p by item; small p = the order shown moved the answers):",
    sprintf("  %-12s n = %3d  parse failures = %2d  order p = %s",
            ba$item_id, ba$n, ba$parse_failures,
            ifelse(is.na(ba$order_effect_p), "n/a",
                   format(round(ba$order_effect_p, 4)))),
    "STANCE. Silicon panels are design-stage instruments; estimation of human quantities requires calibration to carry that reading."
  )
  structure(lines, class = "panel_report")
}

#' @export
print.panel_report <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n")
  invisible(x)
}
