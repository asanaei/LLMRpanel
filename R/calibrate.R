# calibrate.R --------------------------------------------------------------------
# Calibration and bias audits: the part of silicon sampling that is usually
# skipped, here made the precondition for reading results as anything more
# than model behavior.

#' Calibrate silicon responses against a human benchmark
#'
#' Compares the panel's response marginals, item by item, to human
#' benchmark marginals you supply (from ANES, GSS, Pew, your own fielded
#' study). Calibration here reports deviation from the benchmark without
#' adjusting the underlying estimates: deviations are reported as found, and
#' the comparison is restricted to items the benchmark actually covers.
#' Coverage is partial when only some items have a benchmark, and the print
#' banner reflects it -- a benchmark touching one of five items yields
#' **PARTIALLY CALIBRATED (1/5)**. Nonresponse (parse failures, refusals) is
#' recorded per item alongside, since shares computed only over valid
#' responses flatter an instrument the model often refuses.
#'
#' @param responses A [panel_administer()] result.
#' @param benchmark A data frame with columns `item_id`, `response`, and
#'   `share` (human marginal proportions). Shares within an item should sum
#'   to 1; a deviation beyond rounding draws a warning.
#' @param benchmark_name How the source should be cited in reports (e.g.
#'   `"ANES 2024 pilot"`).
#' @return `responses` with the calibration attribute set:
#'   `$table` (per covered item and response: `share_silicon`,
#'   `share_human`, `deviation`), `$nonresponse` (per item),
#'   `$items_covered` / `$items_total`, `$mad`, `$max_dev`.
#' @examples
#' \dontrun{
#' set.seed(110)
#' panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 12)
#' instr <- panel_instrument(item_choice("plan", "Which plan do you prefer?",
#'                                       c("A", "B")))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instr, cfg)
#' r   # UNCALIBRATED banner
#' bench <- data.frame(item_id = "plan", response = c("A", "B"),
#'                     share = c(.5, .5))
#' panel_calibrate(r, bench, "toy human study")
#' }
#' @export
panel_calibrate <- function(responses, benchmark, benchmark_name = "benchmark") {
  stopifnot(inherits(responses, "panel_responses"), is.data.frame(benchmark))
  need <- c("item_id", "response", "share")
  if (!all(need %in% names(benchmark))) {
    abort("`benchmark` needs columns item_id, response, share.")
  }
  sums <- stats::aggregate(share ~ item_id, data = benchmark, FUN = sum)
  off <- sums$item_id[abs(sums$share - 1) > 0.01]
  if (length(off)) {
    cli::cli_warn("Benchmark shares do not sum to 1 for item(s): {.val {off}}.")
  }

  closed_all <- responses[responses$type != "open", ]
  if (!nrow(closed_all)) abort("No closed-item responses to calibrate.")
  items_total <- unique(closed_all$item_id)
  items_covered <- intersect(items_total, unique(benchmark$item_id))
  if (!length(items_covered)) {
    abort("The benchmark covers none of the administered items.")
  }

  # Benchmark response labels that are not among an item's offered options can
  # never match a silicon response (matching is by exact string), so their
  # silicon share is 0 by construction. That is a labeling problem, not a
  # finding; say so instead of letting a case difference or a typo masquerade
  # as total divergence.
  instr <- attr(responses, "instrument")
  if (!is.null(instr) && is.list(instr$items)) {
    for (id in items_covered) {
      it <- Find(function(x) identical(x$id, id), instr$items)
      if (is.null(it) || is.null(it$options)) next
      lev <- unique(as.character(benchmark$response[benchmark$item_id == id]))
      bad <- setdiff(lev, it$options)
      if (length(bad)) {
        cli::cli_warn(paste(
          "Benchmark response(s) {.val {bad}} for item {.val {id}} are not",
          "among its offered options ({.val {it$options}}); their silicon",
          "share is 0 by construction. Check case and spelling."))
      }
    }
  }

  nonresp <- do.call(rbind, lapply(split(closed_all, closed_all$item_id),
    function(ri) tibble::tibble(item_id = ri$item_id[1],
                                nonresponse_rate = mean(is.na(ri$response)))))

  closed <- closed_all[!is.na(closed_all$response) &
                         closed_all$item_id %in% items_covered, ]
  if (nrow(closed)) {
    sil <- stats::aggregate(persona_id ~ item_id + response, data = closed,
                            FUN = length)
    names(sil)[names(sil) == "persona_id"] <- "n"
    totals <- stats::aggregate(n ~ item_id, data = sil, FUN = sum)
    names(totals)[names(totals) == "n"] <- "n_total"
    sil <- merge(sil, totals, by = "item_id")
    sil$share_silicon <- sil$n / sil$n_total
  } else {
    # Every covered closed item was all parse failures: there are no valid
    # silicon responses to tabulate. Keep the calibration artifact rather than
    # erroring -- the benchmark merge below fills share_silicon = 0 and the
    # nonresponse table records the full nonresponse.
    sil <- data.frame(item_id = character(0), response = character(0),
                      n = integer(0), n_total = integer(0),
                      share_silicon = numeric(0), stringsAsFactors = FALSE)
  }

  bench_cov <- benchmark[benchmark$item_id %in% items_covered, need]
  cmp <- merge(sil[, c("item_id", "response", "share_silicon")],
               stats::setNames(bench_cov,
                               c("item_id", "response", "share_human")),
               by = c("item_id", "response"), all = TRUE)
  cmp$share_silicon[is.na(cmp$share_silicon)] <- 0
  cmp$share_human[is.na(cmp$share_human)] <- 0
  cmp$deviation <- cmp$share_silicon - cmp$share_human

  attr(responses, "calibration") <- list(
    benchmark_name = benchmark_name,
    table = tibble::as_tibble(cmp),
    nonresponse = tibble::as_tibble(nonresp),
    items_covered = length(items_covered),
    items_total = length(items_total),
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
#' - **First-option sensitivity**: for items administered with randomized
#'   option order, a chi-squared test of the chosen response against which
#'   option was listed first. This is a narrow slice of the broader question of
#'   option-order effects: it asks whether the answer depends on what appeared in
#'   position one, not on the full permutation of positions. With LLMs even this
#'   reduced signal is routinely significant; a result that survives
#'   [LLMRcontent](https://github.com/asanaei/LLMRcontent)-style scrutiny should
#'   not depend on it.
#' - **Non-response**: parse failures and refusals per item.
#'
#' @param responses A [panel_administer()] result.
#' @return A tibble: `item_id`, `n`, `parse_failures`, `order_effect_p`
#'   (the first-option chi-squared p-value; NA when order was not randomized or
#'   cells are too sparse).
#' @export
panel_bias_audit <- function(responses) {
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
  tibble::as_tibble(do.call(rbind, out))
}

#' @exportS3Method LLMR::diagnostics
diagnostics.panel_responses <- function(x, ...) {
  out <- panel_bias_audit(x)
  cal <- attr(x, "calibration")
  if (is.null(cal)) {
    state <- "UNCALIBRATED"
    items_covered <- 0L
    items_total <- length(unique(x$item_id[x$type != "open"]))
    mad <- NA_real_
  } else {
    state <- if (cal$items_covered < cal$items_total) "PARTIAL" else "CALIBRATED"
    items_covered <- cal$items_covered
    items_total <- cal$items_total
    mad <- cal$mad %||% NA_real_
  }
  out$calibration_state <- state
  out$items_covered <- items_covered
  out$items_total <- items_total
  out$mad <- mad
  out
}

#' Two-arm power for the planned human study, priced from the silicon pilot
#'
#' Analytic two-arm sample sizes with dispersion priors taken from the
#' silicon responses: Likert items use the pilot standard deviation of
#' `score`; choice items use the pilot share of the modal option; open
#' items are skipped. The priors inherit the panel's calibration status --
#' an uncalibrated pilot prices the design stage, it does not certify
#' effect sizes.
#'
#' @param responses A [panel_administer()] result (the silicon pilot).
#' @param effect Raw minimum detectable difference between the two arms:
#'   scale points for Likert items, a difference in proportions for choice
#'   items. A scalar (recycled) or a named vector keyed by `item_id`.
#' @param items Optional character vector restricting which items to price.
#' @param focal Optional named character vector keyed by `item_id`, giving the
#'   focal response level whose proportion the power calculation should target.
#'   For a binary choice item the modal option is a well-defined estimand and
#'   `focal` is optional; for a choice item with three or more options the
#'   "modal share" is not a meaningful single proportion, so name the focal
#'   response here. Without a `focal` for a 3+-option item, the modal share is
#'   used and a warning is issued. A named focal that the pilot never elicited is
#'   still powered: as long as it is one of the item's options, the observed rate
#'   is taken as 0 (with a warning) rather than an error; only a focal that is not
#'   an offered option of the item is rejected.
#' @param alpha Two-sided test size.
#' @param power Target power.
#' @return A tibble: `item_id`, `type`, `dispersion` (sd for Likert, the focal
#'   or modal share for choice), `effect`, `n_per_arm`.
#' @examples
#' \dontrun{
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 8)
#' instr <- panel_instrument(list(
#'   item_likert("lik", "Rate the proposal.", scale = c("low", "mid", "high")),
#'   item_choice("pick", "Pick one.", c("A", "B"))),
#'   randomize = character(0))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instr, cfg)
#' panel_power(r, effect = c(lik = 0.5, pick = 0.2))
#' }
#' @export
panel_power <- function(responses, effect, items = NULL, focal = NULL,
                        alpha = 0.05, power = 0.80) {
  stopifnot(inherits(responses, "panel_responses"))
  if (!is.numeric(effect) || !length(effect)) {
    abort("`effect` must be a numeric scalar or named vector.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    abort("`alpha` must be a number between 0 and 1.")
  }
  if (!is.numeric(power) || length(power) != 1L || is.na(power) ||
      power <= 0 || power >= 1) {
    abort("`power` must be a number between 0 and 1.")
  }
  all_items <- unique(responses$item_id)
  if (is.null(items)) {
    items <- all_items
  } else {
    stopifnot(is.character(items))
    if (length(setdiff(items, all_items))) {
      abort("`items` contains item_id(s) not found in `responses`.")
    }
  }
  effect_for <- function(id) {
    nms <- names(effect)
    if (is.null(nms) || !any(nzchar(nms))) {
      if (length(effect) != 1L) {
        abort("`effect` must be scalar or a named vector by item_id.")
      }
      return(unname(effect[1]))
    }
    if (!(id %in% nms)) {
      abort("A named `effect` must include every analyzed item_id.")
    }
    unname(effect[match(id, nms)])
  }

  # The number of offered options decides whether "modal share" is a
  # well-defined estimand. Read it from the instrument when present (a pilot may
  # not exercise every option, so the observed table can undercount); fall back
  # to the observed options only when no instrument is attached.
  instr <- attr(responses, "instrument")
  offered_opts <- function(id) {
    if (is.null(instr) || is.null(instr$items)) return(NULL)
    it <- Find(function(x) identical(x$id, id), instr$items)
    if (is.null(it)) return(NULL)
    it$options
  }
  offered_n_opts <- function(id) {
    opts <- offered_opts(id)
    if (is.null(opts)) return(NA_integer_)
    length(opts)
  }

  z <- stats::qnorm(1 - alpha / 2) + stats::qnorm(power)
  rows <- list()
  for (id in items) {
    ri <- responses[responses$item_id == id, , drop = FALSE]
    type <- as.character(ri$type[1])
    if (identical(type, "open")) next
    eff <- effect_for(id)
    if (!is.finite(eff) || eff <= 0) {
      abort("`effect` must be positive for every analyzed item.")
    }
    if (identical(type, "likert")) {
      sigma <- stats::sd(ri$score, na.rm = TRUE)
      if (is.na(sigma) || sigma == 0) {
        cli::cli_warn("Item {.val {id}} shows no variance in the pilot; n_per_arm is NA.")
        n <- NA_integer_
      } else {
        n <- ceiling(2 * sigma^2 * z^2 / eff^2)
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        item_id = id, type = type, dispersion = sigma, effect = eff,
        n_per_arm = n)
    } else if (identical(type, "choice")) {
      valid <- ri$response[!is.na(ri$response)]
      if (!length(valid)) {
        cli::cli_warn("Item {.val {id}} has no valid pilot responses; n_per_arm is NA.")
        p <- NA_real_; n <- NA_integer_
      } else {
        tab <- table(valid)
        # Prefer the instrument's offered-option count; the observed table can
        # undercount when a pilot never elicits some option.
        n_opts_offered <- offered_n_opts(id)
        n_opts <- if (!is.na(n_opts_offered)) n_opts_offered else length(tab)
        focal_id <- if (!is.null(focal) && id %in% names(focal)) focal[[id]] else NULL
        if (!is.null(focal_id)) {
          if (!focal_id %in% names(tab)) {
            # A focal the pilot never elicited is informative for planning (a rare
            # response still needs powering), as long as it is a real option for
            # the item. Use a near-zero observed rate and let the arm-proportion
            # clamp below keep the sample-size finite. Only a focal that is not an
            # offered response at all is a usage error.
            opts <- offered_opts(id)
            if (!is.null(opts) && !(focal_id %in% opts)) {
              abort(sprintf(
                "`focal` for item '%s' ('%s') is not one of the item's options.",
                id, focal_id))
            }
            cli::cli_warn(paste(
              "Focal response {.val {focal_id}} for item {.val {id}} did not appear",
              "in the pilot; using an observed rate of 0. Interpret the power",
              "estimate cautiously."))
            p <- 0
          } else {
            p <- as.numeric(tab[[focal_id]] / sum(tab))
          }
        } else {
          if (n_opts > 2L) {
            cli::cli_warn(paste(
              "Item {.val {id}} has {n_opts} options; the modal share is not a",
              "well-defined estimand. Name a `focal` response for a meaningful",
              "power calculation. Using the modal share for now."))
          }
          p <- as.numeric(max(tab) / sum(tab))
        }
        if (p == 1) {
          cli::cli_warn("Item {.val {id}} shows no variance in the pilot; n_per_arm is NA.")
          n <- NA_integer_
        } else {
          p1 <- p - eff / 2; p2 <- p + eff / 2
          p1c <- min(max(p1, 0.005), 0.995)
          p2c <- min(max(p2, 0.005), 0.995)
          if (p1 != p1c || p2 != p2c) {
            cli::cli_warn("Arm proportions for item {.val {id}} were clamped to [0.005, 0.995].")
          }
          n <- ceiling(z^2 * (p1c * (1 - p1c) + p2c * (1 - p2c)) / eff^2)
        }
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        item_id = id, type = type, dispersion = p, effect = eff,
        n_per_arm = n)
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(item_id = character(), type = character(),
                          dispersion = numeric(), effect = numeric(),
                          n_per_arm = integer()))
  }
  tibble::as_tibble(do.call(rbind, rows))
}

#' AMCEs from a conjoint administration
#'
#' Average marginal component effects from a [conjoint_instrument()]
#' administration: one OLS regression of profile choice on
#' treatment-coded dummies for all attributes simultaneously, with CR1
#' cluster-robust standard errors clustered by persona and 95% intervals
#' on the t distribution with G - 1 degrees of freedom (G personas).
#' Under uniform, independent profile randomization this is the standard
#' AMCE estimator.
#'
#' @param responses A [panel_administer()] result whose instrument came from
#'   [conjoint_instrument()].
#' @return A tibble: `attribute`, `level`, `estimate`, `std_error`,
#'   `ci_lo`, `ci_hi`. Baseline levels (the first level present, in the
#'   design's order) appear with estimate 0 and `std_error = NA`, so the
#'   table feeds the familiar conjoint plot directly. Attributes
#'   `n_profiles`, `n_respondents`, and `n_dropped_na` record the profile
#'   rows used, the respondents administered, and missing task responses
#'   dropped.
#' @examples
#' \dontrun{
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 6)
#' design <- conjoint_design(
#'   list(color = c("blue", "red"), cost = c("low", "high")),
#'   n_tasks = 6)
#' instr <- conjoint_instrument(design)
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instr, cfg)
#' amce(r)
#' }
#' @references Hainmueller, Jens, Daniel J. Hopkins, and Teppei Yamamoto
#'   (2014). "Causal Inference in Conjoint Analysis: Understanding
#'   Multidimensional Choices via Stated Preference Experiments."
#'   \emph{Political Analysis} 22(1), 1-30.
#' @export
amce <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  instr <- attr(responses, "instrument")
  design <- instr$conjoint
  if (is.null(design)) {
    abort("amce() needs an administration of a conjoint_instrument().")
  }
  attrs <- attr(design, "attributes")
  if (is.null(attrs) || !is.list(attrs) ||
      is.null(names(attrs)) || any(!nzchar(names(attrs)))) {
    abort("The conjoint design is missing its attribute metadata.")
  }
  attr_names <- names(attrs)[names(attrs) %in% names(design)]
  if (!length(attr_names)) abort("The conjoint design has no attribute columns.")

  tasks <- sort(unique(design$task))
  task_ids <- paste0("task_", tasks)
  task_map <- stats::setNames(tasks, task_ids)
  r_all <- responses[responses$item_id %in% task_ids, , drop = FALSE]
  n_dropped_na <- sum(is.na(r_all$response))
  r <- r_all[!is.na(r_all$response), , drop = FALSE]
  if (!nrow(r)) abort("No non-missing conjoint responses to estimate AMCEs.")

  rows <- list()
  for (i in seq_len(nrow(r))) {
    tk <- unname(task_map[as.character(r$item_id[i])])
    dk <- design[design$task == tk, c("task", "profile", attr_names),
                 drop = FALSE]
    dk$persona_id <- r$persona_id[i]
    dk$chosen <- as.integer(as.character(r$response[i]) ==
                              paste("Profile", dk$profile))
    rows[[length(rows) + 1L]] <-
      dk[, c("persona_id", "task", "profile", attr_names, "chosen"),
         drop = FALSE]
  }
  long <- tibble::as_tibble(do.call(rbind, rows))

  clusters <- unique(long$persona_id)
  G <- length(clusters)
  if (G < 2L) {
    abort("amce() needs at least two personas for clustered standard errors.")
  }

  level_list <- stats::setNames(lapply(attr_names, function(a) {
    lev <- as.character(attrs[[a]])
    lev[lev %in% as.character(unique(long[[a]]))]
  }), attr_names)

  x_cols <- list("(Intercept)" = rep(1, nrow(long)))
  term_index <- list()
  for (a in attr_names) {
    lev <- level_list[[a]]
    if (length(lev) <= 1L) next
    vals <- as.character(long[[a]])
    for (lv in lev[-1]) {
      x_cols[[paste(a, lv, sep = "=")]] <- as.numeric(vals == lv)
      term_index[[paste(a, lv, sep = "\r")]] <- length(x_cols)
    }
  }
  X <- do.call(cbind, x_cols)
  storage.mode(X) <- "double"
  y <- as.numeric(long$chosen)
  n <- nrow(X); k <- ncol(X)
  if (n <= k) abort("The AMCE design has no residual degrees of freedom.")
  if (qr(X)$rank < k) {
    abort("Singular AMCE design matrix; some levels are collinear in this design.")
  }
  bread <- solve(crossprod(X))
  beta <- as.vector(bread %*% crossprod(X, y))
  e <- y - as.vector(X %*% beta)

  meat <- matrix(0, nrow = k, ncol = k)
  for (g in clusters) {
    idx <- long$persona_id == g
    sc <- crossprod(X[idx, , drop = FALSE], e[idx])
    meat <- meat + tcrossprod(sc)
  }
  V <- (G / (G - 1)) * ((n - 1) / (n - k)) * bread %*% meat %*% bread
  se <- sqrt(pmax(diag(V), 0))
  crit <- stats::qt(0.975, df = G - 1)

  out_rows <- list()
  for (a in attr_names) {
    lev <- level_list[[a]]
    if (!length(lev)) next
    out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
      attribute = a, level = lev[1], estimate = 0,
      std_error = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_)
    if (length(lev) <= 1L) next
    for (lv in lev[-1]) {
      col <- term_index[[paste(a, lv, sep = "\r")]]
      est <- beta[col]; serr <- se[col]
      out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
        attribute = a, level = lv, estimate = est, std_error = serr,
        ci_lo = est - crit * serr, ci_hi = est + crit * serr)
    }
  }
  out <- do.call(rbind, out_rows)
  attr(out, "n_profiles") <- nrow(long)
  attr(out, "n_respondents") <- length(unique(r_all$persona_id))
  attr(out, "n_dropped_na") <- n_dropped_na
  out
}

#' The design-stage report
#'
#' Panel composition, response and parse rates, the bias audit, and --
#' first, in capitals, when absent -- the calibration status.
#'
#' @param responses A [panel_administer()] result.
#' @param ... Ignored; reserved for generic dispatch.
#' @return Character lines of class `panel_report`, with a print method.
#' @export
panel_report <- function(responses, ...) {
  stopifnot(inherits(responses, "panel_responses"))
  panel <- attr(responses, "panel")
  cal <- attr(responses, "calibration")
  ba <- panel_bias_audit(responses)
  lines <- c(
    if (is.null(cal))
      "UNCALIBRATED. No benchmark comparison was run; readings below describe the model under these personas, not any human population."
    else if (cal$items_covered < cal$items_total)
      sprintf("PARTIALLY CALIBRATED (%d/%d items). Against '%s': mean absolute deviation %.3f on covered items; uncovered items remain design-stage readings. Nonresponse rates in attr(x,'calibration')$nonresponse.",
              cal$items_covered, cal$items_total, cal$benchmark_name, cal$mad)
    else
      sprintf("CALIBRATION (%d/%d items). Against '%s': mean absolute deviation %.3f, max %.3f (full table in attr(x,'calibration')$table; nonresponse in $nonresponse).",
              cal$items_covered, cal$items_total, cal$benchmark_name,
              cal$mad, cal$max_dev),
    sprintf("PANEL. %d persona(s) drawn from margins over: %s.",
            nrow(panel), paste(names(attr(panel, "margins")), collapse = ", ")),
    sprintf("RESPONSES. %d total; %d parse failure(s).",
            nrow(responses), sum(ba$parse_failures)),
    "FIRST-OPTION SENSITIVITY (chi-squared p by item; small p = which option was listed first moved the answers):",
    sprintf("  %-12s n = %3d  parse failures = %2d  order p = %s",
            ba$item_id, ba$n, ba$parse_failures,
            ifelse(is.na(ba$order_effect_p), "n/a",
                   format(round(ba$order_effect_p, 4)))),
    "STANCE. Silicon panels are design-stage instruments; estimation of human quantities requires calibration to carry that reading."
  )
  structure(lines, class = "panel_report")
}

#' @exportS3Method LLMR::report
report.panel_responses <- function(x, ...) {
  panel_report(x, ...)
}

#' @export
print.panel_report <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n")
  invisible(x)
}
