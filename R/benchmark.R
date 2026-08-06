# benchmark.R --------------------------------------------------------------------
# Benchmark comparisons, response diagnostics, and design calculations.

#' Compare silicon responses with a human benchmark
#'
#' Compares closed-item response shares with human benchmark shares supplied
#' by the user. The result contains deviations for covered item-response
#' pairs, the number of closed items covered, and nonresponse rates by item.
#' The function does not alter responses or adjust response shares.
#' Without a benchmark, response shares describe the configured model under the
#' supplied personas, not a human population.
#'
#' @param responses A [panel_administer()] result.
#' @param benchmark A data frame with columns `item_id`, `response`, and
#'   `share` (human marginal proportions). Shares within an item should sum
#'   to 1; a deviation beyond rounding draws a warning.
#' @param benchmark_name How the source should be cited in reports (e.g.
#'   `"ANES 2024 pilot"`).
#' @return `responses` with its `benchmark` field set:
#'   `$table` (per covered item and response: `share_silicon`,
#'   `share_human`, `deviation`), `$nonresponse` (nonresponse and execution
#'   failure rates per item),
#'   `$items_covered` / `$items_total`, `$mean_abs_dev`, `$max_dev`.
#' @examples
#' \dontrun{
#' set.seed(110)
#' panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 12)
#' instrument <- panel_instrument(item_choice("plan", "Which plan do you prefer?",
#'                                            c("A", "B")))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instrument, cfg)
#' r
#' bench <- data.frame(item_id = "plan", response = c("A", "B"),
#'                     share = c(.5, .5))
#' panel_benchmark(r, bench, "toy human study")
#' }
#' @export
panel_benchmark <- function(responses, benchmark, benchmark_name = "benchmark") {
  stopifnot(inherits(responses, "panel_responses"), is.data.frame(benchmark))
  data <- responses$data
  need <- c("item_id", "response", "share")
  if (!all(need %in% names(benchmark))) {
    abort("`benchmark` needs columns item_id, response, share.")
  }
  if (!is.numeric(benchmark$share) || anyNA(benchmark$share) ||
      any(benchmark$share < 0 | benchmark$share > 1)) {
    abort("`benchmark$share` must contain probabilities in [0, 1] with no NA.")
  }
  key <- paste(benchmark$item_id, benchmark$response, sep = "\r")
  if (anyDuplicated(key)) {
    abort("Each benchmark item_id-response pair must appear exactly once.")
  }
  sums <- stats::aggregate(share ~ item_id, data = benchmark, FUN = sum)
  off <- sums$item_id[abs(sums$share - 1) > 0.01]
  if (length(off)) {
    abort(paste0(
      "Benchmark shares must sum to 1 within each item; item(s) ",
      paste(sprintf("'%s'", off), collapse = ", "),
      " do not. A reference that is not a distribution cannot anchor a",
      " comparison."))
  }

  closed_all <- data[data$type != "open", ]
  if (!nrow(closed_all)) abort("No closed-item responses to benchmark.")
  if (!is.null(responses$instrument$conjoint)) {
    abort(paste(
      "Conjoint responses cannot be benchmarked against marginal shares:",
      "every respondent saw different profiles, so item-level response",
      "shares are not comparable quantities."))
  }
  items_total <- unique(closed_all$item_id)
  items_covered <- intersect(items_total, unique(benchmark$item_id))
  if (!length(items_covered)) {
    abort("The benchmark covers none of the administered items.")
  }
  covered <- closed_all[closed_all$item_id %in% items_covered, ]
  successful <- !(covered$success %in% FALSE)
  successful_items <- unique(covered$item_id[successful])
  failed_items <- setdiff(items_covered, successful_items)
  if (length(failed_items)) {
    abort(paste0(
      "Cannot compare item(s) ", paste(sprintf("'%s'", failed_items),
                                      collapse = ", "),
      ": every execution failed."))
  }

  # Benchmark response labels that are not among an item's offered options can
  # never match a silicon response (matching is by exact string), so their
  # silicon share is 0 by construction. That is a labeling problem, not a
  # finding; say so instead of letting a case difference or a typo masquerade
  # as total divergence.
  instrument <- responses$instrument
  if (!is.null(instrument) && is.list(instrument$items)) {
    for (id in items_covered) {
      it <- Find(function(x) identical(x$id, id), instrument$items)
      if (is.null(it) || is.null(it$options)) next
      lev <- unique(as.character(benchmark$response[benchmark$item_id == id]))
      bad <- setdiff(lev, it$options)
      if (length(bad)) {
        abort(paste0(
          "Benchmark response(s) ", paste(sprintf("'%s'", bad), collapse = ", "),
          " for item '", id, "' are not among its offered options (",
          paste(sprintf("'%s'", it$options), collapse = ", "),
          "); a label that can never match would masquerade as total",
          " divergence. Recode the benchmark to the offered labels."))
      }
    }
  }

  nonresp <- do.call(rbind, lapply(split(closed_all, closed_all$item_id),
    function(ri) {
      failed <- ri$success %in% FALSE
      tibble::tibble(
        item_id = ri$item_id[1],
        execution_failures = sum(failed),
        execution_failure_rate = mean(failed),
        nonresponse_rate = if (all(failed)) NA_real_ else
          mean(is.na(ri$response[!failed])))
    }))

  closed <- closed_all[!(closed_all$success %in% FALSE) &
                         !is.na(closed_all$response) &
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
    # silicon responses to tabulate. Keep the benchmark artifact rather than
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
  # An item with no valid responses at all has an undefined silicon
  # distribution; zero shares would claim certainty that nothing was chosen.
  valid_items <- unique(closed$item_id)
  undefined <- !(cmp$item_id %in% valid_items)
  cmp$share_silicon[undefined] <- NA_real_
  cmp$deviation <- cmp$share_silicon - cmp$share_human

  responses$benchmark <- list(
    benchmark_name = benchmark_name,
    table = tibble::as_tibble(cmp),
    nonresponse = tibble::as_tibble(nonresp),
    items_covered = length(items_covered),
    items_total = length(items_total),
    mean_abs_dev = mean(abs(cmp$deviation)),
    max_dev = max(abs(cmp$deviation)),
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  responses
}

#' Plot a benchmark comparison
#'
#' Plots the comparison recorded by [panel_benchmark()]. Each covered response
#' level has one point for the panel share and one for the benchmark share,
#' joined by a segment. Response levels follow the instrument's option order,
#' and items appear in separate panels. The method requires a benchmark
#' record.
#'
#' @param x A [panel_administer()] result that [panel_benchmark()] has been run
#'   on (the comparison is stored in `x$benchmark`).
#' @param ... Ignored; reserved for generic dispatch.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(110)
#'   panel <- panel_from_margins(list(party = c(left = .5, right = .5)),
#'                               n = 12,
#'                               persona_template = "A voter who leans {party}.")
#'   instrument <- panel_instrument(
#'     item_choice("plan", "Which plan do you prefer?", c("Plan A", "Plan B")))
#'   cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#'   by_party <- function(experiments, ...) {
#'     experiments$response_text <- ifelse(
#'       grepl("leans left", vapply(experiments$messages, `[[`, "", "system")),
#'       "Plan A", "Plan B")
#'     experiments
#'   }
#'   r <- panel_administer(panel, instrument, cfg, .runner = by_party)
#'   bench <- data.frame(item_id = "plan",
#'                       response = c("Plan A", "Plan B"),
#'                       share = c(.55, .45))
#'   plot(panel_benchmark(r, bench, "city survey 2025"))
#' }
#' @export
plot.panel_responses <- function(x, ...) {
  benchmark_record <- x$benchmark
  if (is.null(benchmark_record)) {
    abort(paste(
      "Nothing to plot: this result is NOT BENCHMARKED (no benchmark comparison",
      "has been run). Run panel_benchmark() first; the plot shows silicon",
      "against human shares."))
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting a benchmark comparison needs the ggplot2 package; install it with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
  tab <- as.data.frame(benchmark_record$table)

  wrap_text <- function(x, width) {
    vapply(as.character(x), function(value) {
      if (is.na(value) || !nzchar(value)) return(value)
      words <- strsplit(value, "[[:space:]]+")[[1]]
      words <- unlist(lapply(words, function(word) {
        starts <- seq.int(1L, nchar(word), by = width)
        substring(word, starts, pmin(starts + width - 1L, nchar(word)))
      }), use.names = FALSE)
      paste(strwrap(paste(words, collapse = " "), width = width),
            collapse = "\n")
    }, character(1), USE.NAMES = FALSE)
  }

  # The item-response key lets each facet retain its own canonical option order
  # even when two items reuse the same labels in different orders.
  instrument <- x$instrument
  item_levels <- unique(as.character(tab$item_id))
  if (!is.null(instrument) && is.list(instrument$items)) {
    instrument_ids <- vapply(instrument$items, `[[`, "", "id")
    item_levels <- c(intersect(instrument_ids, item_levels),
                     setdiff(item_levels, instrument_ids))
  }
  tab$item_id <- factor(tab$item_id, levels = item_levels)
  response_levels <- unlist(lapply(item_levels, function(id) {
    rs <- unique(as.character(tab$response[as.character(tab$item_id) == id]))
    canonical <- character(0)
    if (!is.null(instrument) && is.list(instrument$items)) {
      item <- Find(function(candidate) identical(candidate$id, id),
                   instrument$items)
      if (!is.null(item)) canonical <- item$options
    }
    ordered <- c(intersect(canonical, rs), setdiff(rs, canonical))
    paste(id, ordered, sep = "\r")
  }), use.names = FALSE)
  tab$response_key <- factor(
    paste(as.character(tab$item_id), as.character(tab$response), sep = "\r"),
    levels = response_levels)
  label_rows <- !duplicated(tab$response_key)
  response_labels <- stats::setNames(
    wrap_text(tab$response[label_rows], width = 32),
    as.character(tab$response_key[label_rows]))

  sil_lab <- "silicon"
  hum_lab <- "human"
  long <- rbind(
    data.frame(item_id = tab$item_id, response_key = tab$response_key,
               share = tab$share_silicon, series = sil_lab),
    data.frame(item_id = tab$item_id, response_key = tab$response_key,
               share = tab$share_human, series = hum_lab))
  long$series <- factor(long$series, levels = c(hum_lab, sil_lab))

  benchmark_line <- wrap_text(
    paste0("Benchmark: ", benchmark_record$benchmark_name), width = 76)

  ggplot2::ggplot(tab) +
    ggplot2::geom_segment(
      ggplot2::aes(x = share_human, xend = share_silicon,
                   y = response_key, yend = response_key),
      colour = "grey65", linewidth = 0.6) +
    ggplot2::geom_point(
      data = long,
      ggplot2::aes(x = share, y = response_key, colour = series),
      size = 3) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(c("grey25", "#2C7FB8"), c(hum_lab, sil_lab))) +
    ggplot2::scale_y_discrete(labels = response_labels) +
    ggplot2::expand_limits(x = c(0, 1)) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(item_id), scales = "free_y", space = "free_y",
      switch = "y",
      labeller = ggplot2::labeller(
        item_id = function(value) wrap_text(value, width = 36))) +
    ggplot2::labs(
      x = "share of valid responses", y = NULL, colour = NULL,
      title = "Benchmark: silicon and human response shares",
      subtitle = paste(
        benchmark_line,
        sprintf("%d/%d item(s) covered; mean absolute deviation %.3f, max %.3f",
                benchmark_record$items_covered, benchmark_record$items_total,
                benchmark_record$mean_abs_dev, benchmark_record$max_dev),
        sep = "\n")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(lineheight = 1.05),
      axis.text = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(lineheight = 1.05),
      legend.text = ggplot2::element_text(size = 11),
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(
        angle = 0, hjust = 0, size = 11),
      plot.margin = ggplot2::margin(10, 14, 10, 10))
}

#' Summarize execution failures, parse failures, and first-option sensitivity
#'
#' Counts execution and parse failures by item. For closed items administered
#' with randomized option order, it also applies a chi-squared test to the
#' chosen response and the option shown first. The test does not use the full
#' option permutation.
#'
#' @param responses A [panel_administer()] result.
#' @return A tibble: `item_id`, `n`, `parse_failures`, `execution_failures`,
#'   `order_effect_p` (the first-option chi-squared p-value; NA when order was
#'   not randomized or cells are too sparse).
#' @examples
#' panel <- panel_from_margins(list(group = c(A = 1)), n = 4)
#' instrument <- panel_instrument(
#'   item_choice("pick", "Choose one.", c("A", "B")),
#'   randomize = character(0))
#' config <- LLMR::llm_config("groq", "example-model")
#' runner <- function(experiments, ...) {
#'   experiments$response_text <- "A"
#'   experiments$success <- TRUE
#'   experiments
#' }
#' responses <- panel_administer(panel, instrument, config, .runner = runner)
#' panel_bias_audit(responses)
#' @export
panel_bias_audit <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  data <- responses$data
  items <- split(data, data$item_id)
  out <- lapply(items, function(ri) {
    closed <- ri$type[1] != "open"
    failed <- ri$success %in% FALSE
    pf <- if (closed) sum(is.na(ri$response) & !failed) else 0L
    p <- NA_real_
    order_note <- NA_character_
    if (closed && length(unique(stats::na.omit(ri$option_order))) > 1L) {
      first_seen <- vapply(strsplit(ri$option_order, "|", fixed = TRUE),
                           `[[`, "", 1L)
      ok <- !is.na(ri$response)
      if (sum(ok) >= 4L && length(unique(ri$response[ok])) > 1L) {
        tab <- table(ri$response[ok], first_seen[ok])
        if (all(dim(tab) >= 2L)) {
          test <- tryCatch(suppressWarnings(stats::chisq.test(tab)),
                           error = function(e) NULL)
          if (!is.null(test)) {
            if (any(test$expected < 5)) {
              order_note <- "sparse cells; chi-square approximation unreliable"
            } else {
              p <- test$p.value
            }
          }
        }
      }
    }
    tibble::tibble(item_id = ri$item_id[1], n = nrow(ri),
                   parse_failures = pf, execution_failures = sum(failed),
                   order_effect_p = p,
                   order_test_note = order_note)
  })
  tibble::as_tibble(do.call(rbind, out))
}

#' @exportS3Method LLMR::diagnostics
diagnostics.panel_responses <- function(x, ...) {
  out <- panel_bias_audit(x)
  data <- x$data
  benchmark <- x$benchmark
  if (is.null(benchmark)) {
    state <- "NOT BENCHMARKED"
    items_covered <- 0L
    items_total <- length(unique(data$item_id[data$type != "open"]))
    mean_abs_dev <- NA_real_
  } else {
    state <- if (benchmark$items_covered < benchmark$items_total) {
      "PARTIALLY BENCHMARKED"
    } else {
      "BENCHMARKED"
    }
    items_covered <- benchmark$items_covered
    items_total <- benchmark$items_total
    mean_abs_dev <- benchmark$mean_abs_dev %||% NA_real_
  }
  out$benchmark_state <- state
  out$items_covered <- items_covered
  out$items_total <- items_total
  out$mean_abs_dev <- mean_abs_dev
  out
}

#' Calculate two-arm sample sizes from panel responses
#'
#' Calculates analytic sample sizes for a two-arm study. Likert items use the
#' standard deviation of `score`. Choice items use the share of a focal response
#' or, when appropriate, the modal response. Open items are omitted.
#'
#' The dispersion entering this arithmetic comes from the model panel, not
#' from human respondents. Model-persona variance routinely differs from
#' human variance (often it is smaller), so treat these figures as a rough
#' reading of the pilot instrument, not as a defensible sample-size plan for
#' a human study; for that, use dispersion from human pilot data or
#' published estimates of the same outcome.
#'
#' @param responses A [panel_administer()] result (the silicon pilot).
#' @param effect Raw minimum detectable difference between the two arms:
#'   scale points for Likert items, a difference in proportions for choice
#'   items. A scalar (recycled) or a named vector keyed by `item_id`.
#' @param items Optional character vector restricting which items to include.
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
#' instrument <- panel_instrument(list(
#'   item_likert("lik", "Rate the proposal.", scale = c("low", "mid", "high")),
#'   item_choice("pick", "Pick one.", c("A", "B"))),
#'   randomize = character(0))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instrument, cfg)
#' panel_power(r, effect = c(lik = 0.5, pick = 0.2))
#' }
#' @export
panel_power <- function(responses, effect, items = NULL, focal = NULL,
                        alpha = 0.05, power = 0.80) {
  stopifnot(inherits(responses, "panel_responses"))
  data <- responses$data
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
  all_items <- unique(data$item_id)
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
  instrument <- responses$instrument
  offered_opts <- function(id) {
    if (is.null(instrument) || is.null(instrument$items)) return(NULL)
    it <- Find(function(x) identical(x$id, id), instrument$items)
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
    ri <- data[data$item_id == id, , drop = FALSE]
    type <- as.character(ri$type[1])
    if (identical(type, "open")) next
    failed <- ri$success %in% FALSE
    if (all(failed)) {
      abort(sprintf(
        "panel_power() cannot use item '%s': every execution failed.", id))
    }
    if (any(failed)) {
      cli::cli_warn(paste(
        "Item {.val {id}} has {sum(failed)} execution failure(s);",
        "the power calculation uses successful executions only."))
      ri <- ri[!failed, , drop = FALSE]
    }
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
#' AMCE estimator. The regression uses the respondent-level profiles recorded
#' during administration, not the profiles in the initial design table.
#'
#' @param responses A [panel_administer()] result whose instrument came from
#'   [conjoint_instrument()].
#' @return A `conjoint_amce` tibble: `attribute`, `level`, `estimate`, `std_error`,
#'   `ci_lo`, `ci_hi`. Baseline levels (the first level present, in the
#'   design's order) appear with estimate 0 and `std_error = NA`, so the
#'   table feeds the familiar conjoint plot directly. The ordinary columns
#'   `n_profiles`, `n_respondents`, `n_dropped_na`, and
#'   `n_execution_failures` record the profile rows used, the respondents
#'   administered, missing task responses dropped, and failed executions.
#' @examples
#' \dontrun{
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 6)
#' design <- conjoint_design(
#'   list(color = c("blue", "red"), cost = c("low", "high")),
#'   n_tasks = 6)
#' instrument <- conjoint_instrument(design)
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' r <- panel_administer(panel, instrument, cfg)
#' conjoint_amce(r)
#' }
#' @references Hainmueller, Jens, Daniel J. Hopkins, and Teppei Yamamoto
#'   (2014). "Causal Inference in Conjoint Analysis: Understanding
#'   Multidimensional Choices via Stated Preference Experiments."
#'   \emph{Political Analysis} 22(1), 1-30.
#' @export
conjoint_amce <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  data <- responses$data
  instrument <- responses$instrument
  design <- instrument$conjoint
  if (is.null(design)) {
    abort("conjoint_amce() needs an administration of a conjoint_instrument().")
  }
  attrs <- design$attributes
  if (is.null(attrs) || !is.list(attrs) ||
      is.null(names(attrs)) || any(!nzchar(names(attrs)))) {
    abort("The conjoint design is missing its attribute metadata.")
  }
  profiles <- design$profiles
  attr_names <- names(attrs)[names(attrs) %in% names(profiles)]
  if (!length(attr_names)) abort("The conjoint design has no attribute columns.")
  if (!("profiles" %in% names(data))) {
    abort("The conjoint responses do not contain recorded profile draws.")
  }

  tasks <- sort(unique(profiles$task))
  task_ids <- paste0("task_", tasks)
  r_all <- data[data$item_id %in% task_ids, , drop = FALSE]
  failed <- r_all$success %in% FALSE
  n_execution_failures <- sum(failed)
  if (all(failed)) abort("conjoint_amce() cannot run: every execution failed.")
  if (n_execution_failures) {
    cli::cli_warn(paste(
      "The conjoint administration has {n_execution_failures} execution",
      "failure(s); the estimate uses successful executions only."))
  }
  n_dropped_na <- sum(is.na(r_all$response) & !failed)
  r <- r_all[!failed & !is.na(r_all$response), , drop = FALSE]
  if (!nrow(r)) abort("No non-missing conjoint responses to estimate AMCEs.")

  rows <- list()
  for (i in seq_len(nrow(r))) {
    dk <- r$profiles[[i]][, c("task", "profile", attr_names), drop = FALSE]
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
    abort("conjoint_amce() needs at least two personas for clustered standard errors.")
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
  out <- tibble::as_tibble(do.call(rbind, out_rows))
  out$n_profiles <- as.integer(nrow(long))
  out$n_respondents <- as.integer(length(unique(r_all$persona_id)))
  out$n_dropped_na <- as.integer(n_dropped_na)
  out$n_execution_failures <- as.integer(n_execution_failures)
  class(out) <- c("conjoint_amce", class(out))
  out
}

#' @export
print.conjoint_amce <- function(x, ...) {
  cat(sprintf(paste0(
    "<conjoint_amce | %d profile row(s) | %d respondent(s) | ",
    "%d missing, %d execution failure(s)>\n"),
    x$n_profiles[1], x$n_respondents[1], x$n_dropped_na[1],
    x$n_execution_failures[1]))
  estimates <- x
  class(estimates) <- setdiff(class(estimates), "conjoint_amce")
  print(estimates[, c("attribute", "level", "estimate", "std_error",
                      "ci_lo", "ci_hi")], ...)
  invisible(x)
}

#' @export
`[.conjoint_amce` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod("[")
  class(out) <- setdiff(class(out), "conjoint_amce")
  out
}

# Internal constructor for the LLMR::report() method.
panel_report <- function(responses, ...) {
  stopifnot(inherits(responses, "panel_responses"))
  data <- responses$data
  panel <- responses$panel
  benchmark <- responses$benchmark
  ba <- panel_bias_audit(responses)
  source <- switch(
    attr(panel, "source") %||% "unknown",
    margins = "drawn from supplied margins",
    microdata = "sampled from microdata rows",
    personas = "built from supplied personas",
    "built from source data")
  distribution_fields <- names(attr(panel, "margins"))
  distribution_text <- if (length(distribution_fields)) {
    paste(distribution_fields, collapse = ", ")
  } else {
    "no recognized demographic fields"
  }
  lines <- c(
    if (is.null(benchmark))
      "NOT BENCHMARKED. No benchmark comparison was run; readings below describe the model under these personas, not any human population."
    else if (benchmark$items_covered < benchmark$items_total)
      sprintf("PARTIALLY BENCHMARKED (%d/%d). Against '%s': mean absolute deviation %.3f on covered items. Nonresponse rates are in $benchmark$nonresponse.",
              benchmark$items_covered, benchmark$items_total,
              benchmark$benchmark_name, benchmark$mean_abs_dev)
    else
      sprintf("BENCHMARKED (%d/%d items). Against '%s': mean absolute deviation %.3f, max %.3f (full table in $benchmark$table; nonresponse in $benchmark$nonresponse).",
              benchmark$items_covered, benchmark$items_total,
              benchmark$benchmark_name, benchmark$mean_abs_dev,
              benchmark$max_dev),
    sprintf("PANEL. %d persona(s) %s over: %s.",
            nrow(panel), source, distribution_text),
    sprintf("RESPONSES. %d total; %d execution failure(s); %d parse failure(s).",
            nrow(data), sum(ba$execution_failures),
            sum(ba$parse_failures)),
    "FIRST-OPTION SENSITIVITY (chi-squared p by item; small p = which option was listed first moved the answers):",
    sprintf("  %-12s n = %3d  execution failures = %2d  parse failures = %2d  order p = %s",
            ba$item_id, ba$n, ba$execution_failures, ba$parse_failures,
            ifelse(is.na(ba$order_effect_p), "n/a",
                   format(round(ba$order_effect_p, 4))))
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
