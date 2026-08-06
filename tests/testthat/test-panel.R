fix_panel <- function(n = 20, seed = 110) {
  set.seed(seed)
  panel_from_margins(
    list(party = c(left = .5, right = .5),
         age = c(young = .5, old = .5)),
    n = n,
    persona_template = "A {age} voter who leans {party}.")
}

fix_instr <- function(randomize = "option_order") {
  panel_instrument(list(
    item_likert("wk4", "A four-day work week would benefit society.",
                scale = c("disagree", "neutral", "agree")),
    item_choice("plan", "Which plan do you prefer?", c("Plan A", "Plan B")),
    item_open("why", "Why, in one sentence?")
  ), randomize = randomize)
}

fix_cfg <- function() LLMR::llm_config("groq", "fake-model")

runner_by_party <- function(experiments, ...) {
  experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
    sys <- experiments$messages[[i]][["system"]]
    usr <- experiments$messages[[i]][["user"]]
    right <- grepl("leans right", sys)
    if (grepl("work week", usr)) { if (right) "agree" else "disagree" }
    else if (grepl("Which plan", usr)) { if (right) "Plan A" else "Plan B" }
    else "Because it suits my life."
  }, character(1))
  experiments$success <- TRUE
  experiments
}

runner_first_option <- function(experiments, ...) {
  experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
    usr <- experiments$messages[[i]][["user"]]
    if (!grepl("Options:", usr)) return("free text")
    opts <- strsplit(sub(".*Options: ", "", usr), " | ", fixed = TRUE)[[1]]
    opts[1]
  }, character(1))
  experiments$success <- TRUE
  experiments
}

test_that("panels draw from margins and render personas", {
  p <- fix_panel(50)
  expect_s3_class(p, "silicon_panel")
  expect_equal(nrow(p), 50L)
  expect_setequal(unique(p$party), c("left", "right"))
  expect_match(p$persona[1], "voter who leans")
  expect_output(print(p), "silicon_panel")
  expect_error(panel_from_margins(list(c(a = .5, b = .5)), 5), "named list")
  expect_error(panel_from_margins(list(party = c(.5, .5)), 5), "named")

  pt <- tibble::as_tibble(p)
  expect_s3_class(pt, "tbl_df")
  expect_false(inherits(pt, "silicon_panel"))
  expect_null(attr(pt, "margins"))
})

test_that("instruments validate and conjoint designs have the right shape", {
  expect_output(print(fix_instr()), "3 item")
  expect_error(panel_instrument(list("not an item")), "item_likert")
  expect_error(panel_instrument(list(item_open("a", "t"), item_open("a", "t"))),
               "unique")
  expect_error(panel_instrument(list(item_open("a", "t")), randomize = "colors"),
               "only 'option_order'")

  set.seed(110)
  cj <- conjoint_design(list(price = c("low", "high"),
                             origin = c("domestic", "imported")),
                        n_tasks = 3, profiles_per_task = 2)
  expect_equal(nrow(cj$profiles), 6L)
  expect_setequal(unique(cj$profiles$profile), 1:2)
  for (tk in unique(cj$profiles$task)) {
    prof <- cj$profiles[cj$profiles$task == tk, c("price", "origin")]
    expect_false(any(duplicated(prof)))
  }
  expect_warning(
    conjoint_design(list(a = "x", b = "y"), n_tasks = 1, profiles_per_task = 2),
    "too small")
})

test_that("administer collects matched responses and Likert scores", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  expect_s3_class(r, "panel_responses")
  expect_named(r, c("data", "panel", "instrument", "benchmark", "usage"))
  expect_s3_class(r$data, "tbl_df")
  expect_null(attr(r, "panel"))
  expect_null(attr(r, "instrument"))
  expect_null(attr(r, "benchmark"))
  expect_null(attr(r, "usage"))
  expect_equal(nrow(r$data), 30L)
  lik <- r$data[r$data$item_id == "wk4", ]
  expect_setequal(unique(lik$response), c("agree", "disagree"))
  expect_setequal(unique(lik$score), c(1, 3))
  expect_true(all(is.na(r$data$score[r$data$item_id != "wk4"])))
  expect_true(all(nzchar(r$data$response[r$data$item_id == "why"])))
  expect_true(all(grepl("\\|", stats::na.omit(lik$option_order))))
})

test_that("the synchronous runner is realigned by request_id", {
  panel <- fix_panel(3)
  instrument <- panel_instrument(
    item_choice("q", "Choose.", c("No", "Yes")),
    randomize = character(0))
  runner <- function(experiments, ...) {
    experiments$response_text <- ifelse(
      as.integer(sub("llmr-", "", experiments$request_id)) %% 2L == 1L,
      "Yes", "No")
    experiments[rev(seq_len(nrow(experiments))), , drop = FALSE]
  }

  responses <- panel_administer(panel, instrument, fix_cfg(), .runner = runner)
  expect_identical(responses$data$persona_id, 1:3)
  expect_identical(responses$data$response, c("Yes", "No", "Yes"))
})

test_that("the synchronous runner requires each submitted request_id once", {
  panel <- fix_panel(3)
  instrument <- panel_instrument(
    item_choice("q", "Choose.", c("No", "Yes")),
    randomize = character(0))
  duplicate <- function(experiments, ...) {
    experiments$response_text <- "Yes"
    experiments$request_id[2] <- experiments$request_id[1]
    experiments
  }
  missing <- function(experiments, ...) {
    experiments$response_text <- "Yes"
    experiments[-1, , drop = FALSE]
  }
  id_error <- "exact submitted.*request_id.*each id appearing once"

  expect_error(
    panel_administer(panel, instrument, fix_cfg(), .runner = duplicate),
    id_error)
  expect_error(
    panel_administer(panel, instrument, fix_cfg(), .runner = missing),
    id_error)
})

test_that("item positions are the instrument's fixed order, never shuffled", {
  set.seed(110)
  r <- panel_administer(fix_panel(20), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  expect_true("item_position" %in% names(r$data))
  # each persona-item pair is an independent request: the recorded position
  # is the item's canonical instrument index, identical for every persona
  expect_equal(unique(r$data$item_position[r$data$item_id == "wk4"]), 1L)
  expect_equal(unique(r$data$item_id[r$data$item_position == 2]), "plan")
  expect_equal(unique(r$data$item_id[r$data$item_position == 3]), "why")
  # and item_order randomization is refused with an explanation
  expect_error(fix_instr(randomize = c("item_order", "option_order")),
               "not implemented")
})

test_that("reserved column names abort early in all three constructors", {
  expect_error(panel_from_margins(list(persona = c(a = .5, b = .5)), n = 2),
               "reserved")
  expect_error(panel_from_margins(list(persona_id = c(a = .5, b = .5)), n = 2),
               "reserved")

  src <- data.frame(persona = c("x", "y"), g = c("a", "b"),
                    stringsAsFactors = FALSE)
  expect_error(panel_from_data(src, n = 2), "reserved")
  # deselecting the offending column is enough
  expect_no_error(panel_from_data(src, n = 2, columns = "g"))

  skip_if_not_installed("LLMR")
  df <- data.frame(persona = c("p1", "p2"), age = c("30", "40"),
                   stringsAsFactors = FALSE)
  pf <- as_persona_frame(df, demographics = c("persona", "age"))
  expect_error(panel_from_personas(pf, n = 2), "reserved")
})

test_that("conjoint_design keeps a single numeric level literal", {
  set.seed(110)
  d <- conjoint_design(list(price = c(10, 20), rooms = 3), n_tasks = 4)
  expect_true(all(d$profiles$rooms == "3"))
  expect_setequal(unique(d$profiles$price), c("10", "20"))
})

test_that(".match_option strips quotes and trailing punctuation on a second pass", {
  opts <- c("disagree", "agree")
  expect_equal(LLMRpanel:::.match_option("Agree.", opts), "agree")
  expect_equal(LLMRpanel:::.match_option("\"agree\"", opts), "agree")
  expect_equal(LLMRpanel:::.match_option("'Agree!'", opts), "agree")
  expect_true(is.na(LLMRpanel:::.match_option("agreeable.", opts)))
  # the exact first pass stays primary: options with their own punctuation
  opts2 <- c("Yes.", "No.")
  expect_equal(LLMRpanel:::.match_option("yes.", opts2), "Yes.")

  # and the second pass reaches the administer path
  dotted <- function(experiments, ...) {
    experiments$response_text <- "Agree."
    experiments
  }
  instr <- panel_instrument(
    item_likert("q", "S.", scale = c("disagree", "neutral", "agree")),
    randomize = character(0))
  r <- panel_administer(fix_panel(4), instr, fix_cfg(), .runner = dotted)
  expect_true(all(r$data$response == "agree"))
  expect_true(all(r$data$score == 3))
})

test_that("panel_benchmark refuses benchmark levels outside the offered options", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  bench_typo <- data.frame(item_id = "plan", response = c("plan a", "Plan B"),
                           share = c(.5, .5))
  expect_error(panel_benchmark(r, bench_typo, "typo bench"),
               "not among its offered options")
  bench_ok <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                         share = c(.5, .5))
  expect_no_warning(panel_benchmark(r, bench_ok, "clean bench"))
})

test_that("the banner walks through benchmark coverage states", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  expect_output(print(r), "NOT BENCHMARKED")

  bench_partial <- data.frame(item_id = "plan",
                              response = c("Plan A", "Plan B"),
                              share = c(.5, .5))
  rp <- panel_benchmark(r, bench_partial, benchmark_name = "narrow study")
  expect_output(print(rp), "PARTIALLY BENCHMARKED")
  expect_output(print(rp), "PARTIALLY BENCHMARKED \\(1/2\\)")
  bmp <- rp$benchmark
  expect_equal(bmp$items_covered, 1L)
  expect_equal(bmp$items_total, 2L)
  expect_true(all(bmp$table$item_id == "plan"))
  expect_true(all(c("item_id", "nonresponse_rate") %in% names(bmp$nonresponse)))
  expect_true(all(LLMR::diagnostics(rp)$benchmark_state ==
                    "PARTIALLY BENCHMARKED"))

  bench_full <- rbind(bench_partial,
                      data.frame(item_id = "wk4",
                                 response = c("disagree", "neutral", "agree"),
                                 share = c(.4, .2, .4)))
  rb <- panel_benchmark(r, bench_full, benchmark_name = "toy human study")
  expect_output(print(rb), "BENCHMARKED")
  expect_output(print(rb), "toy human study")
  expect_output(print(rb), "2/2 items")
  expect_false(any(grepl("NOT BENCHMARKED|PARTIALLY",
                         utils::capture.output(print(rb)))))

  expect_error(
    panel_benchmark(r, data.frame(item_id = "plan",
                                  response = c("Plan A", "Plan B"),
                                  share = c(.7, .6))),
    "sum to 1")
  expect_error(panel_benchmark(r, data.frame(x = 1)), "item_id")
  expect_error(panel_benchmark(r, data.frame(item_id = "ghost", response = "z",
                                             share = 1)),
               "covers none")
})

test_that("bias_audit detects option-order effects", {
  set.seed(110)
  r <- panel_administer(fix_panel(40), fix_instr(), fix_cfg(),
                        .runner = runner_first_option)
  ba <- panel_bias_audit(r)
  expect_identical(
    names(ba),
    c("item_id", "n", "parse_failures", "execution_failures",
      "order_effect_p", "order_test_note"))
  p_choice <- ba$order_effect_p[ba$item_id == "plan"]
  expect_lt(p_choice, 0.01)
  expect_true(is.na(ba$order_effect_p[ba$item_id == "why"]))

  set.seed(110)
  r2 <- panel_administer(fix_panel(40), fix_instr(randomize = character(0)),
                         fix_cfg(), .runner = runner_first_option)
  ba2 <- panel_bias_audit(r2)
  expect_true(all(is.na(ba2$order_effect_p)))
})

test_that("the generic report leads with benchmark status and coverage", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  rep <- LLMR::report(r)
  expect_match(rep[1], "^NOT BENCHMARKED")
  expect_output(print(rep), "RESPONSES")
  expect_false(any(grepl("^STANCE", rep)))

  bench <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                      share = c(.5, .5))
  rep2 <- LLMR::report(panel_benchmark(r, bench, "toy"))
  expect_match(rep2[1], "^PARTIALLY BENCHMARKED \\(1/2")
})

test_that("the generic report identifies the panel's actual source", {
  instrument <- panel_instrument(
    item_choice("q", "Choose.", c("yes", "no")),
    randomize = character(0))
  runner <- function(experiments, ...) {
    experiments$response_text <- "yes"
    experiments
  }
  config <- fix_cfg()

  from_margins <- panel_from_margins(
    list(group = c(a = .5, b = .5)), n = 2)
  from_data <- panel_from_data(
    data.frame(group = c("a", "b"), stringsAsFactors = FALSE), n = 2)
  persona_data <- as_persona_frame(
    data.frame(age = c("30", "40"), opinion = c("a", "b"),
               stringsAsFactors = FALSE),
    demographics = "age")
  from_personas <- panel_from_personas(persona_data)

  reports <- vapply(
    list(from_margins, from_data, from_personas),
    function(panel) paste(LLMR::report(panel_administer(
      panel, instrument, config, .runner = runner)), collapse = "\n"),
    character(1))
  expect_true(grepl("supplied margins", reports[1], fixed = TRUE))
  expect_true(grepl("microdata rows", reports[2], fixed = TRUE))
  expect_true(grepl("supplied personas", reports[3], fixed = TRUE))
})

test_that("response data can change without losing provenance", {
  set.seed(110)
  with_usage <- function(experiments, ...) {
    out <- runner_by_party(experiments, ...)
    out$sent_tokens <- 5L
    out$rec_tokens <- 1L
    out$total_tokens <- 6L
    out
  }
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = with_usage)
  bench <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                      share = c(.5, .5))
  benchmarked <- panel_benchmark(r, bench, "toy")
  expect_null(attr(benchmarked, "benchmark"))
  original_panel <- benchmarked$panel
  original_instrument <- benchmarked$instrument
  original_benchmark <- benchmarked$benchmark
  original_usage <- benchmarked$usage
  priced <- panel_usage(
    benchmarked,
    price_table = data.frame(model = "fake-model", input = 1, output = 2))

  benchmarked$data <- benchmarked$data[
    benchmarked$data$item_id == "plan", , drop = FALSE]
  benchmarked$data$response_text[1] <- "edited raw response"

  expect_s3_class(benchmarked, "panel_responses")
  expect_identical(benchmarked$panel, original_panel)
  expect_identical(benchmarked$instrument, original_instrument)
  expect_identical(benchmarked$benchmark, original_benchmark)
  expect_identical(benchmarked$usage, original_usage)
  expect_s3_class(LLMR::report(benchmarked), "panel_report")
  expect_identical(
    panel_usage(
      benchmarked,
      price_table = data.frame(model = "fake-model", input = 1, output = 2)),
    priced)
})

test_that("shared generics dispatch for panel responses", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)

  dg <- LLMR::diagnostics(r)
  expect_s3_class(dg, "tbl_df")
  expect_equal(names(dg),
               c("item_id", "n", "parse_failures", "execution_failures",
                 "order_effect_p", "order_test_note", "benchmark_state",
                 "items_covered", "items_total", "mean_abs_dev"))
  expect_true(all(dg$benchmark_state == "NOT BENCHMARKED"))
  expect_equal(unique(dg$items_covered), 0L)
  expect_equal(unique(dg$items_total), 2L)
  expect_true(all(is.na(dg$mean_abs_dev)))

  gen_rep <- LLMR::report(r)
  expect_s3_class(gen_rep, "panel_report")
  expect_match(gen_rep[1], "^NOT BENCHMARKED")

  rt <- tibble::as_tibble(r)
  expect_s3_class(rt, "tbl_df")
  expect_false(inherits(rt, "panel_responses"))
  expect_identical(rt, r$data)

  bench <- rbind(
    data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
               share = c(.5, .5)),
    data.frame(item_id = "wk4", response = c("disagree", "neutral", "agree"),
               share = c(.4, .2, .4)))
  rb <- panel_benchmark(r, bench, "toy human study")
  dgb <- LLMR::diagnostics(rb)
  expect_true(all(dgb$benchmark_state == "BENCHMARKED"))
  expect_equal(unique(dgb$items_covered), 2L)
  expect_equal(unique(dgb$items_total), 2L)
  bm <- rb$benchmark
  expect_true("mean_abs_dev" %in% names(bm))
  expect_false("mad" %in% names(bm))
  expect_equal(bm$mean_abs_dev, mean(abs(bm$table$deviation)))
  expect_equal(unique(dgb$mean_abs_dev), bm$mean_abs_dev)
  expect_false("mad" %in% names(dgb))
})

test_that("the ecosystem hash convention is pinned (drift guard vs LLMR)", {
  expect_identical(
    LLMR::llm_hash(list(model = "gpt-oss-20b", temperature = 0)),
    "7c5ffbb0b308f20bf188a3efd962a2895f45ad202307234ee1965d86abc0606c")
})

test_that("benchmarking an all-parse-failure item does not crash", {
  # a runner whose choice answers never match the options -> all NA responses
  unparseable <- function(experiments, ...) {
    experiments$response_text <- "this is not one of the options"
    experiments$success <- TRUE
    experiments
  }
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(), .runner = unparseable)
  bench <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                      share = c(.5, .5))
  expect_no_error(rb <- panel_benchmark(r, bench, "all-NA study"))
  bm <- rb$benchmark
  # The benchmark artifact still exists; the covered item is full nonresponse.
  expect_true(!is.null(bm))
  nr <- bm$nonresponse
  expect_equal(nr$nonresponse_rate[nr$item_id == "plan"], 1)
  # zero valid responses is an undefined distribution, not a zero one
  plan_rows <- bm$table[bm$table$item_id == "plan", ]
  expect_true(all(is.na(plan_rows$share_silicon)))
  expect_true(all(is.na(plan_rows$deviation)))
})

test_that("benchmarking refuses an item with only execution failures", {
  failed <- function(experiments, ...) {
    experiments$response_text <- "Plan A"
    experiments$success <- FALSE
    experiments$error_message <- "provider unavailable"
    experiments
  }
  responses <- panel_administer(
    fix_panel(4), fix_instr(), fix_cfg(), .runner = failed)
  benchmark <- data.frame(
    item_id = "plan", response = c("Plan A", "Plan B"), share = c(.5, .5))

  expect_error(panel_benchmark(responses, benchmark, "failed run"),
               "execution|successful")
})

test_that("panel_power requires a focal response for 3+ option choice items", {
  # Build a panel_responses with a 3-option choice item answered unparseably-free
  # so we control the response distribution directly.
  multi_runner <- function(experiments, ...) {
    # cycle answers across the three options so the modal share is ambiguous
    opts <- c("red", "green", "blue")
    experiments$response_text <- rep(opts, length.out = nrow(experiments))
    experiments$success <- TRUE
    experiments
  }
  instr3 <- panel_instrument(
    item_choice("color", "Pick a color.", c("red", "green", "blue")),
    randomize = character(0))
  r <- panel_administer(fix_panel(12), instr3, fix_cfg(), .runner = multi_runner)

  # without focal -> warns about the ambiguous modal estimand
  expect_warning(panel_power(r, effect = 0.2), "well-defined estimand")
  # with a valid focal -> no such warning, and prices on the focal share
  expect_no_warning(out <- panel_power(r, effect = 0.2, focal = c(color = "red")))
  expect_true(out$dispersion[out$item_id == "color"] > 0)
  # a focal that is not even an option of the item is a usage error
  expect_error(panel_power(r, effect = 0.2, focal = c(color = "purple")),
               "not one of the item's options")
})

test_that("panel_power counts offered options, not just observed ones", {
  # A 3-option item where the pilot only ever elicits two options. The observed
  # table has length 2, but the instrument offers 3, so the modal share is still
  # ill-defined and panel_power must warn (not take the silent binary path).
  two_of_three <- function(experiments, ...) {
    experiments$response_text <- rep(c("red", "green"), length.out = nrow(experiments))
    experiments$success <- TRUE
    experiments
  }
  instr3 <- panel_instrument(
    item_choice("color", "Pick a color.", c("red", "green", "blue")),
    randomize = character(0))
  r <- panel_administer(fix_panel(12), instr3, fix_cfg(), .runner = two_of_three)
  expect_warning(panel_power(r, effect = 0.2), "well-defined estimand")
  # stripping the instrument removes the offered-count signal: it then falls
  # back to the observed two options and prices them without the warning.
  r2 <- r; r2$instrument <- NULL
  expect_no_warning(panel_power(r2, effect = 0.2))

  # "blue" is a real option the pilot never elicited. Naming it as the focal is
  # informative for planning a rare response: warn and price at an observed rate
  # of 0 rather than aborting. (The 0 rate also trips the incidental arm-clamp
  # warning, which we muffle so only the focal warning is asserted.)
  muffle_clamp <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("clamped", conditionMessage(w))) invokeRestart("muffleWarning")
  })
  expect_warning(out <- muffle_clamp(panel_power(r, effect = 0.2,
                                                 focal = c(color = "blue"))),
                 "did not appear in the pilot")
  expect_true(is.finite(out$n_per_arm[out$item_id == "color"]))
})

test_that("panel_power still prices a binary choice without focal", {
  bin_runner <- function(experiments, ...) {
    experiments$response_text <- rep(c("A", "B"), length.out = nrow(experiments))
    experiments$success <- TRUE
    experiments
  }
  instr2 <- panel_instrument(
    item_choice("ab", "A or B?", c("A", "B")), randomize = character(0))
  r <- panel_administer(fix_panel(12), instr2, fix_cfg(), .runner = bin_runner)
  expect_no_warning(out <- panel_power(r, effect = 0.2))
  expect_true(is.finite(out$n_per_arm[out$item_id == "ab"]))
})

test_that("a weighted panel cites weighted source margins", {
  set.seed(110)
  # education is 2:1 college vs none by raw count, but weights flip it to 1:2
  src <- data.frame(
    education = c("college", "college", "none"),
    weight = c(1, 1, 4),
    stringsAsFactors = FALSE)
  p <- panel_from_data(src, n = 50, columns = "education", weights = "weight",
                       persona_template = "A {education} respondent.")
  m <- attr(p, "margins")$education
  # weighted: college = 2/6, none = 4/6 (NOT the unweighted 2/3 vs 1/3)
  expect_equal(unname(m["college"]), 2 / 6, tolerance = 1e-9)
  expect_equal(unname(m["none"]), 4 / 6, tolerance = 1e-9)
})

test_that("panel_from_personas builds a silicon_panel that administers", {
  skip_if_not_installed("LLMR")
  set.seed(110)
  p <- panel_from_personas(LLMR::anes_2024_personas, n = 6)
  expect_s3_class(p, "silicon_panel")
  expect_true(all(c("persona_id", "persona") %in% names(p)))
  expect_equal(nrow(p), 6L)
  expect_false(is.null(attr(p, "margins")))
  # the persona text is the survey-answering frame, carrying the row's answers
  expect_true(grepl("You are this person", p$persona[1]))
  expect_true(grepl("questionnaire", p$persona[1]))

  # it administers offline like any silicon_panel: the persona is the system msg
  runner_echo <- function(experiments, ...) {
    experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
      sys <- experiments$messages[[i]][["system"]]
      if (grepl("Strong Democrat|Not very strong Democrat", sys)) "agree" else "disagree"
    }, character(1))
    experiments$success <- TRUE
    experiments
  }
  r <- panel_administer(p, fix_instr(), fix_cfg(), .runner = runner_echo)
  expect_s3_class(r, "panel_responses")
  expect_equal(length(unique(r$data$persona_id)), 6L)
})

test_that("panel_from_personas respects a rows predicate", {
  skip_if_not_installed("LLMR")
  p <- panel_from_personas(LLMR::anes_2024_personas,
                           rows = function(d) d$ideology_score > 0.5, n = 4)
  expect_equal(nrow(p), 4L)
})

test_that("panel_from_personas draws beyond the ANES pool with replacement", {
  skip_if_not_installed("LLMR")
  set.seed(110)
  expect_warning(
    p <- panel_from_personas(LLMR::anes_2024_personas, n = 250),
    "Drawing 250 personas from 100 distinct respondent"
  )
  expect_equal(nrow(p), 250L)
})
