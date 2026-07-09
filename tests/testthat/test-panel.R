fix_panel <- function(n = 20, seed = 110) {
  set.seed(seed)
  panel_from_margins(
    list(party = c(left = .5, right = .5),
         age = c(young = .5, old = .5)),
    n = n,
    persona_template = "A {age} voter who leans {party}.")
}

fix_instr <- function(randomize = c("item_order", "option_order")) {
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

test_that("instruments validate and stimulus designs have the right shape", {
  expect_output(print(fix_instr()), "3 item")
  expect_error(panel_instrument(list("not an item")), "item_likert")
  expect_error(panel_instrument(list(item_open("a", "t"), item_open("a", "t"))),
               "unique")
  expect_error(panel_instrument(list(item_open("a", "t")), randomize = "colors"),
               "item_order")

  v <- vignette_design("A {age} applicant with {exp} experience.",
                       list(age = c("younger", "older"),
                            exp = c("5 years", "20 years")))
  expect_equal(nrow(v), 4L)
  expect_match(v$text[1], "younger applicant with 5 years")

  set.seed(110)
  cj <- conjoint_design(list(price = c("low", "high"),
                             origin = c("domestic", "imported")),
                        n_tasks = 3, profiles_per_task = 2)
  expect_equal(nrow(cj), 6L)
  expect_setequal(unique(cj$profile), 1:2)
  for (tk in unique(cj$task)) {
    prof <- cj[cj$task == tk, c("price", "origin")]
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
  expect_equal(nrow(r), 30L)
  lik <- r[r$item_id == "wk4", ]
  expect_setequal(unique(lik$response), c("agree", "disagree"))
  expect_setequal(unique(lik$score), c(1, 3))
  expect_true(all(is.na(r$score[r$item_id != "wk4"])))
  expect_true(all(nzchar(r$response[r$item_id == "why"])))
  expect_true(all(grepl("\\|", stats::na.omit(lik$option_order))))
})

test_that("item presentation order is recorded per respondent", {
  set.seed(110)
  r <- panel_administer(fix_panel(20), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  expect_true("item_position" %in% names(r))
  # each persona saw every item exactly once, at positions 1..3
  for (pid in unique(r$persona_id)) {
    expect_setequal(r$item_position[r$persona_id == pid], 1:3)
  }
  # with item_order randomized, an item's position varies across personas
  expect_gt(length(unique(r$item_position[r$item_id == "wk4"])), 1L)

  # with randomization off, positions follow the instrument's canonical order
  set.seed(110)
  r2 <- panel_administer(fix_panel(5), fix_instr(randomize = character(0)),
                         fix_cfg(), .runner = runner_by_party)
  expect_equal(unique(r2$item_id[r2$item_position == 1]), "wk4")
  expect_equal(unique(r2$item_id[r2$item_position == 2]), "plan")
  expect_equal(unique(r2$item_id[r2$item_position == 3]), "why")
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
  expect_true(all(d$rooms == "3"))
  expect_setequal(unique(d$price), c("10", "20"))
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
  expect_true(all(r$response == "agree"))
  expect_true(all(r$score == 3))
})

test_that("panel_calibrate warns when benchmark levels mismatch the offered options", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  bench_typo <- data.frame(item_id = "plan", response = c("plan a", "Plan B"),
                           share = c(.5, .5))
  expect_warning(panel_calibrate(r, bench_typo, "typo bench"),
                 "not among its offered options")
  bench_ok <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                         share = c(.5, .5))
  expect_no_warning(panel_calibrate(r, bench_ok, "clean bench"))
})

test_that("the banner walks UNCALIBRATED -> PARTIAL -> calibrated", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  expect_output(print(r), "UNCALIBRATED")

  bench_partial <- data.frame(item_id = "plan",
                              response = c("Plan A", "Plan B"),
                              share = c(.5, .5))
  rp <- panel_calibrate(r, bench_partial, benchmark_name = "narrow study")
  expect_output(print(rp), "PARTIALLY CALIBRATED")
  expect_output(print(rp), "1/2 items")
  calp <- attr(rp, "calibration")
  expect_equal(calp$items_covered, 1L)
  expect_equal(calp$items_total, 2L)
  expect_true(all(calp$table$item_id == "plan"))
  expect_true(all(c("item_id", "nonresponse_rate") %in% names(calp$nonresponse)))

  bench_full <- rbind(bench_partial,
                      data.frame(item_id = "wk4",
                                 response = c("disagree", "neutral", "agree"),
                                 share = c(.4, .2, .4)))
  rc <- panel_calibrate(r, bench_full, benchmark_name = "toy human study")
  expect_output(print(rc), "toy human study")
  expect_output(print(rc), "2/2 items")
  expect_false(any(grepl("UNCALIBRATED|PARTIALLY",
                         utils::capture.output(print(rc)))))

  expect_warning(
    panel_calibrate(r, data.frame(item_id = "plan",
                                  response = c("Plan A", "Plan B"),
                                  share = c(.7, .6))),
    "sum to 1")
  expect_error(panel_calibrate(r, data.frame(x = 1)), "item_id")
  expect_error(panel_calibrate(r, data.frame(item_id = "ghost", response = "z",
                                             share = 1)),
               "covers none")
})

test_that("bias_audit detects option-order effects", {
  set.seed(110)
  r <- panel_administer(fix_panel(40), fix_instr(), fix_cfg(),
                        .runner = runner_first_option)
  ba <- panel_bias_audit(r)
  p_choice <- ba$order_effect_p[ba$item_id == "plan"]
  expect_lt(p_choice, 0.01)
  expect_true(is.na(ba$order_effect_p[ba$item_id == "why"]))

  set.seed(110)
  r2 <- panel_administer(fix_panel(40), fix_instr(randomize = character(0)),
                         fix_cfg(), .runner = runner_first_option)
  ba2 <- panel_bias_audit(r2)
  expect_true(all(is.na(ba2$order_effect_p)))
})

test_that("the report leads with calibration status, coverage included", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)
  rep <- panel_report(r)
  expect_match(rep[1], "^UNCALIBRATED")
  expect_output(print(rep), "STANCE")

  bench <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                      share = c(.5, .5))
  rep2 <- panel_report(panel_calibrate(r, bench, "toy"))
  expect_match(rep2[1], "^PARTIALLY CALIBRATED \\(1/2")
})

test_that("shared generics dispatch for panel responses", {
  set.seed(110)
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(),
                        .runner = runner_by_party)

  dg <- LLMR::diagnostics(r)
  expect_s3_class(dg, "tbl_df")
  expect_equal(names(dg),
               c("item_id", "n", "parse_failures", "order_effect_p",
                 "calibration_state", "items_covered", "items_total", "mad"))
  expect_true(all(dg$calibration_state == "UNCALIBRATED"))
  expect_equal(unique(dg$items_covered), 0L)
  expect_equal(unique(dg$items_total), 2L)
  expect_true(all(is.na(dg$mad)))

  gen_rep <- LLMR::report(r)
  expect_s3_class(gen_rep, "panel_report")
  expect_match(gen_rep[1], "^UNCALIBRATED")

  rt <- tibble::as_tibble(r)
  expect_s3_class(rt, "tbl_df")
  expect_false(inherits(rt, "panel_responses"))
  expect_null(attr(rt, "panel"))
  expect_null(attr(rt, "instrument"))
  expect_null(attr(rt, "calibration"))

  bench <- rbind(
    data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
               share = c(.5, .5)),
    data.frame(item_id = "wk4", response = c("disagree", "neutral", "agree"),
               share = c(.4, .2, .4)))
  rc <- panel_calibrate(r, bench, "toy human study")
  dgc <- LLMR::diagnostics(rc)
  expect_true(all(dgc$calibration_state == "CALIBRATED"))
  expect_equal(unique(dgc$items_covered), 2L)
  expect_equal(unique(dgc$items_total), 2L)
  expect_equal(unique(dgc$mad), attr(rc, "calibration")$mad)
})

test_that("the ecosystem hash convention is pinned (drift guard vs LLMR)", {
  expect_identical(
    LLMR::llm_hash(list(model = "gpt-oss-20b", temperature = 0)),
    "7c5ffbb0b308f20bf188a3efd962a2895f45ad202307234ee1965d86abc0606c")
})

test_that("calibration of an all-parse-failure item does not crash", {
  # a runner whose choice answers never match the options -> all NA responses
  unparseable <- function(experiments, ...) {
    experiments$response_text <- "this is not one of the options"
    experiments$success <- TRUE
    experiments
  }
  r <- panel_administer(fix_panel(10), fix_instr(), fix_cfg(), .runner = unparseable)
  bench <- data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
                      share = c(.5, .5))
  expect_no_error(rc <- panel_calibrate(r, bench, "all-NA study"))
  cal <- attr(rc, "calibration")
  # the calibration artifact still exists; the covered item is full nonresponse
  expect_true(!is.null(cal))
  nr <- cal$nonresponse
  expect_equal(nr$nonresponse_rate[nr$item_id == "plan"], 1)
  # the benchmark responses survive with silicon share 0
  plan_rows <- cal$table[cal$table$item_id == "plan", ]
  expect_true(all(plan_rows$share_silicon == 0))
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
  r2 <- r; attr(r2, "instrument") <- NULL
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
  expect_equal(length(unique(r$persona_id)), 6L)
})

test_that("panel_from_personas respects a rows predicate", {
  skip_if_not_installed("LLMR")
  p <- panel_from_personas(LLMR::anes_2024_personas,
                           rows = function(d) d$ideology_score > 0.5, n = 4)
  expect_equal(nrow(p), 4L)
})
