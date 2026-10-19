# Conjoint instruments, AMCEs, power, and microdata panels run offline
# through the .runner seam.

test_panel <- function(n = 6) {
  group <- rep(c("A", "B"), length.out = n)
  out <- tibble::tibble(persona_id = seq_len(n), group = group,
                        persona = paste("group:", group))
  structure(out, class = c("silicon_panel", class(out)),
            margins = list(group = prop.table(table(group))))
}

known_design <- function() {
  profiles <- tibble::tibble(
    task = rep(1:8, each = 2),
    profile = rep(1:2, times = 8),
    color = c("red", "blue", "blue", "red", "red", "blue", "blue", "red",
              "red", "red", "blue", "blue", "red", "blue", "blue", "red"),
    cost  = c("low", "high", "low", "high", "high", "low", "high", "low",
              "low", "high", "low", "high", "low", "low", "high", "high"))
  structure(
    list(
      profiles = profiles,
      attributes = list(color = c("blue", "red"),
                        cost = c("low", "high"))),
    class = "conjoint_design")
}

prefer_red_runner <- function(experiments, ...) {
  user <- vapply(experiments$messages, `[[`, "", "user")
  sys  <- vapply(experiments$messages, `[[`, "", "system")
  experiments$response_text <- vapply(seq_along(user), function(i) {
    lines <- grep("^Profile [0-9]+:",
                  strsplit(user[i], "\n", fixed = TRUE)[[1]], value = TRUE)
    hit <- grep("color: red", lines, fixed = TRUE)
    if (length(hit)) sub(":.*", "", lines[hit[1]])
    else if (grepl("group: A", sys[i], fixed = TRUE)) "Profile 1" else "Profile 2"
  }, character(1))
  experiments
}

mixed_runner <- function(experiments, ...) {
  user <- vapply(experiments$messages, `[[`, "", "user")
  sys  <- vapply(experiments$messages, `[[`, "", "system")
  experiments$response_text <- vapply(seq_along(user), function(i) {
    lines <- grep("^Profile [0-9]+:",
                  strsplit(user[i], "\n", fixed = TRUE)[[1]], value = TRUE)
    target <- if (grepl("group: A", sys[i], fixed = TRUE)) "color: red" else "cost: high"
    hit <- grep(target, lines, fixed = TRUE)
    if (length(hit)) sub(":.*", "", lines[hit[1]])
    else if (grepl("group: A", sys[i], fixed = TRUE)) "Profile 1" else "Profile 2"
  }, character(1))
  experiments
}

test_that("conjoint_instrument builds task items and stores the design", {
  design <- known_design()
  instr <- conjoint_instrument(design, "Choose one.")
  expect_s3_class(instr, "panel_instrument")
  expect_equal(instr$randomize, "option_order")
  expect_equal(length(instr$items), length(unique(design$profiles$task)))
  expect_equal(instr$items[[1]]$id, "task_1")
  expect_equal(instr$items[[1]]$options, c("Profile 1", "Profile 2"))
  expect_identical(instr$items[[1]]$text, "Choose one.")
  expect_identical(instr$items[[1]]$conjoint$attributes,
                   design$attributes)
  expect_identical(instr$conjoint, design)
})

test_that("conjoint_design keeps profiles and attributes in separate components", {
  set.seed(110)
  d <- conjoint_design(list(a = c("x", "y"), b = c("p", "q")), n_tasks = 3)
  expect_s3_class(d, "conjoint_design")
  expect_named(d, c("profiles", "attributes"))
  expect_s3_class(d$profiles, "tbl_df")
  expect_named(d$profiles, c("task", "profile", "a", "b"))
  expect_identical(d$attributes,
                   list(a = c("x", "y"), b = c("p", "q")))
  expect_null(attr(d, "attributes"))
  printed <- utils::capture.output(print(d))
  expect_match(printed[1], "conjoint_design")
  expect_true(any(grepl("3 task", printed, fixed = TRUE)))
  expect_lte(length(printed), 2L)
})

test_that("conjoint metadata loss is detected before use", {
  design <- known_design()
  design$attributes <- NULL
  expect_error(conjoint_instrument(design), "metadata|conjoint_design")

  design <- known_design()
  class(design) <- setdiff(class(design), "conjoint_design")
  expect_error(conjoint_instrument(design), "conjoint_design")
})

test_that("conjoint_amce recovers a known positive effect and includes baselines", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = prefer_red_runner)
  out <- conjoint_amce(responses)
  expect_s3_class(out, "conjoint_amce")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out),
               c("attribute", "level", "estimate", "std_error", "ci_lo", "ci_hi",
                 "n_profiles", "n_respondents", "n_dropped_na",
                 "n_execution_failures"))
  expect_equal(nrow(out), 4)
  expect_output(print(out), "conjoint_amce")
  estimates_only <- out[, c("attribute", "level", "estimate", "std_error",
                            "ci_lo", "ci_hi")]
  expect_false(inherits(estimates_only, "conjoint_amce"))
  expect_no_warning(utils::capture.output(print(estimates_only)))
  red <- out[out$attribute == "color" & out$level == "red", ]
  expect_gt(red$estimate, 0.6)
  baselines <- out[(out$attribute == "color" & out$level == "blue") |
                     (out$attribute == "cost" & out$level == "low"), ]
  expect_true(all(baselines$estimate == 0))
  expect_true(all(is.na(baselines$std_error)))
})

test_that("conjoint_amce clustered standard errors are finite and positive", {
  panel <- test_panel(8)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = mixed_runner)
  out <- conjoint_amce(responses)
  nb <- out[!is.na(out$std_error), ]
  expect_true(all(is.finite(nb$std_error)))
  expect_true(all(nb$std_error > 0))
})

test_that("conjoint_amce drops NA task responses and counts them", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  fake <- function(experiments, ...) {
    out <- prefer_red_runner(experiments, ...)
    out$response_text[1] <- NA_character_
    out
  }
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = fake)
  out <- conjoint_amce(responses)
  expect_true(all(out$n_dropped_na == 1L))
  expect_true(all(out$n_profiles == (nrow(responses$data) - 1L) * 2L))
})

test_that("conjoint_amce reports and excludes execution failures", {
  panel <- test_panel(6)
  instrument <- conjoint_instrument(known_design())
  config <- LLMR::llm_config("groq", "any-model")
  failed_one <- function(experiments, ...) {
    out <- prefer_red_runner(experiments, ...)
    out$success <- TRUE
    out$success[1] <- FALSE
    out
  }
  set.seed(110)
  responses <- panel_administer(panel, instrument, config,
                                .runner = failed_one)
  expect_warning(out <- conjoint_amce(responses), "execution failure")
  expect_true(all(out$n_execution_failures == 1L))
  expect_true(all(out$n_profiles == (nrow(responses$data) - 1L) * 2L))
})

test_that("conjoint administration records independent respondent profile draws", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design(), "Choose one.")
  cfg <- LLMR::llm_config("groq", "any-model")
  grid <- NULL
  scripted <- function(experiments, ...) {
    grid <<- experiments
    prefer_red_runner(experiments, ...)
  }

  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = scripted)
  expect_true("profiles" %in% names(responses$data))
  expect_identical(responses$data$profiles, grid$profiles)
  expect_true(all(vapply(responses$data$profiles, function(x) {
    identical(names(x), c("task", "profile", "color", "cost"))
  }, logical(1))))

  respondent_profiles <- lapply(split(responses$data$profiles,
                                      responses$data$persona_id), function(x) {
    paste(vapply(x, function(task) {
      paste(unlist(task[c("color", "cost")], use.names = FALSE),
            collapse = "|")
    }, character(1)), collapse = "\n")
  })
  expect_false(identical(respondent_profiles[[1]], respondent_profiles[[2]]))

  prompt_matches_record <- vapply(seq_len(nrow(grid)), function(i) {
    task <- grid$profiles[[i]]
    lines <- vapply(seq_len(nrow(task)), function(j) {
      sprintf("Profile %s: color: %s; cost: %s.", task$profile[j],
              task$color[j], task$cost[j])
    }, character(1))
    grepl(paste(lines, collapse = "\n"), grid$messages[[i]][["user"]],
          fixed = TRUE)
  }, logical(1))
  expect_true(all(prompt_matches_record))

  set.seed(110)
  replay <- LLMRpanel:::.panel_build_grid(panel, instr, cfg)
  expect_identical(replay$profiles, responses$data$profiles)

  estimates <- conjoint_amce(responses)
  expect_true(all(is.finite(estimates$estimate)))
  expect_true(all(is.finite(stats::na.omit(estimates$std_error))))
})

test_that("the batch builder uses the synchronous conjoint profile structure", {
  panel <- test_panel(3)
  instr <- conjoint_instrument(known_design(), "Choose one.")
  cfg <- LLMR::llm_config("groq", "any-model")
  submitted <- NULL
  fake_submit <- function(config, messages, state_path = NULL) {
    submitted <<- messages
    structure(
      list(provider = "groq",
           custom_ids = sprintf("llmr-%06d", seq_along(messages))),
      class = "llmr_batch_job")
  }

  set.seed(110)
  synchronous <- LLMRpanel:::.panel_build_grid(panel, instr, cfg)
  set.seed(110)
  testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    .package = "LLMR",
    {
      job <- panel_batch_submit(panel, instr, cfg)
      expect_identical(submitted, synchronous$messages)
      expect_identical(job$meta$profiles, synchronous$profiles)
      expect_true(all(vapply(job$meta$profiles, function(x) {
        identical(names(x), c("task", "profile", "color", "cost"))
      }, logical(1))))
    })
})

test_that("conjoint_amce aborts for non-conjoint administrations", {
  panel <- test_panel(4)
  instr <- panel_instrument(item_choice("vote", "Choose.", c("A", "B")),
                            randomize = character(0))
  fake <- function(experiments, ...) { experiments$response_text <- "A"; experiments }
  cfg <- LLMR::llm_config("groq", "any-model")
  responses <- panel_administer(panel, instr, cfg, .runner = fake)
  expect_error(conjoint_amce(responses),
               "needs an administration of a conjoint_instrument")
})

power_responses <- function() {
  panel <- test_panel(8)
  instr <- panel_instrument(list(
    item_likert("lik", "Rate it.", scale = c("low", "mid", "high")),
    item_choice("pick", "Pick one.", c("A", "B")),
    item_open("why", "Why?")), randomize = character(0))
  fake <- function(experiments, ...) {
    experiments$response_text <- NA_character_
    experiments$response_text[experiments$item_id == "lik"] <-
      c("low", "mid", "high", "high", "mid", "low", "high", "mid")
    experiments$response_text[experiments$item_id == "pick"] <-
      c("A", "A", "A", "B", "A", "B", "A", "A")
    experiments$response_text[experiments$item_id == "why"] <- "text"
    experiments
  }
  cfg <- LLMR::llm_config("groq", "any-model")
  panel_administer(panel, instr, cfg, .runner = fake)
}

test_that("panel_from_data preserves joint structure", {
  set.seed(110)
  src <- data.frame(x = c("a", "a", "b", "b"), y = c("A", "A", "B", "B"))
  panel <- panel_from_data(src, n = 200)
  expect_true(all((panel$x == "a") == (panel$y == "A")))
  expect_s3_class(panel, "silicon_panel")
  expect_named(attr(panel, "margins"), c("x", "y"))
})

test_that("panel_from_data respects weights approximately", {
  set.seed(110)
  src <- data.frame(segment = c("low", "high"), mirror = c("L", "H"),
                    w = c(0.05, 0.95))
  panel <- panel_from_data(src, n = 1000, columns = c("segment", "mirror"),
                           weights = "w")
  share_high <- mean(panel$segment == "high")
  expect_gt(share_high, 0.90)
  expect_lt(share_high, 0.99)
  expect_true(all((panel$segment == "high") == (panel$mirror == "H")))
})

collect_warnings <- function(expr) {
  messages <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = messages)
}

test_that("conjoint_amce names its reference levels and estimation clusters", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = prefer_red_runner)
  expect_no_warning(out <- conjoint_amce(responses))
  expect_identical(attr(out, "reference"), c(color = "blue", cost = "low"))
  expect_identical(attr(out, "n_clusters"), 6L)
  expect_output(print(out), "Reference levels: color = blue; cost = low",
                fixed = TRUE)
  expect_output(print(out), "6 respondent(s), 6 estimation cluster(s)",
                fixed = TRUE)

  saved <- out
  attr(saved, "reference") <- NULL
  attr(saved, "n_clusters") <- NULL
  printed <- utils::capture.output(print(saved))
  expect_match(printed[1], "6 respondent(s) |", fixed = TRUE)
  expect_false(any(grepl("Reference levels", printed, fixed = TRUE)))
})

test_that("a lost first level is reported and the new reference is recorded", {
  panel <- test_panel(12)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  blue_fails <- function(experiments, ...) {
    out <- prefer_red_runner(experiments, ...)
    user <- vapply(out$messages, `[[`, "", "user")
    out$success <- !grepl("color: blue", user, fixed = TRUE)
    out
  }
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg, .runner = blue_fails)
  got <- collect_warnings(conjoint_amce(responses))
  out <- got$value
  expect_true(any(grepl("execution failure", got$warnings)))
  expect_true(any(grepl(
    "`color`: the first declared level \"blue\" is absent, so effects are relative to \"red\".",
    got$warnings, fixed = TRUE)))
  expect_identical(attr(out, "reference"), c(color = "red", cost = "low"))
  expect_false("blue" %in% out$level)
  expect_identical(out$estimate[out$attribute == "color"], 0)
})

test_that("an absent later level is named, and failed personas are not clusters", {
  panel <- test_panel(8)
  set.seed(110)
  design <- conjoint_design(
    list(color = c("blue", "red", "green"), cost = c("low", "high")),
    n_tasks = 6)
  instr <- conjoint_instrument(design)
  cfg <- LLMR::llm_config("groq", "any-model")
  green_and_persona_1_fail <- function(experiments, ...) {
    out <- prefer_red_runner(experiments, ...)
    user <- vapply(out$messages, `[[`, "", "user")
    out$success <- !grepl("color: green", user, fixed = TRUE) &
      out$persona_id != 1L
    out
  }
  set.seed(110)
  responses <- panel_administer(panel, instr, cfg,
                                .runner = green_and_persona_1_fail)
  got <- collect_warnings(conjoint_amce(responses))
  out <- got$value
  expect_true(any(grepl("`color`: \"green\" is absent and has no estimate.",
                        got$warnings, fixed = TRUE)))
  expect_false(any(grepl("first declared level", got$warnings, fixed = TRUE)))
  expect_identical(attr(out, "reference"), c(color = "blue", cost = "low"))
  expect_false("green" %in% out$level)
  estimated <- responses$data$success & !is.na(responses$data$response)
  expect_false(any(estimated[responses$data$persona_id == 1L]))
  expect_identical(out$n_respondents[1], 8L)
  expect_identical(attr(out, "n_clusters"),
                   length(unique(responses$data$persona_id[estimated])))
  expect_lt(attr(out, "n_clusters"), 8L)
})
