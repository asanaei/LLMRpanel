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
  out <- tibble::tibble(
    task = rep(1:8, each = 2),
    profile = rep(1:2, times = 8),
    color = c("red", "blue", "blue", "red", "red", "blue", "blue", "red",
              "red", "red", "blue", "blue", "red", "blue", "blue", "red"),
    cost  = c("low", "high", "low", "high", "high", "low", "high", "low",
              "low", "high", "low", "high", "low", "low", "high", "high"))
  attr(out, "attributes") <- list(color = c("blue", "red"),
                                  cost = c("low", "high"))
  out
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
  expect_equal(length(instr$items), length(unique(design$task)))
  expect_equal(instr$items[[1]]$id, "task_1")
  expect_equal(instr$items[[1]]$options, c("Profile 1", "Profile 2"))
  expect_identical(instr$conjoint, design)
})

test_that("conjoint_design stores its attribute list", {
  set.seed(110)
  d <- conjoint_design(list(a = c("x", "y"), b = c("p", "q")), n_tasks = 3)
  expect_identical(attr(d, "attributes"), list(a = c("x", "y"), b = c("p", "q")))
})

test_that("amce recovers a known positive effect and includes baselines", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  responses <- panel_administer(panel, instr, cfg, .runner = prefer_red_runner)
  out <- amce(responses)
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out),
               c("attribute", "level", "estimate", "std_error", "ci_lo", "ci_hi"))
  expect_equal(nrow(out), 4)
  red <- out[out$attribute == "color" & out$level == "red", ]
  expect_gt(red$estimate, 0.6)
  baselines <- out[(out$attribute == "color" & out$level == "blue") |
                     (out$attribute == "cost" & out$level == "low"), ]
  expect_true(all(baselines$estimate == 0))
  expect_true(all(is.na(baselines$std_error)))
})

test_that("amce clustered standard errors are finite and positive", {
  panel <- test_panel(8)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  responses <- panel_administer(panel, instr, cfg, .runner = mixed_runner)
  out <- amce(responses)
  nb <- out[!is.na(out$std_error), ]
  expect_true(all(is.finite(nb$std_error)))
  expect_true(all(nb$std_error > 0))
})

test_that("amce drops NA task responses and counts them", {
  panel <- test_panel(6)
  instr <- conjoint_instrument(known_design())
  cfg <- LLMR::llm_config("groq", "any-model")
  fake <- function(experiments, ...) {
    out <- prefer_red_runner(experiments, ...)
    out$response_text[1] <- NA_character_
    out
  }
  responses <- panel_administer(panel, instr, cfg, .runner = fake)
  out <- amce(responses)
  expect_equal(attr(out, "n_dropped_na"), 1L)
  expect_equal(attr(out, "n_profiles"), (nrow(responses) - 1L) * 2L)
})

test_that("amce aborts for non-conjoint administrations", {
  panel <- test_panel(4)
  instr <- panel_instrument(item_choice("vote", "Choose.", c("A", "B")),
                            randomize = character(0))
  fake <- function(experiments, ...) { experiments$response_text <- "A"; experiments }
  cfg <- LLMR::llm_config("groq", "any-model")
  responses <- panel_administer(panel, instr, cfg, .runner = fake)
  expect_error(amce(responses),
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

test_that("panel_power matches the analytic formulas", {
  responses <- power_responses()
  out <- panel_power(responses, effect = c(lik = 0.5, pick = 0.2))
  z <- stats::qnorm(1 - 0.05 / 2) + stats::qnorm(0.80)
  sigma <- stats::sd(c(1, 2, 3, 3, 2, 1, 3, 2))
  expected_lik <- ceiling(2 * sigma^2 * z^2 / 0.5^2)
  p <- 6 / 8; p1 <- p - 0.1; p2 <- p + 0.1
  expected_pick <- ceiling(z^2 * (p1 * (1 - p1) + p2 * (1 - p2)) / 0.2^2)
  expect_equal(out$n_per_arm[out$item_id == "lik"], expected_lik)
  expect_equal(out$n_per_arm[out$item_id == "pick"], expected_pick)
  expect_equal(out$dispersion[out$item_id == "pick"], p)
  expect_false("why" %in% out$item_id)
})

test_that("panel_power is monotone in effect and accepts named effects", {
  responses <- power_responses()
  small <- panel_power(responses, effect = 0.2, items = "lik")
  large <- panel_power(responses, effect = 0.6, items = "lik")
  expect_gt(small$n_per_arm, large$n_per_arm)
  named <- panel_power(responses, effect = c(pick = 0.2, lik = 0.5),
                       items = c("pick", "lik"))
  expect_equal(named$effect, c(0.2, 0.5))
})

test_that("panel_power warns on a zero-variance prior", {
  responses <- power_responses()
  responses$response[responses$item_id == "lik"] <- "high"
  responses$score[responses$item_id == "lik"] <- 3
  expect_warning(out <- panel_power(responses, effect = c(lik = 0.5),
                                    items = "lik"),
                 "no variance in the pilot")
  expect_true(is.na(out$n_per_arm))
})

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
