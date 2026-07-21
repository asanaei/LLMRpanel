# Tests for the large-panel scaling work: opt-in rich rendering, weights, the
# call-count preflight/gate, the asynchronous batch path (with an id-keyed join
# that must survive shuffled results), and panel_usage. All offline.

# --- A: text translation -----------------------------------------------------

test_that("plain panel_from_data rendering is unchanged (no silent text change)", {
  src <- data.frame(education = c("college", "no college"),
                    income = c("high", "low"), stringsAsFactors = FALSE)
  set.seed(110)
  p <- panel_from_data(src, n = 4, columns = c("education", "income"))
  # the historical key:value rendering
  expect_match(p$persona[1], "education: ")
  expect_match(p$persona[1], "income: ")
  expect_false(grepl("You are this person", p$persona[1]))   # not the rich frame
})

test_that("as_persona_frame opts into rich rendering with question wording", {
  df <- data.frame(age = c("35-44", "65+"),
                   pid = c("Strong Democrat", "Strong Republican"),
                   ab  = c("Always legal", "Never legal"),
                   stringsAsFactors = FALSE)
  pf <- as_persona_frame(
    df, questions = c(pid = "Party identification", ab = "Abortion position"),
    demographics = "age")
  expect_s3_class(pf, "persona_frame")
  set.seed(110)
  p <- panel_from_data(pf, n = 4, columns = c("age", "pid", "ab"))
  # rich frame: demographics as background, answers keyed by QUESTION wording
  expect_match(p$persona[1], "You are this person")
  expect_true(any(grepl("Party identification", p$persona)))
  expect_false(any(grepl("\\bpid\\b", p$persona)))           # not the column handle
})

test_that("as_persona_frame keeps id and weight columns out of the persona text", {
  df <- data.frame(age = c("30", "40"), wt = c(1.5, 0.5),
                   respondent_id = c("R1", "R2"),
                   pid = c("Democrat", "Republican"), stringsAsFactors = FALSE)
  pf <- as_persona_frame(df, questions = c(pid = "Party id"), demographics = "age")
  af <- attr(pf, "answer_fields")
  expect_false("wt" %in% af)
  expect_false("respondent_id" %in% af)
  expect_true("pid" %in% af)
  # the exclusion holds at render time too, not only in the attribute
  set.seed(110)
  p <- panel_from_data(pf, n = 4)
  expect_false(any(grepl("R1|R2|respondent_id|\\bwt\\b", p$persona)))
})

test_that("compound id/weight spellings are excluded; look-alike words are kept", {
  df <- data.frame(caseid = "1", respid = "2", userid = "3", caseweight = "4",
                   identity = "artist", weightlifting = "yes", pid = "D",
                   stringsAsFactors = FALSE)
  pf <- as_persona_frame(df, demographics = character(0))
  af <- attr(pf, "answer_fields")
  expect_false(any(c("caseid", "respid", "userid", "caseweight") %in% af))
  expect_true(all(c("identity", "weightlifting", "pid") %in% af))
})

test_that("an `answers` restriction keeps excluded columns out of the persona text", {
  df <- data.frame(age = c("30", "40"),
                   pid = c("Democrat", "Republican"),
                   propensity_bin = c("EXCLUDEME-LOW", "EXCLUDEME-HIGH"),
                   stringsAsFactors = FALSE)
  pf <- as_persona_frame(df, questions = c(pid = "Party identification"),
                         demographics = "age", answers = "pid")
  set.seed(110)
  p <- panel_from_data(pf, n = 4)
  expect_true(all(grepl("Party identification", p$persona)))
  expect_false(any(grepl("EXCLUDEME", p$persona)))
  # the analysis-only column still travels as panel data; it just stays silent
  expect_true("propensity_bin" %in% names(p))

  # the same restriction holds on the panel_from_personas path
  p2 <- panel_from_personas(pf, n = 2)
  expect_false(any(grepl("EXCLUDEME", p2$persona)))
})

test_that("panel_from_personas handles a frame with no recognized demographics", {
  pf <- as_persona_frame(
    data.frame(pid = c("Democrat", "Republican"), stringsAsFactors = FALSE),
    demographics = character(0))
  expect_message(p <- panel_from_personas(pf, n = 2),
                 "No demographic columns")
  expect_s3_class(p, "silicon_panel")
  expect_equal(nrow(p), 2L)
  expect_named(p, c("persona_id", "persona"))
  expect_true(all(grepl("questionnaire", p$persona)))
  expect_output(print(p), "silicon_panel")
})

test_that("panel_from_personas uses its second positional argument as n", {
  pf <- as_persona_frame(
    data.frame(age = c("20", "30", "40", "50"),
               answer = c("a", "b", "c", "d"),
               stringsAsFactors = FALSE),
    demographics = "age")
  set.seed(110)
  p <- panel_from_personas(pf, 2)
  expect_equal(nrow(p), 2L)
})

# --- A3: weights + large-n warning -------------------------------------------

test_that("panel_from_personas accepts weights and warns on heavy duplication", {
  df <- data.frame(age = c("a", "b", "c"), w = c(10, 1, 1), stringsAsFactors = FALSE)
  pf <- as_persona_frame(df, demographics = "age")
  set.seed(110)
  p <- suppressWarnings(panel_from_personas(pf, n = 30, weights = "w"))
  expect_equal(nrow(p), 30L)
  # heavily-weighted "a" should dominate the draw
  expect_true(mean(grepl("\\ba\\b", p$persona)) > 0.5)
  # n >> distinct pool warns once
  expect_warning(panel_from_personas(pf, n = 30), "Diversity is capped")
})

# --- B1: preflight + gate ----------------------------------------------------

test_that("panel_administer gates a run above max_calls", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 5)
  instr <- panel_instrument(item_likert("q", "A statement."),
                            randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake")
  det <- function(experiments, ...) { experiments$response_text <- "1. Strongly agree"; experiments }
  # 5 personas x 1 item = 5 calls; max_calls = 3 -> abort unless confirm
  expect_error(panel_administer(panel, instr, cfg, .runner = det, max_calls = 3),
               "max_calls")
  expect_s3_class(
    panel_administer(panel, instr, cfg, .runner = det, max_calls = 3, confirm = TRUE),
    "panel_responses")
})

test_that("panel_batch_submit applies the same max_calls gate", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 5)
  instrument <- panel_instrument(item_likert("q", "A statement."),
                                 randomize = character(0))
  config <- LLMR::llm_config("groq", "fake")
  submissions <- 0L
  fake_submit <- function(config, messages, state_path = NULL) {
    submissions <<- submissions + 1L
    structure(list(provider = "groq",
                   custom_ids = sprintf("llmr-%06d", seq_along(messages))),
              class = "llmr_batch_job")
  }

  testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    .package = "LLMR",
    {
      expect_error(
        panel_batch_submit(panel, instrument, config, max_calls = 3),
        "max_calls")
      expect_equal(submissions, 0L)
      expect_s3_class(
        panel_batch_submit(panel, instrument, config, max_calls = 3,
                           confirm = TRUE),
        "panel_batch_job")
      expect_equal(submissions, 1L)
    })
})

test_that("the preflight computes a cost figure from caller-supplied numbers", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 5)
  instr <- panel_instrument(item_likert("q", "S.", scale = c("No", "Yes")),
                            randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake-model")
  det <- function(experiments, ...) { experiments$response_text <- "Yes"; experiments }
  pt <- data.frame(model = "fake-model", input = 1, output = 2)

  # a single total-token figure prices as a range (all-input to all-output):
  # 5 calls x 1000 tokens = 0.005 mtok -> 0.005 to 0.01 in price_table units
  expect_message(
    panel_administer(panel, instr, cfg, .runner = det,
                     price_table = pt, tokens_per_call = 1000),
    "est. cost 0.005-0.01", fixed = TRUE)

  # c(input, output) prices exactly: 5 x (600*1 + 400*2) / 1e6 = 0.007
  expect_message(
    panel_administer(panel, instr, cfg, .runner = det,
                     price_table = pt, tokens_per_call = c(600, 400)),
    "est. cost 0.007", fixed = TRUE)

  # a model with no row in a multi-row table warns instead of fabricating
  pt2 <- data.frame(model = c("m1", "m2"), input = c(1, 2), output = c(2, 4))
  expect_warning(
    panel_administer(panel, instr, cfg, .runner = det,
                     price_table = pt2, tokens_per_call = 1000),
    "no row in")
})

# --- B2: async batch path with an id-keyed join that survives shuffling ------

test_that("the batch path joins responses by id even when rows are shuffled", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 4)
  instr <- panel_instrument(list(
    item_likert("q1", "Statement one.", scale = c("No", "Yes")),
    item_likert("q2", "Statement two.", scale = c("No", "Yes"))),
    randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake")

  # Fake the LLMR batch functions: submit echoes the grid; fetch returns a frame
  # keyed by custom_id, deliberately SHUFFLED, with a distinct answer per request.
  grid <- LLMRpanel:::.panel_build_grid(panel, instr, cfg)
  fake_submit <- function(config, messages, state_path = NULL) {
    structure(list(provider = "groq", n = length(messages),
                   custom_ids = sprintf("llmr-%06d", seq_along(messages))),
              class = "llmr_batch_job")
  }
  fake_fetch <- function(job) {
    ids <- job$custom_ids
    # answer "Yes" for odd request index, "No" for even -- a known pattern
    ans <- ifelse(seq_along(ids) %% 2 == 1, "Yes", "No")
    out <- tibble::tibble(custom_id = ids, response_text = ans,
                          response_id = paste0("response-", ids),
                          sent_tokens = 5L, rec_tokens = 1L, total_tokens = 6L,
                          success = TRUE, model = "provider-returned-model",
                          provider = "provider-returned-provider")
    out[sample(nrow(out)), ]                       # SHUFFLE the rows
  }
  synchronous_runner <- function(experiments, ...) {
    experiments$response_text <- ifelse(
      seq_len(nrow(experiments)) %% 2 == 1, "Yes", "No")
    experiments$response_id <- paste0("response-", experiments$request_id)
    experiments$sent_tokens <- 5L
    experiments$rec_tokens <- 1L
    experiments$total_tokens <- 6L
    experiments$success <- TRUE
    experiments
  }

  testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    llm_batch_fetch = fake_fetch,
    .package = "LLMR",
    {
      job <- panel_batch_submit(panel, instr, cfg)
      expect_s3_class(job, "panel_batch_job")
      expect_output(print(job), "panel_batch_fetch")
      resp <- panel_batch_fetch(job)
      expect_s3_class(resp, "panel_responses")
      expect_equal(nrow(resp), 8L)                 # 4 personas x 2 items
      # the id-keyed join must map each request's known answer to the right row:
      # build the expected mapping from request order and compare.
      expected <- ifelse(seq_len(8) %% 2 == 1, "Yes", "No")
      ord <- order(resp$persona_id, resp$item_id)
      grid_ord <- order(grid$persona_id, grid$item_id)
      expect_equal(resp$response[ord], expected[grid_ord])
      # and the fetched rows come back re-sorted to grid (submission) order,
      # so the batch result is row-identical to a synchronous run of the grid
      expect_equal(resp$persona_id, grid$persona_id)
      expect_equal(resp$item_id, grid$item_id)
      expect_equal(resp$item_position, grid$item_position)
      expect_equal(resp$response, expected)
      expect_true(all(resp$model == "fake"))
      expect_true(all(resp$provider == "groq"))

      sync <- panel_administer(panel, instr, cfg,
                               .runner = synchronous_runner)
      expect_identical(names(resp), names(sync))
      expect_identical(vapply(resp, typeof, ""), vapply(sync, typeof, ""))
      expect_identical(tibble::as_tibble(resp), tibble::as_tibble(sync))
  })
})

test_that("panel_batch_fetch refuses missing or duplicate request ids", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 2)
  instrument <- panel_instrument(
    item_choice("q", "Choose.", c("No", "Yes")),
    randomize = character(0))
  config <- LLMR::llm_config("groq", "fake")
  fake_submit <- function(config, messages, state_path = NULL) {
    structure(list(provider = "groq",
                   custom_ids = sprintf("llmr-%06d", seq_along(messages))),
              class = "llmr_batch_job")
  }
  job <- testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    .package = "LLMR",
    panel_batch_submit(panel, instrument, config))

  missing_fetch <- function(job) {
    tibble::tibble(custom_id = job$custom_ids[1], response_text = "Yes")
  }
  duplicate_fetch <- function(job) {
    tibble::tibble(custom_id = rep(job$custom_ids[1], 2),
                   response_text = c("Yes", "No"))
  }
  testthat::with_mocked_bindings(
    llm_batch_fetch = missing_fetch,
    .package = "LLMR",
    expect_error(panel_batch_fetch(job), "omit"))
  testthat::with_mocked_bindings(
    llm_batch_fetch = duplicate_fetch,
    .package = "LLMR",
    expect_error(panel_batch_fetch(job), "duplicate"))
})

test_that("the batch job carries no api key", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 2)
  instr <- panel_instrument(item_likert("q", "S."), randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake")
  fake_submit <- function(config, messages, state_path = NULL) {
    structure(list(provider = "groq", custom_ids = sprintf("llmr-%06d", seq_along(messages))),
              class = "llmr_batch_job")
  }
  testthat::with_mocked_bindings(llm_batch_submit = fake_submit, .package = "LLMR", {
    job <- panel_batch_submit(panel, instr, cfg)
    # The panel job keeps sanitized model identity, but no config or key field.
    expect_null(job$config)
    expect_true(all(job$meta$model == "fake"))
    expect_true(all(job$meta$provider == "groq"))
    flat <- unlist(job$meta)
    expect_false(any(grepl("api[_-]?key|sk-", flat, ignore.case = TRUE)))
  })
})

test_that("panel and provider batch state do not share one state file", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 2)
  instrument <- panel_instrument(item_likert("q", "S."),
                                 randomize = character(0))
  config <- LLMR::llm_config("groq", "fake")
  provider_state_path <- "not-called"
  fake_submit <- function(config, messages, state_path = NULL) {
    provider_state_path <<- state_path
    structure(list(provider = "groq",
                   custom_ids = sprintf("llmr-%06d", seq_along(messages))),
              class = "llmr_batch_job")
  }
  panel_state_path <- tempfile(fileext = ".rds")

  testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    .package = "LLMR",
    {
      job <- panel_batch_submit(panel, instrument, config,
                                state_path = panel_state_path)
      expect_s3_class(job, "panel_batch_job")
      expect_true(file.exists(panel_state_path))
      expect_s3_class(readRDS(panel_state_path), "panel_batch_job")
      expect_false(identical(provider_state_path, panel_state_path))
    })
})

# --- B3: response provenance + panel_usage -----------------------------------

test_that("responses retain raw replies, execution state, and model identity", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 3)
  instrument <- panel_instrument(
    item_likert("q", "S.", scale = c("No", "Yes")),
    randomize = character(0))
  config <- LLMR::llm_config("groq", "fake-model")
  mixed <- function(experiments, ...) {
    experiments$response_text <- c("Yes", "unmatched raw reply", "Yes")
    experiments$response_id <- c("response-1", NA_character_, "response-3")
    experiments$success <- c(TRUE, TRUE, FALSE)
    experiments$error_message <- c(NA_character_, NA_character_,
                                   "provider unavailable")
    experiments$finish_reason <- c("stop", "stop", NA_character_)
    experiments
  }

  responses <- panel_administer(panel, instrument, config, .runner = mixed)
  expect_true(all(c("response_text", "response_id", "success",
                    "error_message", "finish_reason", "model", "provider") %in%
                  names(responses)))
  expect_type(responses$response_text, "character")
  expect_type(responses$response_id, "character")
  expect_type(responses$success, "logical")
  expect_type(responses$error_message, "character")
  expect_type(responses$finish_reason, "character")
  expect_type(responses$model, "character")
  expect_type(responses$provider, "character")
  expect_true(all(responses$model == "fake-model"))
  expect_true(all(responses$provider == "groq"))
  expect_identical(responses$response_text[2], "unmatched raw reply")
  expect_true(is.na(responses$response[2]))
  expect_false(responses$success[3])
  expect_identical(responses$response_text[3], "Yes")
  expect_identical(responses$error_message[3], "provider unavailable")
  expect_true(is.na(responses$response[3]))
  expect_true(is.na(responses$score[3]))
  expect_false(any(c("config", "messages") %in% names(responses)))
  audit <- panel_bias_audit(responses)
  expect_identical(audit$parse_failures, 1L)
  expect_identical(audit$execution_failures, 1L)
})

test_that("bare runners still produce typed provenance columns", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 2)
  instrument <- panel_instrument(
    item_choice("q", "Choose.", c("No", "Yes")),
    randomize = character(0))
  config <- LLMR::llm_config("groq", "fake-model")
  bare <- function(experiments, ...) {
    experiments$response_text <- "Yes"
    experiments
  }
  responses <- panel_administer(panel, instrument, config, .runner = bare)

  expect_named(
    responses,
    c("persona_id", "item_id", "type", "item_position", "option_order",
      "response_text", "response_id", "success", "error_message",
      "finish_reason", "model", "provider", "response", "score"))
  expect_true(all(is.na(responses$response_id)))
  expect_true(all(is.na(responses$success)))
  expect_true(all(is.na(responses$error_message)))
  expect_true(all(is.na(responses$finish_reason)))
  expect_true(all(responses$model == "fake-model"))
  expect_true(all(responses$provider == "groq"))
})

test_that("panel_usage reports retained tokens and has a typed empty state", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 3)
  instr <- panel_instrument(item_likert("q", "S.", scale = c("No", "Yes")),
                            randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake-model")
  # a runner that returns token columns
  with_tokens <- function(experiments, ...) {
    experiments$response_text <- "Yes"
    experiments$sent_tokens <- 5L; experiments$rec_tokens <- 1L
    experiments$total_tokens <- 6L; experiments$success <- TRUE
    experiments
  }
  r1 <- panel_administer(panel, instr, cfg, .runner = with_tokens)
  expect_false(is.null(attr(r1, "usage")))
  u <- panel_usage(r1)
  expect_false(is.null(u))
  expect_true(all(c("model", "provider") %in% names(u)))
  expect_identical(u$model, "fake-model")
  expect_identical(u$provider, "groq")
  priced <- panel_usage(
    r1,
    price_table = data.frame(model = "fake-model", input = 1, output = 2))
  expect_equal(priced$cost_estimate, 21e-6)
  expect_identical(priced$model, "fake-model")
  expect_identical(priced$provider, "groq")

  # A runner with no token columns yields a stable, typed empty usage table.
  bare <- function(experiments, ...) { experiments$response_text <- "Yes"; experiments }
  r2 <- panel_administer(panel, instr, cfg, .runner = bare)
  empty <- panel_usage(r2)
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(u))
  expect_identical(vapply(empty, typeof, ""), vapply(u, typeof, ""))
  expect_identical(names(empty)[1:2], c("model", "provider"))
  expect_type(empty$model, "character")
  expect_type(empty$provider, "character")
})
