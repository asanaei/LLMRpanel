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
                          sent_tokens = 5L, rec_tokens = 1L, total_tokens = 6L,
                          success = TRUE)
    out[sample(nrow(out)), ]                       # SHUFFLE the rows
  }

  testthat::with_mocked_bindings(
    llm_batch_submit = fake_submit,
    llm_batch_fetch = fake_fetch,
    .package = "LLMR",
    {
      job <- panel_administer_batch(panel, instr, cfg)
      expect_s3_class(job, "panel_batch_job")
      resp <- panel_administer_fetch(job)
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
    })
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
    job <- panel_administer_batch(panel, instr, cfg)
    # the panel-side job stores only grid metadata + the LLMR job, no config field
    expect_null(job$config)
    flat <- unlist(job$meta)
    expect_false(any(grepl("fake|api[_-]?key|sk-", flat, ignore.case = TRUE)))
  })
})

# --- B3: token retention + panel_usage ---------------------------------------

test_that("panel_usage reports retained tokens and is NULL without them", {
  panel <- panel_from_margins(list(g = c(a = .5, b = .5)), n = 3)
  instr <- panel_instrument(item_likert("q", "S.", scale = c("No", "Yes")),
                            randomize = character(0))
  cfg <- LLMR::llm_config("groq", "fake")
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

  # a runner with no token columns -> usage attr absent, panel_usage NULL
  bare <- function(experiments, ...) { experiments$response_text <- "Yes"; experiments }
  r2 <- panel_administer(panel, instr, cfg, .runner = bare)
  expect_null(panel_usage(r2))
})
