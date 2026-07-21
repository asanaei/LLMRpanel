# Live integration demo for LLMRpanel: a small silicon-survey administration
# against a real (cheap) model. Lives under inst/integration so R CMD check
# never runs it; you are billed only when you run it yourself. It exercises:
#   item_likert() + item_choice() -> panel_instrument()
#   panel_from_margins() (synthetic personas) -> panel_administer() (live)
#   -> LLMR::report()
#
# The survey items are neutral, authored for this demo (no copyrighted text).
# Options are terse because panel_administer() exact-matches the model's reply
# against the option strings.
#
# Run with a key in the environment, e.g.:
#   GROQ_API_KEY=... Rscript inst/integration/demo-panel.R

# A live runner that fails loud on any unsuccessful row (see demo-coder.R).
.live_runner <- function(experiments, ...) {
  res <- LLMR::call_llm_par(experiments, progress = FALSE, max_workers = 1L, ...)
  if ("success" %in% names(res) && !all(res$success %in% TRUE)) {
    nfail <- sum(!(res$success %in% TRUE))
    stop(sprintf("%d/%d live calls failed (check the API key, model id, and rate limits).",
                 nfail, nrow(res)))
  }
  res
}

run_panel_demo <- function(provider = Sys.getenv("LLMR_DEMO_PROVIDER", "groq"),
                           model = Sys.getenv("LLMR_DEMO_MODEL", "openai/gpt-oss-20b"),
                           n = 6L) {
  stopifnot(requireNamespace("LLMRpanel", quietly = TRUE))
  library(LLMRpanel)

  # Terse option strings: the model is asked to answer with exactly one of them,
  # and panel_administer() exact-matches the reply.
  instrument <- panel_instrument(
    list(
      item_likert("libraries_value",
                  paste0("On a scale of 1 to 5, how strongly do you agree that public ",
                         "libraries are a good use of town funds? Answer with one digit ",
                         "1 to 5 only."),
                  scale = c("1", "2", "3", "4", "5")),
      item_choice("library_priority",
                  paste0("Which matters more for a public library: longer hours or more ",
                         "books? Answer with exactly one word: hours or books."),
                  options = c("hours", "books"))
    ),
    randomize = character(0))

  # A synthetic panel of n personas drawn from simple, named margins.
  set.seed(110)
  panel <- panel_from_margins(
    margins = list(
      age = c(younger = 0.5, older = 0.5),
      reads = c(frequent_reader = 0.5, rare_reader = 0.5)
    ),
    n = n,
    persona_template = "You are a {age} resident who is a {reads}."
  )

  # See demo-coder.R: reasoning models need room past hidden reasoning. 160
  # tokens reliably leaves room for the one-word answer (64 did not).
  cfg <- LLMR::llm_config(provider, model, temperature = 0, max_tokens = 160)

  # Live administration (the model answers each item as each persona).
  responses <- panel_administer(panel, instrument, cfg, .runner = .live_runner)
  report <- LLMR::report(responses)

  list(responses = responses, report = report, panel = panel, config = cfg)
}

if (sys.nframe() == 0L) {
  res <- run_panel_demo()
  cat("\n==== LLMRpanel responses ====\n")
  print(as.data.frame(res$responses)[, c("persona_id", "item_id", "response", "score")])
  cat("\n==== LLMR report ====\n")
  print(res$report)
}
