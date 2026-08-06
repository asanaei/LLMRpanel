---
name: llmrpanel
description: Create persona panels, administer survey and experimental instruments, and compare response shares with supplied benchmarks.
---

# LLMRpanel usage guide

This guide summarizes the principal objects and functions. For a worked
example, see `vignette("design", package = "LLMRpanel")`.

## Install

```r
remotes::install_github("asanaei/LLMRpanel")   # depends on LLMR (>= 0.8.9)
```

## Scope and benchmark comparison

Use persona panels to pretest instruments, pilot experimental designs, or
measure responses from a configured model under specified personas.
`panel_benchmark()` compares response shares with a supplied human benchmark.
Printed results state `NOT BENCHMARKED`, `PARTIALLY BENCHMARKED (n/m)`, or
`BENCHMARKED`. Without such a benchmark, the shares describe the model run and
do not estimate a human population.

## Core API (exact signatures)

```r
panel_from_margins(margins, n, persona_template = NULL)
panel_from_data(data, n, persona_template = NULL, columns = NULL,
                weights = NULL)             # joint draws from microdata rows
panel_from_personas(data, n = NULL, rows = NULL, weights = NULL)
as_persona_frame(data, questions = NULL, demographics = NULL, answers = NULL)
item_likert(id, text, scale = c("strongly disagree", "disagree", "neutral",
                                "agree", "strongly agree"))
item_choice(id, text, options)
item_open(id, text)
panel_instrument(items, randomize = "option_order")
conjoint_design(attributes, n_tasks = 5L, profiles_per_task = 2L)
conjoint_instrument(design, question = "Which profile do you prefer?")

panel_administer(panel, instrument, config, max_calls = 5000L, confirm = FALSE,
                 price_table = NULL, tokens_per_call = NULL, .runner = NULL, ...)
panel_batch_submit(panel, instrument, config, state_path = NULL,
                   max_calls = 5000L, confirm = FALSE)
panel_batch_fetch(job)
panel_batch_status(job)
panel_usage(responses, price_table = NULL)
panel_benchmark(responses, benchmark, benchmark_name = "benchmark")
panel_bias_audit(responses)
conjoint_amce(responses)                     # AMCEs, persona-clustered SEs
panel_power(responses, effect, items = NULL, focal = NULL, alpha = 0.05, power = 0.80)
run_panel_studio(...)

LLMR::diagnostics(responses)
LLMR::report(responses)
tibble::as_tibble(responses)
tibble::as_tibble(panel)
```

## Canonical workflow

```r
library(LLMRpanel)
set.seed(110)   # panel draws and order randomization are local RNG

panel <- panel_from_margins(
  list(age   = c("18-34" = .3, "35-64" = .45, "65+" = .25),
       party = c(left = .45, right = .45, independent = .10)),
  n = 500,
  persona_template = "A {age}-year-old voter who leans {party}.")

instrument <- panel_instrument(list(
  item_likert("wk4",  "A four-day work week would benefit society."),
  item_choice("fund", "Fund first?", c("public transit", "road repair"))))

cfg  <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
resp <- panel_administer(panel, instrument, cfg)
panel_bias_audit(resp)                    # order effects, nonresponse
LLMR::diagnostics(resp)                   # audit plus benchmark state
bench <- data.frame(item_id = "fund",     # from ANES/GSS/your fielded study
                    response = c("public transit", "road repair"),
                    share = c(.41, .59))
resp <- panel_benchmark(resp, bench, "city survey 2025")
LLMR::report(resp)

# conjoint: design -> instrument -> administer -> AMCEs
set.seed(110)
design <- conjoint_design(list(price = c("low", "high"),
                               origin = c("domestic", "imported")), n_tasks = 4)
cj <- panel_administer(panel, conjoint_instrument(design), cfg)
conjoint_amce(cj)

# calculate sample sizes from the pilot's dispersion
panel_power(resp, effect = c(fund = 0.15))
```

## Rules

- The package contains no population data. Supply the margins or source rows
  used to construct a panel.
- Administration requires an explicit `config`; there is no implicit provider
  default.
- `panel_batch_submit(..., state_path = path)` saves the panel batch job at
  `path` for later `panel_batch_status(path)` or `panel_batch_fetch(path)` calls.
  A saved job requires an API key handle built with
  `LLMR::llm_api_key_env()`, not a literal key.
- `panel_from_margins()` samples attributes independently.
  `panel_from_data()` samples complete microdata rows and retains relationships
  among the selected columns. `LLMR::report()` identifies margins, microdata
  rows, or supplied personas as the panel source.
- A benchmark data frame has columns `item_id`, `response`, and `share`.
  Shares should sum to 1 within each item. The comparison uses covered items
  and reports the number of closed items covered.
- `panel_instrument()` randomizes item and option order by default.
  `panel_bias_audit()` tests whether responses are associated with the first
  option shown.
- Likert `score` is the 1-based position in the scale supplied to
  `item_likert()` and is stored in `responses$data`.
- Unmatched closed-item replies remain `NA` and are counted as nonresponse.
  Their raw `response_text` remains available in `responses$data`. That tibble
  also retains `response_id`, `success`, `model`, and `provider`, plus
  `finish_reason` when the runner supplies it.
- `panel_administer()` and `panel_batch_fetch()` return the same response fields
  and attached study information: a classed list with `data`, `panel`,
  `instrument`, `benchmark`, and `usage` fields.
- `panel_usage()` summarizes `responses$usage` and keeps model and provider in
  its result so a supplied price table attaches to the model that incurred the
  usage.
- `conjoint_design()` returns a classed list with a profile tibble in
  `$profiles` and the attribute universe in `$attributes`.
- `conjoint_amce()` requires responses from `conjoint_instrument()` and clusters
  standard errors by persona. Its classed result retains run counts as ordinary
  columns. Baseline rows have estimate 0 and missing standard errors.
- `panel_power()` uses pilot dispersion to calculate sample sizes. It does not
  validate the specified effect.
- `LLMR::diagnostics()` returns the bias audit and benchmark fields.
  `LLMR::report()` returns the classed report.
- `tibble::as_tibble(responses)` returns the plain `responses$data` tibble.
- Subset or transform `responses$data`; the response object's panel,
  instrument, benchmark, and usage fields remain separate.
- Set a seed before reproducible panel or design draws. Package functions do
  not set one.

## Offline tests and recorded-response runs

`panel_administer()` takes a `.runner` argument after its ordinary arguments.
The runner is a `function(experiments, ...)` that receives a data frame with
`config` and `messages` list-columns and returns those rows with `request_id`
and `response_text` columns. Each submitted `request_id` must appear once;
returned rows may be in any order. Pass a runner to run offline or
deterministically in tests. The default is a live `LLMR::call_llm_par()` call.
In the request data frame the persona is in `messages[["system"]]` and the item
and options are in `messages[["user"]]`.

## Error meanings

- "covers none of the administered items" -> benchmark `item_id`s do not
  match the instrument's ids.
- "too small for distinct profiles" (warning) -> enlarge conjoint attribute
  levels or accept duplicate profiles knowingly.
- "must be a *named* probability vector" -> name every margin level.
- "needs an administration of a conjoint_instrument()" -> `conjoint_amce()`
  was given a plain instrument; build it with `conjoint_instrument(design)`.
