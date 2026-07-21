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
`panel_calibrate()` compares response shares with a supplied human benchmark.
Printed results state whether the benchmark covers none, some, or all closed
items. Without such a benchmark, the shares describe the model run and do not
estimate a human population.

## Core API (exact signatures)

```r
panel_from_margins(margins, n, persona_template = NULL)
panel_from_data(data, n, persona_template = NULL, columns = NULL,
                weights = NULL)             # joint draws from microdata rows
panel_from_personas(data = NULL, rows = NULL, n = NULL, weights = NULL)
as_persona_frame(data, questions = NULL, demographics = NULL, answers = NULL)
item_likert(id, text, scale = c("strongly disagree", "disagree", "neutral",
                                "agree", "strongly agree"))
item_choice(id, text, options)
item_open(id, text)
panel_instrument(items, randomize = c("item_order", "option_order"))
vignette_design(template, factors)
conjoint_design(attributes, n_tasks = 5L, profiles_per_task = 2L)
conjoint_instrument(design, question = "Which profile do you prefer?")

panel_administer(panel, instr, config, .runner = NULL, max_calls = 5000L,
                 confirm = FALSE, price_table = NULL, tokens_per_call = NULL, ...)
panel_administer_batch(panel, instr, config, state_path = NULL)
panel_administer_fetch(job)
panel_batch_status(job)
panel_usage(responses, price_table = NULL)
panel_calibrate(responses, benchmark, benchmark_name = "benchmark")
panel_bias_audit(responses)
amce(responses)                              # AMCEs, persona-clustered SEs
panel_power(responses, effect, items = NULL, focal = NULL, alpha = 0.05, power = 0.80)
panel_report(responses)
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

instr <- panel_instrument(list(
  item_likert("wk4",  "A four-day work week would benefit society."),
  item_choice("fund", "Fund first?", c("public transit", "road repair"))))

cfg  <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
resp <- panel_administer(panel, instr, cfg)
panel_bias_audit(resp)                    # order effects, nonresponse
LLMR::diagnostics(resp)                   # audit plus calibration state
bench <- data.frame(item_id = "fund",     # from ANES/GSS/your fielded study
                    response = c("public transit", "road repair"),
                    share = c(.41, .59))
resp <- panel_calibrate(resp, bench, "city survey 2025")
panel_report(resp)
LLMR::report(resp)

# conjoint: design -> instrument -> administer -> AMCEs
set.seed(110)
design <- conjoint_design(list(price = c("low", "high"),
                               origin = c("domestic", "imported")), n_tasks = 4)
cj <- panel_administer(panel, conjoint_instrument(design), cfg)
amce(cj)

# calculate sample sizes from the pilot's dispersion
panel_power(resp, effect = c(fund = 0.15))
```

## Offline runner

`panel_administer()` takes a `.runner` argument: a `function(experiments, ...)`
that returns the experiments with a `response_text` column. Pass one to run
offline or deterministically in tests; the default is a live `LLMR::call_llm_par()`
call. In the experiments frame the persona is in `messages[["system"]]` and the
item and options in `messages[["user"]]`.

## Rules

- The package contains no population data. Supply the margins or source rows
  used to construct a panel.
- `panel_from_margins()` samples attributes independently.
  `panel_from_data()` samples complete microdata rows and retains relationships
  among the selected columns.
- A benchmark data frame has columns `item_id`, `response`, and `share`.
  Shares should sum to 1 within each item. The comparison uses covered items
  and reports the number of closed items covered.
- `panel_instrument()` randomizes item and option order by default.
  `panel_bias_audit()` tests whether responses are associated with the first
  option shown.
- Likert `score` is the 1-based position in the scale supplied to
  `item_likert()`.
- Unmatched closed-item replies remain `NA` and are counted as nonresponse.
- `amce()` requires responses from `conjoint_instrument()` and clusters
  standard errors by persona. Baseline rows have estimate 0 and missing
  standard errors.
- `panel_power()` uses pilot dispersion to calculate sample sizes. It does not
  validate the specified effect.
- `LLMR::diagnostics()` returns the bias audit and calibration fields.
  `LLMR::report()` returns `panel_report()`.
- `tibble::as_tibble()` removes the panel class. For response objects it also
  removes the panel, instrument, and calibration attributes.
- Set a seed before reproducible panel or design draws. Package functions do
  not set one.

## Error meanings

- "covers none of the administered items" -> benchmark `item_id`s do not
  match the instrument's ids.
- "too small for distinct profiles" (warning) -> enlarge conjoint attribute
  levels or accept duplicate profiles knowingly.
- "must be a *named* probability vector" -> name every margin level.
- "needs an administration of a conjoint_instrument()" -> `amce()` was given
  a plain instrument; build it with `conjoint_instrument(design)`.
