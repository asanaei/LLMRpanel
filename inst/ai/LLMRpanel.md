---
name: llmrpanel
description: Calibrated silicon samples for survey and experiment design in R - persona panels from population margins, Likert/choice/vignette/conjoint instruments with recorded order randomization, coverage-aware calibration against human benchmarks, bias audits.
---

# LLMRpanel -- usage capsule for AI assistants

This file is the compact manual: enough to use the package correctly
without reading every help page. `vignette("design", package =
"LLMRpanel")` goes deeper.

## Install

```r
remotes::install_github("asanaei/LLMRpanel")   # depends on LLMR (>= 0.8.9)
```

## The stance (encoded in the objects, not just prose)

Silicon panels are design-stage instruments -- pretesting, piloting, power
planning -- and instruments for measuring model behavior. Results print an
**UNCALIBRATED** banner until `panel_calibrate()` compares them to a human
benchmark; partial benchmarks earn only **PARTIALLY CALIBRATED (k/m)**.
Do not present uncalibrated silicon marginals as population estimates.

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

# price the human study from the pilot's dispersion
panel_power(resp, effect = c(fund = 0.15))
```

## Offline runner

`panel_administer()` takes a `.runner` argument: a `function(experiments, ...)`
that returns the experiments with a `response_text` column. Pass one to run
offline or deterministically in tests; the default is a live `LLMR::call_llm_par()`
call. In the experiments frame the persona is in `messages[["system"]]` and the
item and options in `messages[["user"]]`.

## Rules

- Supply margins yourself (ACS/ANES/CES); the package ships no populations.
- `panel_from_margins()` samples each margin independently (no joint
  structure); use `panel_from_data()` to draw from microdata rows and keep
  the joint distribution. For estimation-like uses the difference matters,
  and `panel_calibrate()` shows it.
- Benchmark frame needs `item_id`, `response`, `share`; shares per item
  should sum to 1 (warned otherwise). Calibration compares only covered
  items; coverage is part of the printed verdict.
- Option order is a treatment: leave randomization on and read
  `panel_bias_audit()`'s per-item order-effect p-values.
- Likert `score` is the 1-based position on the scale as given.
- Parse failures stay `NA` and are reported as nonresponse; do not drop.
- `amce()` needs an administration of a `conjoint_instrument()`; it
  clusters standard errors by persona and reports baselines as 0 / NA.
- `panel_power()` priors inherit the pilot's calibration status; an
  uncalibrated pilot prices the design stage, it does not certify effects.
- `LLMR::diagnostics()` returns the bias audit plus calibration-state
  fields; `LLMR::report()` delegates to `panel_report()`.
- `tibble::as_tibble()` strips panel-specific classes from panels and
  response objects.
- Set a seed BEFORE calls for reproducible panels/designs; functions never
  set seeds internally.

## Error meanings

- "covers none of the administered items" -> benchmark `item_id`s do not
  match the instrument's ids.
- "too small for distinct profiles" (warning) -> enlarge conjoint attribute
  levels or accept duplicate profiles knowingly.
- "must be a *named* probability vector" -> name every margin level.
- "needs an administration of a conjoint_instrument()" -> `amce()` was given
  a plain instrument; build it with `conjoint_instrument(design)`.
