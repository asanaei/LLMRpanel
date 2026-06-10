# LLMRpanel

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Calibrated silicon samples for survey and experiment **design**, built on
[LLMR](https://github.com/asanaei/LLMR). Persona panels from population
margins you supply; Likert, choice, open, vignette, and conjoint
instruments; option-order randomization recorded per response; and a
methodological stance encoded in the objects themselves — every result
prints an **UNCALIBRATED** banner until comparison against a human
benchmark fills the calibration slot.

```r
# remotes::install_github("asanaei/LLMRpanel")
library(LLMRpanel)
set.seed(110)

panel <- panel_from_margins(
  list(age   = c("18-34" = .3, "35-64" = .45, "65+" = .25),
       party = c(left = .45, right = .45, independent = .1)),
  n = 500,
  persona_template = "A {age}-year-old voter who leans {party}.")

instr <- instrument(list(
  item_likert("wk4",  "A four-day work week would benefit society."),
  item_choice("fund", "Fund first?", c("public transit", "road repair"))))

cfg  <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
resp <- administer(panel, instr, cfg)
resp                       # UNCALIBRATED banner, by design
bias_audit(resp)           # option-order effects, refusals
resp <- calibrate(resp, anes_marginals, "ANES 2024")
panel_report(resp)         # deviations measured, never massaged
```

**What silicon panels are for** — pretesting instruments overnight for the
price of electricity, piloting vignette/conjoint designs before spending
human-subjects budgets, and measuring model behavior under personas.
Estimating human quantities is the reading that must be *earned*, item by
item, in `calibrate()`.

**Status: design scaffold (0.1).** Panels, instruments, vignette/conjoint
designs, administration with order randomization, calibration, bias
audits, and reports work today (offline-tested). Arriving in 0.2:
`administer(mode = "logprob")` (full option distributions from one forward
pass), `persona_paraphrase()`, `silicon_power()`, `amce()`, joint
microdata draws. See `vignette("design")`.

Part of the LLMR ecosystem: [LLMR](https://github.com/asanaei/LLMR) ·
[LLMRAgent](https://github.com/asanaei/LLMRAgent) ·
[LLMRcoder](https://github.com/asanaei/LLMRcoder) ·
[LLMRtrail](https://github.com/asanaei/LLMRtrail) ·
[LLMRvalid](https://github.com/asanaei/LLMRvalid). FocusGroup (same
author) is the qualitative sibling.

MIT. Author: Ali Sanaei.
