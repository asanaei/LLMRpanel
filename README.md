# LLMRpanel <img src="man/figures/logo.png" align="right" width="120" alt="LLMRpanel icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

LLMRpanel administers survey instruments to panels of language-model
personas, for the design stage of human studies and for the study of model
behavior itself. It is built on
[LLMR](https://asanaei.github.io/LLMR/). Panels come from population
margins the researcher supplies (`panel_from_margins()`) or from rows of
microdata (`panel_from_data()`, which preserves the joint distribution of
attributes); the package ships no demographic data of its own. Instruments
combine Likert, forced-choice, and open items, and factorial stimuli come
from `vignette_design()` and `conjoint_design()`. `panel_administer()` has every
persona answer every item, randomizes item and option order per respondent,
and records what each respondent saw, because with language models the
order in which options are listed is a treatment.

The methodological stance is carried by the objects rather than the
documentation. Every result prints an UNCALIBRATED banner until
`panel_calibrate()` compares the panel's marginals to a human benchmark, item by
item; partial benchmarks earn only a partial banner, and deviations are
reported as found. `panel_bias_audit()` reports two properties of the responses
themselves: nonresponse, and first-option sensitivity, meaning whether the answer
depends on which option was listed first. For design work,
`conjoint_instrument()` renders a conjoint design into forced-choice
items, `amce()` estimates average marginal component effects with
respondent-clustered standard errors, and `panel_power()` computes
analytic two-arm power for the planned human study from the silicon
pilot's dispersion. `panel_report()` assembles the design-stage report,
calibration status first. The shared generic surface supports
`LLMR::diagnostics()`, `LLMR::report()`, and `tibble::as_tibble()`.

A silicon panel pilots the instrument and design, not the population: it probes
question wording, response options, ordering, and statistical power. When used to
study the model itself, a larger panel mainly reduces Monte-Carlo noise in
estimating patterns such as order effects, rather than making the synthetic
respondents representative of people.

For a large panel, hand `panel_from_data()` a frame wrapped with
`as_persona_frame()` (it then renders each row by its question wording, not a flat
template), and administer it through the provider's discounted asynchronous batch
API with `panel_administer_batch()` and `panel_administer_fetch()`.
`panel_administer()` reports the call count before a run and stops above
`max_calls` unless you pass `confirm = TRUE`.

## Which package?

| Package | Use it when | Not for |
|---|---|---|
| [LLMRcontent](https://asanaei.github.io/LLMRcontent/) | Validated text measurement: codebook coding with sealed gold-set validation, robustness audits, and replication archives | Accessible qualitative coding or text segmentation |
| [LLMRpanel](https://asanaei.github.io/LLMRpanel/) | Synthetic survey panels for design-stage work, calibrated against a human benchmark when one is supplied | Human-population estimates without calibration to a human benchmark |

Both packages share one workflow: you build a first object, extend it, run it
(supplying your own runner for offline tests), then read `diagnostics()` and
draft `report()`. LLMRcontent can also seal the whole run into a replication
archive.


```r
# remotes::install_github("asanaei/LLMRpanel")
library(LLMRpanel)
set.seed(110)

panel <- panel_from_margins(
  list(cohort = c(young = .3, middle = .45, older = .25),
       party  = c(left = .45, right = .45, independent = .10)),
  n = 60,
  persona_template = "A {cohort} voter who leans {party}.")

instr <- panel_instrument(list(
  item_likert("wk4",  "A four-day work week would benefit society."),
  item_choice("fund", "Which investment should be funded first?",
              c("public transit", "road repair"))))

cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)

resp <- panel_administer(panel, instr, cfg)
resp                 # prints the UNCALIBRATED banner
panel_bias_audit(resp)
LLMR::diagnostics(resp)

bench <- data.frame(item_id = "fund",
                    response = c("public transit", "road repair"),
                    share = c(.41, .59))
resp <- panel_calibrate(resp, bench, "city survey, 2025")
panel_report(resp)
LLMR::report(resp)

panel_power(resp, effect = 0.3)
```

## The LLMR ecosystem

LLMRpanel is one of several packages for LLM-assisted research built on
[LLMR](https://asanaei.github.io/LLMR/), the provider layer on CRAN. The
family also includes [LLMRcontent](https://asanaei.github.io/LLMRcontent/),
which organizes annotation around codebooks and sealed gold-set validation,
audits the robustness of estimates computed from model labels, and turns audit
logs into verifiable replication archives; [FocusGroup](https://asanaei.github.io/FocusGroup/),
which simulates moderated group discussion; and
[LLMRagent](https://asanaei.github.io/LLMRagent/), which provides agents
and multi-agent designs. An overview of the family lives at the
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/).
