# LLMRpanel

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
reported as found. `panel_bias_audit()` measures option-order effects and
nonresponse from the responses themselves. For design work,
`conjoint_instrument()` renders a conjoint design into forced-choice
items, `amce()` estimates average marginal component effects with
respondent-clustered standard errors, and `panel_power()` computes
analytic two-arm power for the planned human study from the silicon
pilot's dispersion. `panel_report()` assembles the design-stage report,
calibration status first. The shared generic surface supports
`LLMR::diagnostics()`, `LLMR::report()`, and `tibble::as_tibble()`.


## Which package?

| Package | Use it when | Not for |
|---|---|---|
| [LLMRcoder](https://asanaei.github.io/LLMRcoder/) | Validated text measurement for quantitative inference | Accessible qualitative coding or text segmentation |
| [LLMRvalid](https://asanaei.github.io/LLMRvalid/) | Robustness of a downstream estimand across measurement choices | Construct validity or pass/fail validation |
| [LLMRarchive](https://asanaei.github.io/LLMRarchive/) | Reviewer-runnable replication archives from LLM audit logs | Measurement or estimation |
| [LLMRpanel](https://asanaei.github.io/LLMRpanel/) | Calibrated silicon survey samples for design-stage work | Human-population estimates without calibration to a human benchmark |

All four packages follow the same workflow contract: construct a first
object, build or extend it, run it with a `.runner` seam, read
`diagnostics()`, draft `report()`, and optionally archive with LLMRarchive.


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
family also includes [LLMRcoder](https://asanaei.github.io/LLMRcoder/),
which organizes annotation around codebooks and sealed gold-set validation
and is the natural coder for open-ended answers;
[LLMRvalid](https://asanaei.github.io/LLMRvalid/), which audits the
robustness of estimates computed from model labels;
[LLMRarchive](https://asanaei.github.io/LLMRarchive/), which turns audit logs
into verifiable replication archives; and
[LLMRAgent](https://asanaei.github.io/LLMRAgent/), which provides agents
and multi-agent designs. An overview of the family lives at the
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/).
