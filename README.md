# LLMRpanel <img src="man/figures/logo.png" align="right" width="120" alt="LLMRpanel icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

LLMRpanel administers survey and experimental instruments to panels of
language model personas. Use it to pretest questions and designs or to
measure how a configured model responds under specified personas. It is
built on [LLMR](https://asanaei.github.io/LLMR/).

`panel_from_margins()` draws persona attributes independently from supplied
population margins. `panel_from_data()` samples complete microdata rows, and
`panel_from_personas()` uses a prepared persona data frame such as
`LLMR::anes_2024_personas`. The package contains no population data.
Instruments may contain Likert, choice, and open items.
`vignette_design()` and `conjoint_design()` create factorial designs.

`panel_administer()` sends every item to every persona. It can randomize item
and option order for each persona and records both orders.
`panel_bias_audit()` counts parse failures and tests whether responses are
associated with the first option shown.

`panel_calibrate()` compares closed-item response shares with a supplied human
benchmark. It records benchmark coverage, deviations, and nonresponse.
`plot()` displays the compared shares, and `panel_report()` summarizes the
administration. Without a benchmark, response shares describe the configured
model under the supplied personas. They do not estimate a human population.

`conjoint_instrument()` creates forced-choice items from a conjoint design.
`amce()` estimates average marginal component effects with standard errors
clustered by persona. `panel_power()` calculates two-arm sample sizes from
pilot dispersion.

`as_persona_frame()` attaches question wording and identifies demographic and
answer columns in microdata. For large administrations,
`panel_administer_batch()` submits requests to a provider's asynchronous batch
API, and `panel_administer_fetch()` retrieves the results. The synchronous
`panel_administer()` reports the request count and stops above `max_calls`
unless `confirm = TRUE`.

## Installation

```r
install.packages("LLMR")   # from CRAN
remotes::install_github("asanaei/LLMRpanel")
```

## Which package?

| Package | Use it when | Not for |
|---|---|---|
| [LLMRcontent](https://asanaei.github.io/LLMRcontent/) | Coding text with a codebook and checking labels against human-coded text | Administering survey instruments to persona panels |
| [LLMRpanel](https://asanaei.github.io/LLMRpanel/) | Administering survey and experimental instruments to persona panels | Coding collections of source text |

Both packages implement `LLMR::diagnostics()` and `LLMR::report()` methods.
LLMRcontent can also save a replication archive.

### From a silicon panel to a focus group

`FocusGroup::create_agents_from_data()` accepts a `silicon_panel`, so the same
respondent descriptions can define participants in a moderated group
discussion. `LLMR::anes_2024_personas` supplies personas used by both
packages.

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
resp
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

LLMRpanel is built on [LLMR](https://asanaei.github.io/LLMR/), the provider
layer on CRAN. [LLMRcontent](https://asanaei.github.io/LLMRcontent/) annotates
text with codebooks and checks model labels against human-coded text. It can
save robustness summaries and replication archives.
[FocusGroup](https://asanaei.github.io/FocusGroup/) simulates moderated group
discussions.
[LLMRagent](https://asanaei.github.io/LLMRagent/) provides tools for agent
experiments. The [ecosystem page](https://asanaei.github.io/LLMR-ecosystem/)
describes how the packages relate.
