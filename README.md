# LLMRpanel <img src="man/figures/logo.png" align="right" width="120" alt="LLMRpanel icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRpanel/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## What LLMRpanel is for

LLMRpanel administers survey and experimental instruments to panels of
language model personas. Use it to pretest instruments, pilot experimental
designs, or measure how a configured model responds under specified personas.
Without comparison against a supplied human benchmark, its response shares
describe the configured model and personas, not a human population.

To work without writing code, `run_panel_studio()` opens a point-and-click
interface that builds a panel, administers an instrument, and compares the
result with a human benchmark. It has an offline demonstration mode, so it can
be explored without a provider key. See "Graphical interface" below.

## Install and configure a model

```r
install.packages("LLMR")
remotes::install_github("asanaei/LLMRpanel")

cfg <- LLMR::llm_config(
  "groq",
  "openai/gpt-oss-20b",
  temperature = 0.8
)
```

Administration requires an explicit `LLMR::llm_config()`. Under the hood,
LLMRpanel calls language models through the LLMR package, which reads your API
key from an environment variable such as `GROQ_API_KEY`. Set it once in your
`~/.Renviron` file, a plain text file in your home directory.

## A complete panel study

The workflow constructs a panel, defines an instrument, records randomized
assignments, compares closed items with human data, and uses the response
dispersion for study planning.

### Build a persona panel

`panel_from_margins()` samples each attribute independently from supplied
population margins. This route is appropriate when the available population
information consists of marginal tables.

```r
library(LLMRpanel)
set.seed(110)

panel <- panel_from_margins(
  list(
    cohort = c(young = .30, middle = .45, older = .25),
    party = c(left = .45, right = .45, independent = .10)
  ),
  n = 60,
  persona_template = "A {cohort} voter who leans {party}."
)
```

When microdata are available, `panel_from_data()` samples complete rows and can
use a sampling weight. It therefore retains relationships among the selected
attributes. `as_persona_frame()` attaches question wording and distinguishes
demographic fields from stated answers before those rows are rendered.

```r
source_rows <- data.frame(
  age = c("18 to 34", "35 to 64", "65 plus"),
  party = c("left", "independent", "right"),
  survey_weight = c(1.2, 0.9, 1.4)
)

persona_rows <- as_persona_frame(
  source_rows,
  questions = c(party = "Party identification"),
  demographics = "age",
  answers = "party"
)

microdata_panel <- panel_from_data(
  persona_rows,
  n = 60,
  columns = c("age", "party"),
  weights = "survey_weight"
)

anes_panel <- panel_from_personas(LLMR::anes_2024_personas, n = 60)
```

`panel_from_personas()` uses prepared persona rows, including the question
wording and demographic metadata in `LLMR::anes_2024_personas`. Reports identify
whether a panel came from margins, microdata rows, or prepared personas.

### Define an instrument

An instrument can combine Likert, choice, and open response items.

```r
instrument <- panel_instrument(list(
  item_likert(
    "wk4",
    "A four-day work week would benefit society."
  ),
  item_choice(
    "fund",
    "Which investment should be funded first?",
    c("public transit", "road repair")
  ),
  item_open(
    "reason",
    "What is the main reason for your answer?"
  )
))
```

### Administer with respondent randomization

`panel_administer()` sends each item to each persona. By default,
`panel_instrument()` randomizes item order and closed item option order for each
respondent. The response data record `item_position` and `option_order`, so the
realized assignments remain available for analysis.

```r
resp <- panel_administer(panel, instrument, cfg)
resp
resp$data
```

### Compare responses with human data

`panel_benchmark()` compares valid model response shares with supplied human
shares for matching item and response values. It records benchmark coverage,
deviations, and nonresponse. Printed results distinguish unbenchmarked,
partially benchmarked, and benchmarked studies.

```r
bench <- data.frame(
  item_id = "fund",
  response = c("public transit", "road repair"),
  share = c(.41, .59)
)

resp <- panel_benchmark(resp, bench, "city survey, 2025")
resp
resp$benchmark$table
resp$benchmark$nonresponse
plot(resp)
LLMR::report(resp)
```

Coverage of a closed item permits comparison for that item. It does not turn
uncovered items into estimates of a human population.

### Inspect order sensitivity and parsing failures

`panel_bias_audit()` counts execution and parsing failures by item. For closed
items with randomized options, it tests whether the selected response is
associated with the option shown first.

```r
panel_bias_audit(resp)
LLMR::diagnostics(resp)
```

The test concerns the first option shown, not the full option permutation.

### Run a conjoint study and estimate AMCEs

`conjoint_design()` defines the attribute universe and number of tasks.
Administration draws profiles for each respondent and records those profiles
with the response. `conjoint_amce()` estimates average marginal component
effects from the recorded assignments, with standard errors clustered by
persona.

```r
set.seed(110)
design <- conjoint_design(
  list(
    price = c("low", "high"),
    origin = c("domestic", "imported")
  ),
  n_tasks = 4
)

cj_instrument <- conjoint_instrument(
  design,
  "Which product would you buy?"
)
cj_resp <- panel_administer(panel, cj_instrument, cfg)
conjoint_amce(cj_resp)
```

### Plan a human study from pilot dispersion


## Graphical interface

`run_panel_studio()` provides a Shiny interface for panel construction,
instrument administration, benchmark comparison, and study artifacts.

```r
run_panel_studio()
```

## Batch administration, usage, and result fields

For large studies, `panel_batch_submit()` sends the administration to a
provider's asynchronous batch API. `panel_batch_status()` checks the job and
`panel_batch_fetch()` retrieves completed results. A `state_path` stores the job
for later status or fetch calls.

```r
job <- panel_batch_submit(
  panel,
  instrument,
  cfg,
  state_path = "panel-job.rds"
)
panel_batch_status(job)
batch_resp <- panel_batch_fetch(job)
```

Synchronous and batch administration return the same response fields and
attached study information. Both require an explicit configuration and stop
above `max_calls` unless `confirm = TRUE`.

A `panel_responses` object stores response rows in `$data` and the panel,
instrument, benchmark, and usage records as separate components. Response rows
retain `response_text`, `response_id`, `success`, `model`, and `provider`;
`finish_reason` is present when the response function supplies it. This keeps
an unmatched reply available for inspection. `panel_usage()` summarizes the
usage component and retains model and provider, which permits a supplied price
table to match the model that incurred the usage.

```r
panel_usage(batch_resp)
tibble::as_tibble(batch_resp)
```

## Interoperability

`FocusGroup::create_agents_from_data()` accepts a `silicon_panel`, so panel
personas can define participants in a moderated group discussion.
`LLMR::anes_2024_personas` supplies prepared persona data used by both packages.

## Related packages

LLMRpanel uses [LLMR](https://asanaei.github.io/LLMR/), the common provider
interface on CRAN. [LLMRcontent](https://asanaei.github.io/LLMRcontent/) codes
source text with codebooks, validates labels against text coded by humans, and
builds replication archives. [FocusGroup](https://asanaei.github.io/FocusGroup/)
runs moderated group discussions.
[LLMRagent](https://asanaei.github.io/LLMRagent/) provides tools for agent
experiments. The [ecosystem page](https://asanaei.github.io/LLMR-ecosystem/)
describes the package boundaries.

## Contributing

Report bugs and feature requests in the
[GitHub repository](https://github.com/asanaei/LLMRpanel). Pull requests may be
submitted there.

## License

This project uses the MIT License; see `LICENSE`.
