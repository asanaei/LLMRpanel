# LLMRpanel 0.6.0

Initial CRAN release.

- `panel_from_margins()`, `panel_from_data()`, and
  `panel_from_personas()` create panels from supplied margins, microdata
  rows, or persona data frames. `as_persona_frame()` attaches question and
  field metadata to a data frame. Reports identify which panel source was used.
- `panel_administer()` administers every item to every persona. It can
  randomize item and option order and records `item_position` and
  `option_order`. Its classed result stores response rows in `$data` and the
  panel, instrument, benchmark, and token usage in separate fields. Response
  rows retain `response_text`, `response_id`, `success`, `model`, and
  `provider` as columns.
- `panel_batch_submit()` submits an administration to a provider's
  asynchronous batch API. `panel_batch_status()` and `panel_batch_fetch()`
  inspect and retrieve the job. Synchronous and batch submission share the
  `max_calls` and `confirm` gate.
- `panel_benchmark()` compares response shares with supplied human
  benchmarks and records benchmark coverage. `plot()` displays the
  compared shares.
- `panel_bias_audit()` reports parse failures and first-option sensitivity.
- `conjoint_instrument()` creates conjoint choice tasks with profiles randomized
  independently for each respondent. `conjoint_design()` returns a classed list
  with profile and attribute fields, and `conjoint_amce()` estimates from the
  recorded respondent-level profiles with standard errors clustered by
  respondent in a classed result that retains run counts as columns.
- `panel_power()` calculates two-arm sample sizes from pilot dispersion.
- `run_panel_studio()` provides the panel workflow in an optional Shiny
  application and can download responses, reports, and benchmark tables. The
  studio now preserves readable long-form output, presents response shares and
  diagnostics before technical details, carries administered responses into
  benchmarking and analysis, supports repeated administrations and conjoint
  instruments, exposes power and AMCE calculations, permits full ANES persona
  inspection and field selection, honors shared generation settings, and shows
  recorded call timing when it is available. Numbered configuration sections
  can be collapsed while the administration action and run plan remain in
  view. Persona templates and item wording have editable working defaults.
  Response text and response labels receive most of the width in display
  tables, internal identifiers remain available through a column control, and
  shares, benchmark deviations, and other double columns use concise
  display-only rounding.
