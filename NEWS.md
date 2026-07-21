# LLMRpanel 0.6.0

Initial CRAN release.

- `panel_from_margins()`, `panel_from_data()`, and
  `panel_from_personas()` create panels from supplied margins, microdata
  rows, or persona data frames. `as_persona_frame()` attaches question and
  field metadata to a data frame.
- `panel_administer()` administers every item to every persona. It can
  randomize item and option order and records `item_position` and
  `option_order`.
- `panel_administer_batch()` submits an administration to a provider's
  asynchronous batch API. `panel_batch_status()` and
  `panel_administer_fetch()` inspect and retrieve the job.
- `panel_calibrate()` compares response shares with supplied human
  benchmarks and records benchmark coverage. `plot()` displays the
  compared shares.
- `panel_bias_audit()` reports parse failures and first-option sensitivity.
- `conjoint_instrument()` creates conjoint choice tasks, and `amce()`
  estimates average marginal component effects with standard errors
  clustered by respondent.
- `panel_power()` calculates two-arm sample sizes from pilot dispersion.
- `run_panel_studio()` provides the panel workflow in an optional Shiny
  application and can download responses, reports, and calibration tables.
