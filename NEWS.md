# LLMRpanel 0.6.0

Initial CRAN release.

- Persona panels from population margins (`panel_from_margins()`), from
  microdata rows (`panel_from_data()`), or from a persona data frame
  (`panel_from_personas()`, `as_persona_frame()`); the package ships no
  demographic data of its own.
- Instrument administration (`panel_administer()`, with
  `panel_administer_batch()` for the provider's asynchronous batch API):
  item and option order randomized per respondent and recorded in the
  responses (`item_position`, `option_order`).
- Calibration-first stance: every result prints an UNCALIBRATED banner until
  `panel_calibrate()` compares it to a human benchmark, with coverage counted
  and `plot()` showing silicon and human shares by item and response level.
- Bias audit (`panel_bias_audit()`), analytic two-arm power (`panel_power()`),
  and conjoint AMCEs (`conjoint_instrument()`, `amce()`) with
  respondent-clustered standard errors.
- Optional Shiny GUI (`run_panel_studio()`) with a downloadable archive
  containing the responses table as CSV, the report as text, and the
  calibration table as CSV when present; all GUI dependencies are in Suggests
  and guarded.
