# LLMRpanel 0.3.0

API harmonization (pre-CRAN, no deprecation shims). Generic verbs now carry
the `panel_` stem:

* `instrument()` -> `panel_instrument()`; `administer()` ->
  `panel_administer()`; `calibrate()` -> `panel_calibrate()`; `bias_audit()`
  -> `panel_bias_audit()`; `silicon_power()` -> `panel_power()`. The
  UNCALIBRATED banner and the design-stage stance are unchanged.
* Registers the shared generics `LLMR::diagnostics()`, `LLMR::report()`
  (LLMR >= 0.8.4) and `tibble::as_tibble()` on panel objects.

# LLMRpanel 0.2.0

- `conjoint_instrument()` and `amce()`: render a conjoint design into one
  forced-choice item per task, and estimate average marginal component
  effects by OLS on treatment-coded dummies with CR1 standard errors
  clustered by persona. `conjoint_design()` now retains its attribute list
  so the instrument and the estimator can read it.
- `silicon_power()`: analytic two-arm sample sizes for the planned human
  study, with dispersion priors taken from the silicon pilot (Likert
  standard deviation, choice modal share); the priors inherit the panel's
  calibration status.
- `panel_from_data()`: draw personas from microdata rows, preserving the
  joint distribution of attributes, as the counterpart of
  `panel_from_margins()`.

# LLMRpanel 0.1.0

First public release of calibrated silicon sampling.

- `panel_from_margins()`: persona panels from user-supplied population
  margins (no data shipped, no "default" populations), with
  template-rendered persona text.
- `item_likert()` / `item_choice()` / `item_open()` and `instrument()`
  with per-respondent item- and option-order randomization, recorded per
  response; `vignette_design()` and `conjoint_design()` for factorial
  stimuli (conjoint profiles within a task are guaranteed distinct, with a
  warning when the attribute space is too small to allow it).
- `administer()`: persona-conditioned answering through LLMR's parallel
  engine; replies matched to offered options, Likert positions scored,
  failures kept as `NA`.
- `calibrate()`: coverage-aware comparison to a human benchmark,
  restricted to items the benchmark covers, with per-item nonresponse
  recorded, benchmark shares checked to sum to 1, and a three-state banner
  (UNCALIBRATED, PARTIALLY CALIBRATED, calibrated). Deviations are
  reported, not adjusted away.
- `bias_audit()`: option-order effects (chi-squared per item) and
  nonresponse; `panel_report()` leads with calibration status.
- The design vignette runs fully offline (a simulated respondent through
  the `.runner` seam), with a gated live section.
