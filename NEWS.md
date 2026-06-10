# LLMRpanel 0.1.0 (design scaffold)

First public cut of calibrated silicon sampling. Working today:

- `panel_from_margins()`: persona panels from user-supplied population
  margins (no data shipped, no "default" populations), with template-
  rendered persona text.
- `item_likert()` / `item_choice()` / `item_open()` and `instrument()`
  with per-respondent item- and option-order randomization, recorded per
  response; `vignette_design()` and `conjoint_design()` for factorial
  stimuli.
- `administer()`: persona-conditioned answering through LLMR's parallel
  engine; replies matched to offered options, Likert positions scored,
  failures kept as `NA`.
- `calibrate()`: marginals compared to a human benchmark -- deviations
  reported, never adjusted away; filling the calibration slot is what
  turns off the **UNCALIBRATED** banner every print method shows.
- `bias_audit()`: option-order effects (chi-squared per item) and
  non-response; `panel_report()` leads with calibration status.

Exported as design contracts, arriving in 0.2: `administer(mode =
"logprob")`, `persona_paraphrase()`, `silicon_power()`, `amce()`.
