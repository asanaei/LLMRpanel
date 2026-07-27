## Resubmission

Resubmission of LLMRpanel 0.6.0. The incoming pretest of 2026-07-27 gave one
WARNING under "checking dependencies in R code": three objects used by the
optional GUI (LLMR.shiny::help_tip, LLMR.shiny::llmr_theme,
LLMR.shiny::text_block_output) are not exported by LLMR.shiny 0.1.1, the
version the pretest machines held. LLMR.shiny 0.1.2 exports them. The
Suggests entry now states the version requirement, LLMR.shiny (>= 0.1.2).

The accompanying NOTE flagged "Benchmarked" in the Description as possibly
misspelled; it is ordinary English, the past participle of "benchmark".

## The package

The package Imports LLMR (>= 0.8.9), which is on CRAN. It Suggests
LLMR.shiny (>= 0.1.2); every use of LLMR.shiny (and of the other GUI
dependencies shiny, bslib, DT) is guarded with requireNamespace(), and the
package's core has no Shiny dependency. The test suite and the vignette run
fully offline through the package's `.runner` seam; no example, test, or
vignette makes a network call.

## Test environments

- local macOS 26.5 (arm64), R 4.4.3
- R CMD build; R CMD check --as-cran on the tarball

## R CMD check results

0 errors | 0 warnings | 3 notes

- "checking CRAN incoming feasibility ... NOTE: New submission".
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; no network during the check.
- "checking HTML version of manual ... NOTE": emitted by an older system
  `tidy` that does not recognize the HTML5 elements R generates; it does not
  reproduce on CRAN.
