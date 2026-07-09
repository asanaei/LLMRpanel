## Submission

Initial CRAN submission of LLMRpanel 0.6.0.

The package Imports LLMR (>= 0.8.9), which is on CRAN. It Suggests LLMR.shiny,
which is being submitted in sequence; every use of LLMR.shiny (and of the other
GUI dependencies shiny, bslib, DT) is guarded with requireNamespace(), and the
package's core has no Shiny dependency. The test suite and the vignette run
fully offline through the package's `.runner` seam; no example, test, or
vignette makes a network call.

## Test environments

- local macOS 26.5 (arm64), R 4.4.3
- R CMD build; R CMD check --as-cran on the tarball

## R CMD check results

0 errors | 0 warnings | 2 notes

- "checking CRAN incoming feasibility ... NOTE": New submission; and
  "Suggests or Enhances not in mainstream repositories: LLMR.shiny"
  (the companion GUI substrate submitted in sequence, see above; the check
  was run with _R_CHECK_FORCE_SUGGESTS_=false).
- "checking for future file timestamps ... NOTE: unable to verify current
  time": environmental (no network during the check); it does not reproduce
  on CRAN's machines.
