## Submission

New release of LLMRpanel (0.4.0). It adds an optional Shiny GUI
(`run_panel_studio()`) whose dependencies (shiny, bslib, DT, and LLMR.shiny) are in
Suggests and guarded with requireNamespace(); the package's core has no Shiny
dependency. It Imports LLMR. Submitted after LLMR and LLMR.shiny are on CRAN.

## Test environments

- local macOS (R 4.4.3)
- R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | notes as below.

- "checking for future file timestamps ... NOTE" and "checking HTML version of
  manual ... NOTE": both environmental; neither reproduces on CRAN.

The `Remotes` field has been removed.
