# Evaluate `expr` and return its value with the messages of every warning it
# raised, so a test can check several warnings from one call.
collect_warnings <- function(expr) {
  warned <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warned)
}
