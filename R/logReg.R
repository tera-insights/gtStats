logReg <- function(data, ..., inputs = as.symbol("AUTO"), outputs) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(x$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  constructor <- kmeansMake(...)
  agg <- Aggregate(data, GLA(statistics::Logistic_Regression), inputs, outputs)
  class(agg) <- c("GLA", "data")

  if (exists("grokit.jobid") && !force.frame)
    View(agg)
  else
    as.object(agg)
}
