logReg <- function(data, ..., inputs = as.symbol("AUTO"), outputs) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  constructor <- kmeansMake(...)
  agg <- Aggregate(data, GLA(statistics::Logistic_Regression), inputs, outputs)
  class(agg) <- c("GLA", "data")

  if (exists("grokit.jobid") && !force.frame)
    View(agg)
  else
    as.object(agg)
}
