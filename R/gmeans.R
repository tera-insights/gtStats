GMeans <- function(data, p = 2, alpha = 0.01, epsilon = 0.001,
                   inputs = AUTO, outputs = result) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  gla <- GLA(statistics::G_Means, p = p, alpha = alpha, epsilon = epsilon)
  agg <- Aggregate(data, gla, inputs, outputs)
  agg
}
