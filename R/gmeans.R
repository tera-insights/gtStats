GMeans <- function(data, p = 2, alpha = 0.01, epsilon = 0.001,
                   inputs = AUTO, outputs = result) {
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

  gla <- GLA(statistics::G_Means, p = p, alpha = alpha, epsilon = epsilon)
  agg <- Aggregate(data, gla, inputs, outputs)
  agg
}
