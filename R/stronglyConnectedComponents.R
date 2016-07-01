StronglyConnectedComponents <- function(data, inputs, outputs) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  if (length(inputs) != 2)
    stop("2 inputs expected.");

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 2)
    stop("2 outputs expected.")

  gla <- GLA(statistics::Strongly_Connected_Components)

  Aggregate(data, gla, inputs, outputs)
}
