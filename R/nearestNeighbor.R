NearestNeighbor <- function(data, state, inputs = AUTO, outputs, normalization = TRUE) {
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
  if (length(inputs) != 1)
    Stop("There must be exactly one input specified.")

  gt <- GT(statistics::Nearest_Neighbor,
           normalization = normalization)

  Transform(data, gt, inputs, outputs, list(state))
}
