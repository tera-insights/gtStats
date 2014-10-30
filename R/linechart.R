LineChart <- function(data, inputs, outputs, length, normalize = FALSE, p = 2) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  if (missing(length))
    Stop('agument "length" is missing, with no default')

  if (length(inputs) == 2) {
    outputs <- substitute(outputs)
    check.atts(outputs)
    if (is.auto(outputs))
      Stop("outputs not allowed to be AUTO.")
    outputs <- convert.atts(outputs)
    if (length(outputs) != 1)
      Stop("There must be exactly one output specified.")
    gla <- GLA(statistics::Line_Chart, length = length, normalize = normalize, p = p)
  } else if (length(inputs) == 3) {
    outputs <- NULL
    gla <- GLA(statistics::Line_Chart_Matrix, length = length, normalize = normalize, p = p)
  } else {
    Stop("There must be either 2 or 3 inputs.")
  }

  Aggregate(data, gla, inputs, outputs)
}
