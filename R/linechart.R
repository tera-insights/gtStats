LineChart <- function(data, inputs, outputs, length, normalize = FALSE, p = 2) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  if (missing(length))
    stop('agument "length" is missing, with no default')

  if (length(inputs) == 2) {
    outputs <- substitute(outputs)
    check.atts(outputs)
    outputs <- convert.atts(outputs)
    if (length(outputs) != 1)
      stop("There must be exactly one output specified.")
    gla <- GLA(statistics::Line_Chart, length = length, normalize = normalize, p = p)
  } else if (length(inputs) == 3) {
    outputs <- NULL
    gla <- GLA(statistics::Line_Chart_Matrix, length = length, normalize = normalize, p = p)
  } else {
    stop("There must be either 2 or 3 inputs.")
  }

  Aggregate(data, gla, inputs, outputs)
}
