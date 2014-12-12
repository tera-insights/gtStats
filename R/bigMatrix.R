BigMatrix <- function(data, inputs, outputs, block = 40, scale = 2) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  if (length(inputs) != 2)
    stop("2 inputs expected.");

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 3)
    stop("3 outputs expected.")

  gla <- GLA(statistics::Big_Matrix, block = block, scale = scale)

  Aggregate(data, gla, inputs, outputs)
}
