ReservoirSample <- function(data, size, inputs = AUTO, outputs = AUTO, coefficient = 22) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  gla <- GLA(statistics::Reservoir_Sampling, size = size, coefficient = coefficient)

  Aggregate(data, gla, inputs, outputs)
}
