ReservoirSample <- function(data, size, inputs = AUTO, outputs = AUTO, coefficient = 22) {
  if (missing(inputs)) {
    inputs <- convert.schema(names(data$schema))
  } else {
    inputs <- substitute(inputs)
    check.exprs(inputs)
    inputs <- convert.exprs(inputs)
  }

  if (missing(outputs)) {
    outputs <- convert.names(inputs)
    missing <- which(outputs == "")
    exprs <- grokit$expressions[inputs[missing]]
    if (all(is.symbols(exprs)))
      outputs[missing] <- as.character(exprs)
    else
      stop("no name given for complex inputs:",
           paste("\n\t", lapply(exprs, deparse), collapse = ""))
  } else {
    if (!is.null(names(inputs)))
      warning("both outputs and named inputs given. outputs used.")
    outputs <- convert.atts(substitute(outputs))
  }
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  gla <- GLA(statistics::Reservoir_Sampling, size = size, coefficient = coefficient)

  Aggregate(data, gla, inputs, outputs)
}

SimpleSample <- function(data, size, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (missing(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  gla <- GLA(statistics::Simple_Sampling, size = size)

  Aggregate(data, gla, inputs, outputs)
}

MemoryConsciousSampling <- function(data, minimumGroupSize,
  maximumGroupsAllowed, initialSamplingRate, reductionRate, inputs = AUTO,
  outputs = AUTO) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (missing(outputs)) {
    outputs <- convert.atts(c(inputs, "sampling_rate"))
  }

  if (length(outputs) != 1 + length(inputs)) {
    stop("There must be exactly one output for each input, and one output " +
      "for the sampling rate.")
  }

  gla <- GLA(statistics::Memory_Conscious_Sampling,
    minimumGroupSize = minimumGroupSize,
    maximumGroupsAllowed = maximumGroupsAllowed
    initialSamplingRate = initialSamplingRate,
    reductionRate = reductionRate)
}