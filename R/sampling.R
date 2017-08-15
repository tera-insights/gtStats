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
  if (missing(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  gla <- GLA(statistics::Memory_Conscious_Sampling,
    minimumGroupSize = minimumGroupSize,
    maximumGroupsAllowed = maximumGroupsAllowed,
    initialSamplingRate = initialSamplingRate,
    reductionRate = reductionRate)

  Aggregate(data, gla, inputs, outputs)
}

MemoryConsciousHashing <- function(data, group, ..., minimumTotalScoreMultiplier,maxNumberOfBucketsProduced, initialNumberOfBuckets, numberOfSegments, states = list()) {
  group <- substitute(group)
  keys <- names(group)[-1]
  check.exprs(group)
  group <- convert.exprs(group, data)

  ## Multiplexer is removed if there is a single inner GLA
  GLAs <- MultiplexerMake(..., data = data)
  if (length(GLAs$GLA$args$glas) == 1)
    aggregate <- GLAs$GLA$args$glas[[1]]$gla
  else
    aggregate <- GLAs$GLA

  inputs <- c(group, GLAs$inputs)
  outputs <- "state"

  gla <- GLA(statistics::Memory_Conscious_Hashing,
    group = group,
    aggregate = aggregate,
    minimumTotalScoreMultiplier = minimumTotalScoreMultiplier,
    maxNumberOfBucketsProduced = maxNumberOfBucketsProduced,
    initialNumberOfBuckets = initialNumberOfBuckets,
    numberOfSegments = numberOfSegments)
  Aggregate(data, gla, inputs, outputs, states)
}

HashToGroup <- function(data, group, hasher) {
  gf <- GF(HashToGroup, group)
  Filter(data, gf, states = list(hasher))
}