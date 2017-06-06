BigMatrix <- function(data, inputs, outputs, measures=c(correlation), block = 40, scale = 2, diag = TRUE) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  if (length(inputs) != 2)
    stop("2 inputs expected.");

  measures <- substitute(measures)
  check.choices(measures, substitute(c(covariance, correlation, distance)))
  numMeasures <- length(measures) - 1; ## skip function name

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 2+numMeasures)
    stop(2+numMeasures, " outputs expected.")

  gla <- GLA(statistics::Big_Matrix, measures = measures, block = block, 
             scale = scale, diag = diag)

  Aggregate(data, gla, inputs, outputs)
}
