NBC <- function(data, ..., outputs = result) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  constructor <- nbcMake(...)
  agg <- Aggregate(data, constructor$GLA, constructor$inputs, outputs)
  class(agg) <- c("GLA", "data")
  agg
}

NBCMake <- function(predictors, response, range = 500, bins = 10) {
  if (missing(predictors))
    predictors <- convert.schema(data$schema)
  else
    predictors <- substitute(predictors)
  predictors <- convert.exprs(predictors)

  response <- substitute(response)
  inputs <- c(convert.exprs(predictors), convert.exprs(response))
  response <- convert.atts(response)

  GLA <- GLA(statistics::Naive_Bayes_Classifier,
             histogram.width.factor = range,
             number.bins = bins,
             response = as.character(response))

  list(GLA = GLA, inputs = inputs)
}
