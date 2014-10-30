NBC <- function(data, ..., outputs = result) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  constructor <- nbcMake(...)
  agg <- Aggregate(data, constructor$GLA, constructor$inputs, outputs)
  class(agg) <- c("GLA", "data")
  agg
}

NBCMake <- function(predictors, response, range = 500, bins = 10) {
    predictors <- substitute(predictors)
    check.exprs(predictors)
    if (is.auto(predictors))
      Stop("predictors not allowed to be AUTO.")
    response <- substitute(response)
    check.atts(response)
    if (is.auto(response))
      Stop("response not allowed to be AUTO.")
    inputs <- c(convert.exprs(predictors), convert.exprs(response))
    response <- convert.atts(response)

    GLA <- GLA(
        statistics::Naive_Bayes_Classifier,
        histogram.width.factor = range,
        number.bins = bins,
        response = as.character(response)
        )

    list(GLA = GLA, inputs = inputs)
}
