RandomForest <- function(...) UseMethod("RandomForest")

RandomForest.data <- function(data, predictors, response, file = F,  ...) {
  predictors <- substitute(predictors)
  check.exprs(predictors)
  if (is.auto(predictors))
    predictors <- convert.schema(data$schema)
  predictors <- convert.exprs(predictors)

  response <- substitute(response)
  check.exprs(response)
  if (is.auto(response))
    response <- convert.schema(data$schema)
  response <- convert.exprs(response)

  constructor <- RandomForestMake(...)
  structure(add.class(Aggregate(data, constructor$GLA,
                                c(vectorize(predictors, float), response), character()),
                      c("rf", "model")),
            predictors = setNames(get.exprs(predictors), NULL),
            response = setNames(get.exprs(response), NULL)[[1]])
}

RandomForest.formula <- function(formula, data, ...) {
}

RandomForestMake <- function(file = F, num.vars = 0, max.depth = 25, min.sample = 100, node.epsilon = 0.01,
                             max.categories = 15, num.trees = 100, tree.epsilon = 0.01) {
  list(GLA = GLA(statistics::Random_Forest,
           file = file, num.vars = num.vars, max.depth = max.depth, min.sample = min.sample,
           node.epsilon = node.epsilon, max.categories = max.categories, num.trees = num.trees,
           tree.epsilon = tree.epsilon))
}

Predict <- function(...) UseMethod("Predict")

Predict.rf <- function(model, data, outputs) {
  inputs <- attr(model, "predictors")
  inputs <- convert.exprs(inputs)

  response <- attr(model, "response")

  if (missing(outputs)) {
    if (response)
      if (as.character(response) %in% names(data$schema))
        stop("cannot re-use response variable name from model due to name clash.")
      else
        outputs <- as.character(response)
    else
      stop("response variable was not originally named and must be given.")
  } else {
    outputs <- substitute(outputs)
    check.atts(outputs, FALSE)
    outputs <- convert.atts(outputs)
    assert(length(outputs) == 1, "a single output must be given.")
  }

  gt <- GT(statistics::Random_Forest_Predict)
  Transform(data, gt, inputs, outputs, model)
}

RandomForestPredict <- function(data, file = F, inputs, outputs) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs, FALSE)
  outputs <- convert.atts(outputs)

  gt <- GT(statistics::Random_Forest_Predict, file = file)
  Transform(data, gt, inputs, outputs)
}

BatchPredict <- function(model, data, outputs, extra) {
  inputs <- attr(model, "predictors")
  response <- attr(model, "response")

  if (missing(outputs)) {
    if (is.symbol(response))
      if (as.character(response) %in% names(data$schema))
        stop("cannot re-use response variable name from model due to name clash.")
      else
        outputs <- as.character(response)
    else
      stop("response variable was not originally named and must be given.")
  } else {
    outputs <- substitute(outputs)
    check.atts(outputs, FALSE)
    outputs <- convert.atts(outputs)
    assert(length(outputs) == 1, "a single output must be given.")
  }

  if (missing(extra)) {
    extra <- extra.atts <- character()
  } else {
    extra <- substitute(extra)
    check.atts(extra)
    if (is.auto(extra))
      extra <- subtract(names(data$schema), as.character(inputs[is.symbols(inputs)]))
    else
      extra.atts <- convert.atts(extra)
    extra <- vectorize(as.symbols(extra.atts))
  }

  agg <- Aggregate(data, GLA(statistics::Random_Forest_Batch),
                   c(extra, vectorize(inputs)),
                   c(extra.atts, as.character(inputs), outputs),
                   model)
}
