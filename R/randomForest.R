RandomForest <- function(...) UseMethod("RandomForest")

RandomForest.data <- function(data, predictors, response, file = F,  ...) {
  predictors <- substitute(predictors)
  check.exprs(predictors)
  predictors <- convert.exprs(predictors, data)

  response <- substitute(response)
  check.exprs(response)
  response <- convert.exprs(response, data)

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
                             max.categories = 15, num.trees = 100, tree.epsilon = 0) {
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
    check.atts(outputs)
    outputs <- convert.atts(outputs)
    assert(length(outputs) == 1, "a single output must be given.")
  }

  gt <- GT(statistics::Random_Forest_Predict)
  Transform(data, gt, inputs, outputs, model)
}

RandomForestPredict <- function(data, file = F, inputs, outputs) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)

  gt <- GT(statistics::Random_Forest_Predict, file = file)
  Transform(data, gt, inputs, outputs)
}

BatchPredict <- function(training, features, response, data, predictors, outputs, extra, ...) {
  features <- substitute(features)
  check.exprs(features)
  features <- convert.exprs(features, training)

  inputs <- setNames(get.exprs(features), NULL)

  response <- substitute(response)
  check.exprs(response)
  response <- convert.exprs(response, training)
  response <- setNames(get.exprs(response), NULL)

  predictors <- substitute(predictors)
  check.exprs(predictors)
  predictors <- convert.exprs(predictors, training)

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
    check.atts(outputs)
    outputs <- convert.atts(outputs)
    assert(length(outputs) == 1, "a single output must be given.")
  }

  if (missing(extra)) {
    extra <- extra.atts <- character()
  } else {
    extra <- substitute(extra)
    check.atts(extra)
    extra.atts <- convert.atts(extra)
    extra <- tuple(as.symbols(extra.atts))
  }

  training <- Aggregate(training, GLA(statistics::Gather),
                        c(vectorize(features, float), tuple(call("FLOAT", response[[1]]))),
                        character())

  predicting <- Aggregate(data, GLA(statistics::Gather),
                          c(vectorize(predictors), extra),
                          character())

  Transition(BatchPredictMake(...), c(extra.atts, as.character(inputs), outputs),
             list(training, predicting))
}

BatchPredictMake <- function(num.vars = 0, max.depth = 25, min.sample = 100, node.epsilon = 0.01,
                             max.categories = 15, num.trees = 100, tree.epsilon = 0.01) {
  GIST(statistics::Random_Forest_Batch,
       num.vars = num.vars, max.depth = max.depth, min.sample = min.sample, node.epsilon = node.epsilon,
       max.categories = max.categories, num.trees = num.trees, tree.epsilon = tree.epsilon)
}
