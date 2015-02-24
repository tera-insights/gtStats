glm <- function(...) UseMethod("glm")

glm.default <- stats::glm

GLM <- function(data, ..., model = NULL, outputs = result) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  constructor <- GLMMake(..., model = is.null(model))
  terms <- unlist(lapply(grokit$expressions[unlist(constructor$GLA$args$predictors)], deparse))
  intercept <- constructor$GLA$args$intercept
  if (is.null(model)) {
    agg <- Aggregate(data, constructor$GLA, constructor$inputs, outputs)
    class(agg) <- c("GLM", class(agg))
  } else {
    agg <- Transform(data, constructor$GLA, constructor$inputs, outputs, list(model))
  }
  agg

  ## if (exists("grokit.jobid") && !force.frame) {
  ##   View(agg)
  ## } else {
  ##   result <- as.object(agg)$content[[1]][[1]]
  ##   result$coefficients <- result$coefficients$data
  ##   names(result$coefficients) <- c(if (intercept) "(Intercept)" else NULL, terms)
  ##   result
  ## }
}

glm.data <- GLM

GLMMake <- function(formula, family = gaussian, weights = NULL, start = NULL,
                    eta.start = NULL, mu.start = NULL, offset = NULL,
                    maxit = 25, epsilon = 1e-8, trace = FALSE, debug = FALSE,
                    convergence = "relative", model = TRUE,  ...) {
  if(length(formula) < 3 ) stop("No response variable specified")

  ## creation of inputs
  inputs <- convert.exprs(unique(extract.exprs(formula)))
  formula <- as.formula(convert.formula(formula))

  ## calculation of specific formula parts using terms object
  response <- deparse(formula[[2]])

  predictors <- as.list(attr(terms(formula), "term.labels"))
  intercept <- attr(terms(formula), "intercept")[[1]] == 1
  order <- as.list(attr(terms(formula), "order"))

  ## only first start parameter accepted
  if (!is.null(start)) {
    if (!is.numeric(start))
      stop("start must be a numeric vector or null.")
    eta.start <- mu.start <- FALSE
  } else if (!is.null(eta.start)) {
    eta.start <- substitute(eta.start)
    check.exprs(eta.start)
    eta.start <- convert.exprs(eta.start)
    if (length(eta.start) != 1)
      stop("eta.start must be a single expression or NULL.")
    inputs <- c(inputs, eta.start)
    mu.start <- start <- FALSE
  } else if (!is.null(mu.start)) {
    mu.start <- substitute(mu.start)
    check.exprs(mu.start)
    mu.start <- convert.exprs(mu.start)
    if (length(mu.start) != 1)
      stop("mu.start must be a single expression or NULL.")
    inputs <- c(inputs, mu.start)
    eta.start <- start <- FALSE
  } else {
    mu.start <- eta.start <- start <- FALSE
  }

  if (!is.null(weights)) {
    weights <- substitute(weights)
    check.exprs(weights)
    weights <- convert.exprs(weights)
    if (length(weights) != 1)
      stop("weights must be a single expression or NULL.")
    inputs <- c(inputs, weights)
  } else {
    weights <- FALSE
  }

  if (!is.null(offset)) {
    offset <- substitute(offset)
    check.exprs(offset)
    offset <- convert.exprs(offset)
    if (length(offsets) != 1)
      stop("offset must be a single expression or NULL.")
    inputs <- c(inputs, offset)
  } else {
    offset <- FALSE
  }

  ## conversion of family to character
  ## calculation of link function
  if (typeof(family) == "closure") {
    link <- family()$link
    family <- family()$family
  }
  else if (typeof(family) == "character") {
    family <- family$family
    link <- list(gaussian = "identity", binomial = "logit", poisson = "log", Gamma = "inverse")$family
  }
  else {
    link <- family$link
    family <- family$family
  }

  if( link == "1/mu^2")
    link <- "inverseSquared"
  if (family == "Gamma")
    family <- "gamma"
  if (family == "inverse.gaussian")
    family <- "inverseGaussian"

  if (model)
    GLA <- GLA(
        statistics::GLM,
        link = link,
        family = family,
        weights = weights,
        start = start,
        eta.start = eta.start,
        mu.start = mu.start,
        offset = offset,
        intercept = intercept,
        response = response,
        predictors = predictors,
        debug = debug,
        trace = trace,
        epsilon = epsilon,
        maxit = maxit,
        order = order,
        convergence = convergence
        )
  else
    GLA <- GT(
        statistics::GLM_Predict,
        link = link,
        family = family,
        weights = weights,
        start = start,
        eta.start = eta.start,
        mu.start = mu.start,
        offset = offset,
        intercept = intercept,
        response = response,
        predictors = predictors,
        debug = debug,
        trace = trace,
        epsilon = epsilon,
        maxit = maxit,
        order = order,
        convergence = convergence
        )

  list(GLA = GLA, inputs = inputs)
}

