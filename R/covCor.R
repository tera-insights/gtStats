cov <- function(x, ...) UseMethod("cov")

cov.default <- stats::cov

Covariance <- cov.data <- function(data, inputs = AUTO, outputs = result) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(statistics::CovCor_Matrix, which = "cov"), inputs, outputs)

  ## if (exists("grokit.jobid") && !force.frame) {
  ##   View(agg)
  ## } else {
  ##   result <- as.object(agg)$content[[1]][[1]]
  ##   terms <- unlist(lapply(grokit$expressions[inputs], deparse))
  ##   cov <- matrix(result$covariance$data, result$covariance$n_cols, result$covariance$n_rows)
  ##   rownames(cov) <- colnames(cov) <- terms
  ##   cov
  ## }
}

cor <- function(x, ...) UseMethod("cor")

cor.default <- stats::cor

Correlation <- cor.data <- function(data, inputs = AUTO, outputs = result) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(statistics::CovCor_Matrix, which = "cor"), inputs, outputs)

  if (exists("grokit.jobid") && !force.frame) {
    View(agg)
  } else {
    result <- as.object(agg)$content[[1]][[1]]
    terms <- unlist(lapply(grokit$expressions[inputs], deparse))
    cor <- matrix(result$correlation$data, result$correlation$n_cols, result$correlation$n_rows)
    rownames(cor) <- colnames(cor) <- terms
    cor
  }
}


covcor <- function(x, ...) UseMethod("covcor")

covcor.default <- function(...) list(cov = stats::cov(...), cor = stats::cor(...))

covcor.data <- function(data, inputs = AUTO, outputs = result) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(statistics::CovCor_Matrix, which = "both"), inputs, outputs)

  ## if (exists("grokit.jobid") && !force.frame) {
  ##   View(agg)
  ## } else {
  ##   result <- as.object(agg)$content[[1]][[1]]
  ##   terms <- unlist(lapply(grokit$expressions[inputs], deparse))
  ##   cov <- matrix(result$covariance$data, result$covariance$n_cols, result$covariance$n_rows)
  ##   cor <- matrix(result$correlation$data, result$correlation$n_cols, result$correlation$n_rows)
  ##   rownames(cov) <- colnames(cov) <- rownames(cor) <- colnames(cor) <- terms
  ##   list(cov = cov, cor = cor)
  ## }
}
