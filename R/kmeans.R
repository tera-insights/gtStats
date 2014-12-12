KMeans <- function(data, ..., inputs = AUTO, outputs = result) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  constructor <- KMeansMake(...)
  agg <- Aggregate(data, constructor$GLA, inputs, outputs)
  agg

  ## if (exists("grokit.jobid") && !force.frame) {
  ##   View(agg)
  ## } else {
  ##   result <- as.object(agg)$content[[1]][[1]]
  ##   result$centers <- t(matrix(result$centers$data, result$centers$n_rows, result$centers$n_cols))
  ##   colnames(result$centers) <- unlist(lapply(grokit$expressions[inputs], deparse))
  ##   rownames <- 1:constructor$GLA$args$number.clusters
  ##   result
  ## }
}

## p = 1 for k-medioids, add a helper function for this
## Add nstarts perhaps with a for loop or maybe in program
KMeansMake <- function(centers, p = 2, m = 1, max.iteration = 25,
                       algorithm = "k-means++", epsilon = .0001,
                       sample.size = 10000, convergence = "relative",
                       normalized = FALSE, debug = FALSE) {
  if (missing(epsilon) && m > 1) {
    epsilon = .01
    warning("Default for epsilon changed from 0.001 to 0.01 because fuzzy clustering used.")
  }
  if (is.matrix(centers)) {
    if (!missing(algorithm))
      warning("Algorithm for initial starts was specified but not needed.")
    number.clusters = ncol(centers)
  } else if (length(centers) == 1) {
    number.clusters = centers
    centers = algorithm
    if (algorithm == "standard" && !missing(sample.size))
      warning("Sample size not needed for standard starting algorithm.")
    if (!is.numeric(number.clusters) || number.clusters < 0.5)
      stop("Invalid specification of number of clusters.")
    if (number.clusters != (round(number.clusters) -> number.clusters))
      warning("Number of clusters supplied was not an integer and was rounded.")
  } else {
    if(!missing(algorithm))
      warning("Only first value of centers used as k.")
    number.clusters = centers[[1]]
  }

  GLA <- GLA(statistics::K_Means,
             centers = centers,
             epsilon = epsilon,
             sample.size = sample.size,
             max.iteration = max.iteration,
             number.clusters = number.clusters,
             convergence = convergence,
             p = p,
             m = m,
             debug = debug,
             normalized = normalized
             )

  list(GLA = GLA)
}

kmeans <- function(x, ...) UseMethod("kmeans")

kmeans.default <- stats::kmeans

kmeans.data <- KMeans
