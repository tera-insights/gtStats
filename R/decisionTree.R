DecisionTree <- function(data, response, regression, continuous, categorical, outputs = result,
                         split = "anova", threshold = 0.01, conv.limit = 1e-6, em.restarts = 3,
                         em.disc.iters = 2, em.learn.iters = 30, em.max.trials = 3) {
  response <- substitute(response)
  check.exprs(response)
  continuous <- substitute(continuous)
  check.exprs(continuous)
  categorical <- substitute(categorical)
  check.exprs(categorical)
  regression <- substitute(regression)
  check.exprs(regression)

  cont.split <- length(as.exprs(continuous))
  cont.reg <- length(as.exprs(regression))

  if (is.auto(response) || is.auto(continuous) || is.auto(categorical))
    Stop("response, continuous, and categorical cannot be AUTO")
  if (length(as.exprs(response)) != 1)
    Stop("a single response expression must be given")

  categorical <- as.call(c(call("[", substitute(statistics::MakeVector)), as.exprs(categorical)))
  continuous <- as.call(c(call("[", substitute(statistics::MakeVector)),
                          as.exprs(continuous), as.exprs(regression), as.exprs(response)))

  categorical <- convert.exprs(categorical)
  continuous <- convert.exprs(continuous)

  inputs <- c(categorical, continuous)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- "result"
  else
    outputs <- convert.atts(outputs)

  gla <- TemplateFunction(GLA, statistics::Decision_Tree,
             split = split,
             threshold = threshold,
             conv.limit = conv.limit,
             em.restarts = em.restarts,
             em.disc.iters = em.disc.iters,
             em.learn.iters = em.learn.iters,
             em.max.trials = em.max.trials,
             cont.split = cont.split,
             cont.reg = cont.reg
             )

  Aggregate(data, gla, inputs, outputs)
}
