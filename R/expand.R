Expand <- function(data, inputs, outputs) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)

  if (!((length(outputs) - length(inputs)) %in% 0:1))
    stop("There must be an equal number of outputs and inputs.")

  gt <- GT(statistics::Expand)

  Transform(data, gt, inputs, outputs)
}
