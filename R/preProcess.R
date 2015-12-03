PreProcess <- function(data, inputs = AUTO, outputs = AUTO, normalize = F,
                       vectorize = T, center = F, scale = F, range = F) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- vectorize(convert.exprs(inputs))

  args <- num.args(get.exprs(inputs))

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (missing(outputs))
    if (all(is.symbols(get.args(get.exprs(inputs)))))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)

  if ( vectorize && length(outputs) != 1)
    stop("there must be a single output to contain the vector.")
  if (!vectorize && length(outputs) != args)
    stop("There must be exactly one output specified per input.")

  center <- rep_len(as.list(center), args)
  if (any(!(sapply(center, is.logical) | sapply(center, is.numeric))))
    stop("each center must be logical or numeric.")

  scale <- rep_len(as.list(scale), args)
  if (any(!(sapply(scale, is.logical) | sapply(scale, is.numeric))))
    stop("each scale must be logical or numeric.")
  if (any(sapply(scale, identical, 0)))
    stop("scale cannot be 0.")

  if (is.logical(range)) {
    range <- rep_len(range, args)
    range <- ifelse(range, list(0:1), list(F))
  } else if (is.numeric(range)) {
    if (length(range) == 2)
      range <- rep_len(range, 2 * args)
    if (length(range) != 2 * args)
      stop("there must be one range total or per input.")
    range <- split(range, ceiling(seq_along(range) / 2))
  }

  if (is.list(range)) {
    range <- rep_len(range, args)
    for (val in range)
      if (is.numeric(val))
        assert(length(val) != 2 && val[[1]] > val[[2]], "bad numeric range.")
      else if (is.logical(val))
        assert(length(val) != 1, "bad logical range.")
      else
        stop("each range must be numeric or logical.")
  } else {
    stop("range should be numeric, logical, or a list.")
  }

  assert(is.logical(normalize) && length(normalize) == 1,
         "normalize should be a single logical.")

  if (any(sapply, c(center, scale), isTRUE) || any(sapply(range, is.numeric))) {
    gla <- GLA(statistics::Pre_Process, normalize, scale, center, range)
    state <- Aggregate(data, GLA, inputs, character())
  } else {
    state <- NULL
  }

  gt <- GT(statistics::Process, normalize, scale, center, range)
  Transform(data, gt, inputs, outputs, list(state))
}
