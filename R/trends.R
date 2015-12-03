Trends <- function(data, ..., inputs = AUTO, outputs = result, force.frame = FALSE) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 3)
    stop("There must be exactly three outputs specified.")

  constructor <- TrendsMake(...)
  agg <- Aggregate(data, constructor$GLA, inputs, outputs)
  agg
}

TrendsMake <- function(positive = list(exp), negative = list(function(x) exp(-x)),
                       threshold = 256, begin, end, scale = 1, intervals = 32L) {
  if (!is.integer(intervals) || intervals <= 0)
    stop("intervals must be a positive integer.")
  transform <- function(x) {
    if (is.numeric(x)) {
      if (length(x) == intervals)
        as.unit(x)
      else
        stop("numeric vector must be length ", intervals)
    } else if (is.function(x)) {
      x <- unlist(lapply(1:intervals, x))
      if (is.numeric(x) && length(x) == intervals)
        as.unit(x)
      else
        stop("a function specifying a trend must return numeric of length ", intervals)
    } else
      stop("trends each must be a numeric or a function.")
  }

  pos <- lapply(lapply(positive, transform), as.unit)
  neg <- lapply(lapply(negative, transform), as.unit)

  if (!any(c(class(begin), class(end)) %in% c("Date", "POSIXlt", "POSIXct", "integer")))
    stop("begin and end must specify a date-time with one of the following four classes:\n",
         "Date, POSIXct, POSIXlt, integer")
  if (any(c("Date", "POSIXlt") %in% class(begin)))
      begin <- as.POSIXct(begin)
  if (any(c("Date", "POSIXlt") %in% class(end)))
      end <- as.POSIXct(end)
  begin <- as.integer(begin)
  end <- as.integer(end)
  if (begin >= end)
    stop("begin time must be chronologically before end time.")

  GLA <- GLA(
      gs::Trends,
      pos = pos,
      neg = neg,
      threshold = threshold,
      begin = begin,
      end = end,
      scale = scale,
      intervals = intervals
      )

  list(GLA = GLA)
}
