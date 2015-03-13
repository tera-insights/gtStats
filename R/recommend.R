Recommend <- function(..., outputs = error) {
  gby1 <- eval(call("GroupBy", Read(gs_streams), groupAtts = as.symbol("songid"),
                    substitute(Count(inputs = tsadded, outputs = Count1))))
  gby2 <- eval(call("GroupBy", Read(gs_streams), groupAtts = as.symbol("userid"),
                    substitute(Count(inputs = tsadded, outputs = Count2))))
  data <- eval(call("GroupBy", Read(gs_streams), groupAtts = substitute(c(userid, songid)),
                    substitute(Count(inputs = tsadded, outputs = Count3))))

  inputs <- substitute(c(userid, songid, Count3))
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(x$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  constructor <- RecommendMake(...)
  agg <- Aggregate(data, constructor$GLA, inputs, outputs, states = list(gby1, gby2))
  agg
}

RecommendMake <- function(f = 100, threshold = 100,
                          y1 = 0.007, y2 = 0.007, y3 = 0.001,
                          l6 = 0.005, l7 = 0.015, l8 = 0.015) {
  GLA <- GLA(
      gs::Recommend,
      y1 = y1,
      y2 = y2,
      y3 = y3,
      l6 = l6,
      l7 = l7,
      l8 = l8,
      f = f,
      t = threshold
      )

  list(GLA = GLA)
}
