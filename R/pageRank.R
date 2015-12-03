PageRank <- function(data, inputs, outputs, block = 40, scale = 2, adj = TRUE, hash = TRUE) {
  if (missing(inputs))
    inputs <- convert.schema(data$schema)
  else
    inputs <- substitute(inputs)
  inputs <- convert.exprs(inputs)

  if (length(inputs) != 2)
    stop("2 inputs expected.");

  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 2)
    stop("2 outputs expected.")

  gla <- GLA(statistics::Page_Rank, block = block, scale = scale, adj = adj, hash = hash)

  Aggregate(data, gla, inputs, outputs)
}
