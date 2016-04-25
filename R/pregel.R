Pregel <- function(vertices, vertex, attributes,
                   edges, source, target, properties,
                   message, combine = "", directed = TRUE) {
  ## Processing of the vertices.
  vertex <- convert.exprs(substitute(vertex))
  inputs <- convert.exprs(substitute(attributes))

  attributes <- convert.names(inputs)
  missing <- which(attributes == "")
  exprs <- grokit$expressions[inputs[missing]]
  if (all(is.symbols(exprs)))
    attributes[missing] <- as.character(exprs)
  attributes <- as.list(attributes)

  ## The name for the vertex column
  expr <- grokit$expressions[[vertex]]
  col <- if (is.symbol(expr)) as.character(expr) else "Vertex"

  ## The input state is constructed.
  gla <- GLA(statistics::Pregel_Setup, attributes)
  setup <- Aggregate(vertices, gla, c(vertex, inputs), c(col, attributes))

  ## Processing of the edges.
  source <- convert.exprs(substitute(source))
  target <- convert.exprs(substitute(target))
  inputs <- convert.exprs(substitute(properties))

  properties <- convert.names(inputs)
  missing <- which(properties == "")
  exprs <- grokit$expressions[inputs[missing]]
  if (all(is.symbols(exprs)))
    properties[missing] <- as.character(exprs)
  properties <- as.list(properties)

  vertices <- convert.names(c(source, target))
  missing <- which(vertices == "")
  exprs <- grokit$expressions[c(source, target)[missing]]
  if (all(is.symbols(exprs)))
    vertices[missing] <- as.character(exprs)
  vertices <- as.list(vertices)

  name <- convert.names(vertex)
  missing <- which(name == "")
  exprs <- grokit$expressions[vertex[missing]]
  if (all(is.symbols(exprs)))
    name[missing] <- as.character(exprs)

  ## The aggregate is constructed.
  gla <- GLA(statistics::Pregel, properties, message, combine, directed, vertices, name)
  Aggregate(edges, gla, c(source, target, inputs), c(col, attributes), setup)
}
