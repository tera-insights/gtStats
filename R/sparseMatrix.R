SparseMatrix <- function(data, input) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  inputs <- convert.exprs(inputs)

  gla <- GLA(statistics::Build_Sparse_Array)

  Aggregate(data, gla, inputs, outputs)
}
