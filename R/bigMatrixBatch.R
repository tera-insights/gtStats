BigMatrixBatch <- function(states, outputs, block = 40, diag = true) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)

  gist <- GIST(statistics::Big_Matrix_Batch, block = block, diag = diag)
  Transition(gist, outputs, states)
}
