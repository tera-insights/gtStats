remove.inhibit <- function(formula, is.response = FALSE) {
  if(is.symbol(formula) || is.numeric(formula))
    formula
  else if(is.call(formula))
    if(is.symbol(formula[[1]]) && as.character(formula[[1]]) == "I")
      if(length(formula) > 2)
        stop("AsIs is a unary operator - too many arguments.")
      else {
        if(is.response)
          warning("AsIs included in the response variable.")
        remove.inhibit(formula[[2]], is.response)
      }
    else
      as.call(lapply(formula, remove.inhibit, is.response))
  else
    stop("Unsupported type in formula.")
}

get.inhibit <- function(formula, inhibited = FALSE) {
  if(is.symbol(formula))
    if(inhibited)
      formula
    else
      NULL
  else
    if(inhibited && as.character(formula[[1]]) == ":")
      stop("Interaction included within an AsIs function.")
    else
      if(is.symbol(formula[[1]]) && as.character(formula[[1]]) == "I")
        unlist(lapply(formula[-1], get.inhibit, inhibited = TRUE))
      else
        unlist(lapply(formula[-1], get.inhibit, inhibited))
}

convert.formula <- function(formula, lookup = grokit$expressions) {
  if (is.symbol(formula))
    as.symbol(tail(names(lookup)[lookup == formula], 1))
  else if (is.call(formula))
    if (is.symbol(formula[[1]]) && formula[[1]] == "I")
      as.symbol(tail(names(lookup)[lookup == formula[[2]]], 1))
    else
      as.call(c(formula[[1]], lapply(formula[-1], convert.formula)))
  else
    formula
}

extract.exprs <- function(expr) {
  if(is.symbol(expr))
    expr
  else if (is.call(expr))
    if (is.symbol(expr[[1]]) && expr[[1]] == "I")
      if (length(expr) != 2)
        stop("AsIs is an urnary function.")
      else
        expr[[2]]
    else
      unlist(lapply(expr[-1], extract.exprs))
  else
    NULL
}

## Takes a list of inputs and places the UDF MakeVector on top of them.
vectorize <- function(exprs, type = double, data = NULL) {
  if (is.character(exprs))
    exprs <- get.exprs(exprs)
  convert.exprs(as.call(c(substitute(statistics::MakeVector[type = TYPE(type)]), exprs)), data)
}

tuple <- function(exprs, data = NULL) {
  if (is.character(exprs))
    exprs <- get.exprs(exprs)
  convert.exprs(as.call(c(substitute(statistics::MakeTuple[]), exprs)), data)
}
