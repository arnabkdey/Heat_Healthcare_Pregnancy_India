coefcis <- function(coef, se, exponentiate = TRUE) {
  lci = coef - 1.96*se
  uci = coef + 1.96*se
  res <- cbind(coef, lci, uci)
  res_exp = exp(res)
  if (exponentiate == TRUE) {
    res_full <- exp(res)
  } else {
    res_full <- res
  }
  return(res_full)
}