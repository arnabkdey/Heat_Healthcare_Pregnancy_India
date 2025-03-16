# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script provides functions for calculating confidence intervals with flexible confidence levels and scale options.
# @date: Dec 12, 2024

coefcis <- function(coef, se, conf.level = 0.97, exponentiate = TRUE) {
  # Calculate z-score based on confidence level
  z_score <- qnorm((1 + conf.level) / 2)
  
  lci = coef - z_score * se
  uci = coef + z_score * se
  
  res <- cbind(coef, lci, uci)
  res_exp = exp(res)
  
  if (exponentiate == TRUE) {
    res_full <- exp(res)
  } else {
    res_full <- res
  }
  
  return(res_full)
}
