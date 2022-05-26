#' logit2prob
#'
#' This function converts a value on logit scale into a probability
#' @param logit a vector of values on logit scale
#' @export
#' @examples
#' logit2prob(logit = data$logit)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}