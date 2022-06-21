#' rubin
#'
#' Creates regression model statistics for multiple imputation using Rubin's rules. 
#' @param df a data.frame of minimum 3 columns with the following information in order: variable name, coefficient, std. error  
#' @export
#' @examples

rubin <- function(df){
  if(ncol(df) < 3) stop("<3 columns. make sure all required information is included.")
  if(!is.data.frame(df)) stop("df must be a data.frame")
  
  names(df) <- c("variable", "estimate", "std.error", names(df)[4:ncol(df)])
  
  out <- df %>% 
    group_by(variable) %>% 
    summarise(nimp = n(),
              theta = mean(estimate),
              vwith = sum(std.error^2)/n(),
              vbet = sum((estimate-theta)^2)/(n()-1),
              vtot = vwith + vbet + (vbet/n()),
              sepool = sqrt(vtot),
              cv = theta/sepool,
              p = 2*pnorm(abs(cv), lower.tail = F)
    ) %>% 
    select(nimp, variable, theta, sepool, cv, p)
  
  return(out)
}

