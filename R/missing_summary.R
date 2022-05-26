#' Compile missing data summary of data set
#' 
#' Generates missing data summaries.  Adapted from `tgsify`'s `missingness_info`
#' @param data A data.frame object for which a missing data report will be generated
#' @param type Type of output. One of `"both"`, `"row"`, `"col"`, or `"complete"`. The default is `"both"` for both `"row"`-wise and `"col"`-wise missing data summaries. `type="complete"` will report proportion of records with complete data. 
#' @param upper_limit A number.  The right tail of the frequency distribution reported in the `type="row"` summary is truncated to "upper_limit +".
#' @param max_vars  A number. If not specified, will report the variables with missing data. If specified, limits the list of variables reported `type="col"` to the first `max_vars` most frequently missing variables. 
#'If there are multiple variables with the same number of missing values, all such the variables will be reported. (This means more than max_vars variables can appear in the output). Can specify `Inf` if all variables are desired, including those without missing data.
#' @param include_vars A vector of variable names or a regular expression to select variables that match a pattern. You may drop variables by providing a regular expression preceded by `!` (`include_vars = "!qol"`, for example, would drop variables matched by "qol") 
#' @details The output is a list with the "by row" summary and the "by column" summary.
#' @keywords missingness_info
#' @export
#' @examples
#' missing_summary(airquality)
#' missing_summary(airquality, type = "complete")
#' missing_summary(airquality, type = "row")
#' missing_summary(airquality, type = "col")
#' ## Include only Ozone and Solar.R variables
#' missing_summary(airquality, type = "row", include_vars = c("Ozone", "Solar.R"))
#' missing_summary(airquality, type = "row", include_vars = "Oz|Solar")
#' missing_summary(airquality, type = "both", include_vars = "Oz|Solar")
#' missing_summary(airquality, type = "row", upper_limit = 1)
#' ## Below, the upper_limit will provide the same results
#' missing_summary(airquality, type = "row", upper_limit = 3)
#' ## upper_limit = 6 will not return "N+" like the above example because `airquality` has 6 total variables
#' missing_summary(airquality, type = "row", upper_limit = 6)
#' ## drop Ozone only
#' missing_summary(airquality, type = "col", include_vars = "!Ozone")

missing_summary <- function(
  data,
  upper_limit,
  max_vars,
  include_vars,
  type = "both" ##complete will be # of obs with a complete record (row)
){
  if(!any(type == c("both", "row", "col", "complete")) | length(type) > 1) stop("type must be one of \"both\", \"row\", \"col\", or \"complete\"")

  if(!missing(include_vars)){
    if(length(include_vars) == 1){
      if(grepl("\\!", include_vars)){
        include_vars <- gsub("\\!", "", include_vars)
        data <- data[,!grepl(include_vars, names(data), ignore.case = TRUE)]
      } else {
        data <- data[,grep(include_vars, names(data), ignore.case = TRUE)]
      }
    } else if(is.character(include_vars)){
      data <- data[,include_vars] 
    } else {
      stop("include_vars must be a character vector or regular expression")
    }
  }
  
  if(type %in% c("row", "both", "complete")){  
    missing_count <- apply(is.na(data), 1, sum)
    if(type %in% "complete"){
      missing_count <- cut(
        missing_count,
        breaks = c(-1:(1-1), Inf),
        labels = c("Complete record", "Incomplete record")
      )
    } else if(!missing(upper_limit)){
      missing_count <- cut(
        missing_count,
        breaks = c(-1:(upper_limit-1), Inf),
        labels = c(0:(upper_limit-1), paste0(upper_limit, ifelse(upper_limit < ncol(data), "+", "" )))
      )
    }
    out <- xtabs(~missing_count)
    
    count <- as.numeric(out)
    pct <- count/sum(count)*100
    cumpct <- cumsum(pct)
    out1 <- data.frame(
      `Number of Missing Variables<br>(within a record)` = names(out),
      `Frequency`         = as.numeric(out),
      `Percent`           = round(pct,2),
      `Cumulative Percent`= round(cumpct,2),
      stringsAsFactors    = FALSE,
      check.names         = FALSE
    )
    is100 <- cumpct > 100 - 1e-15
    keep_row <- !is100 | !duplicated(is100)
    out1 <- out1[keep_row,,drop = FALSE]
    
    if(type %in% c("row", "complete")) return(out1)
  } 
  
  if(type %in% c("col", "both")){
    var_missing_count <- apply(is.na(data),2, sum)

    max_var_count <- sort(
      var_missing_count,
      decreasing = TRUE)
    browser()
    if(missing(max_vars)){
      max_vars <- sum(max_var_count > 0, na.rm = T)
    } else if(is.infinite(max_vars)){
      max_vars <- ncol(data)
    } else {
      max_vars <- min(ncol(data), max_vars)
    }
    
    cutoff <- max_var_count[max_vars]
    max_var_count <- max_var_count[max_var_count >= cutoff]
    max_var_pct <- 100*as.numeric(max_var_count) / nrow(data)
    
    
    out2 <- data.frame(
      Variable = names(max_var_count),
      `Obs Available` = nrow(data) - as.numeric(max_var_count),
      `Percent Available` = round(100 - max_var_pct,2),
      `Obs Missing` = as.numeric(max_var_count),
      `Percent Missing`   = round(max_var_pct,2),
      stringsAsFactors    = FALSE,
      check.names         = FALSE
    )
    
    if(type %in% "col") return(out2)
  }
  out <- list(row = out1, col = out2)
  return(out)
}

