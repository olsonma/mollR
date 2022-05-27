#' @name fvn
#' @rdname fvn
#' @title fvn
#'
#' @description Short for "find variable name." List variable names / vector items which match a search expression
#' @param pattern A character string regex search expression
#' @param data A data.frame object
#' 
#' @keywords fvn
#' @examples
#' fvn("length", iris)
#' @export
fvn <- function(pattern, data = NULL){
  if(is.null(data) | !is.data.frame(data)){
    message("Please specify a dataset")
  }else{
    return(sort(names(data)[grep(pattern, names(data),ignore.case=TRUE)]))
  }
}
