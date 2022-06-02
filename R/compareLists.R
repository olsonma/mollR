#' Compile missing data summary of data set
#'
#' Compares lists element-wise. Created for purpose of comparing two lists consisting of data frames, where each list is from a different data-pull date.
#' @param list1 a list
#' @param list2 a list
#' @param lnames a vector of length 2 supplying names of list
#' @details The output is a text summary of element-wise list comparisons
#' @keywords list
#' @export
#' @examples
#' l1 <- list(tmp1 = data.frame(x = 1:100, y = 100:1), tmp2 = data.frame(a = letters, b = LETTERS))
#' l2 <- list(tmp1 = data.frame(x = 1:99, y = 99:1), tmp2 = data.frame(a = letters[1:14], b = LETTERS[1:14]))
#' compareLists(l1, l2)
#' compareLists(l2, l1)

compareLists <- function(list1, list2, lnames)
{
  if(missing(list1) | missing(list2)) stop("two lists must be specified")
  if(!is.list(list1) | !is.list(list2)) stop("input must be of type list")
  if(!missing(lnames)){ if(length(lnames) < 2) stop("names must be of length 2")}

  for(i in 1:max(length(list1), length(list2))){
    if(is.data.frame(list1[[i]]) & is.data.frame(list2[[i]])){
      cat("\n----------------", names(list1)[i], "-----------------\n")
      if(nrow(list1[[i]]) < nrow(list2[[i]])){
        cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      }
      if(missing(lnames)) {
        cat("List1 = ",nrow(list1[[i]]), ", ", "List 2 =", nrow(list2[[i]]))
      } else {
        cat(lnames[1],"=",nrow(list1[[i]]), ", ", lnames[2], "=", nrow(list2[[i]]))
      }
    } else {
      message(paste0("\nList element ", names(list1)[i], ": only data frame list elements supported at this time"))
    }
  }
}


