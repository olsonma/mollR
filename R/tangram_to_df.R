#' tangram_to_df
#'
#' This function converts a `tangram` object to dataframe. Will be in html format.
#' @param tbl a `tangram` object (table)
#' @param add_rown logical. Whether to add a row id (`id`) to the output dataframe
#' @param html logical. Whether to include html code for indentation.
#' @param hide_p logical. Whether to hide p-values for variables with "nopv" in the label (first column). This will remove the "nopv" text as well.
#' @export
#' @examples
#' library(tangram)
#' tab <- tangram(drug~bili+albumin+stage::Categorical+protime+sex+age+spiders,
#'               data = pbc,
#'               style = "lancet",
#'               id = "mytbl3")
#' tangram_to_df(tbl = tab, html = F)
#' 
#' ## hide_p example
#' tab2 <- tangram(drug~bili+albumin+stage::Categorical+protime+sex+age+spiders,
#'               data = pbc %>% 
#'               labelled::set_variable_labels(albumin = "nopv Albumin (gm/dl)"),
#'               style = "lancet",
#'               test = T,
#'               id = "mytbl3")
#' tangram_to_df(tbl = tab, html = F)

tangram_to_df <- function(tbl, add_rown = FALSE, html = TRUE, hide_p = FALSE){
  if(missing(tbl)) stop("must provide argument tbl")
  if(all(class(tbl) != "tangram")) stop("tbl must be a tangram object")
  if(html){
    tbl_list <- Map(function(x) {x[[1]] <- gsub("  ",
                                                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                                x[[1]]); x <- gsub("^ +", "", x); x }, 
                    tbl)
  } else {
    tbl_list <- Map(unlist, 
                    tbl)
  }
  tbl_df <- data.frame(matrix(
    unlist(tbl_list), 
    nrow=length(tbl_list), 
    byrow=TRUE)) 
  
  if(hide_p){
    pcol <- which(tbl_df[1,] == "Test Statistic")
    if(length(pcol) > 0){
      tbl_df[[paste0("X",pcol)]] <- ifelse(grepl("nopv", tbl_df$X1), "", as.character(tbl_df[[paste0("X",pcol)]]))
      tbl_df$X1 <- gsub("nopv", "", tbl_df$X1) %>% trimws
    }
  }
  if(add_rown){
    tbl_df <- tbl_df %>%
      `names<-`(c("Variables", "N", tbl_df[1,][3:ncol(.)])) %>%
      dplyr::slice(-1) %>% 
      mutate(id = dplyr::row_number())
  } else {
    tbl_df <- tbl_df %>%
      `names<-`(c("Variables", "N", tbl_df[1,][3:ncol(.)])) %>%
      dplyr::slice(-1)
  }
  return(tbl_df)
}

