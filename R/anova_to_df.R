#' anova_to_df
#'
#' Converts rms::anova object to data.frame
#' @param anv an anova.rms object
#' @param incl_total boolean to include total row in anova.rms
#' @param p_nice boolean. whether to print p-values nicely with "<0.001" and rounded numbers.
#' @export
#' @examples

anova_to_df <- function(anv, incl_total = F, p_nice = F){
  if(!any(class(anv) == "anova.rms")) stop("anv must be an anova.rms object")
  
  anv_df <- as.data.frame(anv)
  anv_df <- cbind(Variables = gsub("\\.", " ", rownames(anv_df)), anv_df) %>% 
    mutate(Variables = case_when(
      grepl("Nonlinear", Variables, ignore.case = T) ~ "Nonlinear",
      grepl("Interaction", Variables, ignore.case = T) ~ "Interactions",
      TRUE ~ Variables
    )) %>%
    `rownames<-`(NULL)
  
  if(!incl_total){
    anv_df <- anv_df %>% filter(!grepl("total", Variables, ignore.case = T))
  }
  
  if(p_nice){
   anv_df <- anv_df %>% 
     mutate(P = case_when(
       P < 0.001 ~ "<0.001",
       TRUE ~ as.character(round(P, 3)))
     )
  }
  return(anv_df)
}
