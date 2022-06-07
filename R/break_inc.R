#' Helper function for `ggplot`'s `scale_x/y_continuous`
#'
#' Allows faceted free_x and free_y scales to have same break increments
#' @param inc increments
#' @param sf scaling factor (multiplicative expand) - higher `sf` will result in wider margins before ticks begin 
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Length)) +
#'   geom_point() +
#'   facet_grid( ~ Species, scales = "free") +
#'   scale_x_continuous(breaks=break_inc(inc=0.1, sf=.05),
#'                      expand = c(0.05, 0))

break_inc <- function(inc = 10, sf = 0.05, ...){
  function(x){
    # rescaling
    d <- sf * diff(range(x)) / (1+2*sf)
    seq(min(x)+d, max(x)-d, by = inc)
  }
}
