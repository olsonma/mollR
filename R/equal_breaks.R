#' Helper function for `ggplot`'s `scale_x/y_continuous`
#'
#' Allows faceted free_x and free_y scales to have equal number of breaks with differing ranges
#' @param n number of breaks
#' @param sf scaling factor (multiplicative expand) - higher `sf` will result in wider margins before ticks begin 
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Length)) +
#'   geom_point() +
#'   facet_grid( ~ Species, scales = "free") +
#'   scale_x_continuous(breaks=equal_breaks(n=3, sf=.05),
#'                      expand = c(0.05, 0))


equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+d, max(x)-d, length=n)
  }
}
