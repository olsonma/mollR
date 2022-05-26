#' butterfly_plot
#'
#' This function creates a butterfly plot commonly used to show propensity score overlap
#' @param x a vector of values for which the histogram is desired
#' @param y a vector of values for which groups are defined
#' @param col1 character, color of histogram for group 1, default is #FF000060
#' @param col2 character, color of histogram for group 2, default is #3200D360
#' @param border1 character, color of histogram border of group 1, default is black
#' @param border2 character, color of histogram border of group 2, default is black
#' @param breaks a single number giving the number of cells for the histogram, default is 100
#' @param label logical, whether to add group labels above histograms, default is TRUE
#' @param legend logical, whether to add legend, default is FALSE
#' @param legendloc location location specified by setting to a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". Default is "bottomleft."
#' @param flipaxis logical, direction of the butterfly. FALSE is butterfly shape i.e. reflected over y-axis, TRUE is reflected over x-axis. Default is TRUE.
#' @param psrange range of histogram values
#' @export
#' @examples
#' butterfly(x = data$ps, data$group, label = F, flipaxis = T)
#' butterfly(x = data$ps, data$group, label = F, flipaxis = F, breaks = 10)

butterfly_plot <- function (x, y, col1 = "#FF000060", col2 = "#3200D360", border1 = "black", 
                            border2 = "black", breaks = 100, label = T, legend = F, legendloc = "bottomleft", flipaxis = F, psrange = NULL) 
{ 
  if(!is.logical(flipaxis)) stop("flipaxis must be logical")
  if(!is.logical(legend)) stop("legend must be logical")
  h1 <- hist(x, breaks = breaks, plot = FALSE)
  x1 <- cut(x, breaks = h1$breaks)
  t1 <- as.matrix(table(x1, y))
  t2 <- cbind(t1[, 1:2], h1$mids)
  plot.new()
  if(flipaxis){
    plot.window(xlim =c(0, nrow(t1)), ylim =c(-1, 1) * max(t2))
    barplot(t2[, 1], horiz = F, width = 1, space = 0, names.arg = "", 
            add = TRUE, axes = TRUE, col = col1, border = border1)
    barplot(-t2[, 2], horiz = F, width = 1, space = 0, names.arg = "", 
            add = TRUE, axes = TRUE, col = col2, border = border2)
    box()
    rng <- if(is.null(psrange)) range(x) else psrange
    at1 <- axisTicks(rng, log = FALSE)
    axis(1, at = cut(at1, breaks = h1$breaks, labels = FALSE), 
         labels = at1)
    at2 <- axisTicks(c(-1, 1) * max(t1), log = FALSE)
    axis(2, at = at2, labels = abs(at2)) 
    
  } else {
    plot.window(xlim = c(-1, 1) * max(t1), ylim = c(0, nrow(t2)))
    barplot(t2[, 1], horiz = T, width = 1, space = 0, names.arg = "", 
            add = TRUE, axes = FALSE, col = col1, border = border1)
    barplot(-t2[, 2], horiz = T, width = 1, space = 0, names.arg = "", 
            add = TRUE, axes = FALSE, col = col2, border = border2)
    box()
    rng <- if(is.null(psrange)) range(x) else psrange
    at2 <- axisTicks(rng, log = FALSE)
    axis(2, at = cut(at2, breaks = h1$breaks, labels = FALSE), 
         labels = at2)
    at1 <- axisTicks(c(-1, 1) * max(t1), log = FALSE)
    axis(1, at = at1, labels = abs(at1)) 
  }
  
  if(label){
    text(max(t1)/2, nrow(t2) + 1, attributes(t1)$dimnames$y[1])
    text(-max(t1)/2, nrow(t2) + 1, attributes(t1)$dimnames$y[2])
  }
  if(legend){
    legend(legendloc, c("", colnames(t2)[1], colnames(t2)[2]), col = c("white",col1, col2), bty = "n", lwd = 3, lty = c(0,1,1))
  }

}
