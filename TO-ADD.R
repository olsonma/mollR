# BUTTERFLY

butterfly <- function (x, y, col1 = "#FF000060", col2 = "#3200D360", border1 = "black", 
                       border2 = "black", breaks = 100, label = T, flipaxis = F, psrange = NULL) 
{ ## flipaxis = F would be butterfly
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
}




# LOGIT2PROB

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
