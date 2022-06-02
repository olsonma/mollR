#' gg_two_column_consort
#'
#' This function creates a two column consort figure with "before" and "after" population
#' N If you would also like to display subgroup population Ns, please use
#' `gg_two_column_consort_subgroup`
#' @param data data.frame, data with consort variable
#' @param var string, name of consort variable that is in the format "col row description"
#' @param box_height height of exclusion boxes. default is 0.4.
#' @param box_width width of subgroup boxes. default is 0.22.
#' @param top_box_text string, text to display in top box of diagram
#' @param date string, date of data pull. if NULL, won't display date.
#' @export


gg_two_column_consort <- function (data, var, top_box_text, date = NULL)
{
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is needed for this function to work.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is needed for this function to work.",
         call. = FALSE)
  }
  top_box <- data.frame(label = top_box_text, n = nrow(data),
                        row = 1, col = 1, sum_node = 1, node = 1)

  consortdf <- data %>% group_by(.data[[var]]) %>% summarise(n = n()) %>%
    mutate(label = gsub("^([0-9]+) ([0-9]+) (.*)$", "\\3", .data[[var]])) %>%
    mutate(row = gsub("^([0-9]+) ([0-9]+) (.*)$", "\\1", .data[[var]])) %>%
    mutate(col = gsub("^([0-9]+) ([0-9]+) (.*)$", "\\2", .data[[var]])) %>%
    mutate(row = as.numeric(row), col = as.numeric(col)) %>%
    arrange(row) %>%
    mutate(node = row_number() + 1, sum_node = 1) %>%
    select(-.data[[var]]) %>%
    rbind(top_box %>% select(names(.))) %>%
    arrange(row) %>%
    mutate(labeln = paste0(label, "\n", n))

  p <- data.frame(x = c(0,(max(consortdf$col)*2+1)),
                  y = c(0,(max(consortdf$row)*2+1))) %>%
    ggplot(aes(x, y)) +
    scale_y_continuous(trans = "reverse") +
    theme_linedraw()
  p2 <- p + geom_rect(inherit.aes = FALSE,
                      data = consortdf,
                      aes(xmin = col - 0.4,
                          xmax = col + 0.4,
                          ymin = row - 0.4,
                          ymax = row + 0.4),
                      color = "black",
                      fill = "white") +
    geom_text( inherit.aes = FALSE,
               data = consortdf,
               aes(x= col,
                   y=row,
                   label= labeln),
               size=4)

  p3 <- p2 +
    geom_segment(inherit.aes = FALSE,
                 data = consortdf %>% filter(node %nin% c(1, max(consortdf$node))),
                 aes(x = sum_node, y = row, xend = col-0.4, yend = row),
                 arrow = arrow(length = unit(5, "mm"))) +
    geom_segment(inherit.aes = FALSE,
                 data = consortdf,
                 aes(x = sum_node, y = min(row)+0.4, xend = sum_node, yend = max(node)-0.4),
                 arrow = arrow(length = unit(5, "mm")))

  p4 <- p3 +
    theme_void()

  if(!is.null(date)) p4 <- p4 + annotate("text", x = max(consortdf$col)+0.4, y = max(consortdf$row)+0.4,
                                         label = paste0("*As of ", date), size =4, hjust = 1)
  return(p4)

}
