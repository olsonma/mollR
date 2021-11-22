#' gg_two_column_consort_subgroup
#'
#' This function creates a two column consort figure with "before" and "after" population
#' N as well as subgroup population Ns.
#' @param data data.frame, data with consort variable
#' @param var string, name of consort variable that is in the format "col row description"
#' @param var2 string, name of consort variable for subgroup population that is in the format "col row description"
#' @param box_width width of exclusion boxes. default is 0.4.
#' @param box_height height of exclusion boxes. default is 0.4.
#' @param box_width width of subgroup boxes. default is 0.22.
#' @param box_height height of subgroup boxes. default is 0.8.
#' @param top_box_text string, text to display in top box of diagram
#' @param date string, date of data pull. if NULL, won't display date.
#' @export
#' @examples

gg_two_column_consort_subgroup <- function (data, var, var2, 
                                    box_width = 0.4, box_height = 0.4,
                                    box2_width = 0.22, box2_height = 0.8,
                                    top_box_text, date = NULL) 
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
  consortdf2 <- data %>% group_by(.data[[var2]]) %>% summarise(n = n()) %>%
    mutate(label = gsub("^([0-9]+) (\\d+\\.*\\d*) (.*)$", "\\3", .data[[var2]])) %>% 
    mutate(row = gsub("^([0-9]+) (\\d+\\.*\\d*) (.*)$", "\\1", .data[[var2]])) %>% 
    mutate(col = gsub("^([0-9]+) (\\d+\\.*\\d*) (.*)$", "\\2", .data[[var2]])) %>%
    mutate(row = as.numeric(row), col = as.numeric(col)) %>% 
    arrange(row) %>%
    mutate(node = row_number() + 1, sum_node = 1) %>% 
    select(-.data[[var2]]) %>%
    rbind(top_box %>% select(names(.))) %>% 
    arrange(row) %>% 
    mutate(labeln = paste0(label, "\n", n)) %>%
    filter(row == max(row)) %>% 
    mutate(start = max(consortdf$row))
  
  p <- data.frame(x = c(0,(max(consortdf$col)*2+1)),
                  y = c(0,(max(consortdf$row)*2+1))) %>%
    ggplot(aes(x, y)) +
    scale_y_continuous(trans = "reverse") +
    theme_linedraw() 
  
  p2 <- p + geom_rect(inherit.aes = FALSE,
                      data = consortdf, 
                      aes(xmin = col - box_width, 
                          xmax = col + box_width, 
                          ymin = row - box_height, 
                          ymax = row + box_height),
                      color = "black",
                      fill = "white") +
    geom_rect(inherit.aes = FALSE,
              data = consortdf2, 
              aes(xmin = col - box2_width, 
                  xmax = col + box2_width, 
                  ymin = row - box2_height, 
                  ymax = row + box2_height),
              color = "black",
              fill = "white") +
    geom_text( inherit.aes = FALSE,
               data = consortdf, 
               aes(x= col, 
                   y=row,
                   label= labeln),
               size=4) +
    geom_text( inherit.aes = FALSE,
               data = consortdf2, 
               aes(x= col, 
                   y=row,
                   label= labeln),
               size=4) 
  p3 <- p2 + 
    ## lines to the right
    geom_segment(inherit.aes = FALSE,
                 data = consortdf %>% filter(node %nin% 1) %>% filter(row %nin% max(consortdf$row)), 
                 aes(x = sum_node, y = row, xend = col-box_width, yend = row),
                 arrow = arrow(length = unit(5, "mm"))) +
    ## down line
    geom_segment(inherit.aes = FALSE,
                 data = consortdf, 
                 aes(x = sum_node, y = min(row)+box_height, xend = sum_node, yend = max(row)-box_height),
                 arrow = arrow(length = unit(5, "mm"))
    ) +
    ## down left
    geom_segment(inherit.aes = FALSE,
                 data = consortdf2, 
                 aes(x = sum_node, y = start+box_height, xend = col, yend = max(row)-box2_height),
                 arrow = arrow(length = unit(4, "mm"))
                 
    ) 
  
  p4 <- p3 + 
    theme_void()
  if(!is.null(date)) p4 <- p4 + annotate("text", x = max(c(consortdf$col, consortdf2$col), na.rm = T)+0.4, y = max(c(consortdf$row, consortdf2$row), na.rm = T)+0.8,
                                         label = paste0("*As of ", date), size =4, hjust = 1)
  return(p4)  
  
}
