#' ach_get_data
#'
#' This function pulls ACHQC data. 
#' @param date folder name which corresponds to a date. If null, will pull most recent date
#' @param dir file path relative to working directory
#' @export
#' @examples
#' ach_get_data(date = "2021-11-08", dir = "../../../data")

ach_get_data <- function (date = NULL, dir = NULL) 
{
  dir <- if (is.null(dir)) {
    file.path("..", "..", "..", "data", "standard")
  }
  else {
    file.path(dir)
  }
  possible_dirs <- dir(path = dir, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  if (length(possible_dirs) == 0) {
    stop(paste0("Looking in the following dir: ", dir))
    stop("The data directory is empty or its path was not properly specified.")
  }
  if (is.null(date)) {
    recent_dir <- sort(possible_dirs, decreasing = TRUE)[1]
  }
  else {
    if (date %in% possible_dirs) {
      recent_dir <- date
    }
    else {
      stop("The date provided is in the wrong format or is not available")
    }
  }
  data_dir <- file.path(dir, recent_dir, "data", "input")
  out <- readRDS(file.path(data_dir, "master.rds"))
  message("Loading ACHQC data from ", recent_dir)
  return(out)
}
