#' Read in Excel workbook with multiple sheets without using Java
#'
#' Read in Excel workbook with multiple sheets without using Java. Uses `readxl`
#' @param filename increments
#' @param sf scaling factor (multiplicative expand) - higher `sf` will result in wider margins before ticks begin 
#' @return data.frame
#' @export
#' @examples
#' \dontrun{
#' read_msheets(here("myexcel.xlsx"))
#' }

read_msheets <- function(filename) {
  require(readxl)    
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(filename)
  tibble <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  return(data_frame)
}