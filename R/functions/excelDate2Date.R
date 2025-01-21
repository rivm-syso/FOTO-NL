#' @title excelDate2Date
#' @name excelDate2Date
#' @author jaap slootweg
#' @description calculate date from excel approach format
#' @param excelDate  # days since 1900
#' @return real R format date
#' not atexport
excelDate2Date <- function(excelDate) {  #thank you, bbolker
  Date <- excelDate + as.Date("1900-01-01") - 2  #(1 for not starting at 0, 1 for MS forgetting none-leap year 1900
  ## FIXME: add "if >1900-Feb-28" switch? not needed;)
  return(Date)
} 
