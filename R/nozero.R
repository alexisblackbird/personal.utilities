#' No zero before decimal
#'
#' Truncates a decimal to three decimal places
#'
#' Truncates a < 1 number to three decimal places with no 0 before the decmial places, for use in RMarkdown inline code for APA style documents.
#' @param x A number < 1
#' @return a string without quotes with the appropriately formatted p value.
#' @export

nozero <- function(x){
  #should add a warning if input >= 1
  out <- round(x, digits = 3)
  out <- paste0(out)
  out <- stringr::str_replace(out, "0.", ".")
  return(out)
}
