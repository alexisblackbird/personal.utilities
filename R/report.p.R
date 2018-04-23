#' Report p
#'
#' Formats p values appropriately for RMarkdown documents.
#'
#' Formats p values appropriately for Rmarkdown documents in APA style by returning the p value as > .001 or = .xxx to be used for inline code reporting of p values.
#'
#' @param p A numeric p value.
#' @return a string without quotes with the appropriately formatted p value.
#' @export


report.p <- function(p){
  if(p < 0.001) {
    return(noquote("< .001"))
  } else {
    return(noquote(paste0("= ", nozero(p))))
  }
}
