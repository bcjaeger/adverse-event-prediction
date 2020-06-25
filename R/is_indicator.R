##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
is_indicator <- function(x) {

  ux <- unique(na.omit(x))
  if(length(ux) > 2) return(FALSE)
  all(sort(ux) == c(0,1))

}
