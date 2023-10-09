#' convData
#'
#' @description A utils function to convert decimal , to . from
#' manually inputed data
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

convData <- function(value){
  outvalue <- as.numeric(sub(",", ".", value, fixed = TRUE))
  return(outvalue)
}
