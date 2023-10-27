#' conv_data
#'
#' @description A utils function to convert decimal , to . from
#' manually inputed data
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
conv_data <- function(value){
  outvalue <- as.numeric(sub(",", ".", value, fixed = TRUE))
  return(outvalue)
}

#' ytrans
#'
#' @description transform to log scale
#'
#' @noRd
ytrans <- function(x) log(x + .1)


#' get_reactive_translator
#'
#' @description get reactive translator
#'
#' @noRd
get_reactive_translator <- function(translator, selected_language) {

    selected <- selected_language
    if (length(selected) > 0 &&
        selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator

}
