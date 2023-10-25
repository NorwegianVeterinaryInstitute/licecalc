#' lice_model_v2
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


make_plot_from_location <- function(location){

  pressure <- extract_ip(location)

  lice_counts <- extract_lice(location)

  prediction_object <- predict_lice(IP_1wk = pressure$IP_1wk,
               IP_2wk = pressure$IP_2wk,
               ST = pressure$ST,
               AF = lice_counts$AF,
               OM = lice_counts$OM,
               FX = 5 #lice_counts$FX
  )

  p <- result_to_df(prediction_object) |> plot_prediction(location = location)

  p

}
