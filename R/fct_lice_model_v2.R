#' lice_model_v2
#'
#' @description A wrapper function tun run the prediction and make a plot.
#' To be used in the module `mod_lice_model_v2` server.
#'
#' @return A plot object
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


#' make_plot_for_cages_and_location
#'
#' @param location
#' @param user_data
#'
#' @return
#' @export
#'
#' @examples
make_plot_for_cages_and_location <- function(location, user_data){

  pressure <- extract_ip(location)

  user_data <- na.omit(user_data)

  prediction_object <- predict_lice(IP_1wk = pressure$IP_1wk,
                                    IP_2wk = pressure$IP_2wk,
                                    ST = pressure$ST,
                                    AF = user_data$adult_females,
                                    OM = user_data$other_motiles,
                                    FX = user_data$sessiles,
                                    W_SAL = user_data$fish_weight,
                                    N_SAL = user_data$fish_abundance,
                                    CLF = user_data$cleaner_fish)

  p <- result_to_df(prediction_object) |> plot_prediction(location = location)

  p

}
