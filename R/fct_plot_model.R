#' result_to_df
#'
#' This function is used to convert list returned from the prediction
#' function into a data frame for easier manipulation and plotting.
#'
#' The function additionally transforms the data to log with `ytrans`.
#'
#' @param prediction_object a list of the predicted values returned by the
#' prediction fucntion
#' @param log flag for converting the data to log scale. `TRUE` by default.
#'
#' @return a dataframe of the prediction
#' @export
#'
#' @examples
#' \dontrun{
#  result_to_df(prediction_object, log = TRUE)
#' }
result_to_df <- function(prediction_object, log = TRUE) {
  dat <- as.data.frame(prediction_object) |>
    tibble::rownames_to_column(var = "cage_number") |>
    tidyr::pivot_longer(
      cols = !cage_number,
      names_to = "week_bundle",
      values_to = "count"
    ) |>
    tidyr::separate(week_bundle,
                    into = c("week", "value"),
                    sep = "\\.") |>
    tidyr::pivot_wider(
      id_cols = c(cage_number, week),
      names_from = "value",
      values_from = "count"
    ) |>
    tidyr::separate(
      week,
      into = c("lice_age", "predict_week"),
      sep = "_",
      remove = FALSE
    ) |>
    dplyr::mutate(week = factor(
      week,
      levels = c("FX_1wk", "FX_2wk", "OM_1wk", "OM_2wk", "AF_1wk", "AF_2wk")
    )) |>
    dplyr::mutate(lice_age = factor(lice_age, levels = c("FX", "OM", "AF"))) |>
    dplyr::mutate(cage_name = paste("Cage", cage_number)) |>
    dplyr::mutate(cage_name_nb = paste("Bur", cage_number))


  if (log) {
    dat <- dat |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ytrans))
  }

  dat
}


#' plot_predition
#' A function to generate plot from the predicted data.
#'
#' @param prediction_df a dataframe returned by `restult_to_df`
#' @param log flag for converting the data to log scale. `TRUE` by default
#' @param lang the language for the plot. Can be `en` or `nb` only.
#' @param location the number of the farm location.
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#'  result_to_df(prediction_object) |> plot_prediction()
#' }
plot_prediction <-
  function(prediction_df,
           log = TRUE,
           lang = 'en',
           location = NULL) {
    p <- prediction_df |>
      ggplot2::ggplot() +
      ggplot2::aes(x = week, y = mu, color = lice_age) +
      ggplot2::geom_segment(ggplot2::aes(
        x = week,
        y = ci1_count,
        xend = week,
        yend = ci2_count
      )) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = week,
          y = ci1_true,
          xend = week,
          yend = ci2_true
        ),
        size = 3,
        alpha = 0.5
      ) +
      ggplot2::geom_point(size = 5, pch = 21) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")

    if (lang == 'en') {
      p <- p +
        ggplot2::facet_wrap(~ cage_number, ncol = 4) +
        ggplot2::labs(
          title = paste("Prediction model results for location number" , location),
          x = "Week",
          y = "Lice per Fish"
        ) +
        ggplot2::scale_x_discrete(labels = c(1, 2, 1, 2, 1, 2)) +
        ggplot2::scale_color_discrete(name = "",
                                      labels = c("Sessile", "Motile", "Female"))
    }

    if (lang == 'nb') {
      p <- p +
        ggplot2::facet_wrap(~ cage_number, ncol = 4) +
        ggplot2::labs(
          title = paste("Prediksjonsmodellresultater for stedsnummer", location),
          x = "Uke",
          y = "Lus per fisk"
        ) +
        ggplot2::scale_x_discrete(labels = c(1, 2, 1, 2, 1, 2)) +
        ggplot2::scale_color_discrete(name = "",
                                      labels =  c("Fastsittende", "Mobile", "Hunnlus"))
    }



    if (log) {
      p <- p +
        ggplot2::scale_y_continuous(breaks =  ytrans(c(0, .1, .2, .5, 1, 2, 4, 8)),
                                    labels = c(0, .1, .2, .5, 1, 2, 4, 8))
    }

    p

  }
