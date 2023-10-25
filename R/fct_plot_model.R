#' results_to_df
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
#  results_to_df(prediction_object, log = TRUE)
#' }
restult_to_df <- function(prediction_object, log = TRUE) {
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
    ))

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
#' @param log flag for converting the data to log scale. `TRUE` by default.
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#  plot_prediction(prediction_object, log = TRUE)
#' }
plot_prediction <- function(prediction_df, log = TRUE) {
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
    ggplot2::facet_wrap( ~ cage_number, ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Week", y = "Lice per Fish") +
    ggplot2::scale_x_discrete(labels = c(1, 2, 1, 2, 1, 2))

  if (log) {
    p <- p +
      ggplot2::scale_y_continuous(breaks =  ytrans(c(0, .1, .2, .5, 1, 2, 4, 8)),
                                  labels = c(0, .1, .2, .5, 1, 2, 4, 8))
  }

  p

}






























#' plot_lice
#' Function to plot predictions for one cage
#'
#' @param prediction.object is a prediction object created with the predict_lice function
#' @param cage is the cage number
#' @param lan is language ('no' = Norwegian, 'en' = English)
#'
#' @return
#' The output is a prediction plot for one cage
#'
#' @export
#'
#' @examples
#' \dontrun{
#' AF = 1:3
#' OM = 3:1
#' FX = c(0,0,.5)
#' prediction_object = predict_lice(AF=AF, OM=OM, FX=FX)
#' par(mai = c(.7,.7,.6,.01))
#' plot_lice(prediction.object, cage = 2, lan="en")
#' }
plot_lice = function(prediction.object,
                     cage = 1,
                     lan = "no") {
  yat <- c(0, .1, .2, .5, 1, 2, 4, 8)
  ytrans <- function(x)
    log(x + .1)
  ymax <- 5

  # yat=.5*c(0:10)
  # ytrans = function(x) x

  xlab1 <- ifelse(lan == "no", "Uke", "Week")
  if (lan == "no")
    xlab2 <-
    c("Fastsittende", "Mobile", "Hunnlus")
  else
    xlab2 <- c("Sessile", "Motile", "Female")
  xlab3 <-
    ifelse(lan == "no", paste0("Bur ", cage), paste0("Cage ", cage))

  ylab <- ifelse(lan == "no", "Lus per fisk", "Lice per fish")

  extractpoint <- function(stage, wk, metric, cage) {
    return(ytrans(prediction.object[[paste0(stage, "_", wk, "wk")]][[metric]][cage]))
  }

  extractinterval <- function(stage, wk, cage) {
    out <- c(
      extractpoint(stage, wk, "ci1_count", cage),
      extractpoint(stage, wk, "ci1_true", cage),
      extractpoint(stage, wk, "mu", cage),
      extractpoint(stage, wk, "ci2_true", cage),
      extractpoint(stage, wk, "ci2_count", cage)
    )
    return(out)
  }

  plotinterval <- function(stage, wk, cage, xat, col) {
    interval <- extractinterval(stage, wk, cage)
    segments(
      x0 = xat,
      x1 = xat,
      y0 = interval[1],
      y1 = interval[5],
      col = col
    )
    segments(
      x0 = xat,
      x1 = xat,
      y0 = interval[2],
      y1 = interval[4],
      col = col,
      lwd = 5
    )
    points(
      x = xat,
      y = interval[3],
      col = col,
      pch = 16,
      cex = 2
    )
  }

  plot(
    0,
    0,
    type = "n",
    xlim = c(0, 3),
    ylim = c(ytrans(0), ytrans(ymax)),
    axes = F,
    xlab = "",
    ylab = ""
  )

  plotinterval("FX", 1, cage, .33, "grey")
  plotinterval("FX", 2, cage, .66, "grey")

  plotinterval("OM", 1, cage, 1.33, "blue")
  plotinterval("OM", 2, cage, 1.66, "blue")

  plotinterval("AF", 1, cage, 2.33, "red")
  plotinterval("AF", 2, cage, 2.66, "red")

  axis(side = 2,
       at = ytrans(yat),
       lab = yat)

  mtext(c(1, 2, 1, 2, 1, 2),
        side = 1,
        at = c(.33, .66, 1.33, 1.66, 2.33, 2.66))
  mtext(xlab1, side = 1, line = 2)
  mtext(ylab, side = 2, line = 2.5)
  mtext(xlab2, side = 3, at = c(.5, 1.5, 2.5))
  mtext(xlab3,
        side = 3,
        line = 2,
        font = 2)
}

# Example:
# AF = 1:3
# OM = 3:1
# FX = c(0,0,.5)
# prediction.object = predict_lice(AF=AF, OM=OM, FX=FX)
# par(mai = c(.7,.7,.6,.01))
# plot_lice(prediction.object, cage = 2, lan="en")
