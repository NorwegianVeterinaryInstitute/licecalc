# continue the predict function


results <- as.data.frame(prediction.object) |>
    tibble::rownames_to_column(var = "cage_number") |>
    tidyr::pivot_longer(
        cols = !cage_number,
        names_to = "week_bundle",
        values_to = "count"
    ) |>
    tidyr::separate(week_bundle, into = c("week", "value"), sep = "\\.") |>
    tidyr::pivot_wider(
        id_cols = c(cage_number, week),
        names_from = "value",
        values_from = "count"
    ) |>
    tidyr::separate(week, into = c("lice_age", "predict_week"), sep = "_", remove = FALSE) |>
    dplyr::mutate(week = factor(week, levels = c("FX_1wk", "FX_2wk", "OM_1wk", "OM_2wk", "AF_1wk", "AF_2wk"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ytrans))

ytrans <- function(x) log(x + .1)


ytransform <- scales::trans_new(name = "ytrans", transform = function(x) log(x + 0.1), inverse = function(x) exp(x) - 0.1)

results |>
   # dplyr::filter(cage_number == 2) |>
    ggplot() +
    aes(x = week, y = mu, color = lice_age) +
    geom_segment(aes(x = week, y = ci1_count, xend = week, yend = ci2_count)) +
    geom_segment(aes(x = week, y = ci1_true, xend = week, yend = ci2_true),
        size = 3, alpha = 0.5
    ) +
    geom_point(size = 5, pch = 21) +
    facet_wrap(~cage_number, ncol = 1) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks =  ytrans(c(0,.1,.2,.5,1,2,4,8)), labels = c(0,.1,.2,.5,1,2,4,8)) +
    scale_x_discrete(labels = c(1, 2, 1, 2, 1, 2))
