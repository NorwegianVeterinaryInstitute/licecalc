# continue the predict function


restults <- as.data.frame(result_list) |>
    tibble::rownames_to_column(var = "cage_number")  |>  
    tidyr::pivot_longer(
        cols = !cage_number,
        names_to = "week_bundle",
        values_to = "count"

    )  |>  
    tidyr::separate(week_bundle, into = c("week", "value"), sep = "\\.")  |>  
    tidyr::pivot_wider(
        id_cols = c(cage_number, week),
        names_from = "value", 
        values_from = "count"
    )

