
# Step 1. load the dataset ####
x <- vroom::vroom("/Users/fdzul/Library/CloudStorage/OneDrive-Personal/IRS/Graham_Matthews/data1_8002.csv",
                  skip = 5) |>
    dplyr::mutate(boquilla = "8002")

y <- vroom::vroom("/Users/fdzul/Library/CloudStorage/OneDrive-Personal/IRS/Graham_Matthews/data1_8002E.csv",
                  skip = 5) |>
    dplyr::mutate(boquilla = "8002E")

# Step 3. row binding dataset ####
names(y) <- names(x)

xy <- dplyr::bind_rows(x, y)


#
xy |>
ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = Channels,
                                   y = volume),
                      width = 0.6,
                      col = "black",
                      fill = "#2EB67D") +
    ggplot2::facet_grid("boquilla")

