

a <- tibble::tibble(ecotopos = c("Gallineros",
                            "Perreras",
                            "Leña",
                            "Rocas",
                            "Material de Construcción",
                            "Conejeras"),
               positivos = c(54, 7, 6, 3, 4, 3 ),
               revisados = c(261, 202, 51, 41, 24, 19),
               dimidiata = c(241, 28, 4, 9, 16, 18)) |>
    dplyr::mutate(infestacion = round(positivos/revisados*100, 
                                       digits = 1),
                  per_dimidiata = round(dimidiata/sum(dimidiata)*100,
                                        digits = 1)) |>
    dplyr::mutate(ecotopos = forcats::fct_reorder(ecotopos, 
                                                  dplyr::desc(infestacion))) |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = ecotopos,
                                   y = infestacion,
                                   fill = infestacion)) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(x = ecotopos,
                                    y = infestacion,
                                    label = infestacion),
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Ecotopos") +
    ggplot2::ylab("Porcentaje") +
    ggplot2::ggtitle("Infestación")


b <- tibble::tibble(ecotopos = c("Gallineros",
                                           "Perreras",
                                           "Leña",
                                           "Rocas",
                                           "Material de Construcción",
                                           "Conejeras"),
                              positivos = c(54, 7, 6, 3, 4, 3 ),
                              revisados = c(261, 202, 51, 41, 24, 19),
                              dimidiata = c(241, 28, 4, 9, 16, 18)) |>
    dplyr::mutate(infestacion = round(positivos/revisados*100, 
                                      digits = 1),
                  per_dimidiata = round(dimidiata/sum(dimidiata)*100,
                                        digits = 1)) |>
    dplyr::mutate(ecotopos = forcats::fct_reorder(ecotopos, 
                                                  dplyr::desc(infestacion))) |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = ecotopos,
                                   y = per_dimidiata,
                                   fill = per_dimidiata)) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(x = ecotopos,
                                    y = per_dimidiata,
                                    label = per_dimidiata),
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Ecotopos") +
    ggplot2::ylab("Porcentaje") +
    ggplot2::labs(title = "Porcentaje de T. dimidiata",
                     caption = "Arisqueta, 2022. RNV")


library(patchwork)
a / b


