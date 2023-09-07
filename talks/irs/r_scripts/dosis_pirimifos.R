


# Step 1. load the functions ####
source("~/Library/CloudStorage/Dropbox/IRS_doses/functions/Dosis_IRS.R")

# Step 2. calculate the doses for IRS with pirimifos-metil ####
pirimifos_metil1 <- tibble::tibble(flow = rep(seq(from = 100, 
                                                 to = 1300, 
                                                 by = 50), 
                                             times = 2),
                                  dose = c(dose_irs(swath = 75,
                                                    speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                    flowrate = seq(from = 100, 
                                                                   to = 1300, 
                                                                   by = 50),
                                                    formulation = "CS",
                                                    cantidad = 833,
                                                    concentracion = 28.16,
                                                    dilucion = 6.667 + 0.833),
                                           dose_irs(swath = 75,
                                                    speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                    flowrate = seq(from = 100, 
                                                                   to = 1300, 
                                                                   by = 50),
                                                    formulation = "CS",
                                                    cantidad = 833,
                                                    concentracion = 28.16,
                                                    dilucion = 9.167 + 0.833))) |>
    dplyr::mutate(insecticide = "Pirimifos-Metil") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(botes = rep(x = "1 Bote", 
                               each = 50))

pirimifos_metil2 <- tibble::tibble(flow = rep(seq(from = 100, 
                                                  to = 1300, 
                                                  by = 50), 
                                              times = 2),
                                   dose = c(dose_irs(swath = 75,
                                                     speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                     flowrate = seq(from = 100, 
                                                                    to = 1300, 
                                                                    by = 50),
                                                     formulation = "CS",
                                                     cantidad = 833*2,
                                                     concentracion = 28.16,
                                                     dilucion = 6.667 + 0.833),
                                            dose_irs(swath = 75,
                                                     speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                     flowrate = seq(from = 100, 
                                                                    to = 1300, 
                                                                    by = 50),
                                                     formulation = "CS",
                                                     cantidad = 833*2,
                                                     concentracion = 28.16,
                                                     dilucion = 9.167 + 0.833))) |>
    dplyr::mutate(insecticide = "Pirimifos-Metil") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(botes = rep(x = "2 Botes", 
                               each = 50))

pirimifos_metil3 <- tibble::tibble(flow = rep(seq(from = 100, 
                                                  to = 1300, 
                                                  by = 50), 
                                              times = 2),
                                   dose = c(dose_irs(swath = 75,
                                                     speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                     flowrate = seq(from = 100, 
                                                                    to = 1300, 
                                                                    by = 50),
                                                     formulation = "CS",
                                                     cantidad = 833*3,
                                                     concentracion = 28.16,
                                                     dilucion = 6.667 + 0.833),
                                            dose_irs(swath = 75,
                                                     speed = 2.2, # spraying speed (in seconds) per vertical metre on the wall
                                                     flowrate = seq(from = 100, 
                                                                    to = 1300, 
                                                                    by = 50),
                                                     formulation = "CS",
                                                     cantidad = 833*3,
                                                     concentracion = 28.16,
                                                     dilucion = 9.167 + 0.833))) |>
    dplyr::mutate(insecticide = "Pirimifos-Metil") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(botes = rep(x = "3 Botes", 
                              each = 50))


pirimifos <- dplyr::bind_rows(pirimifos_metil1, 
                              pirimifos_metil2,
                              pirimifos_metil3)

#
ggplot2::ggplot() +
    ggplot2::geom_rect(data = pirimifos,
                       ggplot2::aes(xmin = 550, 
                                    xmax = 605, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#36C5F0",
                       alpha = 0.04) +
    ggplot2::geom_rect(data = pirimifos,
                       ggplot2::aes(xmin = Inf, 
                                    xmax = -Inf, 
                                    ymin = 1, 
                                    ymax = 2),
                       fill = "#ECB22E",
                       color = "white",
                       alpha = 0.04) +
ggplot2::geom_point(data = pirimifos, 
                    ggplot2::aes(x = flow,
                                 y = dose), 
                    shape = 21,
                    fill = "#2EB67D",
                    size = 2,
                    col = "white") +
    ggplot2::facet_grid(facets = c("botes", "volumen_total"),
                        scales = "free") +
    ggplot2::ylab("Dosis (gr. i.a/m2)") +
    ggplot2::xlab("Flujo (ml/min)") +
    ggplot2::scale_x_continuous(breaks = seq(from = 0, 
                                             to = 1300, 
                                             by = 200)) +
    ggplot2::labs(title = paste("Dosis Estimadas para ", 
                                #"<span style='color: #4285F4'>**DENV1**</span>",
                                #"<span style='color: #2EB67D'>**DENV2**</span>",
                                "<span style='color: #E01E5A'>**Triatomas**</span>",
                                "&",
                                "<span style='color: #ECB22E'>**Aedes**</span>."))+ 
    ggplot2::theme(strip.text = ggplot2::element_text(size=12, 
                                                      colour="black"),
                   plot.title = ggtext::element_textbox_simple(face = "bold"),
                   plot.subtitle = ggtext::element_textbox_simple(face = "bold",
                                                                  margin = ggplot2::margin(t = 20),
                                                                  lineheight = 3),
                   #axis.text.x = ggplot2::element_text(angle = 90),
                   plot.title.position = "plot") 

