

# Step 1. load the functions ####
source("~/Library/CloudStorage/Dropbox/IRS_doses/functions/Dosis_IRS.R")


# Step 2. calculate the doses for IRS with bendiocarb ####
bendiocarb1 <- tibble::tibble(flow = rep(seq(from = 100, 
                                            to = 1300, 
                                            by = 50), 
                                        times = 2),
                             dose = c(dose_irs(swath = 75, 
                                               speed = 2.2, 
                                               flowrate = seq(from = 100, 
                                                              to = 1300, 
                                                              by = 50), 
                                               formulation = "WP", 
                                               cantidad = 125,
                                               concentracion = 80, 
                                               dilucion = 7.5),
                                      dose_irs(swath = 75, 
                                               speed = 2.2, 
                                               flowrate = seq(from = 100, 
                                                              to = 1300, 
                                                              by = 50), 
                                               formulation = "WP", 
                                               cantidad = 125,
                                               concentracion = 80, 
                                               dilucion = 10))) |>
    dplyr::mutate(insecticide = "Bendiocarb") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(sobres = rep(x = "1 Sobre", 
                                      each = 50))

bendiocarb2 <- tibble::tibble(flow = rep(seq(from = 100, 
                                             to = 1300, 
                                             by = 50), 
                                         times = 2),
                              dose = c(dose_irs(swath = 75, 
                                                speed = 2.2, 
                                                flowrate = seq(from = 100, 
                                                               to = 1300, 
                                                               by = 50), 
                                                formulation = "WP", 
                                                cantidad = 125*2,
                                                concentracion = 80, 
                                                dilucion = 7.5),
                                       dose_irs(swath = 75, 
                                                speed = 2.2, 
                                                flowrate = seq(from = 100, 
                                                               to = 1300, 
                                                               by = 50), 
                                                formulation = "WP", 
                                                cantidad = 125*2,
                                                concentracion = 80, 
                                                dilucion = 10))) |>
    dplyr::mutate(insecticide = "Bendiocarb") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(sobres = rep(x = "2 Sobres", 
                                      each = 50)) 

bendiocarb3 <- tibble::tibble(flow = rep(seq(from = 100, 
                                             to = 1300, 
                                             by = 50), 
                                         times = 2),
                              dose = c(dose_irs(swath = 75, 
                                                speed = 2.2, 
                                                flowrate = seq(from = 100, 
                                                               to = 1300, 
                                                               by = 50), 
                                                formulation = "WP", 
                                                cantidad = 125*3,
                                                concentracion = 80, 
                                                dilucion = 7.5),
                                       dose_irs(swath = 75, 
                                                speed = 2.2, 
                                                flowrate = seq(from = 100, 
                                                               to = 1300, 
                                                               by = 50), 
                                                formulation = "WP", 
                                                cantidad = 125*3,
                                                concentracion = 80, 
                                                dilucion = 10))) |>
    dplyr::mutate(insecticide = "Bendiocarb") |>
    dplyr::mutate(volumen_total = rep(x = c("7.5 Litros", "10 Litros"), 
                                      each = 25)) |>
    dplyr::mutate(sobres = rep(x = "3 Sobres", 
                               each = 50))


bendiocarb <- dplyr::bind_rows(bendiocarb1, bendiocarb2, bendiocarb3)


library(ggtext)
library(htmltools)
ggplot2::ggplot() +
    ggplot2::geom_rect(data = bendiocarb,
                       ggplot2::aes(xmin = 550, 
                                    xmax = 605, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#36C5F0",
                       alpha = 0.04) +
    ggplot2::geom_rect(data = bendiocarb,
                       ggplot2::aes(xmin = Inf, 
                                    xmax = -Inf, 
                                    ymin = 0.1, 
                                    ymax = 0.4),
                       fill = "#ECB22E",
                       color = "white",
                       alpha = 0.04) +
    ggplot2::geom_rect(data = bendiocarb,
                       ggplot2::aes(xmin = Inf, 
                                    xmax = -Inf, 
                                    ymin = 0.4, 
                                    ymax = 1),
                       fill = "#E01E5A",
                       color = "white",
                       alpha = 0.04) +
    #ggplot2::geom_line(data = bendiocarb, 
     #                  ggplot2::aes(x = flow,
     #                               y = dose),
     #                  linewidth = 1) +
    ggplot2::geom_point(data = bendiocarb, 
                        ggplot2::aes(x = flow,
                                     y = dose), 
                        shape = 21,
                        fill = "#2EB67D",
                        size = 2,
                        col = "white") +
    ggplot2::facet_grid(facets = c("sobres", "volumen_total"),
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
