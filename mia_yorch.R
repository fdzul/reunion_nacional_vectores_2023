
# Step 1. load the dataset #####
load("/Users/fdzul/Library/CloudStorage/OneDrive-Personal/automatic_read_sinave/8.RData/epid_channel_data.RData")

# Step 2. extract the dengue cases of veracruz ####
x <- x |>
    dplyr::filter(DES_EDO_RES == "VERACRUZ") 

y <- tibble::tibble(SEM = x$SEM,
                    y = x$q25,
                    arm = "Nuevo Paradigma")
prob <- tibble::tibble(SEM = x$SEM,
                       prob = rep(0, length(x$SEM)))

# plot risk stratificaction  #####
ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = c(-Inf, 25), 
                                    xmax = c(25, Inf), 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = c("#2EB67D", "#E01E5A"),
                       alpha = .2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 20, 
                                    xmax = 30,
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "orange",
                       alpha = 0.8) +
    #ggplot2::geom_rect(ggplot2::aes(xmin = 38, 
    #                                xmax = 40, 
    #                                ymin = -Inf, 
    #                                ymax = Inf),
    #                   fill = "#E01E5A",
    #                   alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 25,
                        color = "gray", #"#36C5F0",
                        size = 1,
                        linetype = 3) +
    ggplot2::geom_vline(xintercept = 20,
                        color = "gray", #"#36C5F0",
                        size = 1,
                        linetype = 3) +
    ggplot2::geom_line(data = x, 
                       ggplot2::aes(x = SEM,
                                    y = q75),
                       col = "#E01E5A",
                       size = 3) +
    ggplot2::ylab("Número de Casos") +
    ggplot2::xlab("Semanas Epidemiológicas") +
    ggplot2::annotate("text",
                      label = "Acciones Preventivas",
                      x = 15, 
                      y = 450,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 6) +
    ggplot2::annotate(geom ="text",
                      label = "Respuesta Temprana",
                      x = 25, 
                      y = 420,
                      #angle = 90,
                      col = "white",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Control Quimico Focalizado",
                      x = 6, 
                      y = 380,
                      #text = "risk",
                     col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Campañas de Eliminación Másiva de Criaderos",
                      x = 10, 
                      y = 360,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Ronda de Medios",
                      x = 4, 
                      y = 340,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Certificación de Espacios Libres de Criaderos",
                      x = 9.5, 
                      y = 320,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Activacion de Comités Municipales",
                      x = 7.5, 
                      y = 300,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3)+ 
    ggplot2::annotate("text",
                      label = "- Rociado Residual en Sitios de Interés",
                      x = 8, 
                      y = 280,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3)+
    ggplot2::annotate("text",
                      label = "- Manteminiento/Reparacion de Vehiculos & Equipo",
                      x = 10.5, 
                      y = 260,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Capacitación de Personal Médico y Vectores",
                      x = 9.5, 
                      y = 240,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "Acciones de Reforzamiento",
                      x = 42, 
                      y = 450,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 6) +
    ggplot2::annotate("text",
                      label = "- Incremento de los Índices Entomológicos",
                      x = 39, 
                      y = 200,
                      #text = "risk",
                      col = "black",
                      size = 3)+
    ggplot2::annotate("text",
                      label = "- Incremento de los Casos (líneal a exponencial)",
                      x = 40, 
                      y = 180,
                      #text = "risk",
                      col = "black",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "- Incremento de la Cobertura del Control Vectorial",
                      x = 40.3, 
                      y = 160,
                      #text = "risk",
                      col = "black",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "Control Focalizado-Acciones de Barrido",
                      x = 40.3, 
                      y = 140,
                      #text = "risk",
                      col = "black",
                      size = 3) +
    ggplot2::scale_x_continuous(breaks = seq(from= 1, 
                                             to = 53, 
                                             by = 5))

