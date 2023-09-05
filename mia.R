
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
                                    xmax = 25, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#E01E5A",
                       alpha = 0.8) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 38, 
                                    xmax = 40, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#E01E5A",
                       alpha = 0.8) +
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
                                    y = q75-10),
                       col = "#E01E5A",
                       size = 3) +
    ggplot2::geom_line(data = y, 
                       ggplot2::aes(x = SEM,
                                    y = y),
                       color = "#2EB67D", #"#36C5F0",
                       size = 3) +
    ggplot2::ylab("Número de Casos") +
    ggplot2::xlab("Semanas Epidemiológicas") +
    ggplot2::annotate("text",
                      label = "Control Proactivo",
                      x = 10, 
                      y = 450,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 6) +
    ggplot2::annotate("text",
                      label = "Escenarios Operativos \nbasados \nen \nTransmisión Persistente",
                      x = 10, 
                      y = 380,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Control Reactivo",
                      x = 42, 
                      y = 450,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 6) +
    ggplot2::annotate("text",
                      label = "Escenarios Operativos \nbasados \nen \nTransmisión Activa",
                      x = 45, 
                      y = 380,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 4) +
    ggplot2::geom_line(data = prob, 
                       ggplot2::aes(x = SEM,
                                    y = prob),
                       color = "#36C5F0",
                       size = 2)  +
    ggplot2::annotate("text",
                      label = "Acciones Específicas Básicas",
                      x = 10, 
                      y = 280,
                      col = "gray",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Control de Probables",
                      x = 10, 
                      y = 250,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Hotspots del Vector",
                      x = 10, 
                      y = 230,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Acciones Específicas de Soporte",
                      x = 10, 
                      y = 150,
                      col = "gray",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Termonebulización,\n Nebulización, & \nEliminación Masiva de Criaderos",
                      x = 10, 
                      y = 110,
                      col = "gray",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "Sin Control",
                      x = 30, 
                      y = 425,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Con Control",
                      x = 35, 
                      y = 105,
                      col = "#2EB67D",
                      size = 4) +
    ggplot2::annotate(geom ="text",
                      label = "Rociado Residual Intradomiciliario & Control Larvario",
                      x = 22, 
                      y = 220,
                      angle = 90,
                      col = "white",
                      size = 4) +
    ggplot2::annotate(geom ="text",
                      label = "Control Larvario",
                      x = 39, 
                      y = 220,
                      angle = 90,
                      col = "white",
                      size = 4)


