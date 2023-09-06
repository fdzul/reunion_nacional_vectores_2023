
x <- tibble::tibble(abanico = c(33.6, 50.4, 67.1, 83.9, 
                     101, 118, 134, 151),
               distancia = c(20, 30, 40, 50, 60, 70, 80, 90))

library(ggplot2)
library(ggside)
ggplot(mpg, aes(displ, hwy, colour = class)) + 
    geom_point(size = 2) +
    geom_xsidedensity(aes(y = after_stat(density)), 
                      position = "stack") +
    geom_ysidedensity(aes(x = after_stat(density)), 
                      position = "stack") +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = .5))
ggplot(data = x,
       aes(y = abanico, 
           x = distancia)) +
    geom_point(size = 2,
               fill = "white",
               shape = 21,
               stroke = 2.5,
               col = "#2EB67D") +
    geom_vline(xintercept = 45, linetype = 2) +
    geom_hline(yintercept = 75.6, linetype = 2) +
    xlab("Distancia a la Pared ") +
    ylab("Abanico")
        
mod <- lm(abanico~distancia, data = x)
predict(mod)
#define new observation
newdata = data.frame(distancia= c(45, 48))

#use model to predict points value
predict(mod, newdata)
