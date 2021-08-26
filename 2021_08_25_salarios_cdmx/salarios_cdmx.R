# Librerias:
library(tidyverse)
library(ggtext)
library(cowplot)

# Base de datos:
bd <- read_csv("https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/cd754b85-d1f6-417f-839b-62d19ec84f16/download/2021_07julio_base_remuneraciones.csv")

# Colores:
color1 <- "#409088"
color2 <- "#4FAE4F"
color3 <- rgb(229,191,93,maxColorValue = 255)
color4 <- rgb(0,0,0,maxColorValue = 255)

scales::show_col(c(color1, color2, color3, color4))

# Grafica:
rug <- bd %>%
  ggplot(aes(x = SUELDO_TABULAR_BRUTO)) +
  geom_boxplot(outlier.alpha = 0.01,
               color = color1) +
  geom_rug(color = color2) +
  scale_x_continuous(label = scales::dollar_format(prefix = "$"),
                     breaks = seq(0, 120000, by = 10000)) +
  labs(title = "SUELDO TABULAR BRUTO",
       subtitle = "Trabajadores del gobierno de la CDMX",
      y = "", x = "") +
  theme(axis.text.x = element_markdown(vjust = 0.5,
                                       color = color2,
                                       face = "bold",
                                       size = 12),
        axis.text.y = element_blank(),
        # panel.background = element_rect(fill = color4),
        # plot.background = element_rect(fill = color4),
        text = element_text(family = "Poppins"),
        plot.title = element_text(face = "bold",
                                  size = 20,
                                  color = color1),
        plot.subtitle = element_markdown(size = 15,
                                         color = color2,
                                         family = "Arial"),
        plot.caption = element_text(color = color1,
                                    face = "bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        legend.position = "none")

dens <- bd %>%
  ggplot(aes(x = SUELDO_TABULAR_BRUTO)) +
  geom_density(color = color1, size = 2, fill = color2) +
  geom_rug(color = color2) +
  scale_x_continuous(label = scales::dollar_format(prefix = "$"),
                     breaks = seq(0, 120000, by = 10000)) +
  labs(x = "MXN, 2021",
       y = "",
       caption = "Fuente: https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/cd754b85-d1f6-417f-839b-62d19ec84f16/download/2021_07julio_base_remuneraciones.csv") +
  theme(axis.text.x = element_markdown(vjust = 0.5,
                                       color = color2,
                                       face = "bold",
                                       size = 12),
        axis.text.y = element_blank(),
        # panel.background = element_rect(fill = color4),
        # plot.background = element_rect(fill = color4),
        text = element_text(family = "Poppins"),
        plot.title = element_text(face = "bold",
                                  size = 20,
                                  color = color1),
        plot.subtitle = element_markdown(size = 15,
                                         color = color2),
        plot.caption = element_text(color = color1,
                                    face = "bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        legend.position = "none")

cowplot::plot_grid(rug, dens, ncol = 1, align = "v")
?plot_grid


# Datos:
prop.table(table(bd$SUELDO_TABULAR_BRUTO < 20000))
prop.table(table(bd$SUELDO_TABULAR_BRUTO > 100000))

min(bd$SUELDO_TABULAR_BRUTO)

