# Librerias ----
library(tidyverse)
library(vapoRwave)

# Grafico de puntos ----
presion <- read_csv("01_Datos/blodPressure.csv")

#Todos los colores:
colors()

# Activamos el lienzo:
presion %>%
  ggplot()

# Relación entre edad y presión sanguínea:
presion %>%
  ggplot(aes(y = sistolicBloodPressure, # Posición eje Y
             x = weightInPounds,        # Posición eje X
             color = ageInYears         # Color de punto
  )) +
  geom_smooth(method = "lm") +          # lineas de regresión
  geom_point(size = 10, alpha = 0.5) +  # geometria de puntos
  # Modificamos el eje X (si es numerico)
  scale_x_continuous(
    breaks = seq(from = 100,
                 to = 230,
                 by = 10), # Cada cuando se ponen las marcas
    labels = scales::comma_format(suffix = " lb") # Sobre las etiquetas
  ) +
  # Modificamos los colores de las geoms que usan el aes "color"
  scale_color_gradientn(colors = c("green", "yellow", "red")) +
  # Etiquetas de la gráfica
  labs(title = "Relationship btw weight and Sistolic Blood Pressure",
       x = "Weight in pounds",
       y = "Sistolic Blood Pressure",
       caption = "Source: Textbook data") +
  vapoRwave::new_retro()  +
  guides(color = guide_colorbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 50,
                                barheight = 0.5)) +
  theme(axis.title.y = element_text(angle = 90),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 1,
                                    family = "Poppins",
                                    face = "bold",
                                    color = "yellow" ))
