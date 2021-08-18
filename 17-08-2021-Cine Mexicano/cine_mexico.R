# Base de datos:
library(tidyverse)
library(ggtext)
library(rvest)
library(ggpomological)

# Página con los datos:
url <- "https://en.wikipedia.org/wiki/List_of_highest-grossing_Mexican_films"

# Jalamos el contenido
html <- read_html(url)

# Sacamos la tabla:
bd <- html %>%
  html_table() %>%
  pluck(1) %>%
  janitor::clean_names()

# Procesamos tabla:
pelis <- bd %>%
  mutate(total_gross_mxn = str_remove_all(total_gross_mxn,
                                          pattern = " million|\\$|\\[\\d\\]")) %>%
  mutate(total_gross_mxn = as.numeric(total_gross_mxn))

# Cambiamos nombres a español:
pelis$movie[c(1, 2, 8, 12, 22, 23, 30)] <-
  c("No se aceptan devoluciones",
    "Nosotros los Nobles",
    "La dictadura perfecta",
    "El crímen del padre Amaro",
    "Perfectos desconocidos",
    "Kilómetro 31",
    "Bajo la misma luna")

# Grafica:
min(pelis$year)
max(pelis$year)

color1 <- rgb(54,112,192,maxColorValue = 255) # Sub
color2 <- rgb(235,133,138,maxColorValue = 255)
color3 <- rgb(229,191,93,maxColorValue = 255)
# color4 <- rgb(0,0,0,maxColorValue = 255)
# color5 <- rgb(79,153,72,maxColorValue = 255) # verde
# color6 <- rgb(229,191,93,maxColorValue = 255) #

pelis %>%
  ggplot(aes(x = reorder(str_c(rank, ".- ", movie),
                         total_gross_mxn), y = total_gross_mxn, fill = year)
         ) +
  geom_col() +
  geom_text(aes(label = str_c("$", total_gross_mxn, "M")),
            hjust = 1.1,
            fontface = "bold",
            family = "Poppins",
            color = "white"
            ) +
  geom_text(aes(label = str_c("(", year, ")"),
                color = year),
            hjust = -0.2,
            fontface = "bold",
            family = "Poppins") +
  scale_y_continuous(expand = expansion(c(0,0.1))) +
  coord_flip() +
  # theme_pomological() +
  scale_fill_gradientn(colors = c(color3, color1, color2)) +
  scale_color_gradientn(colors = c(color3, color1, color2)) +
  labs(x = "", y = "Millones de pesos @ MXN y (Año de estreno)",
       fill = "Año de estreno: ",
       title = "Películas Mexicanas por recaudación de taquilla",
       subtitle = "Las siguientes son las películas más taquilleras dentro de la<br>industria cinematográfica en México.",
       caption = "Unidades monetarias en pesos mexicanos\nFuente: https://en.wikipedia.org/wiki/List_of_highest-grossing_Mexican_films\nPaleta de colores en función del año de estreno\n@JuvenalCamposF") +
  guides(fill = guide_colorbar(title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1,
                             barwidth = 30,
                             ticks.linewidth = 1.2,
                             barheight = 0.5)) +
  theme(axis.text.y = element_markdown(vjust = 0.5,
                                       color = color2,
                                       face = "bold",
                                       size = 12),
        panel.background = element_rect(fill = color4),
        plot.background = element_rect(fill = color4),
        text = element_text(family = "Poppins"),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 20,
                                  color = color2),
        plot.subtitle = element_markdown(size = 15,
                                     color = color1),
        plot.caption = element_text(color = color1,
                                    face = "bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        legend.position = "none")

size = 11
ggsave(filename = "grafica_cine_2.png",
       device = "png",
       width = size,
       height = size)
