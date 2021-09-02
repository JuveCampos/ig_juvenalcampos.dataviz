# Librerias: ----
library(tidyverse)
library(ggimage)
library(magick)

# Datos: ----
verde = "#265B40"
rojo = "#B52B27"
blanco = "white"
escudo = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Coat_of_arms_of_Mexico.svg/1920px-Coat_of_arms_of_Mexico.svg.png"
rectangulos <- tibble(x = c(1,2,3),
                y = c(2,2,2),
                relleno = c(rojo, blanco, verde))
# Grafica ----
ggplot(rectangulos, aes(x,y)) +
  geom_tile(aes(fill = relleno), height = 0.6) +
  geom_image(aes(image = escudo, x = 2, y = 2), size = 0.2) +
  scale_fill_manual(values = c(rojo,verde,blanco)) +
  ylim(1.5, 2.5) +
  theme_void() +
  theme(legend.position = "none")

# Creación del GIF ----
root <- "imgs_pa_gif/"
str_c(root, list.files(root)) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp.gif") # Escribe el gif en el directorio.

