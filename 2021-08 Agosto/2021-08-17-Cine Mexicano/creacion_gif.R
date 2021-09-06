
library(tidyverse)
library(magick)

# CREACIÓN DEL GIF.
root <- "gif_making_of/"
str_c(root,
      list.files(root)) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp.gif") # Escribe el gif en el directorio.
