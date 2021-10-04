# Librerias ----
library(tidyverse)
library(magick)

# Archivos desde la carpeta de imagenes ----
root <- "imgs/" # Carpeta donde estan mis imagenes
files <- str_c(root, list.files(root)) # Obtengo las direcciones de los archivos

# Generamos el gif ----
# CREACIÓN DEL GIF.
files %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp_solar.gif") # Escribe el gif en el directorio.

