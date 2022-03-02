# Librerias: 
library(tidyverse)
library(rtweet)
library(leaflet)
library(ggimage)
library(ggtext)
library(lubridate)
library(magick)
library(jpeg)

# Datos: 
termino <- "mexico truly magical"
# bd <- rtweet::search_tweets(q = termino,
#                       n = 200000,
#                       include_rts = FALSE,
#                       retryonratelimit = TRUE)
# 
# openxlsx::write.xlsx(bd, "mexico_magical.xlsx")

imagenes <- unique(bd$media_url) %>% unlist()

# Bucle de descarga: 
map(.x = seq_along(imagenes), 
    .f = function(x){
      tryCatch({curl::curl_download(imagenes[x], 
                                    destfile = str_c("imagenes/", 
                                                     x,
                                                     ".jpg"))},
               error = function(e){
                 print(str_c("Error en ", x))
               })
    })


# EN ESTA PARTE HICE UNA REVISIÓN MANUAL DE LAS IMÁGENES QUE NO FUERA APROPIADAS -

# Archivos: 
imagenes_magical <- str_c("imagenes/", list.files("imagenes/"))
dimensiones <- lapply(seq_along(imagenes_magical),
       function(x){
         tryCatch({readJPEG(imagenes_magical[x]) %>% 
             dim() %>% 
             append(imagenes_magical[x])}, 
                  error = function(e){
                    print(str_c("error en", x))
                  })
         }) %>% 
  do.call(rbind.data.frame, .) 

names(dimensiones) <- c("ancho", "alto", "canales", "archivo")

imagenes <- dimensiones %>% 
  filter(!str_detect(ancho, "error")) %>% 
  mutate(relacion = as.numeric(ancho) / as.numeric(alto)) %>% 
  filter(between(relacion, 0.9, 1.1))
# %>% 
#   as_tibble() %>% 
#   filter(as.numeric(alto) < 800)


# Muestra de 50 imagenes para armar el gif: 
imagenes_gif <- imagenes$archivo
i = imagenes_magical[1]
for (i in imagenes_magical) {
  tryCatch({
  img <- readJPEG(i)
  jpeg(paste0("imagenes_2", 
              i %>% 
                str_remove(pattern = "imagenes")),
       width=900, height=900)
  plot(as.raster(img))
  dev.off()
  }, error = function(e){
    str_c("error en ", imagenes_magical[i])
  })
}

# CREACIÓN DEL GIF.
str_c("imagenes_2/", list.files("imagenes_2/")) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=5) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("magical.gif") # Escribe el gif en el directorio.

