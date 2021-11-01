# Librerias:
library(tidyverse)
library(sf)

# Bases de datos:
lineas <- read_sf("lineas_de_metro/lineas_de_metro.shp")
estaciones <- read_sf("estaciones_del_metro/estaciones_del_metro.shp")

?separate
fechas <- readxl::read_xlsx("fechas_lineas_metro.xlsx") %>%
  separate(col = Tramo,
           sep = " - ",
           into = c("Origen", "Destino")) %>%
  mutate(date = as.Date(str_c(Dia, mes, año, sep = "-"),
                        format = "%d-%m-%Y")) %>%
  arrange(date) %>%
  mutate(distancia = c(0, diff(date))) %>%
  mutate(linea_tiempo = cumsum(distancia))


sort(estaciones$name)
fechas$Origen[!(fechas$Origen %in% estaciones$name)]
fechas$Destino[!(fechas$Destino %in% estaciones$name)]



