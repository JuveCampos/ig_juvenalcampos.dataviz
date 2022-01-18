# Librerias:
library(sf)
library(tidyverse)
library(leaflet)
library(readxl)
library(ggtext)
library(magick)

# # Datos:
# ent = 17

# Loop:
lapply(c(str_c("0", 1:9), 11:32), function(ent){

  ent = 32
  agebs <- readRDS("../DATASETS/INEGI - MARCO GEOESTADÍSTICO 2021/Productos/todas_agebs_4326.rds") %>%
    filter(CVE_ENT %in% c(ent))
  estrato <- read_excel("../DATASETS/CONAPO - Indice de marginación Urbana/IMU_2020.xls",
                        skip = 4)  %>%
    filter(ENT %in% c(ent))
  mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>%
    filter(CVE_ENT %in% c(ent))
  ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>%
    filter(CVE_EDO %in% c(ent))

  map <- left_join(agebs, estrato, by = c("CVEGEO"="CVE_AGEB")) %>%
    mutate(grado = factor(GM_2020, levels = c("Muy bajo","Bajo", "Medio",  "Alto", "Muy alto")))
  table(is.na(map$GM_2020))
  unique(map$GM_2020)

  map %>%
    ggplot() +
    geom_sf(data = ents, size = 1) +
    geom_sf(data = mpios, size = 0.3, color = "gray50") +
    geom_sf(aes(fill = grado), size = 0.1) +
    scale_fill_manual(values = rev(c("#e4322b","#f4b44c","#c7cd7c","#3f7123", "blue"))) +
    labs(fill = "Grado de Marginación",
         subtitle = "Grado de Marginación de CONAPO\ncon datos del Censo, 2020",
         title = str_c("Entidad: ", first(map$NOM_ENT))) +
    theme(text = element_text(family = "Poppins",
                              face = "bold"),

          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
          plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
          plot.caption.position = "plot",
          plot.title.position = "plot",
          # plot.subtitle.position = "plot",
          axis.ticks = element_blank(),
          panel.grid =  element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          plot.caption = element_markdown(hjust = 0,
                                          color = "gray20",
                                          size = 10,
                                          face = "italic")) +
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5, ncol = 6))


  ggsave(filename = str_c("graficas/marginacion_", na.omit(unique(map$NOM_ENT)), ".png"),
         device = "png",
         width = 10,
         height = 10)

})


# CREACIÓN DEL GIF.
root <- "graficas/"
str_c(root, list.files(root)) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp.gif") # Escribe el gif en el directorio.

# acdmx <- agebs %>%
#   filter(CVE_ENT == "09")
#
# ecdmx <- estrato %>%
#   filter(ENT == "09")
#
# # table(is.na(ecdmx$GM_2020))
#
# table(acdmx$CVEGEO %in% ecdmx$CVE_AGEB)
#
#
# unique(estrato$GM_2020)
#
# # Juntamos los datos:
# map <- left_join(agebs, estrato, by = c("CVEGEO" = "CVE_AGEB"))
# # %>%
# #   # select(CVEGEO, NOM_ENT, NOM_MUN, GM_2020) %>%
# #   mutate(GM_2020 = factor(GM_2020, levels = c("Muy bajo",
# #                                               "Bajo",
# #                                               "Medio" ,
# #                                               "Alto",
# #                                               "Muy alto")))
#
# map_cdmx <- map %>%
#   filter(NOM_ENT == "Ciudad de México")
#
# table(is.na(map_cdmx$GM_2020))
#
#
# unique(map$GM_2020)
#
# # "#3f7123", # Alto
# # "#c7cd7c", # Medio
# # "#f4b44c", # Bajo
# # "#e4322b", # Muy bajo
#
#
# scales::show_col(c("#e4322b","#f4b44c","#c7cd7c","#3f7123"))
#
# pal_colores <- colorFactor(palette = rev(c("#e4322b","#f4b44c","#c7cd7c","#3f7123", "blue")),
#                            domain = map$GM_2020)
#
# pal_colores2 <- colorFactor(palette = "magma",
#                             domain = map$GM_2020)
#
#
# scales::show_col(pal_colores(c("Muy bajo",
#                                "Bajo",
#                                "Medio" ,
#                                "Alto",
#                                "Muy alto")))
#
# # Mapa:
# map %>%
#   filter(NOM_ENT == "Ciudad de México") %>%
#   leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
  # addPolygons(fillColor = pal_colores2(map$GM_2020),
  #             color = "white",
  #             label = map$GM_2020,
  #             fillOpacity = 0.9,
  #             weight = 0.1,
  #             opacity = 0.9) %>%
#   addLegend(position = "bottomright",
#             title = "Grado de Marginación<br>CONAPO, 2020",
#             pal = pal_colores2,
#             values = map$GM_2020)
#
#
# # analisis <- cbind(pal_colores2(map$GM_2020), map$GM_2020) %>% as_tibble()
# # table(is.na(analisis$V2))
#


