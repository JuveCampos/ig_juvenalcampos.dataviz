# Librerias:
library(tidyverse)
library(rvest)
library(rebus)
library(leaflet)
library(sf)
library(ggimage)
library(ggmap)


# Url de los datos:
url <- "https://www.meteored.mx"

# Datos:
coords_ciudades <- readxl::read_xlsx("coords_ciudades.xlsx")
pokemon <- readxl::read_xlsx("coords_ciudades.xlsx", sheet = 2)
edos <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
st_crs(edos) <- NA
bbox <- c(left = -117.12642, bottom = 14.53401, right = -86.74038, top = 32.71877)
site_map = ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 5))+
  theme_bw() +
  labs(x = "Longitude", y = "Latitude")

# Escrapeo:
html <- read_html(url)

# Otros enlaces:
enlaces_locales <- html %>%
  html_nodes("#mapa") %>%
  html_nodes("a") %>%
  html_attr("href")

# Lugares:
lugar <- enlaces_locales %>%
  str_extract(pattern = "clima_[\\w+\\+]+") %>%
  str_replace_all(pattern = "\\+", replacement = " ") %>%
  str_remove(pattern = "clima_")

# Lugares
lugar

# Descripción del clima:
descripcion <- lapply(enlaces_locales, function(x){
  html_local <- x %>%
    read_html()
  redaccion <- html_local %>%
    html_nodes(".zona-color") %>%
    html_nodes(".descripcion") %>%
    html_text() %>%
    str_squish()
  return(redaccion)
})

descripcion_2 <- descripcion %>%
  do.call(rbind, .) %>%
  as_tibble()

# Categorias:
# unique(descripcion_2)

# BD:
bd <- cbind(lugar, descripcion_2) %>%
  rename(descripcion = V1)

bd_shp <- bd_total %>%
  left_join(pokemon, by = c("descripcion" = "Tiempo")) %>%
  st_as_sf(coords = c("lon", "lat")
           # , crs = 4326
           )

# Imgs
bd_total <- bd %>%
  left_join(coords_ciudades) %>%
  left_join(pokemon, by = c("descripcion" = "Tiempo"))


ph_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background', source = 'stamen')
ggmap(ph_basemap)


# site_map
site_map +
  # geom_sf(data = edos,
  #         fill = "#ffe9b3") +
  geom_image(data = bd_total,
             aes(image = Pokemon,
                 x = lon,
                 y = lat),
             size = 0.08)

