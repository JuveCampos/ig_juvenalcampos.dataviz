# Librerias:
library(tidyverse)
library(rvest)
library(rebus)
library(leaflet)
library(sf)
library(ggimage)
library(ggmap)
library(cowplot)
library(ggpomological)


# Url de los datos:
url <- "https://www.meteored.mx"

# Datos:
coords_ciudades <- readxl::read_xlsx("coords_ciudades.xlsx")
pokemon <- readxl::read_xlsx("coords_ciudades.xlsx", sheet = 2)
edos <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
# st_crs(edos) <- NA
# bbox <- c(left = -117.12642, bottom = 14.53401, right = -86.74038, top = 32.71877)
# site_map = ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 5))+
#   theme_bw() +
#   labs(x = "Longitude", y = "Latitude")

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

unique(descripcion_2$V1)

# BD:
bd <- cbind(lugar, descripcion_2) %>%
  rename(descripcion = V1)

bd_shp <- bd %>%
  left_join(pokemon, by = c("descripcion" = "Tiempo")) %>%
  left_join(coords_ciudades) %>%
  st_as_sf(coords = c("lon", "lat"))

# Imgs
bd_total <- bd %>%
  left_join(coords_ciudades) %>%
  left_join(pokemon, by = c("descripcion" = "Tiempo"))

# Mapa general:
(m1 <- ggplot() +
  geom_sf(data = edos,
          fill = "#ffe9b3") +
  geom_rect(aes(xmin = -106,
                  xmax = -95,
                  ymin = 18,
                  ymax = 24),
              fill = NA,
              color = "red") +
  theme_void() +
  theme(panel.background = element_rect(fill = "skyblue3",
                                        color = "skyblue3"),
  plot.background = element_rect(fill = "skyblue3")
  )
)


m2 <- m1 +
  geom_image(data = bd_total,
             aes(image = Pokemon,
                 x = lon,
                 y = lat),
             size = 0.06) +
  labs(title = "title",
                               subtitle = "subtitle",
                               caption = "caption")

m2


# Mapa extra:
m1 +
  coord_sf(
    xlim = c(-106, -95),
    ylim = c(18, 24),
    expand = FALSE) +
  geom_image(data = bd_total,
             aes(image = Pokemon,
                 x = lon,
                 y = lat),
             size = 0.10) +
  theme_void() +
  theme(panel.background = element_rect(fill = "skyblue3",
                                        color = "skyblue3"),
        plot.background = element_rect(fill = "skyblue3"))

# Leyenda como grafica
pokemon %>%
  ggplot(aes(x = Tiempo, y = 1)) +
  geom_image(aes(image = Pokemon),
             size = 0.1, by = "width")

# Fuente: https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/


m2 %>%
  cowplot::ggdraw() +
  draw_plot(
    {
      m1 +
        coord_sf(
          xlim = c(-106, -95),
          ylim = c(18, 24),
          expand = FALSE) +
        geom_image(data = bd_total,
                   aes(image = Pokemon,
                       x = lon,
                       y = lat),
                   size = 0.10) +
        theme_void() +
        theme(panel.background = element_rect(fill = "skyblue3",
                                              color = "skyblue3"),
              plot.background = element_rect(fill = "skyblue3"))

    },
    x = 0.6,
    y = 0.55,
    width = 0.4,
    height = 0.4
  )

