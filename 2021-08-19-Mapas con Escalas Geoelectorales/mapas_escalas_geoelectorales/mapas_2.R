# Funciones:

# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)

# Datos ----
bd <- readxl::read_xlsx("datos_wide.xlsx")
geo <- readRDS("/Users/juvenalcampos/Desktop/Datos\ Electorales/09_ShinyApps/Secciones/www/bases_de_datos/shape.rds")
meta <- read_csv("eceg_2020_csv/diccionario_de_datos/Descriptor_indicadores_ECEG_Seccion_2020.csv",
                 locale = locale(encoding = "WINDOWS-1252"))

# Datos solo de bicicletas :
geo_df <- geo %>%
  filter(entidad == "09") %>%
  mutate(entidad = as.numeric(entidad))

bd_df <- bd %>%
  filter(entidad == 9) %>%
  select(entidad, seccion, nom_mun, NOMBRE_DISTRITO, VPH_BICI)


map_df <- left_join(geo_df, bd_df, by = c("entidad" = "entidad",
                                          "seccion" =  "seccion",
                                          "nom_mun"))


map_df <- map_df %>%
  mutate(valor =   case_when(between(VPH_BICI, 0,78) ~"0 a 78",
                             between(VPH_BICI, 78,154) ~"78 a 154",
                             between(VPH_BICI, 154,317) ~"154 a 317",
                             between(VPH_BICI, 317,757) ~ "317 a 757",
                             between(VPH_BICI, 757,1479) ~ "757 a 1479"
  )) %>%
  mutate(valor = factor(valor, levels = c("0 a 78",
                                          "78 a 154",
                                          "154 a 317",
                                          "317 a 757",
                                          "757 a 1479")))

# unique(map_df$valor) %>% writeLines()

# plt <- map_df %>%
#   ggplot(aes(x =VPH_BICI )) +
#   geom_boxplot()

# min(map_df$VPH_BICI)


pal_colores <- colorFactor(domain = map_df$valor,
                            palette = rev(c("#d66929", "#f3bb71",
                                            "#f8f7f7", "#b2accf",
                                            "#593e95")))

# scales::show_col("#593e95")

#  %>%
map_df %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = pal_colores(map_df$valor),
              fillOpacity = 1,
              color = "white",
              weight = 0.1) %>%
  addLegend(pal = pal_colores,
            values = map_df$valor,
            position = "topright",
            title = "Viviendas particulares habitadas<br>que disponene de bicicleta como<br>medio de transporte")

