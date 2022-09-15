# Librerias: ----
library(tidyverse)
library(sf)

# Datos: ----
munis <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>% 
  select(cve_geo = CVEGEO)

# Matríz de distancias: 
distancias <- st_distance(munis) %>% 
  as.matrix() %>% 
  as_tibble() 

names(distancias) <- bd$cve_geo

distancias <- distancias %>% 
  mutate(from = bd$cve_geo) %>% 
  relocate("from", .before = 1) %>% 
  pivot_longer(cols = 2:ncol(.), 
               names_to = "to", 
               values_to = "distancia")
saveRDS(distancias, "distancias.rds")

