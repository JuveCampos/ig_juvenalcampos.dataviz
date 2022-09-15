# Librerias: ----
library(tidyverse)
library(sf)
library(wesanderson)
library(magick)

# Cargamos Municipios: ----
munis <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>% 
  select(cve_geo = CVEGEO)

# Cargamos distancias: 
distancias <- readRDS("distancias.rds")
head(distancias)

edos_ind <- munis %>% 
  mutate(edo = ifelse(cve_geo == muni_primario, 
                      yes = "Muni primario", 
                      no = str_extract(cve_geo, pattern = "^\\d\\d"))) %>% 
  pull(edo) %>% 
  unique()

scales::show_col(wes_palettes$GrandBudapest2)
scales::show_col(wes_palettes$Royal1)

colores_ind <- c("gold", 
                rep(c(wes_palettes$GrandBudapest2, 
                      wes_palettes$Royal1), 3))[1:33]

pal_colores = colores_ind
names(pal_colores) <- edos_ind

# Clave de municipio: 
# j = 10
muni_primario <- "01001"
gen_crust_municipio <- function(muni_primario){
    
  dir.create(str_c("03_Visualizaciones/", muni_primario))
  
  dist_01001 <- distancias %>% 
      filter(from == muni_primario) %>% 
      filter(to != muni_primario) %>% 
      arrange(distancia) 
    
    to <- pull(dist_01001, to)
    
    # length(to)
    
    lapply(1:49, function(j){
    
      munis_sel <- c(muni_primario, to[1:j])  
        
      dato_mapa <- munis %>% 
        filter(cve_geo %in% munis_sel) %>% 
        mutate(edo = ifelse(cve_geo == muni_primario, 
                            yes = "Muni primario", 
                            no = str_extract(cve_geo, pattern = "^\\d\\d"))) 
      dato_mapa %>% 
        ggplot(aes(fill = edo)) + 
        geom_sf() + 
        scale_fill_manual(values = pal_colores) + 
        theme_void() + 
        theme(legend.position = "none", 
              panel.background = element_rect(fill = "black"), 
              plot.background = element_rect(fill = "black"))
      
      ggsave(str_c("03_Visualizaciones/", muni_primario, "/", j, ".png"), 
             device = "png")
    })
    
    lapply(seq(50, 2000, by = 100), function(j){
      
      munis_sel <- c(muni_primario, to[1:j])  
      
      dato_mapa <- munis %>% 
        filter(cve_geo %in% munis_sel) %>% 
        mutate(edo = ifelse(cve_geo == muni_primario, 
                            yes = "Muni primario", 
                            no = str_extract(cve_geo, pattern = "^\\d\\d"))) 
      dato_mapa %>% 
        ggplot(aes(fill = edo)) + 
        geom_sf() + 
        scale_fill_manual(values = pal_colores) + 
        theme_void() + 
        theme(legend.position = "none", 
              panel.background = element_rect(fill = "black"), 
              plot.background = element_rect(fill = "black"))
      
      ggsave(str_c("03_Visualizaciones/", muni_primario, "/", j, ".png"), 
             device = "png")
    })
    
    lapply(seq(2001, length(to), by = 50), function(j){
      
      munis_sel <- c(muni_primario, to[1:j])  
      
      dato_mapa <- munis %>% 
        filter(cve_geo %in% munis_sel) %>% 
        mutate(edo = ifelse(cve_geo == muni_primario, 
                            yes = "Muni primario", 
                            no = str_extract(cve_geo, pattern = "^\\d\\d"))) 
      dato_mapa %>% 
        ggplot(aes(fill = edo)) + 
        geom_sf() + 
        scale_fill_manual(values = pal_colores) + 
        theme_void() + 
        theme(legend.position = "none", 
              panel.background = element_rect(fill = "black"), 
              plot.background = element_rect(fill = "black"))
      
      ggsave(str_c("03_Visualizaciones/", muni_primario, "/", j, ".png"), 
             device = "png")
    })
    
    munis_sel <- c(muni_primario, to[1:length(to)])  
    
    dato_mapa <- munis %>% 
      filter(cve_geo %in% munis_sel) %>% 
      mutate(edo = ifelse(cve_geo == muni_primario, 
                          yes = "Muni primario", 
                          no = str_extract(cve_geo, pattern = "^\\d\\d"))) 
    dato_mapa %>% 
      ggplot(aes(fill = edo)) + 
      geom_sf() + 
      scale_fill_manual(values = pal_colores) + 
      theme_void() + 
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "black"), 
            plot.background = element_rect(fill = "black"))
    
    ggsave(str_c("03_Visualizaciones/", muni_primario, "/", length(to), ".png"), 
           device = "png")
}

gen_crust_municipio("01001")

# CREACIÓN DEL GIF.
dir <- "03_Visualizaciones/01001/"
files <- str_c(dir,list.files(dir)) 

tibble(files) %>% 
  mutate(dgt = str_extract(files, "\\d+.png") %>% 
           str_remove("\\.png") %>% 
           as.numeric()) %>% 
  arrange(dgt) %>% 
  pull(files) %>% 
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=4) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("gen.gif") # Escribe el gif en el directorio.
