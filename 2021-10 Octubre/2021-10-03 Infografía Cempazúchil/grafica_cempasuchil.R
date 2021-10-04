# Análisis DENUE por cultivo ----
options(scipen = 999)
library(tidyverse)
library(sf)
library(ggtext)
library(ggimage)

# PARA REPLICAR ESTE CÓDIGO. REQUIERES DESCARGAR LOS DATOS DEL SIAP DE PRODUCCIÓN AGRÍCOLA, Y TAMBIÉN GUARDARLOS EN UNA UBICACIÓN SIMILAR A LA QUE SE PRESENTA EN EL CÓDIGO A CONTINUACIÓN:
datos <- readRDS("01_Datos/bases_siap_agricultura/2019.rds") %>%
  mutate(Idestado = case_when(str_length(Idestado) == 1 ~ paste0("0", Idestado),
                              str_length(Idestado) == 2 ~ paste0("", Idestado),
                              )) %>%
  mutate(Idmunicipio = case_when(str_length(Idmunicipio) == 1 ~ str_c("00", Idmunicipio),
                                 str_length(Idmunicipio) == 2 ~ str_c("0", Idmunicipio),
                                 str_length(Idmunicipio) == 3 ~ str_c("", Idmunicipio))) %>%
  mutate(CVE_GEO = str_c(Idestado,Idmunicipio))

shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")


cultivos <- sort(unique(datos$Nomcultivo))
cultivo = cultivos[293]

  datos_muni <- datos %>%
    group_by(CVE_GEO, Nommunicipio) %>%
    filter(Nomcultivo == cultivo) %>%
    summarise(valor_produccion = sum(Valorproduccion, na.rm = T),
              total_produccion = sum(Volumenproduccion, na.rm = T)) %>%
    ungroup()

  mapa <- left_join(shp, datos_muni, by = c("CVEGEO" = "CVE_GEO"))

  mapa <- mapa %>%
    mutate(muni_prod = ifelse(!is.na(valor_produccion),
                              yes = T, no = F))

  munis <- mapa %>%
    filter(muni_prod)

  unique(mapa$NOM_ENT)

  mapa %>%
    filter(NOM_ENT == "Ciudad de México") %>%
    ggplot(aes(fill = muni_prod)) +
    geom_sf(color = "white", size = 2) +
    scale_fill_manual(values = c("transparent", "#712407")) +
    theme_void() +
    theme(legend.position = "none")

  ggsave(filename = "03_resultados/Infografía Cempazúchil/Mapa_cdmx.png",
         device = "png", width = 10, height = 10)


  mapa %>%
    filter(NOM_ENT == "México") %>%
    ggplot(aes(fill = muni_prod)) +
    geom_sf(color = "white", size = 2) +
    scale_fill_manual(values = c("transparent", "#712407")) +
    theme_void() +
    theme(legend.position = "none")

  ggsave(filename = "03_resultados/Infografía Cempazúchil/Mapa_edo_mex.png",
         device = "png", width = 10, height = 10)

  mapa %>%
    filter(NOM_ENT == "Michoacán de Ocampo") %>%
    ggplot(aes(fill = muni_prod)) +
    geom_sf(color = "white", size = 2) +
    scale_fill_manual(values = c("transparent", "#D3A835")) +
    theme_void() +
    theme(legend.position = "none")

  ggsave(filename = "03_resultados/Infografía Cempazúchil/Mapa_mich.png",
         device = "png", width = 10, height = 10)




  datos_plot <- mapa %>%
    as_tibble() %>%
    filter(!is.na(total_produccion))

  datos_plot %>%
    filter(NOM_ENT == "México") %>%
    select(NOM_MUN, total_produccion) %>%
    mutate(pp = total_produccion/sum(total_produccion))


  datos_plot_2 <- datos_plot %>%
    group_by(NOM_ENT) %>%
    summarise(total_produccion = sum(total_produccion, na.rm = T)) %>%
    ungroup() %>%
    mutate(cat_edo = ifelse(NOM_ENT %in% c("Ciudad de México",
                                           "México",
                                           "Michoacán de Ocampo"),
                            yes = NOM_ENT,
                            no = "Otros")) %>%
    group_by(cat_edo) %>%
    summarise(total_produccion = sum(total_produccion)) %>%
    ungroup() %>%
    mutate(pp = 100*(total_produccion/sum(total_produccion)))


  datos_plot %>%
    group_by(NOM_ENT) %>%
    summarise(total_produccion = sum(total_produccion, na.rm = T)) %>%
    mutate(pp = 100*(total_produccion/sum(total_produccion))) %>%
    arrange(-pp)

  datos_plot_2

  colores <- c("#D06600" ,
               "#D5B545",
               "#882D0E",
               "#261711")

  scales::show_col(colores)

  datos_plot_2 %>%
    ggplot(aes(fill = cat_edo,
               y = pp,
               x = 1)) +
    geom_col(color = "white")  +
    scale_fill_manual(values = colores) +
    coord_polar(theta = "y", start = 90/57.2958) +
    xlim(c(-1, 2)) +
    theme_void() +
    theme(legend.position = "none")

  ggsave(filename = "03_resultados/Infografía Cempazúchil/grafica_dona.png",
         device = "png",
         width = 10,
         height = 10)

