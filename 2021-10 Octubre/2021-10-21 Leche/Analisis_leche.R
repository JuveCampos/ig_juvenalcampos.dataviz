# Librerias:
library(tidyverse)
library(sf)
library(geofacet)
library(ggtext)
library(cartogram)
library(spData)
# load("/Volumes/Extreme\ SSD/TRACE\ S.C.\ PROYECTOS/SEQUÍA/mexico_facet.R")

jenkifyer <- function(x, groups = 5, etiquetas = 1:5){

  library(BAMMtools)
  limits = BAMMtools::getJenksBreaks(x, groups + 1)

  ubicaciones = lapply(1:groups,
                       function(i){
                         # i = 2
                         which(between(x, limits[i], limits[i + 1]))
                         # which(x <= limits[i + 1] & x >= limits[i] )
                       })

  vector_nuevo = rep(NA, length(x))
  for(j in 1:groups){
    # j = 2
    vector_nuevo[ubicaciones[[j]]] <- etiquetas[j]
    # table(vector_nuevo)
  }
  # table(vector_nuevo)
  return(vector_nuevo)

}

# Grilla México
mx_grid <- data.frame(
  code = c(2L, 8L, 26L, 3L, 5L, 10L, 19L, 25L, 28L, 1L, 18L, 24L, 32L,
           11L, 13L, 14L, 22L, 30L, 6L, 15L, 29L, 9L, 17L, 21L, 31L, 4L,
           12L, 16L, 20L, 23L, 27L, 7L),
  name = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas"),
  name_official = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila de Zaragoza", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz de Ignacio de la Llave", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán de Ocampo", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas"),
  name_abbr = c("BC", "CHIH", "SON", "BCS", "COAH", "DGO", "NL", "SIN", "TAM", "AGS", "NAY", "SLP", "ZAC", "GTO", "HGO", "JAL", "QRO", "VER", "COL", "MEX", "TLAX", "CDMX", "MOR", "PUE", "YUC", "CAMP", "GRO", "MICH", "OAX", "QROO", "TAB", "CHPS"),
  name_abbr_iso = c("BCN", "CHH", "SON", "BCS", "COA", "DUR", "NLE", "SIN", "TAM", "AGU", "NAY", "SLP", "ZAC", "GUA", "HID", "JAL", "QUE", "VER", "COL", "MEX", "TLA", "CMX", "MOR", "PUE", "YUC", "CAM", "GRO", "MIC", "OAX", "ROO", "TAB", "CHP"),
  name_abbr_official = c("BC", "Chih.", "Son.", "BCS", "Coah.", "Dgo.", "NL", "Sin.", "Tamps.", "Ags.", "Nay.", "SLP", "Zac.", "Gto.", "Hgo.", "Jal.", "Qro.", "Ver.", "Col.", "Mex.", "Tlax.", "CDMX", "Mor.", "Pue.", "Yuc.", "Camp.", "Gro.", "Mich.", "Oax.", "Q. Roo", "Tab.", "Chis."),
  col = c(1, 3, 2, 1, 4, 3, 5, 2, 6, 5, 3, 6, 4, 4, 6, 3, 5, 7, 4, 5, 6, 5, 4, 6, 9, 8, 5, 4, 6, 9, 7, 7),
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8)
)

# Shape files:
shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>%
  select(-c(AREA, PERIMETER,  COV_, COV_ID))
ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# geofacet::grid_preview(mx_grid)

# Datos producción pecuaria:
root <- "../../DATASETS/SIAP - Estadísticas/bases_siap_pecuarias/"
files = str_c(root, list.files(root))
datos <- lapply(files, function(x){
  read_csv(x,
           locale = locale(encoding = "WINDOWS-1252"))
}) %>%
  do.call(rbind, .) %>%
  mutate(Nomestado = str_replace_all(Nomestado, c("Ciudad de México / D.F." = "Ciudad de México",
                                                  "Distrito Federal"  = "Ciudad de México")))

leche = datos %>%
  filter(Nomproducto == "Leche") %>%
  mutate(name = Nomestado)

unique(leche$Nomestado)[!(unique(leche$Nomestado) %in% mx_grid$name)]


# Por año (PAIS) ----
prod_anual_pais = leche %>%
  group_by(Anio) %>%
  summarise(Volumen = sum(Volumen))

# Tendencia alcista
prod_anual_pais %>%
  ggplot(aes(x = Anio, y = Volumen))  +
  geom_line()

# Por año (Entidades)
prod_anual_edos = leche %>%
  filter(Nomespecie == "Bovino") %>%
  mutate(CVE_EDO = case_when(str_length(Cveestado) == 1 ~ paste0("0", Cveestado),
                              str_length(Cveestado) == 2 ~ paste0("", Cveestado))) %>%
  group_by(Anio, name, CVE_EDO) %>%
  summarise(Volumen = sum(Volumen))

# Tendencia alcista
prod_anual_edos %>%
  ggplot(aes(x = Anio, y = Volumen))  +
  geom_line(color = "purple") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_geo(~name, grid = mx_grid, label = "name", scale = "free_y") +
  labs(subtitle = "Tendencias de la producción anual de leche, 2006-2021",
       title = "SIAP - Producción Pecuaria",
       x = "Año", y = "Miles de litros") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7),
        plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        legend.position = "none",
        text = element_text(family = "Poppins"))

# Dorling de leche:
unique(prod_anual_edos$name)[!(unique(prod_anual_edos$name) %in% ents$ENTIDAD)]

map_edos = left_join(ents, prod_anual_edos %>% filter(Anio == 2020))
dorling = cartogram_dorling(st_transform(na.omit(map_edos),
                                         2163), "Volumen") %>%
  mutate(grupo = jenkifyer(Volumen)) %>%
  mutate(pctje = round(100*(Volumen/sum(Volumen)), 1)) %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2]) %>%
  mutate(label = case_when(grupo == 5 | grupo == 4 | grupo == 3 ~ str_c(str_wrap(ENTIDAD, 10), "\n", pctje, "%"),
                           TRUE ~ ""))

colores = "BuPu"
titulo = "Aportaciones de producción de Leche, 2020"

dorling %>%
  ggplot() +
  geom_sf(aes(fill = factor(grupo))) +
  geom_text(aes(x = X, y = Y,label = label),
            size = 2, color = "white") +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, colores)) +
  labs(subtitle= titulo,
       title = "SIAP - Producción Pecuaria", x = "", y = ""
       # ,
       # fill = "Grupo (1: menos - 5: más)"
       ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        legend.position = "none",
        text = element_text(family = "Poppins")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))


# Regionalizacion:
leche_munis <- leche %>%
  filter(Anio == 2020) %>%
  filter(Nomespecie == "Bovino") %>%
  mutate(Idestado = case_when(str_length(Cveestado) == 1 ~ paste0("0", Cveestado),
                              str_length(Cveestado) == 2 ~ paste0("", Cveestado))) %>%
  mutate(Idmunicipio = case_when(str_length(Cvempio) == 1 ~ str_c("00", Cvempio),
                                 str_length(Cvempio) == 2 ~ str_c("0", Cvempio),
                                 str_length(Cvempio) == 3 ~ str_c("", Cvempio))) %>%
  mutate(CVEGEO = str_c(Idestado,Idmunicipio))

mapa <- left_join(shp, leche_munis, by = "CVEGEO")
breaks <- BAMMtools::getJenksBreaks(var = mapa$Volumen,k = 7)
fm <- function(x) prettyNum(round(x, 2), big.mark = ",")
jenks = case_when(between(mapa$Volumen, breaks[1], breaks[2]) ~ str_c("Entre ", fm(breaks[1]), " y ", fm(breaks[2]), " miles de litros"),
                  between(mapa$Volumen, breaks[2], breaks[3]) ~ str_c("Entre ", fm(breaks[2]), " y ", fm(breaks[3]), " miles de litros"),
                  between(mapa$Volumen, breaks[3], breaks[4]) ~ str_c("Entre ", fm(breaks[3]), " y ", fm(breaks[4]), " miles de litros"),
                  between(mapa$Volumen, breaks[4], breaks[5]) ~ str_c("Entre ", fm(breaks[4]), " y ", fm(breaks[5]), " miles de litros"),
                  between(mapa$Volumen, breaks[5], breaks[6]) ~ str_c("Entre ", fm(breaks[5]), " y ", fm(breaks[6]), " miles de litros"),
                  between(mapa$Volumen, breaks[6], breaks[7]) ~ str_c("Entre ", fm(breaks[6]), " y ", fm(breaks[7]), " miles de litros"),
                  is.na(mapa$Volumen) ~ "Sin producción")
sort(unique(jenks))
mapa$quiebres <- factor(jenks, levels = c("Entre 0.04 y 17,408.8 miles de litros"  ,
                                          "Entre 17,408.8 y 72,265.9 miles de litros",
                                          "Entre 72,265.9 y 175,457.2 miles de litros",
                                          "Entre 175,457.2 y 398,554 miles de litros" ,
                                          "Entre 398,554 y 598,319 miles de litros" ,
                                          "Entre 598,319 y 863,747 miles de litros",
                                          "Sin producción"))
cols <- c("#d1a4ed", wesanderson::wes_palettes$Zissou1, "gray50")

mapa %>%
  ggplot(aes(fill = quiebres)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = ents,
          fill = NA,
          color = "white",
          size = 1) +
  scale_fill_manual(values = cols) +
  theme_bw() +
  labs(title = "SIAP - Producción Pecuaria",
       subtitle = "Municipios por rangos de volumen de producción<br>Leche - Miles de litros",
       x = "",
       y = "",
       caption = "<b style = 'color = #00ac7f;'>Fuente: </b> SIAP - Secretaría de Agricultura y Desarrollo Rural. Datos de producción pecuaria, 2020.",
       fill = "Rangos de producción") +
  theme(text = element_text(family = "Poppins",
                            face = "bold"),
        panel.border = element_blank(),
        plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        # plot.subtitle.position = "plot",
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        # legend.position = "bottom",
        plot.caption = element_markdown(hjust = 0,
                                        color = "gray20",
                                        size = 10,
                                        face = "italic")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5, ncol = 1))






