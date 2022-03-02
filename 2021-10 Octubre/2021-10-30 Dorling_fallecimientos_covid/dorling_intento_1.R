# Dorling ----

# Librerias: ----
library(tidyverse)
library(sf)
library(cartogram)
library(ggtext)

# Funciones propias -------------------------------------------------------

# 2. Funciones propias:
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

# Datos -------------------------------------------------------------------
# bd <- read_csv("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Defunciones_20211029.csv")
# write_csv(bd, "defunciones_covid.csv")
bd <- read_csv("defunciones_covid.csv")


# Hacemos pivot_longer a nuestros datos:
datos = bd %>%
  pivot_longer(cols = 4:ncol(.),
               names_to = "fecha",
               values_to = "defunciones") %>%
  mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

# Datos para mapa:
datos_munis = datos %>%
  group_by(cve_ent, nombre) %>%
  summarise(poblacion = last(poblacion),
            defunciones = sum(defunciones, na.rm = T)) %>%
  mutate(defunciones_por_1000 = defunciones/(poblacion/1000)) %>%
  rename(CVEGEO = cve_ent) %>%
  ungroup()

# Leemos los polígonos:
munis <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

# Juntamos los datos en el mapa:
mapa <- left_join(munis, datos_munis)

# Generamos grupos a través de cortes naturales de jenks
mapa$grupo = jenkifyer(x = mapa$defunciones,
                       groups = 6,
                       etiquetas = 1:6)

# Dorlingeamos: ----

# 1: Seleccionamos la variable para generar las bolitas:
var = "defunciones"
unidades = ""

# 2. Generamos el shape de las bolitas (con su respectivo acomodo):
dorling = cartogram_dorling(st_transform(na.omit(mapa),
                                         2163), var)

# 3. Etiquetas:
dorling$label = ifelse(dorling$grupo >= 4,
                       yes = str_c(str_wrap(dorling$NOM_MUN, 10), "\n",
                                   prettyNum(round(dorling[,var] %>%
                                                     pull(1), 2), big.mark = ","),
                                   unidades),
                       no = "")

#4. Centroides:
dorling = dorling %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])


# 3. Probamos las esferas:
dorling %>%
  ggplot(aes(fill = factor(grupo))) +
  geom_sf() +
  geom_text(aes(x = X, y = Y, label = label),
            size = 2, color = "white", family = "Poppins") +
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, "YlOrRd")) +
  theme_bw() +
  labs(title = "Mapa de burbujas",
       subtitle = "Fallecimientos totales por COVID-19 al 29 de Octubre del 2021,<br>por municipio",
       caption = "<b>Fuente: </b>Elaboración propia con datos del Tablero COVID-CONACYT.<br>Datos de defunciones a nivel municipal.",
       x = "", y = "") +
  theme(axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        legend.position = "none",
        plot.caption = element_markdown(size = 12),
        plot.subtitle = element_markdown(hjust = 0.5, size = 20),
        text = element_text(family = "EB Garamond"))

ggsave("dorling_fallecimientos_covid_19.png",
       device = "png",
       height = 13.50/1.4,
       width = 10.80/1.4)

