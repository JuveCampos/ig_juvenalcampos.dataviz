# Librerias:
library(tidyverse)
library(sf)

# Datos: (SECCION)
# rm(bd)
elec <- readRDS("/Users/juvenalcampos/Desktop/Datos\ Electorales/09_ShinyApps/Secciones/www/bases_de_datos/datos_distrito.rds")

diputaciones <- read_delim("Resultados Electorales/20210611_1000_CW_diputaciones/diputaciones.csv",
                              "|", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
                              trim_ws = TRUE, skip = 5)
# 100*400/68806
# xx <- diputaciones %>%
#   select(NOMBRE_ESTADO, ID_DISTRITO, SECCION) %>%
#   unique() %>%
#   group_by(NOMBRE_ESTADO, ID_DISTRITO) %>%
#   count(SECCION) %>%
#   arrange(-n)
# rm(xx)
# table(xx$n != 1)

egeo <- read_csv("eceg_2020_csv/conjunto_de_datos/INE_SECCION_2020.csv")
meta <- read_csv("eceg_2020_csv/diccionario_de_datos/Descriptor_indicadores_ECEG_Seccion_2020.csv",
                 locale = locale(encoding = "WINDOWS-1252"))

# Juntando:
datos <- left_join(elec,egeo,
          by = c("entidad" = "ENTIDAD", "seccion" = "SECCION", "DISTRITO"))

# table(is.na(datos$NOMBRE_DISTRITO))
openxlsx::write.xlsx(datos, "datos_wide.xlsx")

# # Datos Long:
# id <- 1:4
#
# txt <- lapply(datos[,-id], is.character) %>%
#   unlist() %>%
#   which() %>%
#   names()
# num <- lapply(datos[,-id], function(x){
#   is.numeric(x)
# }) %>%
#   unlist() %>%
#   which() %>%
#   names()
#
# datos_txt <- datos %>%
#   select(c(id, txt)) %>%
#   pivot_longer(cols = txt,
#                names_to = "Variable",
#                values_to = "Valores") %>%
#   arrange() %>%
#   arrange(Variable, entidad, seccion)
#
#
# datos_num <- datos %>%
#   select(c(id, num)) %>%
#   pivot_longer(cols = num,
#                names_to = "Variable",
#                values_to = "Valores") %>%
#   arrange() %>%
#   arrange(Variable, entidad, seccion)
#
# # Guardamos datos:
# openxlsx::write.xlsx(list(datos_txt, datos_num),
#                      "datos_long.xlsx")


elec_xx <- diputaciones %>%
  select(ENTIDAD = ID_ESTADO, DISTRITO = ID_DISTRITO, SECCION) %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  unique() %>%
  group_by(ENTIDAD, DISTRITO) %>%
  count(SECCION) %>%
  arrange(-n) %>%
  # rename(ENTIDAD = entidad,
  #        SECCION = seccion) %>%
  mutate(id = str_c(ENTIDAD, DISTRITO, SECCION))

egeo_xx <- egeo %>%
  select(ENTIDAD, DISTRITO, SECCION) %>%
  unique() %>%
  group_by(ENTIDAD, DISTRITO) %>%
  count(SECCION) %>%
  arrange(-n) %>%
  mutate(id = str_c(ENTIDAD, DISTRITO, SECCION))

# xx <- cbind(elec_xx, egeo_xx)
table(egeo_xx$id %in% elec_xx$id)
egeo_xx[which(egeo_xx$id %in% elec_xx$id),]



