# 1. Scrap de las banderas del mundo.
library(tidyverse)
library(rvest)

# Dirección de las banderas
url <- "https://en.wikipedia.org/wiki/Gallery_of_sovereign_state_flags"

# Armamos la carpeta:
dir.create("banderas")

# Descargamos las banderas
urls_flags <- read_html(url) %>%
  html_nodes("#mw-content-text") %>%
  html_nodes("img") %>%
  html_attr("src")
urls_flags <- str_c("https:",urls_flags[str_detect(urls_flags, pattern = "Flag_of_")])
pais <- urls_flags %>% str_extract(pattern = "Flag_of_\\w+(?:%28|%29)?\\.svg") %>%
  str_remove(pattern = "Flag_of_") %>%
  str_remove(pattern = "\\.png") %>%
  str_remove(pattern = "\\.svg") %>%
  str_replace_all(pattern = "\\_", replacement = " ") %>%
  str_to_title() %>%
  str_remove(pattern = "^The ")

# Base de datos ----
bd <- tibble(direccion = urls_flags,
       pais = pais)

# Corrección manual de NAs ----
na_pais <- bd %>%
  filter(is.na(pais))

na_pais$pais <- c("Australia",
                  "Belgium",
                  "Canada",
                  "China",
                  "Guinea-Bissau",
                  "Cote d´ Ivoire",
                  "Transnistria")

bd <- rbind(bd, na_pais) %>%
  filter(!is.na(pais)) %>%
  arrange(pais)

# Guardamos los datos:
openxlsx::write.xlsx(bd, "banderas_paises.xlsx")

