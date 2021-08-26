# Librerias:
library(tidyverse)
library(rvest)

"https://datos.cdmx.gob.mx/dataset/?page=1"

html <- read_html("https://datos.cdmx.gob.mx/dataset/?page=1")

# Datasets:
datasets <- html %>%
  html_nodes("li") %>%
  html_nodes(".dataset-content") %>%
  html_nodes("h2") %>%
  html_text() %>%
  str_squish()


descripcion <- html %>%
  html_nodes("li") %>%
  html_nodes(".dataset-content") %>%
  html_nodes("div") %>%
  html_text() %>%
  str_squish()

enlaces_descarga <- html %>%
  html_nodes(".dataset-item") %>%
  html_nodes(".dataset-resources") %>%
  html_nodes("a")


enlaces_descarga[[1]] %>%
  html_nodes(a)
