# Librerias:
library(tidyverse)
library(httr)
library(jsonlite)

curl::curl_download("https://smn.conagua.gob.mx/webservices/index.php?method=1",
                    destfile = "a.json")

bd <- fromJSON("a.json") %>%
  as_tibble()

unique(sort(bd$desciel))

mor <- bd %>%
  filter(ides == "17")

