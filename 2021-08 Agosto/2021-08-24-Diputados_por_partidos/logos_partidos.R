library(tidyverse)
library(rvest)

url <- "https://es.wikipedia.org/wiki/Partidos_pol%C3%ADticos_de_México"
html <- read_html(url)
"https://upload.wikimedia.org/wikipedia/commons/thumb/7/7f/Logo_Encuentro_Solidario.svg/120px-Logo_Encuentro_Solidario.svg.png"

html %>%
  html_nodes(".floatleft") %>%
  html_nodes("a") %>%
  html_attr("href")
