library(tm)
library(tidyverse)

rom <- read_csv("~/Desktop/Visualización - Diplomado/Actividades/Actividad 02. Discursos/Versión legible para computadora/Romero.txt")

rom_1 = removeWords(rom$CIDE, stopwords("es")) %>%
  removePunctuation() %>%
  stringr::str_remove_all(pattern = rebus::char_class("¿¡")) %>%
  str_c(collapse = " ")

write_csv(tibble(rom_1),
          "../../Actividad 02. Discursos/Versión legible para computadora/romero_sin_stopwords.csv")

vid <- read_csv("~/Desktop/Visualización - Diplomado/Actividades/Actividad 02. Discursos/Versión legible para computadora/Vidal.txt")
?read_csv
vid_1 = removeWords(pull(vid), stopwords("es")) %>%
  removePunctuation() %>%
  stringr::str_remove_all(pattern = rebus::char_class("¿¡")) %>%
  str_c(collapse = " ")

write_csv(tibble(vid_1),
          "../../Actividad 02. Discursos/Versión legible para computadora/vidal_sin_stopwords.csv")
