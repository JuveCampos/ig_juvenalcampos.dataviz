

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readtext)
library(rvest)
library(leaflet)
library(syuzhet)
library(rtweet)

# Datos -------------------------------------------------------------------
# Documentacion:
# https://github.com/ropensci/rtweet

# # Buscar tweets de un tema particular.
query <- '"buscotrabajo" (conacyt) lang:es until:2021-09-16 since:2021-09-14'

# Busqueda
# (Solo se pueden descargar 15,000 tweets cada 15 minutos)
bd <- search_tweets(query,  # Busqueda
                    n = 10000, # Numero Maximo de Tweets
                    include_rts = TRUE, # Incluir Rts
                    retryonratelimit = TRUE)

# # Guardamos los datos
saveRDS(bd, "busco_trabajo.RDS")
bd <- readRDS("busco_trabajo.RDS")

unique(bd$text)

bd <- bd %>%
  filter(!is_retweet) %>%
  mutate(text = tolower(text),
         text = str_replace_all(text, replacement = c("scatedras" = "cátedras",
                                                      "conacyt" = "conacytmx",
                                                      "trabajo" = "buscotrabajo"
                                                      ))
         )

# Función WordCloud -------------------------------------------------------

# Nube de palabras:
# create_wordcloud <- function(data, stop_words = c(),
#                              num_words = 100,
#                              background = "white",
#                              mask = NULL,
#                              tamanio = 0.5) {
  # bd <- readRDS("presupuesto.rds")
  data = bd$text
  rm(bd)
  stop_words = c("")
  num_words = 500
  mask = NULL


  # Checar si esta instalado Pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(wordcloud2, tm, stringr)

  # Pre-Función para eliminar simbolos raros
  quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))

  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minusculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuacion (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los numeros
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiracion e interrogacion al reves
    corpus <- tm_map(corpus, quitar_signos)
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el numero de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }

  freq_palabras <<- data

  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }

  # Exploramos la paleta de colores:
  colores <- "viridis"

  # Paleta en función de los numeros:
  pal_num <- colorNumeric(palette = colores,
                          domain = c(0, max(freq_palabras$freq)),
                          reverse = F)

  color_wc <- pal_num(freq_palabras$freq)
  color_wc <- sample(c("#E0E0E0", "#CC7D42",
                       "#B04220", "#5692A4",
                       "#07184B", "#4866A1",
                       "#D2AB4B"), size = num_words, replace = T)
  # ?sample
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }

  data %>%
    ggplot(aes(x = freq)) +
    geom_boxplot()


  data[1:6,"freq"] <- data[7,"freq"] + 10

  data <- filter(data, word != "trabajo")  %>%
    filter(!str_detect(word, pattern = "https|scatedras"))

  tamanio = 0.4
  background = "black"

  wordcloud2(data,
             backgroundColor = background,
             color = color_wc,
             figPath = mask,
             fontFamily = 'Poppins',
             size = tamanio)

