library(tidyverse)
library(rtweet)
library(leaflet)

# Documentacion:
# https://github.com/ropensci/rtweet

# Buscar tweets de un tema particular.

# Busqueda Avanzada: https://twitter.com/search-advanced?lang=es
# Ejemplo: "Sismo" min_faves:100 lang:es until:2020-09-30 since:2020-09-28

# query <- '"Golfo de México" "Pemex"  until:2021-07-03 since:2021-07-01'
query <- 'consulta popular lang:es'

# Busqueda
# (Solo se pueden descargar 15,000 tweets cada 15 minutos)
bd <- search_tweets(query,  # Busqueda
                    n = 10000, # Numero Maximo de Tweets
                    include_rts = FALSE, # Incluir Rts
                    retryonratelimit = TRUE)

# Paleta de colores
rgb(147, 35, 93, maxColorValue = 255)
pal_num <- colorNumeric(palette = c("#C82B5D",
                                    "#93235D"),
                        domain = c(0, 200),
                        reverse = T)

# Nube de palabras:
create_wordcloud <- function(data, stop_words = c(),
                             num_words = 100,
                             background = "white",
                             mask = NULL,
                             tamanio = 0.5) {
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
  # freq_palabras[1,2] <- 1300

  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }

  pal_num <- colorNumeric(palette = c("#C82B5D",
                                      "#93235D"),
                          domain = c(0, max(freq_palabras$freq)),
                          reverse = T)

  color_wc <- pal_num(freq_palabras$freq)

  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # color = "random-dark"
  wordcloud2(data,
             backgroundColor = background,
             color = color_wc,
             figPath = mask,
             size = tamanio)

}

# ?wordcloud2

cp <- bd %>%
  filter(str_detect(bd$text, pattern = "popular"))

# Creamos una visualización:
create_wordcloud(data = bd$text[1:100],
                 num_words = 250,
                 stop_words = c("consulta",
                                "popular"),
                 background = "white",
                 tamanio = 0.6)

