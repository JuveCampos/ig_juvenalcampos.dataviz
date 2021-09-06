

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readtext)
library(rvest)
library(leaflet)


# Datos -------------------------------------------------------------------

# Version estenográfica:
# Disponible en: https://www.gob.mx/presidencia/es/articulos/version-estenografica-tercer-informe-2020-2021?idiom=es
esten <- read.delim("Copia de estenografica.txt", header = FALSE) # Versión solo con los dialogos del Presidente
esten$V1

# Función WordCloud -------------------------------------------------------

# Nube de palabras:
# create_wordcloud <- function(data, stop_words = c(),
#                              num_words = 100,
#                              background = "white",
#                              mask = NULL,
#                              tamanio = 0.5) {
  data = esten$V1
  stop_words = c()
  num_words = 200
  background = "white"
  mask = NULL
  tamanio = 1

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

  scales::show_col(c("#ff8fb3","#C82B5D", "#93235D"))

  pal_num <- colorNumeric(palette = rev(c("#ff8fb3","#C82B5D", "#93235D")),
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
             fontFamily = 'Poppins',
             size = tamanio)




# create_wordcloud(esten$V1,num_words = 200, tamanio = 0.8)

# # Versión documento: ----
# No jaló al final :(
# doc <- readtext::readtext("https://presidente.gob.mx/wp-content/uploads/2021/09/TERCER-INFORME-DE-GOBIERNO-PRESIDENTE-AMLO-01-09-21.pdf")
# saveRDS(doc, "doc.rds")
# doc <- readRDS("doc.rds")
# create_wordcloud(doc$text,
#                  num_words = 200,
#                  tamanio = 0.8)


