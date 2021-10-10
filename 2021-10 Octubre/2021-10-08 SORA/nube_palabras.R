# Librerias:
library(tidyverse)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(wordcloud2, tm, stringr, leaflet)

# Datos:
bd <- readxl::read_xlsx("sora_smash_es.xlsx")


data = bd$text
num_words = 500
stop_words = c("")
background = "white"
mask = NULL
tamanio = 2

# Nube de palabras:
# create_wordcloud <- function(data, stop_words = c(),
#                              num_words = 100,
#                              background = "white",
#                              mask = NULL,
#                              tamanio = 0.5) {
  # Checar si esta instalado Pacman


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

  freq_palabras <<- data %>%
    slice(-c(4,8,34))

  data[c(1:3),2] <- 800


  # Tuneado de palabras:
  freq_palabras %>%
    as_tibble() %>%
    print(n = 40)



  freq_palabras[9,1] <- "kingdom_hearts"





  # freq_palabras[1,2] <- 1300

  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }


scales::show_col(  c("#C82B5D",
                     "#93235D"))

  pal_num <- colorNumeric(palette = c("#C82B5D",
                                      # "#93235D",
                                      "#516ACF",
                                      "#FBDE42"),
                          domain = c(0, max(freq_palabras$freq)),
                          reverse = T)

  color_wc <- pal_num(sample(x = 1:max(freq_palabras$freq),
                             size = length(freq_palabras$freq),
                             replace = TRUE))

  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }

  # color = "random-dark"


    data[1,1] <- "smash bros"
    data <- data %>%
      filter(word != "bros")
    data[8,1] <- "kingdom hearts"
    data <- data %>%
      filter(word != "hearts")

    # Hacemos la nube de palabras
    set.seed(10)
    color_wc <- pal_num(sample(x = 1:max(freq_palabras$freq),
                               size = length(freq_palabras$freq),
                               replace = TRUE))
    wordcloud2(data,
             backgroundColor = background,
             color = color_wc,
             fontFamily = "Poppins",
             figPath = mask,
             size = 0.8)


