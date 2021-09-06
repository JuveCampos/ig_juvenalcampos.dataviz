

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readtext)
library(rvest)
library(leaflet)
library(syuzhet)


# Datos -------------------------------------------------------------------

# Version estenográfica:
# Disponible en: https://www.gob.mx/presidencia/es/articulos/version-estenografica-tercer-informe-2020-2021?idiom=es
esten <- read.delim("Copia de estenografica.txt", header = FALSE) # Versión solo con los dialogos del Presidente
esten$V1

str_view_all(esten$V1,
           pattern = "gobierno",
           match = T)



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

  # Exploramos la paleta de colores:
  scales::show_col(c("#ff8fb3","#C82B5D", "#93235D"))

  # Paleta en función de los numeros:
  pal_num <- colorNumeric(palette = rev(c("#ff8fb3","#C82B5D", "#93235D")),
                          domain = c(0, max(freq_palabras$freq)),
                          reverse = T)

  color_wc <- pal_num(freq_palabras$freq)

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

  # WORDCLOUD POR SENTIMIENTO: ----
  # Color por sentimiento:
  method <- "nrc"
  lang = "spanish"

  # Obtener sentimiento
  # Escala del -8 (malo) al 8 (bueno)
  sent_pp_i_g_3 <- get_sentiment(data$word,
                               method = method,
                               language = lang)

  table(sent_pp_i_g_3)

  # Paleta en función de los sentimientos de las palabras:
  pal_num <- colorNumeric(palette = c("blue", "gray", "red"),
                          domain = -3:3,
                          reverse = F)

  color_wc <- pal_num(sent_pp_i_g_3)

  sent <- tibble(data, sent_pp_i_g_3)

  wordcloud2(data,
             backgroundColor = background,
             color = color_wc,
             figPath = mask,
             fontFamily = 'Poppins',
             size = tamanio)


ggplot(tibble(x = -3:3, y = 1),
       aes(color = x, x = x, y = y)) +
  geom_point() +
  scale_color_gradientn(colors = c("blue", "gray", "red")) +
guides(color = guide_colorbar(barheight = 30,
                              ticks.linewidth = 3,
                              ticks.colour = "white"))

# SENTIMIENTO GENERAL POR FRASES ----

frases <- tibble(sentimiento = get_sentiment(esten$V1,
                                             method = method,
                                             language = lang),
                 no_frase = 1:length(sentimiento),
                 frase = esten$V1
                 )

# Base intermedia:
frases_high_low <- frases %>%
  mutate(rank = rank(sentimiento, ties.method = "first")) %>%
  filter(rank %in% c(max(rank),max(rank)-1, max(rank)-2,
                     min(rank),min(rank)+1, min(rank)+2)) %>%
  arrange(rank)


# Gráfica:
frases %>%
  ggplot(aes(x = no_frase, y = sentimiento, fill = sentimiento)) +
  geom_col(alpha = 0.5) +
  geom_text(data = frases_high_low,aes(x = no_frase,
                                       y = sentimiento,
                                       label = str_wrap(frase, 50)),
            size = 2, color = "white", family = "Poppins") +
  scale_y_continuous(limits = c(-8, 10)) +
  labs(y = "Medida de sentimiento",
       x = "# de la frase del informe\n",
       title = "Análisis de Sentimientos.\nTercer Informe de Gobierno - Versión estenográfica.",
       subtitle = "Análisis de sentimientos a través de método de diccionario.\nDiccionario NRC para idioma Español.",
       caption = "Datos provenientes de la versión estenográfica disponible en el blog de Presidencia.",
       fill = "Score") +
  scale_fill_gradientn(colours = c("blue", "red")) +
  guides(fill = guide_colorbar(barheight = 30)) +
  theme(plot.title = element_text(size = 20,
                                  color = "#22C9FD",
                                  face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 13,
                                     color = "white"),
        plot.caption = element_text(color = "gray80"),
        plot.background = element_rect(fill = "#252840"),
        panel.background = element_rect(fill = "#252840"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(color = "white", face = "bold"),
        panel.grid.major.y = element_line(color = "gray30"),
        text = element_text(family = "Arial"),
        axis.text.y = element_markdown(color = "white", family="Poppins", size = 15, face = "bold"),
        legend.text = element_text(color = "white", size = 11),
        legend.title = element_text(color = "white", face = "bold"),
        legend.background = element_rect(fill = "#252840"),
        axis.text.x = element_text(color = "white"))

ggsave("analisis_sentimientos.png",
       device = "png",
       dpi= 300,
       height = 8,
       width = 11)


# # Versión documento: ----
# No jaló al final :(
# doc <- readtext::readtext("https://presidente.gob.mx/wp-content/uploads/2021/09/TERCER-INFORME-DE-GOBIERNO-PRESIDENTE-AMLO-01-09-21.pdf")
# saveRDS(doc, "doc.rds")
# doc <- readRDS("doc.rds")
# create_wordcloud(doc$text,
#                  num_words = 200,
#                  tamanio = 0.8)


