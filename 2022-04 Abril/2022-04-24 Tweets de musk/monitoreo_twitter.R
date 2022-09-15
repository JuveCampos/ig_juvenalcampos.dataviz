# Librerias: 
library(tidyverse)
library(rtweet)
library(leaflet)
library(ggimage)
library(ggtext)
library(lubridate)

# Datos: 
termino <- "reforma electoral lang:es"

bd <- rtweet::search_tweets(q = termino,
                      n = 30000,
                      include_rts = FALSE,
                      retryonratelimit = TRUE)

write_csv(bd, "datos/reforma-04-29.csv")

bd <- read_csv("datos/reforma-04-29.csv")

# Datos:
bd_2 <- bd %>% 
  filter(!is_retweet) %>% 
  arrange(-followers_count) %>% 
  filter(is.na(reply_to_status_id)) %>% 
  select(created_at, 
         name,
         screen_name,
         text, 
         favorite_count, 
         retweet_count,
         source, 
         hashtags, 
         urls_url, 
         lang, 
         name, 
         location, 
         followers_count, 
         verified, 
         profile_expanded_url, 
         url, 
         profile_image_url)

users_2 <- bd_2 %>% 
  mutate(axis_label = str_c("<b style = 'font-size:10px;'>", name, "</b>",
                            "<br>",
                            "<b style = 'font-size:8px; color:blue;'>",
                            "@", screen_name, "</b>")) %>% 
  group_by(screen_name) %>% 
  summarise(no_tweets = n(), 
            followers_count = max(followers_count), 
            profile_image_url = first(profile_image_url), 
            axis_label = first(axis_label)) %>% 
  mutate(score = no_tweets*followers_count) %>% 
  arrange(-score) 
  
users_2 %>% 
  filter(no_tweets > 2) %>%
  ggplot(aes(x = reorder(axis_label, no_tweets), 
             y = no_tweets)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = no_tweets), 
            hjust = -0.2, 
            color = "navyblue", 
            fontface = "bold") +
  # geom_image(aes(y = 1,
  #                image = profile_image_url),
  #            size = 0.06) +
  coord_flip() + 
  theme_bw() +
  scale_y_continuous(expand = expansion(c(0, 0.15), 0)) +
  labs(x = NULL, y = NULL, 
       title = str_c("Tweets utilizando el término ", "<b>", termino, "</b>"), 
       subtitle = str_c("Tweets emitidos del ", 
                        day(min(bd_2$created_at)), "-", month(min(bd_2$created_at)), "-", year(min(bd_2$created_at)),
                        " al ",
                        day(max(bd_2$created_at)), "-", month(max(bd_2$created_at)), "-", year(max(bd_2$created_at)))) + 
  theme(plot.title = element_markdown(family = "Poppins", hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_markdown(family = "Poppins"),
        axis.text.y = element_markdown(family = "Poppins"),
        plot.subtitle = element_text(family = "Poppins", hjust = 0.5))

# Por lenguaje: 
bd_2 %>% 
  group_by(lang) %>% 
  summarise(n = n()) %>% 
  arrange(-n) 

# users_2 %>%
#   ggplot(aes(x = log(no_tweets),
#              y = log(followers_count))) +
#   geom_point()

# Guardamos los datos: ----
openxlsx::write.xlsx(list(datos = bd_2, 
                          usuarios = users_2),
                     str_c("datos/",
                                 termino, "_",
                                 lubridate::today(), 
                                 ".xlsx"))

# Nubes de palabras: ----
cols <- "#090446 #786F52 #FEB95F #F71735 #C2095A" %>% 
  str_split(pattern = " ") %>% unlist()
  
  # RColorBrewer::brewer.pal(7, "Blues")[4:7]
pal_num <- colorNumeric(palette = cols,
                        domain = c(0, 200),
                        reverse = T)

# Nube de palabras:
data = unique(bd_2$text)
stop_words = c()
num_words = 500
background = "white"
# mask = "apple.png"
tamanio = 0.3

# create_wordcloud <- function(data,
#                              stop_words = c(),
#                              num_words = 100,
#                              background = "white",
#                              mask = NULL,
#                              tamanio = 0.5) {
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
  corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words, stopwords("english")))
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

pal_num <- colorNumeric(palette = wesanderson::wes_palettes$Darjeeling1,
                        domain = freq_palabras$freq,
                        reverse = T)

# color_wc <- pal_num(freq_palabras$freq)
color_wc <- sample(x = wesanderson::wes_palettes$Darjeeling1
                   , 
       size = length(freq_palabras$freq), 
       replace = T)

# Grab the top n most common words
# Recortamos la base de datos de palabras a un numero `n` especificado
data <- head(data, n = num_words)
if (nrow(data) == 0) {
  return(NULL)
}

data

data$freq = sqrt(data$freq)

# Limpieza final: 
data <- data %>% 
  filter(!(word %in% c("becsherm", "mexico"))) %>% 
  filter(!str_detect(word, "http"))


# data$word[1] <- "#YoDefiendoAlCIDE"
# data$word[2] <- "#SomosCIDE"

# color = "random-dark"
figPath = system.file("examples/t.png",package = "wordcloud2")
class(figPath)
wordcloud2(data,
           backgroundColor = "black",
           color = color_wc,
           # figPath = figPath,
           # shape = "square",
           fontFamily = "Poppins",
           size = 0.4)

