pacman::p_load(tidyverse,
               tidytext,
               tm,
               quanteda,
               textdata,
               syuzhet, 
               ggtext, 
               emojifont) # coreNLP

# Diccionario AFINN
get_sentiments("nrc")

# ESTO NO ES REPRODUCIBLE SI NO TIENES EL ARCHIVO!
# Analizar resultados PParamo ----
pp <- readtext::readtext("../Sesion 01/01_datos/literatura/pedro_páramo.pdf") %>% 
  mutate(text = str_replace_all(text, "\n", " ") %>% str_squish()) 

pp_sen <- get_sentences(pp$text)

# Argumentos: 
method <- "nrc"
lang = "spanish"

# Obtener sentimiento
# Escala del -8 (malo) al 8 (bueno)
sent_pp_sen <- get_sentiment(pp_sen, 
                             method = method, 
                             language = lang)

# Valor emocional: Valencia emocional

# Base de datos: 
bd <- tibble(x = 1:length(sent_pp_sen), 
             y = sent_pp_sen, 
             sentence = pp_sen) %>% 
  mutate(mm = c(rep(0, 9),  
                zoo::rollmean(y, 10)))


# Frases: 
# Primera menos negativa
f1 <- bd %>% 
  arrange(y) %>% 
  slice(1) %>% 
  pull(sentence)

# Segunda mas negativa
f2 <- bd %>% 
  arrange(y) %>% 
  slice(2) %>% 
  pull(sentence)

# Mas positiva
f3 <- bd %>% 
  arrange(-y) %>% 
  slice(1) %>% 
  pull(sentence)

# Segunda mas positiva
f4 <- bd %>% 
  arrange(-y) %>% 
  slice(2) %>% 
  pull(sentence)


# Gráfica: ---- 
bd %>% 
  ggplot(aes(x, y, color = y)) + 
  geom_line() + 
  geom_line(aes(y = mm), color = "green") + 
  scale_color_gradientn(colors = c("red", "blue")) +
  scale_y_continuous(breaks = c(seq(-15, 10, by = 5)), 
                     labels = c("-15 <img height = 35 src = 'https://hotemoji.com/images/emoji/r/g0i0936t34lr.png'>", 
                                as.character(seq(-10, 5, by = 5)), 
                                "10 <img height = 35 src = 'https://hotemoji.com/images/emoji/a/1twrq4g1a40jka.png'>"), 
                     limits = c(-20, 15)
  ) + 
  ggplot2::annotate("text",x = 3100, y = -14, label = str_wrap(f1, 25), size = 3, color = "white") +
  ggplot2::annotate("text",x = 2000, y = -12, label = str_wrap(f2, 25), size = 3, color = "white") +
  ggplot2::annotate("text",x = 2100, y = 8, label = str_wrap(f3, 25), size = 3, color = "white") +
  ggplot2::annotate("text",x = 350, y = 7, label = str_wrap(f4, 25), size = 3, color = "white") +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(y = "Medida de sentimiento", 
       x = "# de la frase del libro\n", 
       title = "Análisis de Sentimientos del libro de 'Pedro Páramo'", 
       subtitle = "Análisis de sentimientos a través de método de diccionario.\nDiccionario NRC para idioma Español.", 
       caption = "Método y datos proporcionados por Manuel Toral en su curso de análisis de texto.",
       color = "Score",
  ) +
  guides(color = guide_colorbar(barheight = 30)) +
  theme(plot.title = element_text(size = 20,
                                  color = "#22C9FD",
                                  face = "bold"),
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
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white", face = "bold"),
        legend.background = element_rect(fill = "#252840"),
        axis.text.x = element_text(color = "white"))


