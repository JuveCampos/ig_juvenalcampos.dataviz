# Grafica de datos: e-sports:
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(rvest)
library(ggtext)

# Datos ----
url <- "https://www.esportsearnings.com/history/2020/countries"
flags <- readxl::read_xlsx("banderas_paises.xlsx")
bd <- read_html(url) %>%
  html_table() %>%
  pluck(1) %>%
  mutate(earnings = as.numeric(str_remove_all(X3, pattern = "\\$|\\,")),
         no_players = as.numeric(str_remove_all(X4, pattern = " Players|Player"))) %>%
  rename(Country = X2) %>%
  select(-X1, -X3, -X4)

# Gráfica ----
( bd_chart <- bd %>%
    mutate(ranking = rank(-earnings)) %>%
  filter(ranking <= 10 | Country == "Mexico") %>%
  left_join(flags,
            by = c("Country" = "pais")) %>%
  mutate(label = str_c("<img style = 'height:15px;' src = '", direccion, "' height = '15'><br>", ranking, ". ", Country)) %>%
    mutate(is.Mexico = ifelse(Country == "Mexico",
                              yes = -0.1,
                              no = 1.1))
  )

# bd_chart$label[2]

bd_chart %>%
  ggplot(aes(x = reorder(label, earnings),
             y = earnings,
             fill = factor(is.Mexico))) +
  geom_col() +
  geom_text(aes(label = str_c("$", round(earnings/1e6, 2), "M"),
                color = factor(is.Mexico)),
            fontface = "bold",
            hjust = bd_chart$is.Mexico,
            family = "Poppins") +
  scale_fill_manual(values = c("#17A398", "#227C9D")) +
  scale_color_manual(values = c("black", "white")) +
  scale_y_continuous(expand = expansion(c(0,0.1))) +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "",
       title = "Ganancias obtenidas en e-sports en 2020.",
       subtitle = "Top 10 países con más ingresos (+ México)",
       caption = "Fuente:https://www.esportsearnings.com/history/2020/countries\nUnidades monetarias en USD @ 2020") +
  theme(axis.text.y = element_markdown(vjust = 0.5, color = "#227c9d"),
        text = element_text(family = "Poppins"),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 20,
                                  color = "#fe6d73"),
        plot.subtitle = element_text(size = 15,
                                     color = "#227C9D"),
        panel.background = element_rect(color = "#fef9ef"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        legend.position = "none")

# EXtRAS ----
# https://rpubs.com/dieghernan/beautifulmaps_II
