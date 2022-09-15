# Codigo para extraer datos de Encuestas de Elecciones Locales 2022 de Wikipedia:
library(rvest)
library(tidyverse)

# Parámetros generales. ----
colores_partidos <- c("PRI"= "#00753b",
                      "PAN"= "blue",
                      "MC"= "orange",
                      "MORENA"= "brown",
                      "PVEM"= "#1eff00",
                      "PT"= "#ff0000",
                      "PRD"= "yellow",
                      "PES"= "purple",
                      "RSP"= "black",
                      "FXM" = "pink")

# HIDALGO ----
url <- "https://es.wikipedia.org/wiki/Elecciones_estatales_de_Hidalgo_de_2022"

tabla_por_partido <- read_html(url) %>%
  html_table() %>%
  pluck(3)

names(tabla_por_partido) <- c("Encuestadora",
                              "Fecha",
                              "PAN",
                              "PRI",
                              "MORENA",
                              "Otro",
                              "Ninguno/No sabe")

levels <- c("PAN",
            "PRI",
            "MORENA",
            "Otro",
            "Ninguno/No sabe")

## Tabla por partido ----
tabla_por_partido = tabla_por_partido %>%
  slice(-1) %>%
  mutate(Encuestadora = str_remove(Encuestadora, pattern = "\\[\\d+\\]")) %>%
  mutate(dia = str_extract(Fecha, pattern = "^\\d+"),
         mes = str_extract(Fecha, pattern = "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"),
         year = str_extract(Fecha, pattern = "\\d+$")) %>%
  mutate(mes = str_replace_all(mes, c(enero="01",
                                                    febrero="02",
                                                    marzo="03",
                                                    abril="04",
                                                    mayo = "05",
                                                    junio = "06",
                                                    julio = "07",
                                                    agosto = "08",
                                                    septiembre = "09",
                                                    octubre = "10",
                                                    noviembre = "11",
                                                    diciembre = "12"))) %>%
  mutate(fecha_fmt = str_c(dia, "/", mes, "/", year) %>%
           as.Date(format = "%d/%m/%Y")) %>%
  mutate(across(.cols = PAN:`Ninguno/No sabe`,
                .fns = function(x){str_remove(x, pattern = "%") %>% as.numeric()})) %>%
  select(-c(dia, mes, year)) %>%
  pivot_longer(cols = 3:7,
               names_to = "Partido",
               values_to = "Porcentaje") %>%
  mutate(Partido = factor(Partido, levels = levels))

# levels(tabla_por_partido$Partido)
tabla_por_partido %>%
  ggplot(aes(x = fecha_fmt, y = Porcentaje, group = Partido, color = Partido, fill = Partido)) +
  geom_smooth(alpha = 0.1) +
  geom_point(pch = 21, color = "black") +
  scale_color_manual(values = c("blue","#00753b", "brown", "gray", "purple")) +
  scale_fill_manual(values = c("blue","#00753b", "brown", "gray", "purple")) +
  scale_y_continuous(labels = scales::comma_format(suffix = "%"), limits = c(0, 50)) +
  labs(title = "¿Cómo van los partidos rumbo a las elecciones de Hidalgo 2022?",
       subtitle = "Resultados de Encuestas.",
       x = "Fecha",
       y = "Porcentaje de encuestados que votarían por...") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", hjust = 0.5, family = "Mulish"),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, family = "Mulish")
        ) +
  guides(color = guide_legend(title.hjust = 0.5,
                           title.position = "top",
                           ncol = 3))

ggsave("03_Visualizaciones/grafica_1.png",
       height = 7,
       width = 6.5,
       device = "png")


## Tabla por candidato:----
tabla_por_candidato <- read_html(url) %>%
  html_table() %>%
  pluck(4) %>%
  slice(-1)





