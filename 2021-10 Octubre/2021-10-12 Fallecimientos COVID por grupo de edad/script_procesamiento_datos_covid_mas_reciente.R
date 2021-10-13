# Librerias
library(tidyverse)

# Datos
bd <- read_csv("01_Datos/COVID_MAS_RECIENTE/211010COVID19MEXICO.csv")

bd1 <- bd[1:1000,]

bd_plot <- bd %>%
  filter(!is.na(FECHA_DEF)) %>%
  select(EDAD) %>%
  mutate(grupo_edad = case_when(EDAD == 0 ~ "0 años",
                                between(EDAD, 1, 10) ~ "1 a 10 años",
                                between(EDAD, 11, 17) ~ "11 a 17 años",
                                between(EDAD, 18, 30) ~ "18 a 30 años",
                                between(EDAD, 31, 40) ~ "31 a 40 años",
                                between(EDAD, 41, 50) ~ "41 a 50 años",
                                between(EDAD, 51, 60) ~ "51 a 60 años",
                                between(EDAD, 61, 70) ~ "61 a 70 años",
                                between(EDAD, 71, 80) ~ "71 a 80 años",
                                EDAD >= 81 ~ "Mayores de 81 años")) %>%
  group_by(grupo_edad) %>%
  count() %>%
  ungroup() %>%
  mutate(pp = 100*(n/sum(n))) %>%
  mutate(label = str_c(prettyNum(n, big.mark = ","), "\n(", round(pp, 2), "%)"))


bd_plot %>%
  ggplot(aes(x = grupo_edad,
             y = n)) +
  geom_col(fill = "#1A2F49") +
  geom_text(aes(label = label),
            hjust = 0.5,
            vjust = -0.25,
            color = "#1A2F49") +
  scale_y_continuous(expand = expansion(c(0,0.1), 0)) +
  labs(x = "", y = "", title = str_c("Fallecimientos totales por grupo de edad, al ",
                                     "10 de octubre, 2021"),
       caption = "Datos diarios de la Dirección General de Epidemiología. Consulta al 10 de Octubre, 2021.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        axis.ticks = element_blank())


# Guardamos gráfica:
ggsave(filename = "03_Graficas/fallecimientos_grupo_edad.png",
       device = "png",
       height = 2*3.68, width = 2*5.62)



