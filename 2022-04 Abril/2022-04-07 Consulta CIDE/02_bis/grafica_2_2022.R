# Librerias:
library(tidyverse)
library(waffle)

# Datos:
datos <- tibble(variable = c("No participaron",
                             # "Alumnos que votaron",
                             "Comunidad a favor del director",
                             "Comunidad a favor de la revocación",
                             "Votos Nulos"
                             ),
       numeros = c(955 - sum(c(22, 360, 1)),
                   # 524,
                   22, 360, 1)) %>%
  mutate(variable = factor(variable, c("No participaron",
                                       # "Alumnos que votaron",
                                       "Comunidad a favor de la revocación",
                                       "Comunidad a favor del director",
                                       "Votos Nulos")))
levels(datos$variable)

# %>%
#   mutate(numeros = numeros/10)

# Raíz cuadrada de 12,221 (para que el waffle salga cuadrado)
sqrt(383)

# Colores:
# colores <- wesanderson::wes_palettes$Zissou1[-2]
colores <- c("gray50","#204d47", "brown","purple")
scales::show_col(colores)

# Gráfica:
datos %>%
  ggplot(aes(label = variable,
             values = numeros,
             color = variable)) +
  geom_pictogram(n_rows = 20,
                 make_proportional = F,
                 size = 3,
                 family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("user"),
    labels = c("Total Alumnos",
              "Alumnos que votaron",
              "Alumnos apoyo",
              "Alumnos no apoyo",
              "Votos Nulos"))  +
  scale_color_manual(values = colores) +
  scale_y_continuous(expand = expansion(c(0.01,0.01), 0)) +
  scale_x_continuous(expand = expansion(c(0.01,0.01), 0)) +
  labs(x = "", y = "",
       title = "Participación en la revocación CIDE",
       # subtitle = "De un total de 12,221 estudiantes...",
       caption = "#30DayChartChallenge Día 2.
       Comparaciones - Pictograma.
       @JuvenalCamposF - IG: juvenalcampos.dataviz
       Fuente: 12,221 alumnos fueron los registrados en Población Total de Licenciatura de la Facultad de Ciencias Políticas de la UNAM en 2019-2020. ") +
  theme_bw() +
  theme_enhance_waffle() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5,
                                  family = "Poppins",
                                  size = 15,
                                  face = "bold",
                                  color = colores),
        plot.subtitle = element_text(hjust= 0.5,
                                     family = "Poppins",
                                     size = 10),
        legend.position = "none")

ggsave("grafica_1.png",
       device = "png",
       height = 8,
       width = 8)


# Grafica de resultados:
colores = colores[-1]
datos$variable

datos %>%
  filter(variable != "No participaron") %>%
  ggplot(aes(label = variable,
             values = numeros,
             color = variable)) +
  geom_pictogram(n_rows = 10,
                 make_proportional = F,
                 size = 4,
                 family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("user"),
    labels = c("No participaron",
               # "Alumnos que votaron",
               "Comunidad a favor de la revocación",
               "Comunidad a favor del director",
               "Votos Nulos"))  +
  scale_color_manual(values = colores) +
  scale_y_continuous(expand = expansion(c(0.05,0.05), 0)) +
  scale_x_continuous(expand = expansion(c(0.05,0.05), 0)) +
  #' labs(x = "", y = "",
  #'      title = "¿Cómo estuvo la participación en el ejercicio de revocación\nde la FCPyS de la UNAM?",
  #'      subtitle = "De un total de 12,221 estudiantes...",
  #'      caption = "#30DayChartChallenge Día 2.
  #'      Comparaciones - Pictograma.
  #'      JuvenalCamposF - IG: juvenalcampos.dataviz
  #'      Fuente: 12,221 alumnos fueron los registrados en Población Total de Licenciatura de la Facultad de Ciencias Políticas de la UNAM en 2019-2020. ") +
  theme_bw() +
  theme_enhance_waffle() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5,
                                  family = "Poppins",
                                  size = 15,
                                  face = "bold",
                                  color = colores),
        plot.subtitle = element_text(hjust= 0.5,
                                     family = "Poppins",
                                     size = 10),
        legend.position = "none")

ggsave("grafica_2.png",
       device = "png",
       height = 3,
       width = 12)
