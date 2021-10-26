# Librerias:
library(tidyverse)
library(ggtext)

# Generamos una tómbola:
tombola = tibble(bolitas = sample(x = c("azul", "roja"),
                            size = 500000,
                            replace = T,
                            prob = c(0.6, 0.4)))

# write.csv(tombola, "tombola.csv")


# Cuantas bolitas hay en cada tómbola? ¿Cuál es la proporción de cada color?
tombola %>%
  group_by(bolitas) %>%
  count() %>%
  ungroup() %>%
  mutate(pp = n/sum(n))


# Proporcion sacando n numero de bolitas:
# Supongamos que sacamos n numero de bolitas... con cuantas bolitas mas o menos
# Podríamos sacar la proporción de bolitas real que hay en toda la tómbola?
proporciones = tibble()
for (tamanio in 2:10000){
  x = tibble(x = sample(tombola$bolitas, size = tamanio)) %>%
    group_by(x) %>%
    count() %>%
    ungroup() %>%
    mutate(pp = n/sum(n)) %>%
    mutate(tamanio = tamanio)
  proporciones = rbind(proporciones, x)
}

plt = proporciones %>%
  filter(x == "azul") %>%
  ggplot(aes(x = tamanio, y = pp)) +
  geom_point()

# GGplotly
plotly::ggplotly(plt)

# Sacamos bolitas de la tombola
# Si pudieramos repetir "x" veces el muestreo de tamaño "n", cuantas veces le
# atinaríamos a la proporción?

# 10 bolitas:

# rez = tibble()
# for(i in 1:100){
#   rez_0 = tibble(x = sample(tombola$bolitas, 100)) %>%
#     group_by(x) %>%
#     count() %>%
#     ungroup() %>%
#     mutate(pp = n/sum(n)) %>%
#     mutate(i = i)
#   rez = rbind(rez, rez_0)
#   print(i)
# }

# Generación de la gráfica:
rez = tibble()
tamanio_muestra = 30
for(i in 1:200){

  rez_0 = tibble(x = sample(tombola$bolitas, tamanio_muestra)) %>%
    group_by(x) %>%
    count() %>%
    ungroup() %>%
    mutate(pp = n/sum(n)) %>%
    mutate(i = i)
  rez = rbind(rez, rez_0) %>%
    filter(x == "azul")

  rez %>%
    ggplot(aes(x = pp)) +
    geom_dotplot(binwidth=0.025,
                 method='histodot',
                 fill = "blue") +
    scale_x_continuous(limits = c(0,1),
                       breaks = seq(0,1,0.1)) +
    scale_y_continuous(limits = c(0,2)) +
    labs(title = "Media muestral \n Bolitas de color '<b style = 'color:blue;'>azul</b>' y '<b style = 'color:red;'>rojo</b>' en una tómbola",
         subtitle = str_c("Muestra número: ", i, "\nTamaño de muestra: ",tamanio_muestra, " bolitas"),
         x = "Proporción de bolitas azules en la muestra") +
    theme(plot.title = element_markdown(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(family = "EB Garamond"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())

  if(i %in% c(1:10, seq(20, 200, by = 10))){

  j = ifelse(str_length(i) == 1,
             yes = str_c("00", i),
             no = ifelse(str_length(i) == 2,
                         yes = str_c("0", i),
                         no = i
                         ))

  ggsave(str_c("graficas/dot_",j, ".png"),
         device = "png",
         height = 6,
         width = 6)
  }
  print(i)
}


# datos_azul = rez %>%
#   filter(x == "azul")
#
# media_azul = mean(datos_azul$pp)
# sd_azul = sd(datos_azul$pp)
# rango = quantile(datos_azul$pp, c(0.025, 0.975))
#
#
# plt_azul = datos_azul %>%
#   ggplot(aes(x = pp)) +
#   geom_density() +
#   scale_x_continuous(limits = c(0, 1)) +
#   geom_vline(xintercept = media_azul, color = "blue")  +
#   geom_vline(xintercept = rango, linetype = 2, color = "olivedrab")
#
# plt_azul
#
# # Con las bolas rojas:
# datos_rojo = rez %>%
#   filter(x == "roja")
#
# media_rojo = mean(datos_rojo$pp)
# sd_rojo = sd(datos_rojo$pp)
# rango = quantile(datos_rojo$pp, c(0.025, 0.975))
#
# plt_rojo = datos_rojo %>%
#   ggplot(aes(x = pp)) +
#   geom_density() +
#   scale_x_continuous(limits = c(0, 1)) +
#   geom_vline(xintercept = mean(datos_rojo$pp), color = "red") +
#   geom_vline(xintercept = rango, linetype = 2, color = "olivedrab")
#
# plt_rojo
#
