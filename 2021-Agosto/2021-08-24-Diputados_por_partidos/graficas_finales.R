# Librerias:
library(tidyverse)
library(ggimage)
library(ggtext)
library(ggparliament)
library(cowplot)

# Datos:
bd <- readxl::read_xlsx("datos_dips_INE.xlsx") %>%
  janitor::clean_names() %>%
  filter(partido != "TOTAL") %>%
  mutate(partido = factor(partido, levels = partido))



logos <- readxl::read_xlsx("imgs_partidos.xlsx") %>%
  janitor::clean_names()

datos <- left_join(bd, logos) %>%
  mutate(label = str_c("<img src = '", direccion, "' height = '25'>"))


datos$label

rep_prop <- datos %>%
  filter(partido != "TOTAL") %>%
  ggplot(aes(x = reorder(label, representacion_proporcional),
             y = representacion_proporcional)) +
  geom_col(fill = bd$color) +
  geom_text(aes(label = representacion_proporcional),
            hjust = 1.5,
            family = "Poppins",
            fontface = "bold",
            color = "white",
            size = 5) +
  # geom_image(aes(image = direccion), asp = 1, size = 0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0, 0.3),
                     limits = c(0, 140)) +
  labs(title = "Representación proporcional",
       x = "", y = "", caption = "Fuente de los datos: Instituto Nacional Electoral") +
  # theme_void() +
  theme(axis.text.y = element_markdown(),
        text = element_text(family = "Poppins"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.background = element_rect(fill = "white"))


may_rel <- datos %>%
  filter(partido != "TOTAL") %>%
  ggplot(aes(x = reorder(label, mayoria_relativa),
             y = mayoria_relativa)) +
  geom_col(fill = bd$color) +
  geom_text(aes(label = mayoria_relativa),
            hjust = 1.5,
            family = "Poppins",
            fontface = "bold",
            color = "white",
            size = 5) +
  # geom_image(aes(image = direccion), asp = 1, size = 0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0, 0.3),
                     limits = c(0, 140)) +
  labs(title = "Mayoría relativa",
       x = "", y = "",
       caption = "Fuente de los datos: Instituto Nacional Electoral"
       ) +
  # theme_void() +
  theme(axis.text.y = element_markdown(),
        text = element_text(family = "Poppins"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.background = element_rect(fill = "white"))


tot <- datos %>%
  filter(partido != "TOTAL") %>%
  ggplot(aes(x = reorder(label, total),
             y = total)) +
  geom_col(fill = bd$color) +
  geom_text(aes(label = total),
            hjust = 1.5,
            family = "Poppins",
            fontface = "bold",
            color = "white",
            size = 5) +
  # geom_image(aes(image = direccion), asp = 1, size = 0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0, 0.3),
                     limits = c(0, 140)) +
  labs(title = "Total",
       x = "", y = "", caption = "Fuente de los datos: Instituto Nacional Electoral") +
  # theme_void() +
  theme(axis.text.y = element_markdown(),
        text = element_text(family = "Poppins"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.background = element_rect(fill = "white"))

# Datos para el parliament.
data <- datos %>%
  filter(partido != "TOTAL")

# Base de datos modificada especialmente para el geom_parliament:
mex_horseshoe <- parliament_data(election_data = data,
                                 party_seats = data$total,
                                 parl_rows = 10,
                                 type = "semicircle") %>%
  mutate(partido = factor(partido, levels = levels(bd$partido)))

# Grafica del parliament:
mex_horseshoe %>%
  ggplot(aes(x, y, color = partido)) +
  geom_parliament_seats() +
  scale_color_manual(values = bd$color) +
  theme_ggparliament() +
  labs(color = "Partido") +
  # guides(color = guide_colorbar(title.position = "top",
  #                                 title.hjust = 0.5,
  #                                 barwidth = 50,
  #                                 barheight = 0.5))   +
  theme(legend.position = "bottom")


cw <- cowplot::plot_grid(rep_prop, may_rel, align = "v")
cw
#
#
#   )
# class(cw)
#
# plot_grid(cowplot::plot_grid(rep_prop, may_rel, align = "v") ,
#           prl, align = "v")
#
# ?plot_grid
