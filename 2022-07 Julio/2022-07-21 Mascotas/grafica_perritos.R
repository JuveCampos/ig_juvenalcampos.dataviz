library(tidyverse)
library(readxl)
library(ggimage)

# Bolitas de estados
b <- str_c("01_Datos/bolita_verde/", list.files("01_Datos/bolita_verde/"))
b <- b[c(1:4,
         8,9,5,6,7,
         # 6,7,8,9,5,
         10:32)]

est <- read_excel("01_Datos/enbiare_2021_tema12_xlsx/01EST_T12-MASCOTAS.xlsx",
                  sheet = 4, skip = 7) %>%
  filter(!is.na(Total)) %>%
  select(-`...2`) %>%
  rename(ent = `...1`) %>%
  filter(ent != "Estados Unidos Mexicanos") %>%
  select(-Total) %>%
  mutate(bolitas = b) %>%
  pivot_longer(2:4) %>%
  group_by(ent) %>%
  mutate(pp = 100*(value/sum(value)))

# b <- tibble(dir = str_c("01_Datos/bolita_verde/", list.files("01_Datos/bolita_verde/")),
#             ent =
# )



est %>%
  filter(name == "Perro") %>%
  ggplot(aes(x = reorder(ent, pp),
             y = pp,
             fill = name)) +
  geom_col(fill = "#505759") +
  ggimage::geom_image(aes(image = bolitas),
                      size = 0.06) +
  geom_text(aes(label = str_c(round(pp,1), "%"),
                y = 80),
            vjust = -1.1,
            hjust = 0.5,
            family = "Mulish",
            fontface = "bold",
            size = 5) +
  geom_hline(yintercept = 0,
             size = 1) +
  coord_polar() +
  ylim(-50,100) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"))

ggsave("perritos.png",
       device = "png",
       height = 12,
       width = 12)

