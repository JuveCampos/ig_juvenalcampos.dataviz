library(tidyverse)

bd <- readxl::read_xlsx("zelda_sales.xlsx", sheet = 2) %>%
  arrange(year)

bd$label <- factor(str_c(bd$Game,
                         "\n(", bd$year, ")"),
                   levels = str_c(bd$Game, "\n(",
                                  bd$year, ")"))

bd %>%
  filter(!is.na(Millones_distribucion)) %>%
  ggplot(aes(x =label, y = Millones_distribucion,
             fill = factor(Remake))) +
  geom_col() +
  labs(y = "Millones de Copias",
       x = "",
       title = "Copias distribuidas de los juegos de The Legend Of Zelda",
       subtitle = "1986-2020") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Hylia Serif Beta"))

