library(tidyverse)
library(plotly)

datos <- readxl::read_xlsx("01_datos/indicador_estados_982.xlsx")
cat <- readxl::read_xlsx("01_datos/cat.xlsx")
datos <- left_join(datos, cat) %>% 
  group_by(year) %>% 
  mutate(ranking = rank(valor)) %>% 
  arrange(year, ranking)

# Visitantes totales: 
plt = datos %>% 
  ggplot(aes(x = year, 
             y = valor, 
             group = cve_ent, 
             color = Entidad)) + 
  geom_line()

ggplotly(plt)

# Lugares: 
plt = datos %>% 
  mutate(ranking_2 = 33-ranking) %>% 
  ggplot(aes(x = year, 
             y = ranking, 
             group = cve_ent, 
             color = Entidad)) + 
  geom_line(size = 2, alpha = 0.7) + 
  geom_point(color = "white", size = 0.7) +
  geom_label(data = . %>% filter(year %in% c(2009, 2014,2020)), 
             aes(x = year, 
                 y = ranking, 
                 label = str_c(ranking_2, " ", ent), 
                 fill = Entidad), 
             color = "white",
             size = 2, 
             fontface = "bold") +
  labs(x = NULL, y = NULL, 
       title = "Lugar de las entidades con más llegada de turistas\nEvolución del Ranking", 
       caption = "Fuente: Datos de llegada de visitantes de Sectur.\n@JuvenalCamposF - IG: JuvenalCampos.Dataviz") + 
  scale_x_continuous(breaks = 2009:2020) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 90), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(family = "Poppins", hjust = 0.5, size = 13, face = "bold"), 
        plot.caption = element_text(family = "Poppins", hjust = 0.5, size = 6))

plt

ggsave("grafica_sectur.png", 
       height = 7, width  = 6, 
       dpi = 300)



