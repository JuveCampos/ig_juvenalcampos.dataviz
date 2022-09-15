# Librerias:
library(tidyverse)
root <- "Presas por estado/Nuevo León/"
archivos = str_c(root, list.files(root))

bd <- lapply(archivos, function(x){
  readxl::read_xlsx(x)
}) %>% do.call(rbind, .)

names(bd)

bd <- bd %>%
  mutate(Porcentaje = 100*(`Almacenamiento actual  (hm³)`/`NAMO almacenamiento (hm³)`))

entidad_sel <- "Nuevo León"

bd %>%
  # filter(`Entidad federativa` == entidad_sel) %>%
  ggplot(aes(x = fecha,
             y = `Almacenamiento actual  (hm³)`,
             group = `Nombre de la presa`,
             color = str_c(`Nombre de la presa`, "- Mpio. de ", Municipio)))+
  geom_line() +
  # scale_y_continuous(limits = c(0, 110),
  #                    breaks = seq(0, 100, by = 10),
  #                    labels = str_c(seq(0, 100, by = 10), "%")) +
  scale_x_datetime(breaks = "2 months") +
  labs(subtitle = entidad_sel,
       title = "Evolución del almacenamiento en las principales presas de Nuevo León",
       color = "Presa: ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.border = element_blank(),
        axis.line = element_line(color = "gray", size = 1),
        axis.ticks = element_line(color = "gray", size = 1),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

