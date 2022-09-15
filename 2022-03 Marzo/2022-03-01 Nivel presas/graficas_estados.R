library(tidyverse)

# Librerias:
col_ods_6 <- c("T1" = "#26bde2",
               "Tex1" = "#000000",
               "Tex2" = "#187991",
               "Sec" = "#26bde2",
               "Prin" = "#187991",
               "Ter" = "#a5a5a5")


archivos = str_c("Entidades/", list.files("Entidades/"))

bd <- lapply(archivos, function(x){readxl::read_xlsx(x)}) %>%
  do.call(rbind, .)
names(bd)

bd <- bd %>%
  mutate(fecha_2 = as.Date(fecha)) %>%
  mutate(Porcentaje = 100*(`Almacenamiento actual  (hm³)`/`NAMO almacenamiento (hm³)`))

entidad_sel <- "Michoacán"

bd$fecha_2 %>% class()
bd %>%
  filter(`Entidad federativa` == entidad_sel) %>%
  ggplot(aes(x = fecha_2, y = Porcentaje, group = `Entidad federativa`))+
  geom_line(color = col_ods_6[1], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             color = "red",
             size = 0.5, linetype = 2) +
  annotate("rect",
           xmax = as.Date("2020-03-01"),
           xmin = as.Date("2022-03-01"),
           ymax = 110,
           ymin = 0,
           fill = col_ods_6[3],
           alpha = 0.2) +
    scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, by = 10),
                     labels = str_c(seq(0, 100, by = 10), "%"),
                     expand = expansion(c(0, 0.01), 0)) +
  scale_x_date(breaks = "2 months") +
  labs(subtitle = entidad_sel,
       caption = "Fuente: Datos prvenientes del Sistema Nacional de Información del Agua:\nMonitoreo de las Principales Presas de México \nhttp://sina.conagua.gob.mx/sina/almacenamientoPresas.php",
       x = NULL,
       title = "Porcentaje de almacenamiento actual con respecto al\nalmacenamiento al NAMO de las principales presas de la entidad") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.border = element_blank(),
        plot.title = element_text(color = col_ods_6[3], size = 16, family = "Mulish", hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "gray", size = 1),
        axis.ticks = element_line(color = "gray", size = 1),
        plot.subtitle = element_text(color = col_ods_6[1], size = 11, family = "Mulish", hjust = 0.5))


