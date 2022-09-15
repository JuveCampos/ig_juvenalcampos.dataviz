# Librerias:
library(tidyverse)
library(geofacet)
library(ggtext)

# Datos:
nac <- readxl::read_xlsx("Bases de datos/Nacimientos/INEGI_exporta_9_3_2022_12_40_36.xlsx",
                         skip = 3) %>%
  rename(cve_ent =  `...1` ,
         ent =  `...2`)

nac$cve_ent[nac$ent == "Total"] <- "00"
nac <- nac %>%
  filter(as.numeric(cve_ent) <= 32) %>%
  pivot_longer(cols = 3:ncol(.),
               names_to = "year") %>%
  mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>%
  filter(ent != "Total") %>%
  mutate(name = ent)

# Grafica:
# Grilla México
mx_grid <- data.frame(
  code = c(2L, 8L, 26L, 3L, 5L, 10L, 19L, 25L, 28L, 1L, 18L, 24L, 32L,
           11L, 13L, 14L, 22L, 30L, 6L, 15L, 29L, 9L, 17L, 21L, 31L, 4L,
           12L, 16L, 20L, 23L, 27L, 7L),
  name = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas"),
  name_official = c("Baja California", "Chihuahua", "Sonora", "Baja California Sur", "Coahuila de Zaragoza", "Durango", "Nuevo León", "Sinaloa", "Tamaulipas", "Aguascalientes", "Nayarit", "San Luis Potosí", "Zacatecas", "Guanajuato", "Hidalgo", "Jalisco", "Querétaro", "Veracruz de Ignacio de la Llave", "Colima", "México", "Tlaxcala", "Ciudad de México", "Morelos", "Puebla", "Yucatán", "Campeche", "Guerrero", "Michoacán de Ocampo", "Oaxaca", "Quintana Roo", "Tabasco", "Chiapas"),
  name_abbr = c("BC", "CHIH", "SON", "BCS", "COAH", "DGO", "NL", "SIN", "TAM", "AGS", "NAY", "SLP", "ZAC", "GTO", "HGO", "JAL", "QRO", "VER", "COL", "MEX", "TLAX", "CDMX", "MOR", "PUE", "YUC", "CAMP", "GRO", "MICH", "OAX", "QROO", "TAB", "CHPS"),
  name_abbr_iso = c("BCN", "CHH", "SON", "BCS", "COA", "DUR", "NLE", "SIN", "TAM", "AGU", "NAY", "SLP", "ZAC", "GUA", "HID", "JAL", "QUE", "VER", "COL", "MEX", "TLA", "CMX", "MOR", "PUE", "YUC", "CAM", "GRO", "MIC", "OAX", "ROO", "TAB", "CHP"),
  name_abbr_official = c("BC", "Chih.", "Son.", "BCS", "Coah.", "Dgo.", "NL", "Sin.", "Tamps.", "Ags.", "Nay.", "SLP", "Zac.", "Gto.", "Hgo.", "Jal.", "Qro.", "Ver.", "Col.", "Mex.", "Tlax.", "CDMX", "Mor.", "Pue.", "Yuc.", "Camp.", "Gro.", "Mich.", "Oax.", "Q. Roo", "Tab.", "Chis."),
  col = c(1, 3, 2, 1, 4, 3, 5, 2, 6, 5, 3, 6, 4, 4, 6, 3, 5, 7, 4, 5, 6, 5, 4, 6, 9, 8, 5, 4, 6, 9, 7, 7),
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8)
)


r <- nac %>%
  filter(year %in% c(2019, 2020)) %>%
  pivot_wider(id_cols = c(1:2),
              names_from = "year",
              values_from = "value") %>%
  mutate(diff = 100*((`2020`-`2019`)/(`2019`))) %>%
  arrange(-diff) %>%
  mutate(categoria = case_when(between(diff, -10, 0) ~   "Entre 0 y -10%",
                           between(diff, -20, -10) ~ "Entre -10% y -20%",
                           between(diff, -30, -20) ~ "Entre -20% y -30%",
                           between(diff, -40, -30) ~ "Entre -30% y -40%"),
         color = factor(categoria, levels = c("Entre 0 y -10%",
                                                 "Entre -10% y -20%",
                                                 "Entre -20% y -30%",
                                                 "Entre -30% y -40%"))) %>%
  select(cve_ent, color)


nac %>%
  left_join(r) %>%
  filter(year >= 2010) %>%
  # filter(ent == "Coahuila de Zaragoza") %>%
  ggplot(aes(x = year, y = value, group = ent, color = color)) +
  geom_vline(xintercept = "2019", color = "red", linetype = 2) +
  geom_line(size = 1) +
  scale_color_manual(values = wesanderson::wes_palettes$Zissou1[-1]) +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_geo(~name, grid = mx_grid,
            label = "name",
            scale = "free_y") +
  labs(x = NULL, y = NULL, color = "Categoría",
       subtitle = "Nuevos nacimientos por año",
       title = "Nuevos nacimientos registrados por año por entidad de residencia de la madre, 2010-2020",
       caption = "INEGI. Tabulado de Nacimientos.\nJuvenalCampos.Dataviz - @JuvenalCamposF") +
  theme(text = element_text(family = "Poppins",
                            face = "bold"),
        strip.text = element_text(size = 6),
        legend.position = "bottom",
        panel.border = element_blank(),
        plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        axis.text.x = element_text(angle = 90, size = 6),
        axis.text.y = element_text(size = 6),
        plot.caption = element_markdown(hjust = 0,
                                        color = "gray20",
                                        size = 10,
                                        face = "italic")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 1))

ggsave("nacimientos_plot.png",
       height = 8,
       width = 11,
       dpi = 300)

