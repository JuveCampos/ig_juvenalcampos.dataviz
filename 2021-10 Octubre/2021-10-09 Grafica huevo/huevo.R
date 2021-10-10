
# Librerias:
library(tidyverse)
library(sf)
library(ggtext)

# Shape de municipios:
mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")
ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# Generando link:
link <- str_c("http://infosiap.siap.gob.mx/gobmx/datosAbiertos/Estadist_Produc_Pecuaria/cierre_",
      2020,
      ".csv")

datos <- read_csv(link,
                  locale = locale(encoding = "WINDOWS-1252"))

unique(datos$Nomproducto)

huevo <- datos %>%
  filter(Nomproducto == "Huevo-plato") %>%
  group_by(Cveestado, Cvempio, Nomproducto) %>%
  summarise(Volumen = sum(Volumen, na.rm = T)) %>%
  ungroup() %>%
  mutate(Cveestado = case_when(str_length(Cveestado) == 1 ~ str_c("0", Cveestado),
                               str_length(Cveestado) == 2 ~ str_c("", Cveestado)),
         Cvempio = case_when(str_length(Cvempio) == 1 ~ str_c("00", Cvempio),
                             str_length(Cvempio) == 2 ~ str_c("0", Cvempio),
                             str_length(Cvempio) == 3 ~ str_c("", Cvempio))) %>%
  mutate(CVEGEO = str_c(Cveestado, Cvempio)) %>%
  select(CVEGEO, Nomproducto, Volumen)


mapa <- left_join(mpios, huevo)

fm <- function(x) prettyNum(round(x, 2), big.mark = ",")

breaks <- BAMMtools::getJenksBreaks(var = mapa$Volumen,k = 7)
jenks = case_when(between(mapa$Volumen, breaks[1], breaks[2]) ~ str_c("Entre ", fm(breaks[1]), " y ", fm(breaks[2]), " toneladas"),
                  between(mapa$Volumen, breaks[2], breaks[3]) ~ str_c("Entre ", fm(breaks[2]), " y ", fm(breaks[3]), " toneladas"),
                  between(mapa$Volumen, breaks[3], breaks[4]) ~ str_c("Entre ", fm(breaks[3]), " y ", fm(breaks[4]), " toneladas"),
                  between(mapa$Volumen, breaks[4], breaks[5]) ~ str_c("Entre ", fm(breaks[4]), " y ", fm(breaks[5]), " toneladas"),
                  between(mapa$Volumen, breaks[5], breaks[6]) ~ str_c("Entre ", fm(breaks[5]), " y ", fm(breaks[6]), " toneladas"),
                  between(mapa$Volumen, breaks[6], breaks[7]) ~ str_c("Entre ", fm(breaks[6]), " y ", fm(breaks[7]), " toneladas"),
                  is.na(mapa$Volumen) ~ "Sin producción")
mapa$quiebres <- factor(jenks, levels = c("Entre 0.09 y 6,716.83 toneladas",
                                          "Entre 6,716.83 y 26,529.5 toneladas",
                                          "Entre 26,529.5 y 68,175.8 toneladas",
                                          "Entre 68,175.8 y 172,114 toneladas",
                                          "Entre 172,114 y 287,234 toneladas",
                                          "Entre 287,234 y 443,337 toneladas",
                                          "Sin producción"))

cols <- c("#d1a4ed", wesanderson::wes_palettes$Zissou1, "gray50")

mapa %>%
  ggplot(aes(fill = quiebres)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = ents, fill = NA, color = "white", size = 1) +
  scale_fill_manual(values = cols) +
  theme_bw() +
  labs(title = "SIAP - Agricultura",
       subtitle = "Municipios por rangos de volumen de producción<br>Huevo - Plato",
       x = "",
       y = "",
       caption = "<b style = 'color = #00ac7f;'>Fuente: </b> SIAP - Secretaría de Agricultura y Desarrollo Rural. Datos de producción pecuaria, 2020.",
       fill = "Rangos de producción") +
  theme(text = element_text(family = "Poppins",
                            face = "bold"),

        panel.border = element_blank(),
        plot.subtitle = element_markdown(size = 20, family = "EB Garamond", hjust = 0.5),
        plot.title = element_markdown(size = 12, family = "EB Garamond", hjust = 0.5),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        # plot.subtitle.position = "plot",
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        # legend.position = "bottom",
        plot.caption = element_markdown(hjust = 0,
                                        color = "gray20",
                                        size = 10,
                                        face = "italic")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5, ncol = 1))

?guide_legend
