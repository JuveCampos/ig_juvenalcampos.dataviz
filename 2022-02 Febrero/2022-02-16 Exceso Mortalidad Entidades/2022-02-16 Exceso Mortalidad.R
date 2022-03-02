library(tidyverse)

# Datos:  -----------------------------------------------------------------
# em <- readxl::read_xlsx("../exceso_mortalidad.xlsx") %>%
#   mutate(ranking = rank(-tasa)) %>%
#   select(entidad,
#          muertes_en_exceso = valor,
#          tasa_por_cada_100_mil_habs = tasa,
#          ranking) %>%
#   filter(entidad != "Nacional")

em <- tibble::tribble(
        ~cve_ent,              ~entidad,       ~ent, ~year,  ~muertes_en_exceso,
            "01",      "Aguascalientes",      "AGS", 2021L,   4755L,
            "02",     "Baja California",       "BC", 2021L,  20729L,
            "03", "Baja California Sur",      "BCS", 2021L,   2861L,
            "04",            "Campeche",      "CAM", 2021L,   3775L,
            "05",            "Coahuila",     "COAH", 2021L,  13802L,
            "06",              "Colima",      "COL", 2021L,   3212L,
            "07",             "Chiapas",     "CHIS", 2021L,  14654L,
            "08",           "Chihuahua",     "CHIH", 2021L,  16564L,
            "09",    "Ciudad de México",     "CDMX", 2021L, 105114L,
            "10",             "Durango",      "DGO", 2021L,   5506L,
            "11",          "Guanajuato",      "GTO", 2021L,  33277L,
            "12",            "Guerrero",      "GRO", 2021L,   7547L,
            "13",             "Hidalgo",      "HGO", 2021L,  13781L,
            "14",             "Jalisco",      "JAL", 2021L,  36256L,
            "15",              "México",      "MEX", 2021L,  99921L,
            "16",           "Michoacán",     "MICH", 2021L,  22085L,
            "17",             "Morelos",      "MOR", 2021L,  13795L,
            "18",             "Nayarit",      "NAY", 2021L,   2942L,
            "19",          "Nuevo León",       "NL", 2021L,  27689L,
            "20",              "Oaxaca",      "OAX", 2021L,  14097L,
            "21",              "Puebla",      "PUE", 2021L,  42985L,
            "22",           "Querétaro",      "QRO", 2021L,  11416L,
            "23",        "Quintana Roo",     "QROO", 2021L,   6762L,
            "24",     "San Luis Potosí",      "SLP", 2021L,   8973L,
            "25",             "Sinaloa",      "SIN", 2021L,  11460L,
            "26",              "Sonora",      "SON", 2021L,  15975L,
            "27",             "Tabasco",      "TAB", 2021L,  10307L,
            "28",          "Tamaulipas",    "TAMPS", 2021L,  10548L,
            "29",            "Tlaxcala",     "TLAX", 2021L,   8006L,
            "30",            "Veracruz",      "VER", 2021L,  36299L,
            "31",             "Yucatán",      "YUC", 2021L,   7084L,
            "32",           "Zacatecas",      "ZAC", 2021L,   9178L,
            "00",            "Nacional", "Nacional", 2021L, 667240L
        ) %>%
  filter(entidad != "Nacional")


# Gráfica: ----
em %>%
  ggplot(aes(x = reorder(entidad, muertes_en_exceso),
             y = muertes_en_exceso)) +
  geom_col(fill = "brown") +
  geom_text(aes(label = prettyNum(muertes_en_exceso, big.mark = ",")),
            hjust = -0.1,
            family = "Mulish",
            size = 3,
            color = "brown") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Exceso de mortalidad en las Entidades de México",
       subtitle = "Exceso de defunciones acumulado 2020-2021 a la semana epidemiológica 52",
       caption = "Fuente: https://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico/ \nConsulta para cada entidad federativa. Obtención de los datos realizada el 15 de Febrero del 2022.\n@JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  theme_bw() +
  theme(axis.text.y = element_text(family = "Mulish", size = 8),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 7))


# Guardamos ---------------------------------------------------------------
ggsave("grafica_exceso_mortalidad.png",
       height = 6,
       width = 5,
       dpi = 300,
       device = "png")
