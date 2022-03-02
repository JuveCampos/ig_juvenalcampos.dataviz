library(tidyverse)

em <- tibble::tribble(
        ~cve_ent,              ~entidad,       ~ent, ~year,  ~valor, ~poblacion,       ~tasa,
            "01",      "Aguascalientes",      "AGS", 2021L,   4755L,   1453452L, 327.1521867,
            "02",     "Baja California",       "BC", 2021L,  20729L,   3690160L, 561.7371604,
            "03", "Baja California Sur",      "BCS", 2021L,   2861L,    821059L, 348.4524255,
            "04",            "Campeche",      "CAM", 2021L,   3775L,   1017011L, 371.1857591,
            "05",            "Coahuila",     "COAH", 2021L,  13802L,   3261259L, 423.2107907,
            "06",              "Colima",      "COL", 2021L,   3212L,    797245L, 402.8874436,
            "07",             "Chiapas",     "CHIS", 2021L,  14654L,   5812375L, 252.1172498,
            "08",           "Chihuahua",     "CHIH", 2021L,  16564L,   3836506L, 431.7470115,
            "09",    "Ciudad de México",     "CDMX", 2021L, 105114L,   9003827L, 1167.436913,
            "10",             "Durango",      "DGO", 2021L,   5506L,   1884622L, 292.1540765,
            "11",          "Guanajuato",      "GTO", 2021L,  33277L,   6280645L, 529.8341174,
            "12",            "Guerrero",      "GRO", 2021L,   7547L,   3668973L, 205.6978888,
            "13",             "Hidalgo",      "HGO", 2021L,  13781L,   3121355L, 441.5069737,
            "14",             "Jalisco",      "JAL", 2021L,  36256L,   8490806L, 427.0030431,
            "15",              "México",      "MEX", 2021L,  99921L,  17603429L, 567.6223649,
            "16",           "Michoacán",     "MICH", 2021L,  22085L,   4857777L, 454.6318203,
            "17",             "Morelos",      "MOR", 2021L,  13795L,   2065014L, 668.0342119,
            "18",             "Nayarit",      "NAY", 2021L,   2942L,   1306145L,  225.242986,
            "19",          "Nuevo León",       "NL", 2021L,  27689L,   5685888L, 486.9775838,
            "20",              "Oaxaca",      "OAX", 2021L,  14097L,   4165619L, 338.4130906,
            "21",              "Puebla",      "PUE", 2021L,  42985L,   6664764L, 644.9590713,
            "22",           "Querétaro",      "QRO", 2021L,  11416L,   2319537L, 492.1671868,
            "23",        "Quintana Roo",     "QROO", 2021L,   6762L,   1761389L, 383.9015686,
            "24",     "San Luis Potosí",      "SLP", 2021L,   8973L,   2885705L, 310.9465451,
            "25",             "Sinaloa",      "SIN", 2021L,  11460L,   3181609L, 360.1951088,
            "26",              "Sonora",      "SON", 2021L,  15975L,   3111119L, 513.4808408,
            "27",             "Tabasco",      "TAB", 2021L,  10307L,   2599658L, 396.4752287,
            "28",          "Tamaulipas",    "TAMPS", 2021L,  10548L,   3679623L, 286.6598018,
            "29",            "Tlaxcala",     "TLAX", 2021L,   8006L,   1395545L, 573.6826831,
            "30",            "Veracruz",      "VER", 2021L,  36299L,   8588469L, 422.6480878,
            "31",             "Yucatán",      "YUC", 2021L,   7084L,   2283943L, 310.1653588,
            "32",           "Zacatecas",      "ZAC", 2021L,   9178L,   1677911L, 546.9896794,
            "00",            "Nacional", "Nacional", 2021L, 667240L, 128972439L, 517.3508427
        ) %>%
  filter(entidad != "Nacional")

# Gráfica: ----
em %>%
  ggplot(aes(x = reorder(entidad, tasa),
             y = tasa)) +
  geom_col(fill = "olivedrab") +
  geom_text(aes(label = prettyNum(round(tasa,1), big.mark = ",")),
            hjust = -0.1,
            family = "Mulish",
            size = 3,
            color = "olivedrab") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Tasa de exceso de mortalidad en las Entidades de México<br>por <b style = 'color:#6b8e23;'>cada 100,000 habitantes</b>",
       subtitle = "Exceso de defunciones acumulado 2020-2021 a la semana epidemiológica 52",
       caption = "Fuente: Exceso de mortalidad: https://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico/
       Proyección de Población: Proyección a mitad de año. Proyección de la población estatal. CONAPO.
       https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050/resource/6bfe6281-f7dc-4661-b7b0-04ba0f2aff4e
       Consulta para cada entidad federativa. Obtención de los datos realizada el 15 de Febrero del 2022.\n@JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  theme_bw() +
  theme(axis.text.y = element_text(family = "Mulish", size = 8),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5))


# Guardamos ---------------------------------------------------------------
ggsave("grafica_exceso_mortalidad.png",
       height = 7,
       width = 6,
       dpi = 300,
       device = "png")
