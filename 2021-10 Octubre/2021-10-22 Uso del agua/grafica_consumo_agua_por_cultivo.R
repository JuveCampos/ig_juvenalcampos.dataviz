# Librerias:
library(tidyverse)
library(ggtext)

# Datos:
bd <- tibble::tribble(
                 ~Crop,            ~Cultivo, ~Green, ~Blue, ~Grey, ~Total,
               "Wheat",             "Trigo",  1277L,  342L,  207L,  1827L,
                "Rice",             "Arroz",  1146L,  341L,  187L,  1673L,
               "Maize",              "Maíz",   947L,   81L,  194L,  1222L,
             "Sorghum",             "Sorgo",  2857L,  103L,   87L,  3048L,
            "Potatoes",             "Papas",   191L,   33L,   63L,   287L,
          "Sugar Cane",    "Caña de azucar",   139L,   57L,   13L,   210L,
          "Beans, Dry",   "Frijoles, secos",  3945L,  125L,  983L,  5053L,
          "Soya beans",              "Soya",  2037L,   70L,   37L,  2145L,
               "Melon",             "Melón",  5087L,   56L,   41L,  5184L,
             "Lettuce",           "Lechuga",   153L,   28L,   77L,   237L,
            "Tomatoes", "Jitomates/Tomates",   108L,   63L,   43L,   214L,
           "Cucumbers",           "Pepinos",   206L,   42L,  105L,   353L,
              "Onions",          "Cebollas",   192L,   88L,   65L,   345L,
             "Carrots",         "Zanahoria",   106L,   28L,   61L,   195L,
             "Bananas",          "Plátanos",   660L,   97L,   33L,   790L,
             "Oranges",          "Naranjas",   401L,  110L,   49L,   560L,
          "Tangerines",          "Toronjas",   479L,  118L,  152L,   748L,
              "Lemons",           "Limones",   432L,  152L,   58L,   642L,
              "Apples",          "Manzanas",   561L,  133L,  127L,   822L,
               "Pears",             "Peras",   645L,   94L,  183L,   922L,
            "Cherries",           "Cerezas",   961L,  531L,  112L,  1604L,
             "Peaches",          "Duraznos",   583L,  188L,  139L,   910L,
        "Strawberries",            "Fresas",   201L,  209L,   37L,   347L,
              "Grapes",              "Uvas",   425L,   97L,   87L,   608L,
             "Mangoes",            "Mangos",  1314L,  362L,  124L,  1800L,
            "Avocados",         "Aguacates",   849L,  283L,  849L,  1981L,
          "Pineapples",             "Piñas",   215L,    9L,   31L,   255L,
              "Coffee",              "Café", 15249L,  116L,  532L, 15897L,
         "Cocoa beans",             "Cacao", 19745L,    4L,  179L, 19928L,
             "Chilies",            "Chiles",  5869L, 1125L,  371L,  7365L,
         "Agave Fibre",   "Fibras de agave",  6434L,    9L,  106L,  6549L,
             "Tobacco",            "Tabaco",  2021L,  205L,  700L,  2925L,
             "Papayas",            "Papaya",   399L,   40L,   21L,   460L,
         "Seed Cotton",         "Algodón",  2282L, 1306L,  440L,  4029L
        )

# m3       1000 L     1 ton
# ----- * ------  *  -------
# ton      1 m3      1000 kg


bd = bd %>%
  mutate(Gotas = round(Total/100))

# Gráfica:
bd %>%
  ggplot(aes(x = reorder(Cultivo, Total),
             y = Total)) +
  geom_col(fill = "#A0C4C7") +
  geom_text(aes(label = prettyNum(Total, big.mark = ",")),
            hjust = -0.2,
            fontface = "bold",
            size = 3,
            color = "#7d3d21") +
  # geom_dotplot(stat = "identity")
  coord_flip() +
  labs(title = "Huella hídrica promedio global por cultivos",
       x = "", y = "",
       subtitle = "Gasto de agua total ocasionado por el consumo de alimentos\n(en litros por kg)",
       caption = "Cultivos seleccionados.\nFuentes: Mekonnen, Hoekstra vía Water Footprint Network") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 1)) +
  theme(text = element_text(family = "Poppins",
                            face = "bold"),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 13,
                                     family = "EB Garamond",
                                     color = "#7d3d21",
                                     face = "bold",
                                     hjust = 0.5),
        plot.title = element_text(size = 10,
                                  family = "EB Garamond",
                                  color = "#7d3d21",
                                  hjust = 0.5),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        # plot.subtitle.position = "plot",
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        axis.text.y = element_text(color = "gray50"),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 1,
                                        color = "gray20",
                                        size = 10,
                                        face = "italic")) +
  guides(fill = guide_legend())

ggsave("gasto_agua_cultivo.png",
       device = "png",
       height = 13.5/2,
       width = 10.8/2)

