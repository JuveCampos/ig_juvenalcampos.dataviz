# Checar: https://www.tylermw.com/pathtracing-neon-landscapes-in-r/
# Librerias:
library(tidyverse)
library(ggtext)
library(raster)
library(sf)


# volcano

# Datos:
r <- raster("cem30_workespace_cem3_r30.tif")
# plot(r)
# st_crs(r)

popo_matrix <- r %>% as.matrix()
# max(popo_matrix)
# popo_matrix <- (popo_matrix * 0.1) - 15

volcano <- popo_matrix

# volcano_contours = isoband::isolines(x = 1:ncol(volcano),
#                                      y = 1:nrow(volcano),
#                                      z = volcano,
#                                      levels=seq(120,190,by=10))



volcano_contours = isoband::isolines(x = 1:ncol(volcano),
                                     y = 1:nrow(volcano),
                                     z = volcano,
                                     levels=seq(mean(volcano),
                                                max(volcano),by=50))

volcano_contours[[1]]$id

# for(i in 1:length(volcano_contours)){
#   volcano_contours[[i]]$id <- 1
# }

contours = isoband::iso_to_sfg(volcano_contours)
sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))
ggplot(sf_contours) +
  geom_sf(aes(color = as.numeric(level))) +
  geom_richtext(aes(x = 590, y = 680,
                    label = "**Volcán Popocatépetl**"),
                size = 6,
                fill = NA,
                color = "orange",
                family = "Times New Roman"
                ) +
  geom_text(aes(x = 590, y = 650,
                    label = "Altura máxima: 5381 msnmm"),
                size = 4,
                fill = NA,
                fontface = "bold",
                color = "orange",
                family = "Times New Roman"
  ) +
  scale_color_gradientn(colors = c("red", "green")) +
  scale_x_continuous(expand = expansion(c(0.1,0.1), 0)) +
  scale_y_continuous(expand = expansion(c(0,0), 0)) +
  labs(caption = "@JuvenalCamposF - Curvas de nivel, Volcán Popocatépetl - Datos del CEM de INEGI",
       color = "Altitud (msnmm)") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black",
                                        color = "orange",
                                        size = 2),
        panel.grid = element_blank(),
        plot.caption = element_text(family = "Times New Roman",
                                 color = "orange",
                                 size = 7,
                                 face = "bold"),
        axis.text = element_text(family = "Times New Roman",
                                 color = "orange",
                                 size = 11,
                                 face = "bold"),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(family = "Times New Roman",
                                   color = "orange",
                                   size = 8,
                                   face = "bold"),
        legend.title = element_text(family = "Times New Roman",
                                   color = "orange",
                                   size = 8,
                                   face = "bold")) +
  guides(color = guide_colorbar(barwidth = 20, barheight = 0.1,
                        title.position = "top",
                        title.hjust = 0.5))

ggsave("grafica_eva.png",
       device = "png",
       height = 8,
       width = 7)

