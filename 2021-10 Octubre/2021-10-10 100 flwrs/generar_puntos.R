# Librerias:
library(tidyverse)
library(ggimage)

# Archivos:
imgs <- str_c("round_imgs/", list.files("round_imgs/"))

c = 50
# Espiral:
vertspacing = 10
horzspacing = 10
thetamax = 40*pi
# % Calculation of (x,y) - underlying archimedean spiral.
b = vertspacing/2/pi;
theta = 0:0.01:thetamax
x = b*theta*cos(theta)+c
y = b*theta*sin(theta)+c

# % Calculation of equidistant (xi,yi) points on spiral.
smax = 0.5*b*thetamax*thetamax;
s = 0:horzspacing:smax
thetai = sqrt(2*s/b);
xi = b*thetai*cos(thetai);
yi = b*thetai*sin(thetai);
plot(x,y,'b-')

# Generamos coordenadas:
coordenadas <- tibble(x = x,
                      y = y) %>%
  tail(length(imgs)) %>%
  cbind(imgs)

coordenadas %>%
  tail(102) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 11,
             alpha = 0.8,
             color = "red") +
  geom_image(aes(image = imgs)) +
  theme_void()

ggsave(filename = "followers.png",
       dpi= 300,
       width = 10,
       height = 10)




