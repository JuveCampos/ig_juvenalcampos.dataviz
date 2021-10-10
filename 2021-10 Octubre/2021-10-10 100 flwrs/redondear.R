# Generamos función para obtener imagenes redondas

# Librerias:
library(plotrix)
library(magick)

# Función para redondear imagenes:
redondear <- function(img_direccion, dest_folder = "."){

  # Sección circular:
  png(tf <- tempfile(fileext = "png"), 1000, 1000)
  par(mar = rep(0,4), yaxs="i", xaxs = "i")
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0,1), axes=F, xlab=NA, ylab=NA)
  draw.circle(.5,.5,.5,col="black")
  dev.off()

  # Leemos la imagen:
  img = image_read(img_direccion)
  mask = image_read(tf)
  radius = min(c(image_info(img)$width, image_info(img)$height))
  mask = image_scale(mask, as.character(radius))

  par(bg = "grey"); plot(1:2, type="n")
  rasterImage(as.raster(image_composite(image = mask, composite_image = img, operator = "plus")),1,1,2,2)
  round_image <- image_composite(image = mask, composite_image = img, operator = "plus")
  round_image <- magick::image_transparent(round_image, color = "white")
  class(round_image)
  magick::image_write(round_image,path = str_c(dest_folder,
                                               str_remove_all(str_remove(img_direccion, pattern = "\\.\\w+"),
                                                              pattern = "\\w+\\/"),
                                               ".png"))
}

# Prueba sencilla:
img_direccion = imgs[10]
redondear(img_direccion, dest_folder = "round_imgs/")

# Aplicamos a las imagenes:
imgs <- str_c("imgs/", list.files("imgs/"))

# Implementamos el bucle:
map(.x = imgs,
    .f = function(x){
      redondear(x, dest_folder = "round_imgs/")})

