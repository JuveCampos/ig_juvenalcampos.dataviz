# Checar: https://www.tylermw.com/pathtracing-neon-landscapes-in-r/
# Librerias:
library(tidyverse)
library(raster)
library(sf)
library(rayrender)

# volcano

# Datos:
r <- raster("cem30_workespace_cem3_r120.tif")

popo_matrix <- r %>% as.matrix()
volcano <- popo_matrix

volcano_contours = isoband::isolines(x = 1:ncol(volcano),
                                     y = 1:nrow(volcano),
                                     z = volcano,
                                     levels=seq(mean(volcano),
                                                max(volcano),by=200))

# Niveles de uno en uno
names(volcano_contours) <- (0:(length(volcano_contours)-1))*5


volcano_contours[[1]]$id

# for(i in 1:length(volcano_contours)){
#   volcano_contours[[i]]$id <- 1
# }

contours = isoband::iso_to_sfg(volcano_contours)
sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))
ggplot(sf_contours) + geom_sf(aes(color = level))


# contours = isoband::iso_to_sfg(volcano_contours)
# sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))

# ggplot(sf_contours) + geom_sf(aes(color = level))


## parte 02: ----
z = 1
m_x = lapply(1:length(volcano_contours),
       function(z){
         volcano_contours[[z]]$x %>%
           median()
  }) %>%
  unlist() %>%
  median()

m_y = lapply(1:length(volcano_contours),
             function(z){
               volcano_contours[[z]]$y %>%
                 median()
             }) %>%
  unlist() %>%
  median()


scenelist = list()
counter = 1

for(i in 1:length(volcano_contours)) {
  # i = 1
  heightval = as.numeric(names(volcano_contours)[i])
  uniquevals = table(volcano_contours[[i]]$id)
  for(k in 1:length(uniquevals)) {
    # k = 1
    tempvals = volcano_contours[[i]]
    tempvals$x = tempvals$x[tempvals$id == k]
    tempvals$y = tempvals$y[tempvals$id == k]
    for(j in 1:(length(tempvals$x)-1)) {
      # j = 1
      scenelist[[counter]] = segment(start = c(tempvals$x[j]-m_x,
                                               (heightval),
                                               tempvals$y[j]-m_y),
                                     end   = c(tempvals$x[j+1]-m_x,
                                               (heightval),
                                               tempvals$y[j+1]-m_y),
                                     radius = 1,
                                     material = light(intensity = 3,
                                                      color=heat.colors(9)[i])
                                     )
      counter = counter + 1
    }
  }
}

fullscene_2 = do.call(rbind, scenelist)


generate_ground(material = metal(color="grey20", fuzz=0.05)) %>%
  add_object(fullscene_2) %>%
  render_scene(lookfrom = c(0,    # Rotación
                            300, #Lejania
                            500),
               lookat = c(0,
                          0,
                          0),
               samples = 200,
               aperture = 0,
               fov=25,
               tonemap = "reinhold",
               width = 800, height = 800)


green_light = light(color="green", intensity = 3)

grid = list()
counter = 1
for(i in seq(110,250,by=10)) {
  grid[[counter]] = segment(start=c(sinpi(i/180)*40,
                                    -0.5,
                                    cospi(i/180)*40-20),
                            end = c(sinpi(i/180)*40,
                                    18.5,
                                    cospi(i/180)*40-20),
                            radius=0.25,
                            material = green_light)
  counter = counter + 1
}

green_grid_vertical = do.call(rbind, grid)

generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
  add_object(green_grid_vertical) %>%
  add_object(fullscene_2) %>%
  render_scene(lookfrom = c(0,    # Rotación
                            300, #Lejania
                            500),
               lookat = c(0,
                          0,
                          0),
               samples = 40,
               aperture = 0, fov = 25, bloom = 5, tonemap="reinhold",
               width=800,height=800)

#Generate the horizontal grid stripes
cylinder(radius=40, z=-20,material = green_light,
         phi_min = 200, phi_max = 340, flipped = FALSE) %>%
  add_object(cylinder(radius=40, y=6,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, y=12,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, y=18,z=-20,material = green_light,
                      phi_min = 200, phi_max = 340, flipped = FALSE)) %>%
  add_object(cylinder(radius=40, z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=6,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=12,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) %>%
  add_object(cylinder(radius=40, y=18,z=-20.01,material = green_light,
                      phi_min = 200, phi_max = 340)) ->
  green_grid_horizontal


base_disk = disk(inner_radius = 90, radius=92, z=-10,
                 material = light(intensity = 3, color="purple")) %>%
  add_object(disk(inner_radius = 90, radius=92, y=-0.1, z=-10,
                  material = light(intensity = 3, color="purple"), flipped=TRUE))

generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
  add_object(green_grid_vertical) %>%
  add_object(green_grid_horizontal) %>%
  add_object(base_disk) %>%
  add_object(fullscene_2) %>%
  render_scene(lookfrom = c(0,    # Rotación
                            200, #Lejania
                            400),
               lookat = c(0,
                          0,
                          0),
               samples = 200,
               aperture = 0, fov = 25, bloom = 5, tonemap = "reinhold",
               width = 1000, height = 800)


xpos = 300 * sinpi(1:360/180)
zpos = 300 * cospi(1:360/180)

disk_height  = 6+6*sinpi(1:360/180*2)
disk_height2 = 6+6*sinpi(1:360/180*2+15/180)
disk_height3 = 6+6*sinpi(1:360/180*2-15/180)
disk_height4 = 6+6*sinpi(1:360/180*2+30/180)
disk_height5 = 6+6*sinpi(1:360/180*2-30/180)


for(i in seq(1,360,by=1)) {
  generate_ground(material = metal(color="grey20",fuzz=0.05)) %>%
    add_object(green_grid_vertical) %>%
    add_object(base_disk) %>%
    # add_object(green_grid_horizontal) %>%
    # add_object(group_objects(base_disk, group_translate = c(0,disk_height[i],0))) %>%
    # add_object(group_objects(base_disk, group_translate = c(0,disk_height2[i],0))) %>%
    # add_object(group_objects(base_disk, group_translate = c(0,disk_height3[i],0))) %>%
    # add_object(group_objects(base_disk, group_translate = c(0,disk_height4[i],0))) %>%
    # add_object(group_objects(base_disk, group_translate = c(0,disk_height5[i],0))) %>%
    add_object(fullscene_2) %>%
    render_scene(lookfrom = c(xpos[i],400,zpos[i]-10),lookat = c(0,-1,-10), samples = 200,
                 aperture = 0, fov = 22, bloom = 5, tonemap = "reinhold",
                 width = 2000, height = 1000, filename = sprintf("imgs_v/neonvolcano%d",i))
  Sys.sleep(30)
}

# Video:
av::av_encode_video(sprintf("imgs_v/neonvolcano%d.png",
                            seq(1,360,by=1)),
                    framerate = 20,
                    output = "neon_popocatepetl.mp4")

??av::av_encode_video

# av::av_encode_video(sprintf("neonvolcano%d.png",
#                             seq(1,360,by=1)),
#                     framerate = 30,
#                     output = "neonvolcano.mp4")

