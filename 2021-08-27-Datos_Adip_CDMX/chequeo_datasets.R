# Librerias:
library(tidyverse) # Manejo de datos
library(rvest)     # Funciones de web scraping
library(DT)        # Visualización de tablas

# Creamos las subpáginas. En total son doce paginas
paginas <- str_c("https://datos.cdmx.gob.mx/dataset/?page=", 1:12)

Sys.sleep(3) # Paro R 3 segundos para grabar el video

# Genero el loop de descarga que consulte en las 12 paginas:
# pag = paginas[7]
datos_final <- lapply(paginas, function(pag){

    # Guardamos el código html en un objeto independiente:
    html <- read_html(pag)

    # Sacamos el numero de la página:
    no <- str_extract(pag, pattern = "\\d+$")

    # Datasets:
    datasets <- html %>%
      html_nodes("li") %>%
      html_nodes(".dataset-content") %>%
      html_nodes("h2") %>%
      html_text() %>%
      str_squish()

    # Descripción ----
    descripcion <- html %>%
      html_nodes(".dataset-item")

    # Extraemos la descripción:
    descripcion_tarjeta <- lapply(1:length(descripcion),
           function(x){
             # x = 5
             comentario <- descripcion[x] %>%
               html_nodes("div") %>%
               html_nodes("div") %>%
               html_text() %>%
               str_squish() %>%
               str_c(collapse = " ")

             if(length(comentario) == 0) comentario <- "Este conjunto de datos no tiene una descripción"
             return(comentario)
           }) %>%
      unlist()


    # Enlaces de descarga: ----
    # Analizamos cada tarjeta de manera individual:
    enlaces_descarga <- html %>%
      html_nodes(".dataset-item")

    # Sacamos los enlaces de cada tarjeta:
    enlaces <- lapply(1:length(enlaces_descarga),
                      function(x){
      str_c("https://datos.cdmx.gob.mx/",
            enlaces_descarga[x] %>%
              html_nodes("a") %>%
              html_attr("href")) %>%
        str_c(collapse = ";\n ")
    }) %>%
      unlist()

    # Juntamos todos los datos: ----
    tabla_parcial <- cbind.data.frame(datasets,
                                      descripcion_tarjeta,
                                      enlaces,
                                      pagina = rep(no, length(datasets))) %>%
      as_tibble()

    # Imprimo la pagina:
    print(str_c("Se descargaron los datos de: ", pag))
    return(tabla_parcial)
})

# Juntamos los datos en un objeto tibble.
bd <- datos_final %>%
  do.call(rbind, .)

# Guardar en un excel los datos:
openxlsx::write.xlsx(bd, "datos_disponibles_tudinero_cdmx.xlsx")

# Visualizamos la tabla:
DT::datatable(
  bd,
  options = list(
    fixedColumns = list(leftColumns = 2),
    #dom = 't',
    scrollX = TRUE,
    pageLength = 5,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    autoWidth = TRUE,
    escape = T)
  )

