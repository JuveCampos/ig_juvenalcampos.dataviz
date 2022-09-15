# Librerias:
library(httr)
library(tidyverse)
library(rvest)

# Armado del loop:
today <- as.Date("2022/02/27")
?seq.Date
map(.x = seq.Date(from = as.Date("2019/02/28"),
                  to = as.Date("2022/02/28"),
                  by = "day"), .f = function(today){

  tryCatch({
    url <- str_c("http://201.116.60.28:8080/ibi_apps/WFServlet?IBIF_webapp=/ibi_apps&IBIC_server=EDASERVE&IBIWF_msgviewer=OFF&IBIF_ex=pc_eco_presas_diarias_sih_dd&CLICKED_ON=&\\=&\\=&\\=&ID=Nuevo%20Le%F3n&ANIO2=", today)
    r <- httr::GET(url)
    # fecha = str_extract(url, pattern = "\\d\\d\\d\\d\\/\\d\\d\\/\\d\\d" )
    tabla <-  r %>%
      read_html() %>%
      html_table() %>%
      pluck(3) %>%
      slice(-1) %>%
      filter(!is.na(X5)) %>%
      filter(X5 != "") %>%
      select(-c(X6, X7))

    nombres <- tabla[1,] %>%
      as_vector()

    tabla = tabla %>%
      slice(-1) %>%
      mutate(X4 = str_remove_all(X4, pattern = ",") %>% as.numeric(),
             X5 = str_remove_all(X5, pattern = ",") %>% as.numeric()) %>%
      mutate(fecha = today)

    names(tabla) <- c(nombres, "fecha")

    tabla

    # Guardamos el dato:
    openxlsx::write.xlsx(tabla,
                         str_c("Presas por estado/Nuevo León/",today,
                               "_nuevo_leon",
                               ".xlsx"))
  },
  error = function(e){
    print(str_c("Error en ", today))
  })
})


