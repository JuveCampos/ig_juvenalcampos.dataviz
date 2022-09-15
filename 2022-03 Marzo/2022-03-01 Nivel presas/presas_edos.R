# Librerias:
library(httr)
library(tidyverse)
library(rvest)

# Armado del loop:
cat = tibble::tribble(
  ~cve_ent,                  ~ent,
      "00",            "Total",
      "01",      "Aguascalientes",
      "02",     "Baja California",
      "03", "Baja California Sur",
      "04",            "Campeche",
      "05",            "Coahuila",
      "06",              "Colima",
      "07",             "Chiapas",
      "08",           "Chihuahua",
      "09",    "Ciudad de México",
      "10",             "Durango",
      "11",          "Guanajuato",
      "12",            "Guerrero",
      "13",             "Hidalgo",
      "14",             "Jalisco",
      "15",              "Estado de México",
      "16",           "Michoacán",
      "17",             "Morelos",
      "18",             "Nayarit",
      "19",          "Nuevo León",
      "20",              "Oaxaca",
      "21",              "Puebla",
      "22",           "Querétaro",
      "23",        "Quintana Roo",
      "24",     "San Luis Potosí",
      "25",             "Sinaloa",
      "26",              "Sonora",
      "27",             "Tabasco",
      "28",          "Tamaulipas",
      "29",            "Tlaxcala",
      "30",            "Veracruz",
      "31",             "Yucatán",
      "32",           "Zacatecas")

# tabla$`Entidad federativa`[!(tabla$`Entidad federativa` %in% cat$ent)]
# tabla$`Entidad federativa` %in% cat$ent

today <- as.Date("2022/02/28")
?seq.Date
map(.x = seq.Date(from = as.Date("2019/02/28"),
                  to = as.Date("2022/02/28"),
                  by = "day"), .f = function(today){

                    tryCatch({
                      url <- str_c("http://201.116.60.28:8080/ibi_apps/WFServlet?IBIF_ex=pc_eco_presas_diarias_sih&ANIO2=", today)
                      r <- httr::GET(url)
                      # fecha = str_extract(url, pattern = "\\d\\d\\d\\d\\/\\d\\d\\/\\d\\d" )
                      tabla <-  r %>%
                        read_html() %>%
                        html_table() %>%
                        pluck(3) %>%
                        slice(-1) %>%
                        filter(!is.na(X5)) %>%
                        filter(X5 != "") %>%
                        select(-c(X1,X6, X7))

                      nombres <- tabla[1,] %>%
                        as_vector()

                      tabla = tabla %>%
                        slice(-1) %>%
                        mutate(X4 = str_remove_all(X4, pattern = ",") %>% as.numeric(),
                               X5 = str_remove_all(X5, pattern = ",") %>% as.numeric()) %>%
                        mutate(fecha = today)

                      names(tabla) <- c(nombres, "fecha")

                      tabla = tabla %>%
                        left_join(cat, by = c(`Entidad federativa` = "ent")) %>%
                        relocate(cve_ent, .before = `Entidad federativa`)

                      # Guardamos el dato:
                      openxlsx::write.xlsx(tabla,
                                           str_c("Entidades/",today,".xlsx"))
                    },
                    error = function(e){
                      print(str_c("Error en ", today))
                    })
                  })

