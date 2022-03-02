# Librerias ----
library(tidyverse)
library(httr)
library(jsonlite)

# Datos ----
a = GET("http://201.116.60.28:8080/ibi_apps/WFServlet?IBIF_ex=pc_eco_presas_diarias_sih&ANIO2=2022/02/26", 
        set_cookies(JSESSIONID =	"B397D5A73A8C58706474DDA6AF0F2043",
                    WF_SESSIONID	= "172242280373888243"))


a
a$content
a$content[[1]]
