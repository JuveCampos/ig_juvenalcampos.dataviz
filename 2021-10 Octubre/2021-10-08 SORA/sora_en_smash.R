# Librerias
library(tidyverse)
library(rtweet)

# Datos
query <- 'sora smash lang:es'

bd <- search_tweets(query,  # Busqueda
                    n = 10000, # Numero Maximo de Tweets
                    include_rts = FALSE, # Incluir Rts
                    retryonratelimit = TRUE)

openxlsx::write.xlsx(bd, "sora_smash_es.xlsx")

# Datos
query <- 'sora smash lang:en'

bd <- search_tweets(query,  # Busqueda
                    n = 10000, # Numero Maximo de Tweets
                    include_rts = FALSE, # Incluir Rts
                    retryonratelimit = TRUE)

openxlsx::write.xlsx(bd, "sora_smash_en.xlsx")




