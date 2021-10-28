# Opciones:
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# Librerias:
library(pacman)
p_load(tidyquant, plotly, tidyverse)

# Lista de Funciones:
get_stock_list <-
  function(stock_index = "SP500") {
    tq_index(stock_index) %>%
      select(symbol, company) %>%
      arrange(symbol) %>%
      mutate(label = str_c(symbol, company, sep = ", ")) %>%
      select(label)
  }

get_stock_candles <- function(stock_symbol = "AAPL",
                              from = today() - days(180),
                              to   = today()){

  data = stock_symbol %>%
    tq_get(get = "stock.prices", from = from, to = to)
  return(data)

}

# Datos:
data = get_stock_candles(stock_symbol = "AAPL")

# Gráfica:
data %>%
  plot_ly(x = ~date,
          type="candlestick",
          open = ~open,
          close = ~close,
          high = ~high,
          low = ~low) %>%
  layout(title = "<br><b style= 'font-family: EB Garamond;'>Precio Acciones de Apple<br>USD</b>",
         yaxis = list(tickformat = "$"),
         xaxis = list(title = "Fecha (en inglés)"))
