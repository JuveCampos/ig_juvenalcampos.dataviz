library(plotly)
library(quantmod)

# Obtenemos los datos:
getSymbols("AAPL",src='yahoo')

# Revisar https://plotly.com/r/candlestick-charts/

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL)) %>% as_tibble()
df <- tail(df, 30)

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~AAPL.Open, close = ~AAPL.Close,
                      high = ~AAPL.High, low = ~AAPL.Low)
fig <- fig %>% layout(title = "Basic Candlestick Chart")

fig

