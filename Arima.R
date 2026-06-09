library(forecast)
library(tseries)
library(XML)
library(dplyr)
library(ggplot2)
library(patchwork)

file <- "~/archive_indices_2025-01-01-2026-05-25.xml"
doc <- xmlParse(file)
rows <- getNodeSet(doc, "//row")
df_list <- lapply(rows, function(x) as.list(xmlAttrs(x)))
df <- do.call(rbind, lapply(df_list, as.data.frame, stringsAsFactors = FALSE))
df <- subset(df, select = -c(ID, NAME, OPEN, HIGH, LOW, DURATION, YIELD))
df$TRADEDATE <- as.Date(df$TRADEDATE)
df$CLOSE <- as.numeric(df$CLOSE)
df$VALUE <- as.numeric(df$VALUE)
head(df)
df <- df %>% arrange(TRADEDATE)

ts_close <- ts(df$CLOSE, frequency = 1)
ts_value <-ts(df$VALUE, frequency = 1)
p1 <- autoplot(ts_close) + ggtitle("Индекс МосБиржи нефти и газа, на момент закрытия")
p2 <- autoplot(ts_value) + ggtitle("Оборот МосБиржи нефти и газа, на момент закрытия")
p1/p2


p1 <- ggAcf(ts_close)
p2 <- ggPacf(ts_close)
p1/p2
adf.test(ts_close)

fit_arima <- auto.arima(ts_close, seasonal = FALSE, stepwise = TRUE)
summary(fit_arima)
checkresiduals(fit_arima)

if(length(coef(fit_arima)) == 0) {
  cat("Формула: y_t = y_{t-1} + epsilon_t\n")
} else {
  cat("Формула: y_t = ", coef(fit)[1], "+ y_{t-1} + epsilon_t\n")
}

forecast_ret <- forecast(fit_arima, h = 5)
plot(ts_close, main="Граф без прогноза")
plot(forecast_ret, main = 'Граф с прогнозом', xlim = c(340, 360))

