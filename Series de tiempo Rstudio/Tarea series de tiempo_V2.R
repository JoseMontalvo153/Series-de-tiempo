install.packages(c("forecast", "tseries", "quantmod", "ggplot2"))
library(forecast)
library(tseries)
library(quantmod)
library(ggplot2)

# Obtener los datos de las acciones de Tecnoglass
getSymbols("TGLS", src = "yahoo", from = "2010-01-01", to = Sys.Date())
# Convertir a xts para facilitar el manejo de series de tiempo
tecnoglass_stock <- as.xts(TGLS)


fit_arima_and_forecast <- function(stock_data, h) {
  # Dividir los datos en conjuntos de entrenamiento y prueba
  train_data <- window(stock_data, end = length(stock_data) - h)
  test_data <- window(stock_data, start = length(stock_data) - h + 1)
  
  # Ajustar el modelo ARIMA al conjunto de entrenamiento
  arima_model <- auto.arima(train_data)
  
  # Realizar la predicción con el horizonte especificado
  forecast_data <- forecast(arima_model, h = h)
  
  # Retornar los datos de entrenamiento, prueba y las predicciones
  return(list(train_data = train_data, test_data = test_data, forecast_data = forecast_data))
}


# Predecir y graficar para cada horizonte
horizons <- c(7, 14, 21, 28)

for (h in horizons) {
  # Ajustar el modelo ARIMA y predecir
  results <- fit_arima_and_forecast(tecnoglass_stock, h)
  
  # Preparar los datos para el gráfico
  actual_data <- c(as.numeric(results$train_data), as.numeric(results$test_data))
  fitted_data <- c(fitted(results$arima_model), results$forecast_data$mean)
  
  # Crear el gráfico con ts_plot
  ts_plot(forecast_data = results$forecast_data, actual_data = actual_data, fitted_data = fitted_data, 
          title = paste("ARIMA Forecast for Tecnoglass Stock Prices -", h, "Day Horizon"))
}