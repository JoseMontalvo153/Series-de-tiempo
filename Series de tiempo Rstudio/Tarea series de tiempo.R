# Cargar bibliotecas necesarias
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("timetk")

library(forecast)
library(tseries)
library(ggplot2)
library(timetk)

library(shiny)
library(dplyr)
library(DBI)
library(odbc)
library(plotly)
library(shinydashboard)

# Conexión a la base de datos
con <- dbConnect(odbc::odbc(), "HANA_QB1", timeout = 10)

# Cargar los datos de lealtad al iniciar la app
loyalty_data <- reactive({
  dbGetQuery(con, "SELECT * FROM Y_BRA_ECOMMERCE.LOYALTY_PROGRAM")
})



# Cargar datos desde el enlace
url <- "https://raw.githubusercontent.com/lihkir/Data/main/TGLS.csv"
technoglass <- read.csv(url)

str(technoglass)

# Visualizar las primeras filas de los datos
head(technoglass)

# Como la columna con las fechas se llama 'Date' y no 'date', cambiar el nombre
colnames(technoglass)[colnames(technoglass) == "Date"] <- "date"

# Convertir la columna 'date' a tipo de dato de fecha
technoglass$date <- as.Date(technoglass$date)

# Visualizar datos de Technoglass
ggplot(data = technoglass, aes(x = date, y = Close)) +
  geom_line() +
  labs(title = "Precio de Technoglass (TGLS)",
       x = "Fecha",
       y = "Precio (USD)")


# Prueba de estacionariedad (ADF)
result <- adf.test(technoglass$close)
print('ADF Statistic:', result$statistic)
print('p-value:', result$p.value)

# Descomposición de la serie temporal
decomposition <- decompose(technoglass$close, type = 'additive', frequency = 30)
plot(decomposition)

# Dividir datos en entrenamiento y prueba
n_BTC <- nrow(technoglass)
n_test <- 28
train_size <- n_BTC - n_test
train <- technoglass$close[1:train_size]

# Definir horizontes de predicción
horizons <- c(7, 14, 21, 28)

# Iterar sobre cada horizonte de predicción
for (horizon in horizons) {
  # Modelado ARIMA
  model <- auto.arima(train, seasonal=FALSE)
  
  # Ajustar modelo ARIMA
  fit <- Arima(train, order=model$arma[c(1,6,2)])
  
  # Predicciones
  forecast <- forecast(fit, h=horizon)$mean
  
  # Evaluar predicciones
  test <- technoglass$close[(train_size + 1):(train_size + horizon)]
  mae <- mean(abs(test - forecast))
  mse <- mean((test - forecast)^2)
  mape <- mean(abs((forecast - test) / test)) * 100
  rmse <- sqrt(mse)
  
  cat(paste("Predicciones para", horizon, "días:\n"))
  cat("Forecast:", forecast, "\n")
  cat("Mean Absolute Error:", mae, "\n")
  cat("Mean Squared Error:", mse, "\n")
  cat("Mean Absolute Percentage Error:", mape, "\n")
  cat("Root Mean Squared Error:", rmse, "\n")
  
  # Visualizar predicciones
  plot(technoglass$date[(train_size + 1):(train_size + horizon)], test, type='l', col='blue', xlab='Fecha', ylab='Precio (USD)', main=paste('Predicciones de Bitcoin (BTC) para', horizon, 'días'))
  lines(technoglass$date[(train_size + 1):(train_size + horizon)], forecast, col='red')
  legend('topright', legend=c('Actual', 'Forecast'), col=c('blue', 'red'), lty=1)
}
