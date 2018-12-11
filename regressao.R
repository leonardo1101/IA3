# Data source: http://archive.ics.uci.edu/ml/datasets/Forest+Fires

# Informações dos atributos: 
# 
# 1. X - Coordenada espacial do eixo x no mapa do parque de Montesinho: 1 a 9 
# 2. Y- Coordenada espacial do eixo Y no eixo do mapa do parque de Montesinho: 2 a 9 
# 3. month - mês do ano: "jan" a "dec" 
# 4. day - dia da semana: "mon" a "sol" 
# 5. FFMC - índice FFMC do sistema FWI: 18.7 a 96.20 
# 6. DMC - índice DMC do sistema FWI: 1.1 a 291.3 
# 7. DC - Índice DC do sistema FWI: 7.9 a 860.6 
# 8. ISI - índice ISI do sistema FWI: 0.0 a 56.10 
# 9. temp - temperatura em graus Celsius: 2.2 a 33.30 
# 10. RH - umidade relativa em %: 15.0 a 100
# 11. wind - velocidade do vento em km / h: 0.40 a 9.40 
# 12. rain - chuva externa em mm / m2: 0.0 a 6.4 
# 13. area - a área queimada da floresta (em ha): 0.00 a 1090.84 


library('rpart')
library('rpart.plot')
library('caret')
source('metricas.R')

regression <- function() {
  dataFrame <- read.table('Datasets/forestfires.csv', header = TRUE, sep = ",")
  
  sample <- createDataPartition(dataFrame$area, p = 0.75, list = FALSE)
  
  trainSample <- dataFrame[sample,]
  testSample <- dataFrame[-sample,]
  
  model <- rpart(formula = area ~  X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain, 
                 data = trainSample, method = "anova", control = rpart.control(minsplit = 1), 
                 parms = list(split = "Information"), model = TRUE)
  
  predicted <- predict(model, testSample, type = "vector")
  
  error <- mse(testSample$area, predicted)
  
  result <- list()
  result$model <- model
  result$mse <- error
  
  return(result)
}

regressionResult <- regression()
cat("Método de Regreção:\n")
cat("Erro Médio Quadrático: ",regrResult$mse,"\n")

rpart.plot(regressionResult$model)
