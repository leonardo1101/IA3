# Data source: http://archive.ics.uci.edu/ml/datasets/Forest+Fires

#  Attribute information:
#
#1. X - x-axis spatial coordinate within the Montesinho park map: 1 to 9
#2. Y - y-axis spatial coordinate within the Montesinho park map: 2 to 9
#3. month - month of the year: "jan" to "dec" 
#4. day - day of the week: "mon" to "sun"
#5. FFMC - FFMC index from the FWI system: 18.7 to 96.20
#6. DMC - DMC index from the FWI system: 1.1 to 291.3 
#7. DC - DC index from the FWI system: 7.9 to 860.6 
#8. ISI - ISI index from the FWI system: 0.0 to 56.10
#9. temp - temperature in Celsius degrees: 2.2 to 33.30
#10. RH - relative humidity in %: 15.0 to 100
#11. wind - wind speed in km/h: 0.40 to 9.40 
#12. rain - outside rain in mm/m2 : 0.0 to 6.4 
#13. area - the burned area of the forest (in ha): 0.00 to 1090.84 
#(this output variable is very skewed towards 0.0, thus it may make
#  sense to model with the logarithm transform). 

library('rpart')
library('rpart.plot')
library('caret')
source('metricas.R')

regression <- function() {
  dataFrame <- read.table('Datasets/forestfires.csv', header = TRUE, sep = ",")
  sample <- createDataPartition(dataFrame$Contraceptive_Method_Used, p = 0.75, list = FALSE)
  
  trainSample <- dataFrame[sample,]
  testSample <- dataFrame[-sample,]
  
  model <- rpart(formula = Contraceptive_Method_Used ~  X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain + area, 
                 data = trainSample, method = "anova", control = rpart.control(minsplit = 1), 
                 parms = list(split = "Information"), model = TRUE)
  
  predicted <- predict(model, testSample, type = "vector")
  
  error <- mse(testSample$Contraceptive_Method_Used, predicted)
  
  result <- list()
  result$model <- model
  result$mse <- error
  
  return(result)
}

regressionResult <- regression()
cat("Método de Regreção:\n")
cat("Erro Médio Quadrático: ",regrResult$mse,"\n")

rpart.plot(regressionResult$model)
