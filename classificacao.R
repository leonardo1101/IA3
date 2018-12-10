library('rpart')
library('rpart.plot')
library('caret')
source('metricas.R')

classification <- function() {
  dataFrame <- read.table('Datasets/cmc.csv', header = TRUE, sep = ",")
  sample <- createDataPartition(dataFrame$Contraceptive_Method_Used, p = 0.75, list = FALSE)
  
  trainSample <- dataFrame[sample,]
  testSample <- dataFrame[-sample,]
  
  model <- rpart(formula = Contraceptive_Method_Used ~  Wife_Age + Wife_Education + Husband_Education + Number_Children_Born + Wife_Religion + Wife_Work + Husband_Occupation + Standard_Of_Living + Media_Exposure, 
                 data = trainSample, method = "class", control = rpart.control(minsplit = 1), 
                 parms = list(split = "Information"), model = TRUE)
  
  predicted <- predict(model, testSample, type = "class")
  
  confMat <- table(predicted=predicted, truth=testSample$Contraceptive_Method_Used)
  accuracy <- accuracy(confMat) 
  precision <- precision(confMat)
  
  result <- list()
  result$model <- model
  result$accuracy <- accuracy
  result$precision <- precision
  
  return(result)
}
classResult <- classification()
cat("Método de Classificação:\n")
cat("Acurácia: ",classResult$accuracy,"\n" )
cat("Precisão: ",classResult$precision,"\n\n")
rpart.plot(classResult$model)
