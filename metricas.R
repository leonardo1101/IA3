# acurácia = soma(true positive + true negative)/soma(todos)
accuracy <- function(confusionMatr) {
  return(sum(diag(confusionMatr))/sum(confusionMatr))
}
  
# precisão = soma(true positive)/soma(true positive + false positive)
precision <- function(confusionMatr) {
  return(sum(confusionMatr[2,2])/sum(confusionMatr[2,]))
}

# erro quadrático medio = media(dados_reais - dados_preditos)**2
mse <- function(real, predicted) {
  return(mean(real-predicted)^2)
} 
