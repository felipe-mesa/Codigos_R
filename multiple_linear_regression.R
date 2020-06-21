#Importar datos
dataset = read.csv('50_Startups.csv')


#Categorizar datos (la regresion se encarga de convertirlas en binarias)
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3))


#Dividir el dataset en train set y test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Aplicamos regresion multiple
#varaibles 'R.D.Spend + Administration + Marketing.Spend + State'
regressor <- lm(formula = Profit ~ ., data = training_set) #El punto significa que usamos todas las variables

#Informacion de la regresion (ecuacion, residuos, coeficientes y valor p)
summary(regressor)

#Dejamos solo las variables que son significativas
regressor2 = lm(formula = Profit ~ R.D.Spend, data = training_set) 

#Predecir los resultados del test set
y_pred = predict(regressor2, newdata = test_set)


#Backward Elimination para la eleccion de las variables que seran incluidas en el modelo. x es el set de datos y sl es el nivel de significancia
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}
#Nivel de significancia de 5%
SL = 0.05
#Se aplica la funcion de backward elimination
backwardElimination(training_set, SL)
