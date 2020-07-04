#Importar set de datos
dataset = read.csv('Position_Salaries.csv')
#Quitamos las columnas que no sirven
dataset = dataset[2:3]


#Ajustar el random forest regressor
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 400)

#Predecir valores usando el modelo
#Como la funcion predict solo acepta tablas y vectores, hay que hacer una tabla con solo 1 valor para predecir
y_predict = predict(regressor, data.frame(Level = 6.5))  


#Visualizar los resultados del Random forest regressor (recordar que el modelo es una funcion escalonada) 
library(ggplot2)

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Desicion Tree Regression)') +
  xlab('Level') +
  ylab('Salary')
