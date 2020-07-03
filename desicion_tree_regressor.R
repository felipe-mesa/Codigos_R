#Importar set de datos
dataset = read.csv('Position_Salaries.csv')
#Quitamos las columnas que no sirven
dataset = dataset[2:3]


#Ajustar el desicion tree regressor
library(rpart)
regressor = rpart(formula = Salary ~ ., data = dataset, control = rpart.control(minsplit = 1))


#Predecir valores usando SVR
#Como la funcion predict solo acepta tablas y vectores, hay que hacer una tabla con solo 1 valor para predecir
y_predict = predict(regressor, data.frame(Level = 6.5))  


#Visualizar los resultados del Desicion tree regressor (recordar que el modelo es una funcion escalonada) 
library(ggplot2)

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Desicion Tree Regression)') +
  xlab('Level') +
  ylab('Salary')
