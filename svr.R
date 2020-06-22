#Importar set de datos
dataset = read.csv('Position_Salaries.csv')
#Quitamos las columnas que no sirven
dataset = dataset[2:3]

#Ajustar el SVR
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression')

#Predecir valores usando SVR
#Como la funcion predict solo acepta tablas y vectores, hay que hacer una tabla con solo 1 valor para predecir
y_predict = predict(regressor, data.frame(Level = 6.5))  


#Visualizar los resultados del SVR
library(ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')