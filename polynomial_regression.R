#Importar set de datos
dataset <- read.csv('Position_Salaries.csv')
#Quitamos las columnas que no sirven
dataset = dataset[2:3]


#Ajustar regresion lineal
lin_reg = lm(formula = Salary ~ ., data = dataset)

#Ajustar regresion polinomial
#Antes hay que agregar una columna que represente la variable independiente al cuadrado
dataset$Level2 = dataset$Level^2   #Se repite esta linea para subir el grado de la ecuacion del modelo
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ ., data = dataset)


#Visualizar los resultados de la regresion lineal
library(ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')



#Visualizar los resultados de la regresion polinomial
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')


#Predecir valores usando regresion lineal
#Como la funcion predict solo acepta tablas y vectores, hay que hacer una tabla con solo 1 valor para predecir
y_predict = predict(lin_reg, data.frame(Level = 6.5))  

#Predecir valores usando regresion polinomial
#Como la funcion predict solo acepta tablas y vectores, hay que hacer una tabla con solo 1 valor para predecir
#PERO, como es una regresion polinomial, hay que ajustarlo al grado de la ecuacion del modelo
y_predict_2 = predict(poly_reg, data.frame(Level = 6.5,
                                           Level2 = 6.5^2,
                                           Level3 = 6.5^3,
                                           Level4 = 6.5^4))  