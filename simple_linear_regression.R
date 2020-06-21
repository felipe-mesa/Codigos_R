#Importar datos
dataset = read.csv('Salary_Data.csv')

#Dividir el dataset en train set y test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Aplicamos regresion simple
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

#Informacion de la regresion (ecuacion, residuos, coeficientes y valor p)
summary(regressor)

#Predecir los resultados del test set
y_pred = predict(regressor, newdata = test_set)

#Visualizar los resultados del training o test set con ggplot
library(ggplot2)

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),                                 #Crea el grafico
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),   #Crea una linea con la prediccion de training set
                colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +                                                   #Titulo
  xlab('Years of experience') +                                                                      #Eje X
  ylab('Salary')                                                                                     #Eje Y
  
