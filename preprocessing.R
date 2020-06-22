###IMPORTAR DATOS####
#Importar set de datos
dataset = read.csv('Data.csv')
#Hacer un subset
dataset = dataset[, 2:3]


#Describir el dataset y contar datos faltantes
summary(dataset)

###DATOS FALTANTES####
#Se hace un ifelse, se aplica la funcion mean a los datos de la columna que tengan NA

#Datos faltantes para columna "Age"
dataset$Age <- ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)

#Datos faltantes para columna "Salary"
dataset$Salary <- ifelse(is.na(dataset$Salary),
                     ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Salary)


###VARIABLES CATEGORICAS####
#Se convierte la variable categorica en factores, especificando levels y labels

dataset$Country <- factor(dataset$Country,
                          levels = c('France', 'Spain', 'Germany'),
                          labels = c(1, 2, 3))

dataset$Purchased <- factor(dataset$Purchased,
                          levels = c('No', 'Yes'),
                          labels = c(0, 1))

###CREAR TRAIN Y TEST SET####
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)   #split ratio es para el train set
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

###ESCALADOR####
#Dado que las columnas 1 y 4 se crearon a partir de las variables categoricas, no hay que escalarlas
#Recordar, en R los indices parten de 1 y no 0
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])
