###LEER SET DE DATOS
#Importar set de datos
dataset = read.csv('Social_Network_Ads.csv')
#Hacer un subset
dataset = dataset[, 3:5]

#Si es que el dataset viene con categorias en vez de numeros, transformamos las categorias a numeros.
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

###CREAR TRAIN Y TEST SET####
library(caTools)
set.seed(123)
#Primero se coloca la variable dependiente. Split ratio es para el train set
split = sample.split(dataset$Purchased, SplitRatio = 0.75)   
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


###ESCALADOR####
#Dado que las columnas 1 y 4 se crearon a partir de las variables categoricas, no hay que escalarlas
#Recordar, en R los indices parten de 1 y no 0
training_set[, -3] = scale(training_set[, -3])
test_set[, -3] = scale(test_set[, -3])


###NAIVE BAYES#####
#Entrenar el modelo
library(e1071)
clf = naiveBayes(x = training_set[-3], y = training_set$Purchased)


#Predecir nuevos valores
y_pred = predict(clf, newdata = test_set[-3])

#Matriz de confusion
cm = table(test_set[,3], y_pred)

###SOLUCIONAR LA VISUALIZACION DE DATOS####