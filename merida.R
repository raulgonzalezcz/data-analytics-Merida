#install.packages("readr")
#install.packages("ggplot2")
library("readr")
library("ggplot2")
library(class)
sismo <- read.csv("/home/raul/Escritorio/Sismica/datos.csv", sep="\t", header=F)
tiempo <- read.csv("/home/raul/Escritorio/Sismica/Horizonte.csv", sep="\t", header=F)
head(sismo,5)
head(tiempo,5)

str(tiempo)
nrow(sismo)
ncol(sismo)
observations <- nrow(tiempo)

plot(sismo[,1], type="l")

#Creating Training and Test data set. Training data will be used to build model whereas test data will be used for validation and optimisation of model by tuning k value.

dat.d <- sample(1:nrow(sismo),size=nrow(sismo)*0.7,replace = FALSE) #random selection of 70% data.
head(sismo[,2])
head(dat.d)
str(dat.d)
summary(dat.d)

train_set <- sismo[dat.d,] # 70% training data
#test_set <- sismo[-dat.d,] # remaining 30% test data

#Now creating seperate dataframe for 'Creditability' feature which is our target.
train.gc_labels <- sismo[dat.d,1]
test.gc_labels  <- sismo[-dat.d,1]  

# Segmentación: Entrenando el modelo
#train_set <- tiempo[1]
test_set <- sismo[2]
str(train_set)
str(test_set)
str(sismo[,2])
str(tiempo[,1])

#To identify optimum value of k, generally square root of total no of observations
noGroups <- sqrt(nrow(tiempo))

#knn(train = train_set, test = test_set, cl = train_set, k = noGroups)
knn_1 <- knn(train = train_set[,1], test = test_set, cl = train_set[,1], k = noGroups)
knn_2 <- knn(train = train_set[,2], test = test_set, cl = train_set[,2], k = noGroups+1)

str(train_set[,1])

#Transformando horizonte.csv
horizonte <- read.csv("/home/raul/Escritorio/Sismica/Horizonte.csv", sep="\t", header=F)
matrizReal <- matrix(0,nrow=625,ncol=461)
matrizReal
str(horizonte[,1])
cont <- 1
dim(matrizReal)
for (i in (horizonte[,1])){
  valor <- floor( (i - 1000) / 4)
  matrizReal[valor,cont] = 1
  cont <- cont +1
}
print(matrizReal[,461])
head(horizonte)

#install.packages("raster")
#Comprobando proyección
library(raster)
plot(raster(matrizReal))
