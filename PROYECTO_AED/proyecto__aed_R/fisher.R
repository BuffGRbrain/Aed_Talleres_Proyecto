library(tidyverse)

library(readxl)
songs_normalize <- read_excel("C:/Users/MEL/Desktop/Universidad/Analisis estadistico de datos/Aed_Talleres_Proyecto/PROYECTO_AED/proyecto__aed_R/songs_normalize.xlsx", 
                              col_types = c("text", "text", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text"))
View(songs_normalize)
dataset <- songs_normalize

###########
#Creando la nueva variable categorica de popularity
# Baja de 0 a 30 
#Media de 31 a 60
#Alta de 61-86

column_pop = songs_normalize$popularity
pop_cat = songs_normalize
contador = 0 #Indice para saber donde poner la nueva cat O PUEDO SIMPLEMENTE REEMPLAZAR
for (i in column_pop){
  contador = contador+1;
  if (i<=30){
    #Agregar fila con popularidad baja que es un 0
    pop_cat$popularity[contador] = 0
  }else if(i<=60){#Pues como no entro en el anterior ya es mayor a 30 
    #Agregar fila con popularidad mediaque es un 1
    pop_cat$popularity[contador] = 1
  }else{#es decir es mayor a 60 
    #Agregar fila con popularidad alta que es un 2
    pop_cat$popularity[contador] = 2
  }
}
View(pop_cat)
songs_normalize <- pop_cat
dataset <- songs_normalize
view(dataset)
#Creando la nueva variable categorica de popularity
############

# Remove Categoriccal variables from the dataset, save that new dataset
datasetWithoutCategoricalFisherWithGenre <- dataset[,c(3,7,8,10,12:18)]
datasetWithoutCategoricalFisherWithPopularity <- dataset[,c(3,6,7,8,10,12:17)]

glimpse(songs_normalize)
glimpse(datasetWithoutCategorical)

# Exploratory Analysis
ggplot(dataset, aes(x = loudness, y = popularity)) +
  geom_point() + 
  facet_wrap(~year)

ggplot(dataset, aes(x = acousticness, y = energy)) + geom_point()
ggplot(dataset, aes(x = acousticness)) + geom_histogram()

#datasetBS <- dataset %>% 
#  filter(acousticness == "Eminem")

# ggplot(datasetBS, aes(x = year, y = popularity, color = factor(genre))) + geom_point()

# PCA

# We need to exclude the categorical variables because PCA works best without them
# these categorical variables are: 
# - Artist
# - song
# - explicit
# - genre

songDataSet.pca <- prcomp(datasetWithoutCategorical, center = TRUE,scale. = TRUE)
summary(songDataSet.pca)


# Fisher linear discriminant

library(MASS)
library(heplots)

# Clean dataset
dataSetWihtoutCategorical <- dataset[,c(3,7,8,10,12:17)]
glimpse(dataSetWihtoutCategorical)


bartlett.test(list(dataSetWihtoutCategorical$loudness, 
                   dataSetWihtoutCategorical$energy, 
                   dataSetWihtoutCategorical$acousticness))
# After making the barlett test, we conclude that there is at least a difference
# between the covariance matrices of each population

# To fix this difference, we will make an adjustment in our database by multiplying
# our sample with matrix P, where P is the matrix of eigenvectors of the covariance
# matrix of the sample.
glimpse(datasetWithoutCategoricalFisherWithGenre)
glimpse(datasetWithoutCategoricalFisherWithPopularity)
glimpse(dataSetWihtoutCategorical)
dim(dataSetWihtoutCategorical)

dataSetWihtoutCategorical.covarianceMatrix = cov(dataSetWihtoutCategorical)
dataSetWihtoutCategorical.covarianceMatrix

# Create eigen vector matrix
vec = eigen(cov(dataSetWihtoutCategorical))$vectors
p = cbind(vec[,1],vec[,2],vec[,3],vec[,4],vec[,5],vec[,6],vec[,7],vec[,8],vec[,9],vec[,10])
dim(p)

glimpse(dataSetWihtoutCategorical)
adjusted_Sample = dataSetWihtoutCategorical %*% p
dim(dataSetWihtoutCategorical)

# Test if covariance matrices are equal
# Partition dataSet (training and testing)
set.seed(123)
ind <- sample(2, nrow(datasetWithoutCategoricalButResponse),
              replace = TRUE,
              prob = c(0.75, 0.25))

training <- datasetWithoutCategoricalButResponse[ind==1,]
testing <- datasetWithoutCategoricalButResponse[ind==2,]
dim(testing)

# Things we might want to consider with LDA

# Inspecting the univariate distributions of each variable and make sure 
# that they are normally distribute. If not, you can transform them using log 
# and root for exponential distributions and Box-Cox for skewed distributions.

# removing outliers from your data and standardize the variables to make their 
# scale comparable.
library(MASS)
modelLDA <- lda(genre~., data = training)
modelLDA

# Confusion matrix
p2 <- predict(modelLDA, testing)$class
confmatrix1 <- table(Predicted = p2, Actual = testing$genre)
confmatrix1

# Now we will use Quadratic discriminant analysis - QDA

# In contrast, QDA is recommended if the training set is very large, so that the
# variance of the classifier is not a major issue, or if the assumption of a common 
# covariance matrix for the K classes is clearly untenable (James et al. 2014).

# We can apply the same but with QDA
modelQDA <- qda(genre~., data = training)

p3 <- predict(modelQDA, testing)$class
confmatrix2 <- table(Predicted = p3, Actual = testing$vegetation_type)
confmatrix2

library(fBasics)
tr(confmatrix1)
tr(confmatrix2)