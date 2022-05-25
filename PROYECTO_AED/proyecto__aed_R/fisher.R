library(tidyverse)

library(readxl)
songs_normalize_genre_fixed <- read_excel("C:/Users/MEL/Desktop/Universidad/Analisis estadistico de datos/Aed_Talleres_Proyecto/PROYECTO_AED/proyecto__aed_R/songs_normalize_genre_fixed.xlsx", 
                              col_types = c("text", "text", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text"))
View(songs_normalize_genre_fixed)
dataset <- songs_normalize_genre_fixed

###########
#Creando la nueva variable categorica de popularity
# Baja de 0 a 30 
#Media de 31 a 60
#Alta de 61-86

column_pop = songs_normalize_genre_fixed$popularity
pop_cat = songs_normalize_genre_fixed
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
songs_normalize_genre_fixed <- pop_cat
View(songs_normalize_genre_fixed)
datasetWithoutCategorical <- songs_normalize_genre_fixed
View(datasetWithoutCategorical)
#Creando la nueva variable categorica de popularity
############

# Remove Categoriccal variables from the dataset, save that new dataset
datasetWithoutCategoricalFisherWithGenre <- dataset[,c(3,7,8,10,12:18)]
datasetWithoutCategoricalFisherWithPopularity <- dataset[,c(3,6,7,8,10,12:17)]
View(datasetWithoutCategoricalFisherWithGenre)
glimpse(songs_normalize_genre_fixed)
glimpse(datasetWithoutCategorical)

# Exploratory Analysis
ggplot(dataset, aes(x = loudness, y = popularity)) +
  geom_point() + 
  facet_wrap(~genre)

ggplot(dataset, aes(x = acousticness, y = energy)) + geom_point()
ggplot(dataset, aes(x = acousticness)) + geom_histogram()

#datasetBS <- dataset %>% 
#  filter(acousticness == "Eminem")

# ggplot(datasetBS, aes(x = year, y = popularity, color = factor(genre))) + geom_point()


# Fisher linear discriminant

# Partition dataSet (training and testing)
set.seed(123)
ind <- sample(2, nrow(datasetWithoutCategoricalFisherWithGenre),
              replace = TRUE,
              prob = c(0.75, 0.25))

training <- datasetWithoutCategoricalFisherWithGenre[ind==1,]
testing <- datasetWithoutCategoricalFisherWithGenre[ind==2,]
dim(testing)

View(training)
View(datasetWithoutCategoricalFisherWithGenre)

# Things we might want to consider with LDA

# Inspecting the univariate distributions of each variable and make sure 
# that they are normally distribute. If not, you can transform them using log 
# and root for exponential distributions and Box-Cox for skewed distributions.

# removing outliers from your data and standardize the variables to make their 
# scale comparable.
library(MASS)
library(fBasics)

modelLDA <- lda(genre~., data = training)
modelLDA

# Confusion matrix

nuevas_obs = subset(testing, select = -c(genre))

predicciones <- predict(object = modelLDA, newdata = nuevas_obs, method = "predictive")
t = table(testing$genre, predicciones$class, dnn = c("Clase real", "Clase predicha"))
t
aper = (length(testing$duration_ms)-tr(t))/length(testing$duration_ms)


## Histograma de las predicciones según el discriminante lineal
ldahist(data = predicciones$x[,2], g=testing$genre)


# Now we will use Quadratic discriminant analysis - QDA

# In contrast, QDA is recommended if the training set is very large, so that the
# variance of the classifier is not a major issue, or if the assumption of a common 
# covariance matrix for the K classes is clearly untenable (James et al. 2014).

###############################################

# Remove all the categorical variables from the dataset
glimpse(dataset)
datasetWithoutCategorical <- dataset[,c(3,7,8,10,12:17)]
glimpse(datasetWithoutCategorical)

typeof(datasetWithoutCategorical)
dim(datasetWithoutCategorical)
datasetWithoutCategorical <- matrix(unlist(datasetWithoutCategorical), ncol = 10, nrow = 1971)
dim(A)
typeof(A)


# Get the eigenvectors of the dataset without the categorical values 
datasetWithoutCategorical.eigens <- eigen(cov(datasetWithoutCategorical))$vectors
dim(datasetWithoutCategorical.eigens)

FixedDataSetWithEqualCovariances <- datasetWithoutCategorical %*% datasetWithoutCategorical.eigens
dim(FixedDataSetWithEqualCovariances)
glimpse(FixedDataSetWithEqualCovariances)

# Convert it to dataframe
FixedDataSetWithEqualCovariances <- as.data.frame(FixedDataSetWithEqualCovariances)
glimpse(FixedDataSetWithEqualCovariances)

typeof(FixedDataSetWithEqualCovariances)
# Add genre variable to the new fixed dataset
FixedDataSetWithEqualCovariancesGenre = cbind(FixedDataSetWithEqualCovariances,genre = datasetWithoutCategoricalFisherWithGenre$genre) 
glimpse(FixedDataSetWithEqualCovariancesGenre)

FixedDataSetWithEqualCovariancesGenre = as.data.frame(FixedDataSetWithEqualCovariancesGenre)
glimpse(FixedDataSetWithEqualCovariancesGenre)

# Fisher linear discriminant

# Partition dataSet (training and testing)
set.seed(123)
ind <- sample(2, nrow(FixedDataSetWithEqualCovariancesGenre),
              replace = TRUE,
              prob = c(0.75, 0.25))

training <- FixedDataSetWithEqualCovariancesGenre[ind==1,]
testing <- FixedDataSetWithEqualCovariancesGenre[ind==2,]
dim(testing)
dim(training)

View(training)
View(FixedDataSetWithEqualCovariancesGenre)

# Things we might want to consider with LDA

# Inspecting the univariate distributions of each variable and make sure 
# that they are normally distribute. If not, you can transform them using log 
# and root for exponential distributions and Box-Cox for skewed distributions.

# removing outliers from your data and standardize the variables to make their 
# scale comparable.
library(MASS)
library(fBasics)

modelLDAEqualV <- lda(genre~., data = training)
modelLDAEqualV

# Confusion matrix

nuevas_obs = subset(testing, select = -c(genre))

predicciones <- predict(object = modelLDAEqualV, newdata = nuevas_obs, method = "predictive")
t = table(testing$genre, predicciones$class, dnn = c("Clase real", "Clase predicha"))
t
aper = (length(testing$genre)-tr(t))/length(testing$genre)


## Histograma de las predicciones según el discriminante lineal
ldahist(data = predicciones$x[,2], g=testing$genre)
par(mar=c(1, 1, 1, 1))
