df_numeric_vals = subset(songs_normalize, select = -c(artist, song, explicit, year, genre, mode, key))
View(df_numeric_vals)

######## Grafico de boxplots de variables numéricas
par(mfrow = c(4,3))
boxplot(df_numeric_vals$duration_ms, outline = FALSE)
title("Duración en ms")
boxplot(df_numeric_vals$popularity, outline = FALSE)
title("Indice de popularidad")
boxplot(df_numeric_vals$danceability, outline = FALSE)
title("Indice de bailabilidad")
boxplot(df_numeric_vals$energy, outline = FALSE)
title("Indice de energia")
boxplot(df_numeric_vals$loudness, outline = FALSE)
title("Indice de loudness")
boxplot(df_numeric_vals$speechiness, outline = FALSE)
title("Indice de speechines")
boxplot(df_numeric_vals$acousticness, outline = FALSE)
title("Indice de acousticness")
boxplot(df_numeric_vals$instrumentalness, outline = FALSE)
title("Indice de instrumentalidad")
boxplot(df_numeric_vals$liveness, outline = FALSE)
title("Indice de vida")
boxplot(df_numeric_vals$valence, outline = FALSE)
title("Indice de valance")
boxplot(df_numeric_vals$tempo, outline = FALSE)
title("Indice de tempo")



####### Grafico de los histogramas de variables numéricas


### Matriz de correlación
cor_mt = cor(df_numeric_vals)
library(corrplot)
corrplot(cor_mt, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

linear_mod = lm(energy ~ ., data = df_numeric_vals)
summary(linear_mod)

step(linear_mod, direction = "both", trace = 1)

#### Linear Model para variable respuesta energy
Gods_plan = lm(formula = energy ~ danceability + loudness + acousticness + 
                 instrumentalness + liveness + valence + tempo, data = df_numeric_vals)
summary(Gods_plan)

## Validacion de condiciones del modelo Gods_plan
library(ggfortify)
autoplot(Gods_plan)


#######################
## PCA
library(MASS)
library(factoextra)
mod_pca = prcomp(df_numeric_vals, scale. = TRUE)
mod_pca$sdev
summary(mod_pca)

fviz_pca_var(mod_pca, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
