# Seleccionamos directorio

setwd("C:/Users/maria/Desktop")

options(scipen = 999)


# Librer�as

library(dplyr)
library(ggpubr)


# Leemos dataset

superservicios <- read.csv('Superservicios.csv')

names(superservicios) <- c("id_empresa", "nombre", "periodo", "a�o",
                           "cod_localidad", "tipo_tecnologia", "km_red_interconexion", "capacidad_kw",
                           "fecha", "horas_servicio", "energia_entregada")

# Convertimos categ�ricos a num�ricos

superservicios$capacidad_kw = as.numeric(gsub(",", "", superservicios$capacidad_kw))
superservicios$energia_entregada = as.numeric(gsub(",", "", superservicios$energia_entregada))




# 1. Estudie si en las ZNI (zonas no interconectadas) la energ�a suministrada, la capacidad de suministro 
# y los km de interconexi�n han cambiado en el tiempo.

# Energ�a suministrada

energia_suministrada <- superservicios %>% group_by(a�o) %>% summarise(mean = mean(energia_entregada, na.rm = TRUE), 
                                                                       sd = sd(energia_entregada, na.rm = TRUE))


ggboxplot(superservicios, x = "a�o", y = "energia_entregada", 
          color = "a�o",
          ylab = "Energ�a Entregada", xlab = "A�o")


# Quitamos outliers

superservicios <- superservicios %>% mutate(energia_entregada = ifelse(energia_entregada > 100, NA, energia_entregada))

energia_suministrada <- superservicios %>% group_by(a�o) %>% summarise(mean = mean(energia_entregada, na.rm = TRUE), 
                                                                       sd = sd(energia_entregada, na.rm = TRUE))


ggboxplot(superservicios, x = "a�o", y = "energia_entregada", 
          color = "a�o",
          ylab = "Energ�a Entregada", xlab = "A�o")


# Supongamos que no tiene outliers

res.aov <- aov(energia_entregada ~ a�o, data = superservicios)
summary(res.aov)




# �Es la capacidad instalada de generaci�n de energ�a igual en todas los departamentos?

res.aov <- aov(capacidad_kw ~ cod_localidad, data = superservicios)
summary(res.aov)

var.test((superservicios %>% filter(cod_localidad==9500100000002))$capacidad_kw,
         (superservicios %>% filter(cod_localidad==4427902100022))$capacidad_kw)

capacidad_kw <- superservicios %>% group_by(cod_localidad) %>% summarise(mean = mean(capacidad_kw, na.rm = TRUE), 
                                                                                 sd = sd(capacidad_kw, na.rm = TRUE))


ggboxplot(superservicios, x = "a�o", y = "energia_entregada", 
          color = "a�o",
          ylab = "Energ�a Entregada", xlab = "A�o")


# Quitamos outliers

superservicios <- superservicios %>% mutate(energia_entregada = ifelse(energia_entregada > 100, NA, energia_entregada))

energia_suministrada <- superservicios %>% group_by(a�o) %>% summarise(mean = mean(energia_entregada, na.rm = TRUE), 
                                                                       sd = sd(energia_entregada, na.rm = TRUE))


ggboxplot(superservicios, x = "a�o", y = "energia_entregada", 
          color = "a�o",
          ylab = "Energ�a Entregada", xlab = "A�o")


# Supongamos que no tiene outliers

res.aov <- aov(energia_entregada ~ a�o, data = superservicios)
summary(res.aov)
