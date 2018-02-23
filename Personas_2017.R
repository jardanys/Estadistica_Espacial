#****************************************************************************************************************
#*****************************    ESTADÍSTICA ESPACIAL - KRIGING UNIVERSAL     **********************************
#****************************************************************************************************************
source("fun.R")
options(scipen=999)

# 0. Cargue de librerías ####
#*********************************************
load.lib("dplyr", "scales", "tidyr", "rgeos", "sp", "maptools", "car", "geoR", 
         "gstat", "stringr", "reshape2")
load.lib("rgeos","sp","maptools","car","geoR","gstat","RColorBrewer")


# 1. Cargue información ####
#*********************************************
tipos<-c("character", "character", "character", "character", "character", "character", "numeric",
         "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "character", 
         "character")

## Cambiamos a lectura de la base. En esta nueva BD, las coordenadas vienen encerradas en comillas, así que
## resulta necesario leerlas como caracter y luego pasar a número
BD <- read.table("BD.txt", sep = "\t", header = T, dec = ",", colClasses = tipos, quote="\"")
str(BD)
head(BD)

colnames(BD) <- c("Id_Empresa", "RazonSocial", "FechaAfiliacion", "FechaRetiro", "Piramide.1", "Piramide.2_Actual", 
                  "AÑO", "MES", "Afiliados", "A", "B", "C", "ZONA", "Aportes", "X", "Y")

## corrige aportes
BD$Aportes <- str_replace(BD$Aportes, ",", ".")
BD$Aportes <- str_replace(BD$Aportes, "\\$ ", "")
BD$Aportes <- as.numeric(BD$Aportes)

## Coerción de coordenadas a clase numérica. Se observa que hay coordenadas nulas
BD$X <- as.numeric(str_replace(BD$X, ",", "."))
BD$Y <- as.numeric(str_replace(BD$Y, ",", "."))
BD$Y <- ifelse(BD$Y>1000000, 4.7316250, BD$Y)

Pers_2017<-BD %>% 
           filter(AÑO == 2017 & Piramide.2_Actual != "4.5 Transaccional" & 
                  Piramide.2_Actual != "4.6 Transaccional - Facultativo" & 
                  Piramide.2_Actual != "4.7 Transaccional - Independiente" & 
                  Piramide.2_Actual != "4.8 Transaccional - Pensionado" & 
                  Piramide.2_Actual != "4.9 Colsubsidio" &
                  Piramide.2_Actual !=  "4.3 Trans.Juridica Ent. 11 a 99 Trab." &
                  Piramide.2_Actual != "4.4 Trans.Natural Ent. 11 a 99 Trab.") %>% 
           select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
           group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
           summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T)) %>% 
           filter(ZONA == "ZONA CENTRO" | ZONA == "ZONA NORTE" | ZONA == "ZONA CHAPINERO" | ZONA == "ZONA SUR") %>%
           filter(Aportes_total > 0 & Afiliados_max > 0) %>%
           group_by(X, Y) %>%
           summarise(Aportes_total=sum(Aportes_total), Afiliados_max=sum(Afiliados_max))

summary(Pers_2017$X)
summary(Pers_2017$Y)

# 2. Cargue de capas ####
#*********************************************

# Capa de Bogotá sin localidad de sumapaz
bogota = readShapePoly("./localidades1/localidades_WGS84.shp") #readShapePoly("./barrios_SDP/barrios.shp")

# Puntos de ubicación empresas
xy = SpatialPoints(Pers_2017[c("X", "Y")])	


# 2. Análisis Gráfico ####
#*********************************************

# Gráfico de la capa y las ubicaciones de las empresas que se encuentran afiliadas
plot(bogota)
points(xy, pch = 3, cex = 0.3, col = "red")
title(main="Ubicación empresas en Bogotá")


# Análisis descriptivo para la cantidad de afiliados por empresa
#par(mfrow = c(1, 3))
hist(Pers_2017$Afiliados_max, freq = T, main = "Histograma", breaks=200, col="gray", xlim=c(0, 2000),
     xlab = "Afiliados x empresa", ylab = "Frecuencia")

hist(log(Pers_2017$Afiliados_max), freq = T, main = "", breaks=50, col="gray", xlim=c(0, log(5000)),
     xlab = "log(Afiliados) x empresa", ylab = "Frecuencia")


## Cláramente hay trendencia Log-normal
curve(dnorm(x, mean(log(Pers_2017$Afiliados_max)), sd(log(Pers_2017$Afiliados_max))), add = T)
boxplot(log(Pers_2017$Afiliados_max))
qqPlot(log(Pers_2017$Afiliados_max), ylab = "log(Afiliados) x empresa")

Pers_2017$Log_Afiliados <- log(Pers_2017$Afiliados_max)


## El gráfico de sp 
#************************************************************************************
# creamos un vector de límites
limites=c(min(Pers_2017$Log_Afiliados), quantile(Pers_2017$Log_Afiliados, probs = c(0.2, 0.4, 0.6, 0.8),type = 5), 
          max(Pers_2017$Log_Afiliados))
limites

# Gráfico para determinar estacionariedad. 
# En este caso resulta NO ESTacionaria prque los punticos no están formados aleatoriamente

##Creamos 'spatial data.frame'
datossp <- select(as.data.frame(Pers_2017), X, Y, Log_Afiliados)
coordinates(datossp) = c("X", "Y")
spplot(datossp, zcol="Log_Afiliados", cuts = limites)
#************************************************************************************


## 3. Análisis de estacionariedad ####
#*******************************************

# Al parecer la media de el log de los afiliados es constante sobre las dos coordenadas en Bogota
scatterplot(Log_Afiliados~X, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=Pers_2017)
scatterplot(Log_Afiliados~Y, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=Pers_2017)

# Se ajusta un modelo cuadrático para verificar si el proceso ES o NO estacionario.
# Se observa que ningún término es significativo
modelo1=lm(Log_Afiliados ~ X+Y+I(X^2)+I(Y^2)+I(X*Y), data=Pers_2017)
anova(modelo1)

# Ajustamos modelo con sólo variables de coordenadas.
# Se concluye que el proceso es estacionario ya que los valores no dependen de las coordenadas
modelo2=lm(Log_Afiliados ~ X+Y, data=Pers_2017)
anova(modelo2)


## 4. Análisis variograma ####
#*******************************************

# Creamos objeto geodata para semivariograma
data_afiliados <- select(as.data.frame(Pers_2017), X, Y, Log_Afiliados)
geo_afiliados = as.geodata(data_afiliados, coords.col = 1:2, data.col = 3)

# Estimación del variograma
var = variog(geo_afiliados, direction = "omnidirectional", max.dist = 0.28)

# Estimación variograma por lat y lon
var_lat = variog(geo_afiliados, direction = 90, unit.angle="degre", width = 100)
var_lon = variog(geo_afiliados, direction = 0, unit.angle="degre", width = 100)

# graficamos los semivariogramas
# Se observa que la variación no depende de la dirección
plot(var,main="Semivariograma", xlim=c(0, 0.5), type="l", ylim=c(0,4))
points(var_lat$u, var_lat$v, col=2, type="l")
points(var_lon$u, var_lon$v, col=3, type="l")
legend("bottomright",c("Omnidir","Latitud","Longitud"), col=1:3,pch=1, inset=0.03, box.lwd=0)

#*********************************
## Ajuste modelo semivariograma
#*********************************

## Dada la evidencia del semivariograma, el modelo es de apariencia pepita puro
var_eyefit <- eyefit(var)
var_eyefit

## Definición manual del modelo pepita puro
var_pepita <- vgm(1.8, "Nug", 0, nugget = 1.8)
var_pepita

#**********************
# Estimamos parámetros
#**********************

#Asignando valores iniciales
modelo3 <- variofit(var, ini=var_pepita, cov.model="pure.nugget", weights="equal")
modelo3

#Minimos cuadrados ponderados
modelo4 <- variofit(var, ini=variofit, cov.model="pure.nugget", weights="npairs")
modelo4

#Minimos cuadrados ponderados
modelo5 <- variofit(var, ini=variofit, cov.model="pure.nugget", weights="cressie")
modelo5

#Maxima verosimilitud
modelo6 <- likfit(geo_afiliados, ini=variofit, cov.model="pure.nugget", lik.method="ML")
modelo6

#Maxima verosimilitud restringida
modelo7 <- likfit(geo_afiliados, ini=variofit, cov.model="pure.nugget", lik.method="REML")
modelo7


#**********************
# Comparamos modelos
#**********************
plot(var)
lines(modelo3, max.dist = 800, col = 1)
lines(modelo4, max.dist = 800, col = 2)
lines(modelo5, max.dist = 800, col = 3)
lines(modelo6, max.dist = 800, col = 4)
lines(modelo7, max.dist = 800, col = 5)
legend("bottomright",legend = c("OLS", "WLS - npairs", "WLS - cressie", "ML", "REML"),col = 1:5, lwd = 2, inset = .03)


## 5. Validación cruzada ####
#*******************************************
# Se realiza validación cruzada para el modelo del variograma sobre el log(Afiliados)
# Como se realiza sobre los datos directamente, se cambia objeto del modelo
require(gstat)
modelo3_vgm <- as.vgm.variomodel(modelo3)
class(modelo3)
class(modelo3_vgm)

# aplicamos krige con validación cruzada ya sobre los datos originales
krig_ord1 <- krige.cv(Log_Afiliados ~ 1 , datossp, modelo3_vgm, maxdist = 0.28)
head(krig_ord1)
mape <- mean(abs(krig_ord1$residual)/krig_ord1$observed, na.rm = T)
mape # (error medio porc absoluto) Medida bastante mala, erro del pronóstico de cerca del 30%

#Se repite el proceso para los otros modelos candidatos...

9.03




## 6. Mapa resultados
#***************************************************

#Mapa para la precipitación
# cambia mapa respecto a precipitación
spplot(krig_ord1, c("var1.pred"), main = "Kriging Ordinario - Afiliados Bogotá", contour = FALSE, 
       labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))
# cambia el mapa con respecto  a las vars de predicción
spplot(krig_ord1, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, 
       labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

## EL KRILIN ORDINARIO PAILA!!!!

#Para visualizar los puntos de las estaciones
li = list("sp.polygons", bogota)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_ord1, c("var1.pred"), main = "Kriging Ordinario - Afiliados Bogotá", 
       sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", 
       col.regions = terrain.colors(100))



