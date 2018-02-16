#****************************************************************************************************************
#*****************************    ESTADÍSTICA ESPACIAL - KRIGING UNIVERSAL     **********************************
#****************************************************************************************************************
source("fun.R")
options(scipen=999)

# 0. Cargue de librerías ####
#*********************************************
load.lib("dplyr", "scales", "tidyr", "plotly", "rgeos", "sp", "maptools", "car", "geoR", 
         "gstat", "stringr", "reshape2")


# 1. Cargue información ####
#*********************************************
tipos<-c("factor", "character", "character", "character", "character", "character", "numeric",
         "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "numeric")

BD <- read.csv("BD.txt", sep = "\t", header = T, dec = ",", colClasses=tipos)
str(BD)

## corrige aportes
BD$Aportes <- str_replace(BD$Aportes, ",", ".")
BD$Aportes <- str_replace(BD$Aportes, "\\$ ", "")
BD$Aportes <- as.numeric(BD$Aportes)

head(BD$Aportes)
a <- BD %>% filter(AÑO == 2017) 
sum(a$Aportes)
sum(BD$Aportes, na.rm=T)

head(a)
table(a$Piramide.1)
barplot(table(a$Piramide.2_Actual))

suma <- a %>% select(Piramide.2_Actual, Aportes) %>% group_by(Piramide.2_Actual) %>% 
  summarise(Aportes_total = sum(Aportes, na.rm=T))

suma

table(BD$Piramide.1)
barplot(table(BD$Piramide.1))
table(BD$Piramide.2_Actual)
barplot(table(BD$Piramide.2_Actual))

BD_2014 <- BD %>% 
           filter( BD$AÑO == 2014 & BD$Piramide.2_Actual != "4.5 Transaccional"& 
                   BD$Piramide.2_Actual != "4.6 Transaccional - Facultativo" & 
                   BD$Piramide.2_Actual != "4.7 Transaccional - Independiente"& 
                   BD$Piramide.2_Actual != "4.8 Transaccional - Pensionado" & 
                   BD$Piramide.2_Actual != "4.9 Colsubsidio") %>%
           select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
           group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
           summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T))

BD_2015 <- BD %>% 
           filter( BD$AÑO == 2015 & BD$Piramide.2_Actual != "4.5 Transaccional" & 
                   BD$Piramide.2_Actual != "4.6 Transaccional - Facultativo" & 
                   BD$Piramide.2_Actual != "4.7 Transaccional - Independiente"& 
                   BD$Piramide.2_Actual != "4.8 Transaccional - Pensionado" & 
                   BD$Piramide.2_Actual != "4.9 Colsubsidio") %>% 
            select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
            group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
            summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T))

BD_2016 <- BD %>% 
           filter( BD$AÑO == 2016 & BD$Piramide.2_Actual != "4.5 Transaccional" & 
                   BD$Piramide.2_Actual != "4.6 Transaccional - Facultativo" & 
                   BD$Piramide.2_Actual != "4.7 Transaccional - Independiente" & 
                   BD$Piramide.2_Actual != "4.8 Transaccional - Pensionado" & 
                   BD$Piramide.2_Actual != "4.9 Colsubsidio") %>% 
           select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
           group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
           summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T))

BD_2017 <- BD %>% 
           filter( BD$AÑO == 2017 & BD$Piramide.2_Actual != "4.5 Transaccional" & 
                   BD$Piramide.2_Actual != "4.6 Transaccional - Facultativo" & 
                   BD$Piramide.2_Actual != "4.7 Transaccional - Independiente" & 
                   BD$Piramide.2_Actual != "4.8 Transaccional - Pensionado" & 
                   BD$Piramide.2_Actual != "4.9 Colsubsidio" &
                   BD$Piramide.2_Actual !=  "4.3 Trans.Juridica Ent. 11 a 99 Trab." &
                   BD$Piramide.2_Actual != "4.4 Trans.Natural Ent. 11 a 99 Trab.") %>% 
           select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
           group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
           summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T)) %>% 
           filter(ZONA == "ZONA CENTRO" | ZONA == "ZONA NORTE" | ZONA == "ZONA CHAPINERO" | ZONA == "ZONA SUR")

barplot(table(BD_2017$ZONA))
barplot(table(BD_2017$Piramide.1))
barplot(table(BD_2017$Piramide.2_Actual))
table(BD_2017$Piramide.2_Actual)

head(BD_2017)
colSums(BD_2017[8])

Aportes <- BD_2017 %>% select(Piramide.2_Actual, Aportes_total) %>% group_by(Piramide.2_Actual) %>% 
  summarise(Aportes = sum(Aportes_total))

plot(Aportes$Aportes)

Aportes
table(BD_2017$Piramide.2_Actual)

# 2. Mapas ####
#*********************************************
bogota = readShapePoly("./barrios_catastrales/barrios_catastrales.shp")
xy = SpatialPoints(BD_2017[c("X", "Y")])	# Puntos Empresas

#*********************************************
#	Análisis gráfio
#*********************************************

plot(bogota)
plot(xy)
points(xy, pch = 3, cex = 0.3, col = "red")

library(leaflet)

BD_2017$Y <- ifelse(BD_2017$Y>1000000, 4.7316250, BD_2017$Y)

map <- leaflet(data=BD_2017) %>%
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addCircleMarkers(lng =~X, lat =~Y, popup="The birthplace of R")

map

#Análisis descriptivo para la precipitación
par(mfrow = c(1, 3))
hist(BD_2017$Aportes_total, freq = FALSE, main = "", xlab = "Aportes", ylab = "Frecuencia")
curve(dnorm(x, mean(BD_2017$Aportes_total), sd(BD_2017$Aportes_total)), add = T)
boxplot(BD_2017$Aportes_total)
qqPlot(BD_2017$Aportes_total, ylab = "Aportes")
title(main=list("Gráficos descriptivos para los aportes", cex=2,col="black", font=3), outer=T,line=-2)

limites=c(min(BD_2017$Aportes_total), quantile(BD_2017$Aportes_total, probs = c(0.2, 0.4, 0.6, 0.8),type = 5), max(BD_2017$Aportes_total))

##################################################################
############ 		Análisis de estacionariedad		##############
##################################################################

#Gráficos contra las direcciones
scatterplot(Aportes_total~X, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=BD_2017)
scatterplot(Aportes_total~X, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=BD_2017)

# Al parecer, la media de la precipitación no es constante sobre la región de observacion, luego el proceso no es estacionario
# Es necesario remover esta dependencia

# Una alternativa es utilizar un modelo en términos de las direcciones
# Utilizo un modelo cuadrático en las direcciones con un stepwise
modelo1 = lm(Aportes_total ~ X + Y + I(X * Y) + I(X^2) + I(Y^2), data = BD_2017)
summary(modelo1)
step(modelo1)

#Ajuste del modelo seleccionado
modelo2 = lm(Aportes_total ~ Y + I(Y^2) , data = BD_2017)
anova(modelo2)
summary(modelo2)

#Gráficos sobre los residuales del modelo ajustado
par(mfrow = c(1, 3))
hist(modelo2$res, freq = FALSE, main = "", xlab = "Residuales", ylab = "Frecuencia")
curve(dnorm(x, mean(modelo2$res), sd(modelo2$res)), add = T)
boxplot(modelo2$res)
qqPlot(modelo2$res, ylab = "Precipitacion")
#Los residuales no siguen una distribución normal, pero no es necesario, solo que sean estacionarios.

#Para revisar si los residuales del modelo dependen de la dirección, 
#se ajusta un modelo de segundo orrden sobre los residuales.

modelo3=lm(modelo2$res ~ X + Y + I(X * Y) + I(X^2) + I(Y^2), data = BD_2017)
summary(modelo3)

#se verifica que los residuales son estacionarios!
#Gráficos contra las direcciones para los residuales

scatterplot(modelo2$res~X, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=BD_2017)
scatterplot(modelo2$res~Y, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=BD_2017)




##################################################################
############ 			Modelo a sentimiento		##############
##################################################################
# Ahora, se construye el semivariograma sobre los residuales del modelo ajustado
datos2=data.frame(x=BD_2017$X,y=BD_2017$Y,res=modelo2$res)

# Creando objeto de tipo geodata para el calculo del semivariograma
geo = as.geodata(datos2, coords.col = 1:2, data.col = 3)
#variog para estimar semivariograma
var = variog(geo, max.dist = 125000,direction = "omnidirectional")
par(mfrow=c(1,1))
plot(var)
#Ajuste de modelos al semivariograma
#Aca se puede "jugar" con varios ajustes
ev=eyefit(var)
ev

##################################################################
############ 			Ajuste de modelos			##############
##################################################################

#Estimacion del modelo de semivarianza
#Asignando valores iniciales
mod1=variofit(var,ini=ev,weights="equal")
mod1

#Minimos cuadrados ponderados
mod2=variofit(var,ini=ev,weights="npairs")
mod2

#Minimos cuadrados ponderados
mod3=variofit(var,ini=ev,weights="cressie")
mod3


plot(var)
ev=eyefit(var)
lines(mod1, max.dist = 125000, col = 1)
lines(mod2, max.dist = 125000, col = 2)
lines(mod3, max.dist = 125000, col = 3)
legend("bottomright",legend = c("MCO", "MCP - npairs", "MCP - cressie"),
       col = 1:5, lwd = 2, inset = .03)

##################################################################
############ 			Validación cruzada			##############
###########       con kriging ordinario   ##############
###########       sobre los residuales    ##############
##################################################################

cruzada1=xvalid(geo,model=mod1,reestimate = F)
cruzada2=xvalid(geo,model=mod2,reestimate = F)
cruzada3=xvalid(geo,model=mod3,reestimate = F)

sqrt(mean(cruzada1$error^2))
sqrt(mean(cruzada2$error^2))
sqrt(mean(cruzada3$error^2))

# Aqui arriba se está haciendo validación cruzada sobre los residuales
# Lo ideal es hacer validación cruzada para la precipitación directamente
# Para hacer esto es conveniente cambiar de paquete, de geoR a gstat
# Se crea un objeto de tipo gstat para utilizarlo en el kriging
mod1_1 <- as.vgm.variomodel(mod2)
class(mod2)
class(mod1_1)
# mod1 es el modelo en la libreria geoR
# mod1_1 es el mismo modelo pero de la libreria gstat

coordinates(BD_2017) = ~X+Y
spplot(BD_2017, "Aportes_total", cuts = limites)

kr <- krige.cv(Aportes_total ~ X + Y + I(X*Y) + I(X^2) , BD_2017,  mod1_1, maxdist = 125000)
head(kr)
mape=mean(abs(kr$residual)/kr$observed)
mape

#Se repite el proceso para los otros modelos candidatos
mod2_1 <- as.vgm.variomodel(mod2)
kr <- krige.cv(prec ~x+y+I(x*y)+I(x^2) , datossp,  mod2_1, maxdist = 125000)
mape=mean(abs(kr$residual)/kr$observed)
mape

mod3_1 <- as.vgm.variomodel(mod3)
kr <- krige.cv(prec ~x+y+I(x*y)+I(x^2) , datossp,  mod3_1, maxdist = 125000)
mape=mean(abs(kr$residual)/kr$observed)
mape
###### Mejor modelo parece el primero, aunque tienen muy pocas diferencias con los demás

##################################################################
############ 			Kriging universal			##############
##################################################################

cundinamarca = readShapePoly("./Cundinamarca/CUNDINAMARCA.shp")
bogota_upr <- readShapePoly("./UPR/UPR.shp")
bogota_loc <- readShapePoly("./localidades/localidades.shp")


poligonos = polygons(bogota_loc)
plot(poligonos)

muestra = spsample(poligonos, n = 10000, "regular")
muestra1 = data.frame(muestra)
names(muestra1) = c("x", "y")
gridded(muestra1) = c("x", "y")
plot(muestra)

#Para cuadricular la muestra generada! porque se ha generado de forma regular

krig_u=krige(formula=Aportes_total ~ X+Y+I(X*Y)+I(X^2),BD_2017,muestra1,model=mod1_1)
#kriging universal sobre la precipitación.
head(krig_u$var1.pred)
head(krig_u$var1.var)

#Mapa para la precipitación
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", contour = T, labels = T, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

#Con algunas opciones distintas
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", contour = FALSE, labels = FALSE, pretty = F, col = "black", col.regions = terrain.colors(100))
spplot(krig_u, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))


#Para visualizar los puntos de las estaciones
li = list("sp.polygons", cundinamarca)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))







