
#****************************************************************************************************************
#*****************************    ESTADÍSTICA ESPACIAL - KRIGING UNIVERSAL     **********************************
#****************************************************************************************************************
source("fun.R")
options(scipen=999)

# 0. Cargue de librerías ####
#*********************************************
load.lib("dplyr", "scales", "tidyr", "plotly", "rgeos", "sp", "maptools", "car", "geoR", 
         "gstat", "stringr", "reshape2")
load.lib("rgeos","sp","maptools","car","geoR","gstat","RColorBrewer")

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
  filter(ZONA == "ZONA CENTRO" | ZONA == "ZONA NORTE" | ZONA == "ZONA CHAPINERO" | ZONA == "ZONA SUR") %>%
  filter(Piramide.2_Actual == "1.1 Platinum")

table(BD_2017$Piramide.2_Actual)

BD_2017$Y <- ifelse(BD_2017$Y>1000000, 4.7316250, BD_2017$Y)

#BD_2017$Aportes_total <- BD_2017$Aportes_total / 1000000
head(BD_2017$Aportes_total)

# 2. Mapas ####
#*********************************************
bogota = readShapePoly("./localidades/localidades_WGS84.shp")
xy = SpatialPoints(BD_2017[c("X", "Y")])	# Puntos Empresas

# 3. Exploración datos ####
#*********************************************

hist(BD_2017$Aportes_total) #Distribución no simétrica, sesgada hacia la derecha

summary(BD_2017$Aportes_total) # media mayor a la mediana (sesgada hacia la derecha, distribución no-normal)

#Al ser una distribución no simétrica, se aplica logaritmo para transformar
#los valores y obtener una distribución simetrica (normal). 
#Esto, además, reduce los posibles outliers.

BD_2017$Aportes_log <- log10(BD_2017$Aportes_total)
hist(BD_2017$Aportes_log, breaks = 16)

summary(BD_2017$Aportes_total) # No se observa un sesgo tan alto y la distribución es más uniforme

# 4. Estructura espacial ####
#********************************************

datos <- data.frame(BD_2017$X, BD_2017$Y, BD_2017$Aportes_log, BD_2017$Aportes_total)
head(datos)
colnames(datos)

#Crear Spatial Data Frame
coordinates(datos) <- c("BD_2017.X", "BD_2017.Y")
class(datos)

str(datos)

plot(datos, asp = 1, pch = 1)
plot(bogota)
points(datos)

plot(bogota)
points(datos, asp = 1, cex = 4 * datos$BD_2017.Aportes_total/max(datos$BD_2017.Aportes_total),pch = 1) 

# calcule la distancia y semivarianza entre los dos puntos primeros puntos del dataset

n <- length(datos$BD_2017.Aportes_log)
n * (n - 1)/2

coordinates(datos)[1, ]
coordinates(datos)[2, ]

sep <- dist(coordinates(datos)[1:2, ])
sep


# semivarianza, unidades log(mg kg-1)^2
gamma <- 0.5 * (datos$BD_2017.Aportes_log[1] - datos$BD_2017.Aportes_log[2])^2
gamma

ve <- variogram(BD_2017.Aportes_log ~ 1, datos)
ve

plot(ve)
show.vgms()

vt <- vgm(psill=0.02, model="Sph", range=0.55, nugget=0.55) 
vt
plot(ve, pl = T, model = vt)

va <- fit.variogram(ve, vt) 

va
plot(ve, pl = T, model = va)

# 4. Modelo Kriging ####
#*******************************************************+

# Grafica de poligonos de Bogotá D.C.
poligonos <- polygons(bogota)
# Muestra de los pologonos
muestra <- spsample(poligonos, n = 100, "regular")
# Paso a data frame
muestra1 <- data.frame(muestra)
names(muestra1) = c("X", "Y")
gridded(muestra1) = c("X", "Y")
plot(muestra)
plot(muestra1)

ok <- krige(BD_2017.Aportes_log ~ 1, locations = datos, newdata = muestra1, model = va)
head(ok)

pts.s <- list("sp.points", datos, col="white",pch=1, cex=4*datos$BD_2017.Aportes_log/max(datos$BD_2017.Aportes_log))
print(spplot(ok, "pred", asp=1, col.regions=rev(heat.colors(20)),
             main="Predicción OK, log-ppm Zn",sp.layout = list(pts.s)), 
      split=c(1,1,2,1), more=TRUE)
pts.s <- list("sp.points", meuse, col="black", pch=20)
print(spplot(ok, zcol="var1.var",col.regions=rev(gray(seq(0,1,.01))), asp=1,
             main="Varianza OK, log-ppm Zn^2",sp.layout = list(pts.s)), 
      split=c(2,1,2,1), more=FALSE)








