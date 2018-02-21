
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
tipos<-c("character", "character", "character", "character", "character", "character", "numeric",
         "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "character", 
         "character")

## Cambiamos a lectura de la base. En esta nueva BD, las coordenadas vienen encerradas en comillas, así que
## resulta necesario leerlas como caracter y luego pasar a número
BD <- read.table("BD.txt", sep = "\t", header = T, dec = ",", colClasses = tipos, quote="\"")
str(BD)
head(BD)

colnames(BD) <- c("Id_Empresa", "RazonSocial", "FechaAfiliacion", "FechaRetiro", "Piramide.1", "Piramide.2_Actual", "AÑO", "MES", 
  "Afiliados", "A", "B", "C", "ZONA", "Aportes", "X", "Y")

colnames(BD)

## corrige aportes
BD$Aportes <- str_replace(BD$Aportes, ",", ".")
BD$Aportes <- str_replace(BD$Aportes, "\\$ ", "")
BD$Aportes <- as.numeric(BD$Aportes)

## Coerción de coordenadas a clase numérica. Se observa que hay coordenadas nulas
BD$X <- as.numeric(str_replace(BD$X, ",", "."))
BD$Y <- as.numeric(str_replace(BD$Y, ",", "."))

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
  filter(Aportes_total > 0)

table(BD_2017$Piramide.2_Actual)

BD_2017$Y <- ifelse(BD_2017$Y>1000000, 4.7316250, BD_2017$Y)

#BD_2017$Aportes_total <- BD_2017$Aportes_total / 1000000
head(BD_2017$Aportes_total)

# 2. Mapas ####
#*********************************************
bogota = readShapePoly("./localidades1/localidades_WGS84.shp")
xy = SpatialPoints(BD_2017[c("X", "Y")])	# Puntos Empresas

# 3. Exploración datos ####
#*********************************************

#Distribución no simétrica, sesgada hacia la derecha
hist(BD_2017$Aportes_total/1000000, breaks = 400, xlim = c(0, 1000), col="gray") 

summary(BD_2017$Aportes_total/1000000) # media mayor a la mediana (sesgada hacia la derecha, distribución no-normal)

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
points(datos, asp = 1, cex = 4 * datos$BD_2017.Aportes_total/max(datos$BD_2017.Aportes_total),
       pch = 1, col="green") 

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
muestra <- spsample(poligonos, n = 10000, "regular")
# Paso a data frame
muestra1 <- data.frame(muestra)
names(muestra1) = c("X", "Y")
gridded(muestra1) = c("X", "Y")
plot(muestra)
plot(muestra1)

?krige

sum(ifelse(is.na(datos$BD_2017.Aportes_total),1,0))
datos

## Como modelamos Kriging orinario la fórmula a usar es del tipo z~1.
ok <- krige(BD_2017.Aportes_log ~ 1, datos, muestra1, model = va)
head(ok)
warnings()

pts.s <- list("sp.points", datos, col="white",pch=1, cex=4*datos$BD_2017.Aportes_log/max(datos$BD_2017.Aportes_log))
print(spplot(ok, "pred", asp=1, col.regions=rev(heat.colors(20)),
             main="Predicción OK, log-ppm Zn",sp.layout = list(pts.s)), 
      split=c(1,1,2,1), more=TRUE)
pts.s <- list("sp.points", meuse, col="black", pch=20)
print(spplot(ok, zcol="var1.var",col.regions=rev(gray(seq(0,1,.01))), asp=1,
             main="Varianza OK, log-ppm Zn^2",sp.layout = list(pts.s)), 
      split=c(2,1,2,1), more=FALSE)

## Se define modelo kriging ordinario con funcion de varianza con efecto pepita puro:
## modelo pepita puro >> 'pure.nugget'
## Kriging odinario type='ok'
## nugget >> de acuerdo al gráfico está en aprox 0.55
## REVISAR! >> Asumimos covarianza igual a 0 por lo del efecto pepita? >> si es así definimos 'cov.pars=c(0, 0)'
KC1 <- krige.control(cov.model="pure.nugget", type="OK", cov.pars=c(0, 0), nugget=0.55)

mod1_1 = as.vgm.variomodel(KC1)
KC1
mod1_1

class(mod1_1)
class(KC1)

krig_ord <- krige(formula=BD_2017.Aportes_total ~ 1,datos,muestra1,model=mod1_1)


# 5. Otros metodos ####
#************************************************************

thiessen = krige(log10(BD_2017.Aportes_log) ~ 1, datos, muestra1, nmax = 4)
pts.s <- list("sp.points", datos, col="white",pch=20)

thiessen$var1.pred <- 10^(thiessen$var1.pred)

thiessen

spplot(thiessen, "var1.pred", asp=1, col.regions=rev(heat.colors(50)),
       sp.layout = list(pts.s),main="Thiessen")

spplot(thiessen, c("var1.pred"), main = "Kriging Universal para los aportes", 
       contour = T, labels = T, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

spplot(thiessen, c("var1.pred"), main = "Kriging Universal para los aportes", contour = FALSE, labels = FALSE, 
       pretty = F, col = "black", col.regions = terrain.colors(200))

spplot(thiessen, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, labels = FALSE, 
       pretty = TRUE, col = "black", col.regions = terrain.colors(200))

thiessen = krige(BD_2017.Aportes_log ~ 1, datos, muestra1, nmax = 5)
pts.s <- list("sp.points", datos, col="white",pch=20)

spplot(thiessen, "var1.pred", asp=1, col.regions=rev(heat.colors(50)),
       sp.layout = list(pts.s),main="Thiessen")


li = list("sp.polygons", bogota)
pts = list("sp.points", datos, pch = 3, col = "black", cex = 0.2)
spplot(thiessen, c("var1.pred"), main = "Kriging Universal para los aportes", sp.layout = list(li, pts), 
       contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

