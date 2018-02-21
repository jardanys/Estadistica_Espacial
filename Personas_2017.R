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

colnames(BD) <- c("Id_Empresa", "RazonSocial", "FechaAfiliacion", "FechaRetiro", "Piramide.1", "Piramide.2_Actual", 
                  "AÑO", "MES", "Afiliados", "A", "B", "C", "ZONA", "Aportes", "X", "Y")

## corrige aportes
BD$Aportes <- str_replace(BD$Aportes, ",", ".")
BD$Aportes <- str_replace(BD$Aportes, "\\$ ", "")
BD$Aportes <- as.numeric(BD$Aportes)

## Coerción de coordenadas a clase numérica. Se observa que hay coordenadas nulas
BD$X <- as.numeric(str_replace(BD$X, ",", "."))
BD$Y <- as.numeric(str_replace(BD$Y, ",", "."))

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
           filter(Aportes_total > 0 & Afiliados_max > 0)


# 2. Cargue de capas ####
#*********************************************

# Capa de Bogotá sin localidad de sumapaz
bogota = readShapePoly("./localidades1/localidades_WGS84.shp")

# Puntos de ubicación empresas
xy = SpatialPoints(Pers_2017[c("X", "Y")])	


# 2. Análisis Gráfico ####
#*********************************************

# Gráfico de la capa y las ubicaciones de las empresas que se encuentran afiliadas
plot(bogota)
points(xy, pch = 3, cex = 0.3, col = "red")
title(main="Ubicación empresas en Bogotá")


# Análisis descriptivo para la cantidad de afiliados por empresa
par(mfrow = c(1, 3))
hist(Pers_2017$Afiliados_max, freq = FALSE, main = "", breaks=200, col="gray", xlim=c(0, 2000),
     xlab = "Afiliados x empresa", ylab = "Frecuencia")

## Cláramente no hay trendencia normal (REVISAR)
curve(dnorm(x, mean(Pers_2017$Afiliados_max), sd(Pers_2017$Afiliados_max)), add = T)
boxplot(Pers_2017$Afiliados_max)
qqPlot(Pers_2017$Afiliados_max, ylab = "Afiliados x empresa")
title(main=list("Gráficos descriptivos para la precipitación", cex=2,col="black", font=3), outer=T,line=-2)

# creamos un vector de límites
limites=c(min(Pers_2017$Afiliados_max), quantile(Pers_2017$Afiliados_max, probs = c(0.2, 0.4, 0.6, 0.8),type = 5), 
          max(Pers_2017$Afiliados_max))
limites

# Gráfico para determinar estacionariedad. 
# En este caso resulta NO ESTacionaria prque los punticos no están formados aleatoriamente
coordinates(select(Pers_2017, X, Y, Afiliados_max)) = ~x+y
spplot(datossp, zcol="Afiliados_max", cuts = limites)
#Existe dependencia espacial!


















