#****************************************************************************************************************
#*****************************    ESTADÍSTICA ESPACIAL - KRIGING UNIVERSAL     **********************************
#****************************************************************************************************************
source("fun.R")

# 0. Cargue de librerías ####
#*********************************************
load.lib("dplyr", "scales", "tidyr", "plotly", "rgeos", "sp", "maptools", "car", "geoR", 
         "gstat", "stringr")


# 1. Cargue información ####
#*********************************************
tipos<-c("factor", "character", "character", "character", "character", "character", "numeric",
         "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "numeric")

BD <- read.csv("BD.txt", sep = "\t", header = T, dec = ",", colClasses=tipos)
str(BD)

## corrige aportes
BD$Aportes <- str_replace(BD$Aportes[1], ",", ".")
BD$Aportes <- str_replace(BD$Aportes[1], "\\$ ", "")
BD$Aportes <- as.numeric(BD$Aportes)

head(BD)
table(BD$Piramide.1)
table(BD$Piramide.2_Actual)

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
                   BD$Piramide.2_Actual != "4.9 Colsubsidio") %>% 
           select(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y, Aportes, Afiliados) %>%
           group_by(Id_Empresa, RazonSocial, Piramide.1, Piramide.2_Actual, ZONA, X, Y) %>%
           summarise(Aportes_total = sum(Aportes, na.rm=T), Afiliados_max = max(Afiliados, na.rm=T)) %>% 
           filter(ZONA == "ZONA CENTRO" | ZONA == "ZONA NORTE" | ZONA == "ZONA CHAPINERO" | ZONA == "ZONA SUR")

table(BD_2017$ZONA)


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
  addCicleMarkers(lng =~X, lat =~Y, popup="The birthplace of R")
map

#Análisis descriptivo para la precipitación
par(mfrow = c(1, 3))
hist(BD_2017$Aportes_total, freq = FALSE, main = "", xlab = "Aportes", ylab = "Frecuencia")
curve(dnorm(x, mean(BD_2017$Aportes_total), sd(BD_2017$Aportes_total)), add = T)
boxplot(BD_2017$Aportes_total)
qqPlot(BD_2017$Aportes_total, ylab = "Aportes")
title(main=list("Gráficos descriptivos para los aportes", cex=2,col="black", font=3), outer=T,line=-2)









