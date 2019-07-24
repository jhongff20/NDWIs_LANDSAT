options(repos = c(CRAN = "http://cran.rstudio.com")) 
#CARGAMOS LOS PAQUETES PARA EL TRABAJO 
suppressMessages(library(raster))
suppressMessages(library(RStoolbox))
suppressMessages(library(rgdal))

#Importamos la metadata y bandas basadas en el MTL 
imgsat<- "C:/Users/TOSHIBA/Desktop/TRAB_HIDROINFORMATICA/00DATOS/LT05_20110827/LT05_L1TP_002069_20110827_20161006_01_T1_MTL.txt"
metaData<-readMeta(imgsat)

#Apilamos la imagen
lsat<- stackMeta(imgsat)

#Ploteamos la imagen en falso color RGB
plotRGB(lsat, 5,4,3, stretch ="lin")# ver la figura-1

#convertimos la imagen de nivel digital a radiancia TOA 
lsat_apref <- radCor(lsat, metaData, method ="apref")
plotRGB(lsat_apref, 5,4,3, stretch ="lin") # ver la figura-2

#Realizamos un corte de interes en la zona de estudio 
#El sistema de referencia mundial (WRS) es una notación global utilizada 
#en la catalogación de datos Landsat.
e <- extent(440000,490000,-1400000,-1375000)#xmin,xmax,ymin,ymax
lsat_apref_crop <- crop(lsat_apref,e)
plotRGB(lsat_apref_crop, 5,4,3, stretch ="lin") # ver la figura-3

#Calculando el NDVI
img <- lsat_apref_crop
nir <- img[[4]]
red <- img[[3]]
ndvi <- (nir - red)/(nir + red)
layout(matrix(c(1), 1, 1))
plot(ndvi,main = 'NDVI-LADNSAT 5',cex.lab=0.6,cex.axis=0.6,cex.main=0.6)#Figura-5

#Calculando el NDWI
img <- lsat_apref_crop
nir <- img[[4]]
green <- img[[2]]
ndwi <- (green - nir)/(green + nir)
layout(matrix(c(1), 1, 1))
plot(ndwi,col=rev(topo.colors(100)),main ='NDWI-LADNSAT 5',cex.lab=0.6,cex.axis=0.6,cex.main=0.6)#Figura-6

# Máscara de cuerpos de agua en color azul
cuer.agu <- ndwi
cuer.agu[cuer.agu > -0.01]<-1
cuer.agu[cuer.agu <= -0.01]<-NA
layout(matrix(c(1), 1, 1))
plot(cuer.agu, col="blue",main="CUERPOS DE AGUA",cex.lab=0.6,cex.axis=0.6,cex.main=0.6)#Figura-7

# Visualizamos la máscara en la imagen satelital
layout(matrix(c(1), 1, 1))
plotRGB(img, 4,3,2, axes=T,main="CUERPOS DE AGUA IMAGEN INICIAL", stretch="lin", cex.lab=0.1, cex.axis=0.1,cex.main=0.8)
#plotRGB(img, 5,4,3, axes=T, stretch="lin", cex.lab=0.5, cex.axis=0.3,cex.main=0.5)
plot(cuer.agu,col="blue",cex.lab=1,cex.axis=1,cex.main=1,add=TRUE)#Figura-8

writeRaster(cuer.agu,paste("C:/Users/TOSHIBA/Desktop/TRAB_HIDROINFORMATICA/02SALIDA"))
