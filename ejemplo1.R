
################################################################################
#library
################################################################################
library(terra)
################################################################################
#datos de entrada
################################################################################
x<-rast("E:/tmp/landcover_v4.0.1/landcover_v4.0.1/LandCover_rf_2018_v4.0.1.tif")
y<-vect("E:/subir_ftp/polygon/mapocho.shp")

k<-c("Cursos y cuerpos de agua",
     "Playas, dunas y bancosde arena",
     "Bosque nativo mediterráneo esclerófilo",
     " Bosque nativo templado",
     "Plantación de hoja ancha",
     "Frutales",
     "Nieve y hielos",
     "Vegetación ribereña y humedales",
     "Matorrales y vegetación arbustiva",
     "Plantación de pinófitas",
     "Praderas, pastizales y cultivos anuales",
     "Praderas y pastizales siempreverdes",
     "Suelo desnudo y sectores desprovistos de vegetación",
     "Turberas",
     "Superficies impermeables",
     "Plantación cosechada")

###############################################################################
#funcion
###############################################################################
z<-crop(x,y)
h<-terra::freq(z, bylayer=F)
h$cover <- k[h$value]
h <- h[,c("cover","value","count")]
names(h)<- c("Cover","ID","Count")


