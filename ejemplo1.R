
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


#########################################################################
#Pruebas para organizar los datos
###################################################################

resume <- data.frame(ID = seq(1:length(k)), Count = 0)   #Prepara la tabla resumen con una columna de IDs por cada tipo de suelo según k
#y otra columna Count para ir asignando los valores de otros dataframes

resume$Count <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)$Count_B #Combina ambos dataframe en base al ID,
#dejando el/los faltante/s como NA y los valores de Count de h ordenados los asigna a la columna Count de la tabla resumen

resume$Count[is.na(resume$Count)] <- 0 #Reemplaza los NA por 0

resume





