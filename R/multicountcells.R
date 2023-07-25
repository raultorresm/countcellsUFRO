#########################################################################
# 'x'  :  categorical raster
# 'category'  :  names categorys example:"category=c("a","b","c"), if you dont have
#         default "category=NULL"
#########################################################################
#package required "terra"
#########################################################################
multicountcells <- function(x,category=NULL) {
  if (is.null(category)) {
    h<-terra::freq(x, bylayer=F)
    h <- h[,c("value","count")]
    names(h)<- c("ID","Count")
    h$Percentage <- round(h$Count*100 / sum(h$Count),2)
    h  
  } else {     
    h<-terra::freq(x, bylayer=F)
    h$cover <- category[h$value]
    h <- h[,c("cover","value","count")]
    names(h)<- c("Cover","ID","Count")
    h$Percentage <- round(h$Count*100 / sum(h$Count),2)
    h  
  }
}
#########################################################################
#########################################################################
#Pruebas para organizar los datos
###################################################################
resume <- data.frame(ID = seq(1:length(k)), Count = 0)   #Prepara la tabla resumen con una columna de IDs por cada tipo de suelo según k
#y otra columna Count para ir asignando los valores de otros dataframes
resume$Count <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)$Count_B #Combina ambos dataframe en base al ID,
#dejando el/los faltante/s como NA y los valores de Count de h ordenados los asigna a la columna Count de la tabla resumen
resume$Count[is.na(resume$Count)] <- 0 #Reemplaza los NA por 0
resume





