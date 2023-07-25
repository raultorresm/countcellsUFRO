#########################################################################
# 'x'  :  categorical raster
# 'category'  :  names categorys example:"k=c("a","b","c"), if you dont have
#         default "k=NULL"
#########################################################################
#########################################################################
onecountcells <- function(x,category=NULL) {
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





