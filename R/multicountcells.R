#########################################################################
# 'x1'  :  vector of categorical rasters x=c(x1,x2,x3)
# 'y'  :  vector of polygons  y=c(y1,y2,y3)
# 'category'  :  names category example:"category=c("a","b","c"), if you dont have
#         default "category=NULL"
#'ID1'  :  ID1 category example:"ID=c(1,2,3), if you dont have
#         default "ID=NULL
#'unit'	  : character. Output unit of area. One of "m", "km", or "ha"
#########################################################################
#package required "hydroRTS"
#########################################################################
multicountcells <- function(x1,y,category=NULL,ID1=NULL,unit="ha") {

if(is(y,"SpatVectorCollection")){
    
  }else{
    y <- c(y)
  }
  
  resume_list <-list()
 
  for (z in 1:length(y)){
  x<-cropRTS(x1,y[[z]])
  xha<- expanse(x[[1]],unit=unit)
  if (is.null(category)) {
    ################################################################################
    #primer ciclo
    ################################################################################
    h<-terra::freq(x[[1]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[1]])))
    h$Percentage1 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[1]]))
    
    resume <- data.frame(ID = seq(1:max(values(x),na.rm = T)))
    resume[[new_col_name]]<-0
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,3]
    resume$Percentage1<-variablecompleta[,4]
    resume$Hect1<-round((xha[,2]/100)*resume[,3],4)
    ################################################################################
    
    ################################################################################
    #Segundo ciclo
    ################################################################################
    h<-terra::freq(x[[2]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[2]])))
    h$Percentage2 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[2]]))
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,5]
    resume$Percentage2<-variablecompleta[,6]
    resume$variation1<-round((resume[,5]-resume[,2])*100/resume[,2],2)
    resume$Hect2<-round((xha[,2]/100)*resume[,6],4)
    ################################################################################
    #Ciclos automaticos >3
    ################################################################################
    for (i in 3:nlyr(x)){
      g<-terra::freq(x[[i]], bylayer=F)
      
      pname <- paste0("Percentage",i)
      g[[pname]] <- round(g[,2]*100 / sum(g[,2]),2)
      
      names(g)<- c("ID", paste0("count_",names(x[[i]])),pname)
      new_col_name <- paste0("count_",names(x[[i]]))
      variablecompleta <- merge(resume, g, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
      resume[[new_col_name]]<-variablecompleta[ncol(variablecompleta)-1]
      resume[[pname]]<-variablecompleta[ncol(variablecompleta)]
      variation <- paste0("variation",(i-1))
      resume[variation]<-round(((resume[,(ncol(resume)-1)])-(resume[,(ncol(resume)-5)]))*100/(resume[,(ncol(resume)-5)]),2)
      hect1 <- paste0("hect",i)
      resume[hect1]<-round((xha[,2]/100)*resume[,(ncol(resume)-1)],4)
    }
    if (!is.null(ID1)) {resume[,1]<-ID1}
    z1<-resume
    variation0<-rep(0, length(t(z1[3])))
    
    ##
    z3<-z1[3]
    variation0 <- ifelse(is.na(z3), NA, 0)
    colnames(variation0) <- c("variation0")
    ##
    
    resume <- cbind(z1[,1:3], variation0, z1[, 4:(length(z1))])
    resume_list[[z]] <- resume
  } else {     
    ################################################################################
    #primer ciclo
    ################################################################################
    h<-terra::freq(x[[1]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[1]])))
    h$Percentage1 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[1]]))
    
    resume <- data.frame(ID = seq(1:length(category)))
    resume$cover <- category
    resume[[new_col_name]]<-0
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,4]
    resume$Percentage1<-variablecompleta[,5]
    resume$Hect1<-round((xha[,2]/100)*resume[,4],4)
    ################################################################################
    
    ################################################################################
    #Segundo ciclo
    ################################################################################
    h<-terra::freq(x[[2]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[2]])))
    h$Percentage2 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[2]]))
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,6]
    resume$Percentage2<-variablecompleta[,7]
    resume$variation1<-round((resume[,6]-resume[,3])*100/resume[,3],2)
    resume$Hect2<-round((xha[,2]/100)*resume[,7],4)
    ################################################################################
    #Ciclos automaticos >3
    ################################################################################
    for (i in 3:nlyr(x)){
      g<-terra::freq(x[[i]], bylayer=F)
      
      pname <- paste0("Percentage",i)
      g[[pname]] <- round(g[,2]*100 / sum(g[,2]),2)
      
      names(g)<- c("ID", paste0("count_",names(x[[i]])),pname)
      new_col_name <- paste0("count_",names(x[[i]]))
      variablecompleta <- merge(resume, g, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
      resume[[new_col_name]]<-variablecompleta[ncol(variablecompleta)-1]
      resume[[pname]]<-variablecompleta[ncol(variablecompleta)]
      variation <- paste0("variation",(i-1))
      resume[variation]<-round(((resume[,(ncol(resume)-1)])-(resume[,(ncol(resume)-5)]))*100/(resume[,(ncol(resume)-5)]),2)
      hect1<-paste0("hect",i)
      resume[hect1]<-round((xha[,2]/100)*resume[,(ncol(resume)-1)],4)
    }
    if (!is.null(ID1)) {resume[,1]<-ID1}
    z1<-resume
    variation0<-rep(0, length(t(z1[3])))
    ##
    z3<-z1[3]
    variation0 <- ifelse(is.na(z3), NA, 0)
    colnames(variation0) <- c("variation0")
    ##
    resume <- cbind(z1[,1:4], variation0, z1[, 5:(length(z1))])
    resume_list[[z]] <- resume
  
  }
  }
  resume_list

  
}
