#########################################################################
# 'x'  :  vector of categorical rasters x=c(x1,x2,x3)
# 'category'  :  names categorys example:"category=c("a","b","c"), if you dont have
#         default "category=NULL"
#########################################################################
#package required "terra"
#########################################################################
multicountcells <- function(x,category=NULL) {
  if (is.null(category)) {
    ################################################################################
    #primer ciclo
    ################################################################################
    h<-terra::freq(x[[1]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[1]])))
    h$Percentage1 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[1]]))
    
    resume <- data.frame(ID = seq(1:max(values(x))))
    resume[[new_col_name]]<-0
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,3]
    resume$Percentage1<-variablecompleta[,4]
    
    ################################################################################
    
    ################################################################################
    #Segundo ciclo
    ################################################################################
    h<-terra::freq(x[[2]], bylayer=F)
    names(h)<- c("ID", paste0("count_",names(x[[2]])))
    h$Percentage2 <- round(h[,2]*100 / sum(h[,2]),2)
    
    new_col_name <- paste0("count_",names(x[[2]]))
    variablecompleta <- merge(resume, h, by = "ID", suffixes = c("_A", "_B"), all.x = TRUE)
    resume[[new_col_name]]<-variablecompleta[,4]
    resume$Percentage2<-variablecompleta[,5]
    
    resume$variation1<-round((resume[,4]-resume[,2])*100/resume[,2],2)
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
      resume[variation]<-round(((resume[,(ncol(resume)-1)])-(resume[,(ncol(resume)-4)]))*100/(resume[,(ncol(resume)-4)]),2)  
    }
    resume 
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
    resume$variation1<-0
    
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
    
    resume$variation2<-round((resume[,6]-resume[,3])*100/resume[,3],2)
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
      
      variation <- paste0("variation",(i))
      resume[variation]<-round(((resume[,(ncol(resume)-1)])-(resume[,(ncol(resume)-4)]))*100/(resume[,(ncol(resume)-4)]),2)
      
    }
    resume
  }



