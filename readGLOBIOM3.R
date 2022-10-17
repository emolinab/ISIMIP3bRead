library(terra)
library(mrcommons)

readGLOBIOM3<-function(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Cropland",year=2010){
  
  folder<-"/p/projects/landuse/users/mbacca/ISIMIP_IIASA/"
  file<-paste0("SSP",ssp,"_SPA",SPA,"_RCP",RCP,"_",gcm,".nc")
  mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")
  
  y_tag<-(year-2010)/10+1
  tag<-"LC_area_share_lc_class="
  
  if(type=="Cropland"){
    cat<-c(1,2,10)
    tag_type<-paste0(paste0(tag,cat,"_"),y_tag)
  }else if(type=="Pasture"){
    cat<-c(3)
    tag_type<-paste0(paste0(tag,cat,"_"),y_tag)
  }else if(type=="Forest"){
    cat<-c(4,6,7,9)
    tag_type<-paste0(paste0(tag,cat,"_"),y_tag)
  } else if(type=="Other"){
    cat<-c(5,8)
    tag_type<-paste0(paste0(tag,cat,"_"),y_tag)
  }else if(type=="Urban"){
    cat<-c(11)
    tag_type<-paste0(paste0(tag,cat,"_"),y_tag)
  }
  
  raster_GLOBIOM<-rast(paste0(folder,file),lyrs=tag_type)
  raster_GLOBIOM<-app(raster_GLOBIOM,fun=sum)
  lyrs_names<-paste0(type,".",year)
  names(raster_GLOBIOM)<-lyrs_names
  ext(raster_GLOBIOM)<-round(ext(raster_GLOBIOM),3)
  
  size<-cellSize(raster_GLOBIOM, mask=FALSE, unit="ha", transform=FALSE)
  raster_GLOBIOM_05<-aggregate(raster_GLOBIOM*size,fact=2,fun="sum")
  raster_GLOBIOM_05<-raster_GLOBIOM_05/cellSize(raster_GLOBIOM_05, mask=FALSE, unit="ha", transform=FALSE)



  
  #### Conversion to MAgPIE object
  
  raster_GLOBIOM_05_DF<-extract(raster_GLOBIOM_05,mapping[c("lon", "lat")],xy=TRUE)
  raster_GLOBIOM_05_DF<-reshape(raster_GLOBIOM_05_DF,idvar="ID",varying=lyrs_names,v.name="Value",times=lyrs_names,direction="long")
  rownames(raster_GLOBIOM_05_DF)<-NULL
  sl<-sub(".*\\.", "", raster_GLOBIOM_05_DF$time)
  raster_GLOBIOM_05_DF$Year<-sl
  raster_GLOBIOM_05_DF$time<-sub("\\..*", "", raster_GLOBIOM_05_DF$time)
  
  colnames(raster_GLOBIOM_05_DF)<-c("ID","lon","lat","Data1","Value","Year")
  raster_GLOBIOM_05_DF$lon<-as.character(round(as.numeric(raster_GLOBIOM_05_DF$lon),digits=1))
  raster_GLOBIOM_05_DF$lat<-as.character(round(as.numeric(raster_GLOBIOM_05_DF$lat),digits=1))
  raster_GLOBIOM_05_DF$lon<-as.numeric(gsub("\\.1","\\.2",raster_GLOBIOM_05_DF$lon))
  raster_GLOBIOM_05_DF$lon<-as.numeric(gsub("\\.6","\\.7",raster_GLOBIOM_05_DF$lon))
  raster_GLOBIOM_05_DF$lat<-as.numeric(gsub("\\.1","\\.2",raster_GLOBIOM_05_DF$lat))
  raster_GLOBIOM_05_DF$lat<-as.numeric(gsub("\\.6","\\.7",raster_GLOBIOM_05_DF$lat))
  
  
  
  mapping$lat<-trunc(as.numeric(mapping$lat)*10)/10
  mapping$lon<-trunc(as.numeric(mapping$lon)*10)/10
  
  
  raster_GLOBIOM_05_DF<-merge(raster_GLOBIOM_05_DF,mapping[,c("celliso","lon","lat")],by=c("lon","lat"))
  
  
  GLOBIOM_05_Mag<-(as.magpie(raster_GLOBIOM_05_DF[,c("celliso","Year","Data1","Value")]))
  getCells(GLOBIOM_05_Mag) <- gsub("_", "\\.", getCells(GLOBIOM_05_Mag))
  GLOBIOM_05_Mag<- magpiesort(GLOBIOM_05_Mag)
  
  
  return(GLOBIOM_05_Mag)
  
}