library(terra)
library(mrcommons)



readIMAGE<-function(ssp="1_26",gcm="GFDL",type="FLOOD",subtype=NULL,year=1995,average=TRUE){
  mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")

  lyrs<-c()
  lyrs_names<-c()
  nlyrs<-c()

  if(!is.null(subtype)){
    for(y in year){
      lyrs<-c(lyrs, paste0(subtype,"_",((y-1970)/5+1)))
      lyrs_names<-c(lyrs_names, paste0(subtype,".",y))
    }
    rasterMAg<-rast(paste0("/p/projects/landuse/users/mbacca/ISIMIP_IMAGE/SSP",ssp,"_",gcm,"/LUH2_",type,"_IMAGE.nc"),lyrs=lyrs)

  }else{

    rasterMAg<-rast(paste0("/p/projects/landuse/users/mbacca/ISIMIP_IMAGE/SSP",ssp,"_",gcm,"/LUH2_",type,"_IMAGE.nc"))
    nlyrs<-varnames(rasterMAg)

    for(y in year){
      lyrs<-c(lyrs,paste0(nlyrs,"_",((y-1970)/5+1)))
      lyrs_names<-c(lyrs_names, paste0(nlyrs,".",y))
    }

    rasterMAg<-rasterMAg[[lyrs]]
  }

  if(average){
   rasterMAg_05<-aggregate(rasterMAg,fact=2,fun="mean")
   }else{
         size<-cellSize(rasterMAg, mask=FALSE, unit="ha", transform=FALSE)
         rasterMAg_05<-aggregate(rasterMAg*size,fact=2,fun="sum")
         rasterMAg_05<-rasterMAg_05/cellSize(rasterMAg_05, mask=FALSE, unit="ha", transform=FALSE)
  }
  


  names(rasterMAg_05)<-lyrs_names

  rasterMAg_05_DF<-extract(rasterMAg_05,mapping[c("lon", "lat")],xy=TRUE)
  rasterMAg_05_DF<-reshape(rasterMAg_05_DF,idvar="ID",varying=lyrs_names,v.name="Value",times=lyrs_names,direction="long")
  rownames(rasterMAg_05_DF)<-NULL
  sl<-sub(".*\\.", "", rasterMAg_05_DF$time)
  rasterMAg_05_DF$Year<-sl
  rasterMAg_05_DF$time<-sub("\\..*", "", rasterMAg_05_DF$time)

  colnames(rasterMAg_05_DF)<-c("ID","lon","lat","Data1","Value","Year")
  rasterMAg_05_DF<-merge(rasterMAg_05_DF,mapping[,c("celliso","lon","lat")],by=c("lon","lat"))


  rasterMAg_05_mag<-(as.magpie(rasterMAg_05_DF[,c("celliso","Year","Data1","Value")]))
  getCells(rasterMAg_05_mag) <- gsub("_", "\\.", getCells(rasterMAg_05_mag))
  rasterMAg_05_mag<- magpiesort(rasterMAg_05_mag)


    return(rasterMAg_05_mag)

}
