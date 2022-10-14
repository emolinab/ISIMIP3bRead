library(luplot)
library(luscale)
library(plyr)
library(ggrepel)
library(ggplot2)

source("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/readMAgPIE.R")
source("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/readIMAGE.R")
source("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/readGLOBIOM3.R")
source("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/readHurttHistorical.R")

years<-seq(from = 1990, to = 2100, by = 10)

LUH_raw_mag<-calcOutput("LUH2v2",landuse_types = "magpie", irrigation = FALSE,
                     cellular = TRUE, cells = "magpiecell", selectyears = "past",aggregate=FALSE)
LUH_raw_mag<-setNames(LUH_raw_mag,c("Forest","Other","Urban","Cropland","Grassland"))
LUH_mag<-dimSums(setYears(LUH_raw_mag[,1,],NULL),dim=3)
mapping<-read.csv2("/p/projects/rd3mod/inputdata/mappings/regional/regionmappingSSP.csv")

### MAGPIE ###
ssp1<-c("ssp126","ssp370","ssp585","ssp585noco2")
gcm<-c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL","2015soc")
MAgPIE_accum<-NULL

file2<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/MAgPIE_raw/magpie_raw_region_all.rds")
if(!file.exists(file2)){
for(g in gcm){
  ssp<-if(g=="GFDL-ESM4") ssp1 else ssp1[1:3]
  for(s in ssp){
    
    file<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/MAgPIE_raw/magpie_raw_",s,"_",g,".rds")

    if(!file.exists(file)){
      MAgPIE<-readMAgPIE(ssp=s,gcm=g,type="states",subtype=NULL,year=years[2:length(years)])*LUH_mag
      MAgPIE<-mbind(setNames(MAgPIE[,,c("cropland")],c("Cropland")),
                    setNames(MAgPIE[,,c("pastr")],c("Pasture")),
                    setNames(MAgPIE[,,c("range")],c("Rangeland")),
                    setNames(dimSums(MAgPIE[,,c("primf","secdf","timber")],dim=3),"Forest"),
                    setNames(dimSums(MAgPIE[,,c("primn","secdn")],dim=3),"Other"),
                    setNames((MAgPIE[,,c("urban")]),"Urban"))
      MAgPIE[!is.finite(MAgPIE)]<-0
      saveRDS(MAgPIE,file)
      
    }else{
      MAgPIE<-readRDS(file)
    }
    
    MAGPIE2<-dimSums(MAgPIE,dim=1.2)
    mapping2<-subset(mapping,CountryCode %in% getCells(MAGPIE2))
    MAgPIE<-speed_aggregate(MAGPIE2,rel=mapping2,from="CountryCode",to="RegionCode")
    MAgPIE<-mbind(MAgPIE,setCells(dimSums(MAgPIE,dim=1),"Global"))
    MAgPIE<-as.ggplot(setNames(MAgPIE,paste0(getNames(MAgPIE),"_",s,"_",g)))
    MAgPIE_accum<-if(is.null(MAgPIE_accum)) MAgPIE else rbind(MAgPIE_accum,MAgPIE)
    
  }
}
saveRDS(MAgPIE_accum,file2)
}
##############

### IMAGE ###

file2<-"/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/IMAGE_raw/image_raw_region_all.rds"

if(!file.exists(file2)){
ssp<-c("1_26","3_70","5_85")
gcm<-c("GFDL","IPSL","MPI","MRI","UKESM","nocc")
IMAGE_accum<-NULL

for(g in gcm){
  for(s in ssp){
    
    file<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/IMAGE_raw/image_raw_",s,"_",g,".rds")
    
    if(!file.exists(file)){

      IMAGE<-readIMAGE(ssp=s,gcm=g,type="STATES",subtype=NULL,year=years)*LUH_mag
      IMAGE[!is.finite(IMAGE)]<-0
      IMAGE<-mbind(setNames(dimSums(IMAGE[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3),c("Cropland")),
                   setNames(dimSums(IMAGE[,,c("pastr")],dim=3),c("Pasture")),
                   setNames(dimSums(IMAGE[,,c("range")],dim=3),c("Rangeland")),
                   setNames(dimSums(IMAGE[,,c("primf","secdf","timber")],dim=3),"Forest"),
                   setNames(dimSums(IMAGE[,,c("primn","secdn")]),c("Other")),
                   setNames((IMAGE[,,c("urban")]),"Urban"))
      
      saveRDS(IMAGE,file)
      
    }else{
      IMAGE<-readRDS(file)
    }
    
    IMAGE2<-dimSums(IMAGE,dim=1.2)
    mapping2<-subset(mapping,CountryCode %in% getCells(IMAGE2))
    IMAGE<-speed_aggregate(IMAGE2,rel=mapping2,from="CountryCode",to="RegionCode")
    IMAGE<-mbind(IMAGE,setCells(dimSums(IMAGE,dim=1),"Global"))
    IMAGE<-as.ggplot(setNames(IMAGE,paste0(getNames(IMAGE),"-",s,"-",g)))
    IMAGE_accum<-if(is.null(IMAGE_accum)) IMAGE else rbind(IMAGE_accum,IMAGE)
    
  }
}
saveRDS(IMAGE_accum,file2)
}
##############

###### GLOBIOM ####
ssp<-c("1","5")
rcp<-c("2p6","8p5")
gcm<-c("GFDL","IPSL","MPI","MRI","UKESM1")
GLOBIOM_accum<-NULL

file2<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/GLOBIOM_raw/globiom_raw_region_all.rds")
if(!file.exists(file2)){
  for(g in gcm){
    for(s in 1:2){
      
      file<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/GLOBIOM_raw/globiom_raw_",ssp[s],rcp[s],"_",g,".rds")
      
      if(!file.exists(file)){
        GLOBIOM<-NULL
        for(t in c("Cropland","Pasture","Forest","Other","Urban")){
          GLOBIOM_aux2<-NULL
          for(y in seq(2010,2100,by=10)){
            
            GLOBIOM_aux<-readGLOBIOM3(ssp=ssp[s],SPA=ssp[s],RCP=rcp[s],gcm=g,type=t,year=y)*LUH_mag
            GLOBIOM_aux[!is.finite(GLOBIOM_aux)]<-0
            GLOBIOM_aux2<-if(is.null(GLOBIOM_aux2)) GLOBIOM_aux else mbind (GLOBIOM_aux2,GLOBIOM_aux)
          }
          GLOBIOM<-if(is.null(GLOBIOM)) GLOBIOM_aux2 else mbind(GLOBIOM,GLOBIOM_aux2)
        }
        
        saveRDS(GLOBIOM,file)
        
      }else{
        GLOBIOM<-readRDS(file)
      }
      
      GLOBIOM2<-dimSums(GLOBIOM,dim=1.2)
      mapping2<-subset(mapping,CountryCode %in% getCells(GLOBIOM2))
      GLOBIOM<-speed_aggregate(GLOBIOM2,rel=mapping2,from="CountryCode",to="RegionCode")
      GLOBIOM<-mbind(GLOBIOM,setCells(dimSums(GLOBIOM,dim=1),"Global"))
      GLOBIOM<-as.ggplot(setNames(GLOBIOM,paste0(getNames(GLOBIOM),"_ssp",ssp[s],rcp[s],"_",g)))
      GLOBIOM_accum<-if(is.null(GLOBIOM_accum)) GLOBIOM else rbind(GLOBIOM_accum,GLOBIOM)
      
    }
  }
  saveRDS(GLOBIOM_accum,file2)
}
###################



### Historical LUH ####
file2<-"/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/Historical_LUH/LUH_raw_region_all.rds"
yearsLUH<-seq(from = 1995, to = 2015, by = 5)

if(!file.exists(file2)){
file<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Scripts-Mean/LUMs_rds/Historical_LUH/LUH_his_raw.rds")

if(!file.exists(file)){
  LUH<-readHurttHistorical(type="states",subtype=NULL,year=yearsLUH)*LUH_mag
  LUH[!is.finite(LUH)]<-0
  LUH<-mbind(setNames(dimSums(LUH[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3),c("Cropland")),
               setNames(dimSums(LUH[,,c("pastr")],dim=3),c("Pasture")),
             setNames(dimSums(LUH[,,c("range")],dim=3),c("Rangeland")),
               setNames(dimSums(LUH[,,c("primf","secdf")],dim=3),"Forest"),
               setNames(dimSums(LUH[,,c("primn","secdn")]),c("Other")),
               setNames((LUH[,,c("urban")]),"Urban"))
  
  saveRDS(LUH,file)
  
}else{
  LUH<-readRDS(file)
}

LUH2<-dimSums(LUH,dim=1.2)
mapping2<-subset(mapping,CountryCode %in% getCells(LUH2))
LUH<-speed_aggregate(LUH2,rel=mapping2,from="CountryCode",to="RegionCode")
LUH<-mbind(LUH,setCells(dimSums(LUH,dim=1),"Global"))
LUH<-as.ggplot(LUH)
saveRDS(LUH,file2)
}

#######################


