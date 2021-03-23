
 ###############################################################################
 #### this script requires the NLGIS shapefiles from O.W.A. Boonstra to run ####
 ###############################################################################
 
 #load packages
  library(rgdal); library(dplyr)

 #clean environment
  rm(list=ls())
  
  #define directory
  setwd("C:/Surfdrive/CLARIAH/HDNG")
  getwd()
  
 #load HDNG
  HDNG <- read.csv("HDNG v4/HDNG+prov+missing.txt", header=T, sep="\t", stringsAsFactors=F)
  
 #load municipalities 1812-1997
  #set starting year
    x <- 1812
  #load municipalities in list munip
  repeat{
    #set working directory
      setwd(paste0("C:/Surfdrive/Data/NLGIS/OpslagShapefiles1812_1997_Georef/", as.character(x), "Shapefiles"))
    #load shapefile
      shape.shp <- readOGR(".", paste0("nl_", as.character(x)))
    #set colnames to upper case
      colnames(shape.shp@data) <- toupper(colnames(shape.shp@data))
    #select df 
      l <- list( shape.shp@data[,c("GM_NAAM", "ACODE")] )
    #list df
      if("munip" %in% ls() ){
        munip <- c(munip, l)
      } else{
        munip <- l
      }
    #prepare repeat for next year
      x <- x+1
    #end after 1997
      if(x==1998) {
        break
      }
  }
  #clean environment
    rm(l, shape.shp)
  #set names data frames 
    names(munip) <- paste0("munip", 1812:1997)
    
    
 #filter provincietotalen
  #select provinces
    totalen <- HDNG[is.na(HDNG$ACODE),]
  #drop Flevoland
    Flevoland <- totalen[which(grepl("FLEVOLAND", totalen$NAAM) & totalen$value!=0),]
    totalen <- rbind(totalen[!(grepl("FLEVOLAND", totalen$NAAM)),], Flevoland)
    rm(Flevoland)
    
    
 #filter only existing municipalities (or munips with score)
  #filter municipalities
    jaren <- HDNG[!is.na(HDNG$ACODE),]
  #select first column
    x <- 1
  #set vars per year in list
  repeat{
    #select year from column
      x <- x+1
      yr <- as.character(as.data.frame(table(HDNG$year))[x,1])
    #select all entries for that year 
      l <- jaren[which(jaren$year==yr),]
    #flag non-existing municipalities
      l$nonexisting <- ifelse(l$ACODE %in% munip[[paste0("munip", yr)]][["ACODE"]], 0, 1)
    #list
      l <- list( l[which(l$ACODE %in% munip[[paste0("munip", yr)]][["ACODE"]] |
                           !(l$ACODE %in% munip[[paste0("munip", yr)]][["ACODE"]]) & l$value!=0 ), ] )
    #list df
      if("l_HDNG" %in% ls() ){
        l_HDNG <- c(l_HDNG, l)
      } else{
        l_HDNG <- l
      }
    #end after all years have been parsed
      if(x==nrow(as.data.frame(table(HDNG$year)))) {
        break
      }
  }
  #clean environment
    rm(l, x, yr)
  #merge into one data frame
    HDNG_v4 <- do.call("rbind", l_HDNG)
    
  #add 1809
    jaar1809 <- jaren[which(jaren$year==1809),]
    jaar1809$nonexisting <- NA
    HDNG_v4 <- rbind(HDNG_v4, jaar1809)
    
  #add provincietotalen
    totalen$nonexisting <- NA
    HDNG_v4 <- rbind(HDNG_v4, totalen)
    
  #sort data
    HDNG_v4 <- HDNG_v4 %>% arrange(year, variable, ACODE)
    
  #save output
    write.table(HDNG_v4, file="HDNG v4/HDNG_v4.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
    
  
  
  #clean redundant files
    file.remove("HDNG v4/HDNG_long.txt")
    file.remove("HDNG v4/HDNG+.txt")
    file.remove("HDNG v4/HDNG+prov+missing.txt")
    file.remove("HDNG v4/HEDC_long.txt")
    file.remove("HDNG v4/missingHDNG.txt")
    file.remove("HDNG v4/provinces.txt")
    
