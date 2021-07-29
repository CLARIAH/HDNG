
 ###############################################################################
 #### this script requires the NLGIS shapefiles from O.W.A. Boonstra to run ####
 ###############################################################################
 
 
 #load packages
  library(rgdal); library(dplyr)

 #clean environment
  rm(list=ls())
  
  #define directory
  setwd("U:/Surfdrive/CLARIAH/HDNG")
  getwd()
  
 #load HDNG
  HDNG <- read.csv("HDNG v4/HDNG+prov+missing.txt", header=T, sep="\t", stringsAsFactors=F)
  HDNG$shapefile <- ifelse(HDNG$year==1809, 1849, HDNG$year) 
  
 #set entries with 909090 & -1 to 0 
  #99999
  length(which(HDNG$value=="99999")); as.data.frame(table(HDNG[HDNG$value=="99999","NAAM"]))
  HDNG$value <- ifelse(HDNG$value=="99999", 0, HDNG$value)
  #999910
  length(which(HDNG$value=="999910")); as.data.frame(table(HDNG[HDNG$value=="999910","NAAM"]))
  HDNG$value <- ifelse(HDNG$value=="999910", 0, HDNG$value)
  #909090
  length(which(HDNG$value=="909090")); as.data.frame(table(HDNG[HDNG$value=="909090","NAAM"]))
  HDNG$value <- ifelse(HDNG$value=="909090", 0, HDNG$value)
  #-1
  length(which(HDNG$value=="-1")); as.data.frame(table(HDNG[HDNG$value=="-1", "variable"]))
  HDNG$value <- ifelse(HDNG$value=="-1", 0, HDNG$value)
  
 #remove , from value
  HDNG$value <- gsub(",", ".", HDNG$value)
  HDNG$value <- as.numeric(HDNG$value)
  
 #set year to numeric
  HDNG$year <- as.numeric(HDNG$year)

 #load municipalities 1812-1997
  #set starting year
    x <- 1812
  #load municipalities in list munip
  repeat{
    #set working directory
      setwd(paste0("U:/Surfdrive/Data/NLGIS/OpslagShapefiles1812_1997_Georef/", as.character(x), "Shapefiles"))
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
    x <- 0
  #set vars per year in list
  repeat{
      x <- x+1
    #select year from column
      yr <- as.character(as.data.frame(table(HDNG$shapefile))[x,1])
    #select all entries for that year 
      l <- jaren[which(jaren$shapefile==yr),]
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
      if(x==nrow(as.data.frame(table(HDNG$shapefile)))) {
        break
      }
  }
  #clean environment
    rm(l, x, yr)
  #merge into one data frame
    HDNG_v4 <- do.call("rbind", l_HDNG)
    
  #add provincietotalen
    totalen$nonexisting <- 0
    HDNG_v4 <- rbind(HDNG_v4, totalen)
    
 #deal with nonexisting municipalities
  #recode miscoded municipalities
    #View(HDNG_v4[HDNG_v4$nonexisting==1, ] %>% group_by(ACODE, NAAM, year, sources, remark_1, remark_2) %>% summarise(n=n()) %>% ungroup() %>% arrange(NAAM))
   #Genderen -> wrong AMCO + name 1809
    HDNG_v4$ACODE <- ifelse(HDNG_v4$year==1809 & HDNG_v4$NAAM=="GENDEREN", 10010, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$year==1809 & HDNG_v4$NAAM=="GENDEREN", 0, HDNG_v4$nonexisting)
    HDNG_v4$NAAM <- ifelse(HDNG_v4$year==1809 & HDNG_v4$NAAM=="EETHEN, GENDEREN EN HEESBEEN", 10010, HDNG_v4$NAAM)
   #Genderen -> wrong AMCO + name 1813-1908
    HDNG_v4$ACODE <- ifelse(HDNG_v4$year<1909 & HDNG_v4$NAAM=="GENDEREN", 10010, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$year<1909 & HDNG_v4$NAAM=="GENDEREN", 0, HDNG_v4$nonexisting)
    HDNG_v4$NAAM <- ifelse(HDNG_v4$year<1809 & HDNG_v4$NAAM=="HEESBEEN, EETHEN EN GENDEREN", 10010, HDNG_v4$NAAM)
   #Genderen -> wrong AMCO 1909-1922
    HDNG_v4$ACODE <- ifelse(HDNG_v4$NAAM=="GENDEREN", 10010, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$NAAM=="GENDEREN", 0, HDNG_v4$nonexisting)
   #Halsteren -> mistake in shapefile, municipality exists
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$NAAM=="HALSTEREN", 0, HDNG_v4$nonexisting)
   #Stadskanaal -> wrong AMCO + t/m 1968 Onstwedde
    HDNG_v4$ACODE <- ifelse(HDNG_v4$NAAM=="STADSKANAAL", 10610, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$NAAM=="STADSKANAAL", 0, HDNG_v4$nonexisting)
    HDNG_v4$NAAM <- ifelse(HDNG_v4$year<1969 & HDNG_v4$NAAM=="STADSKANAAL", "ONSTWEDDE", HDNG_v4$NAAM)
   #Valkenburg -> wrong AMCO 1942->
    HDNG_v4$ACODE <- ifelse(HDNG_v4$NAAM=="VALKENBURG HOUTHEM L", 11077, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$NAAM=="VALKENBURG HOUTHEM L", 0, HDNG_v4$nonexisting)
    HDNG_v4$ACODE <- ifelse(HDNG_v4$NAAM=="VALKENBURG AAN DE GEUL", 11077, HDNG_v4$ACODE)
    HDNG_v4$nonexisting <- ifelse(HDNG_v4$NAAM=="VALKENBURG AAN DE GEUL", 0, HDNG_v4$nonexisting)
   #Schoonebeek 1884 municipality did not exist at 1-1-1884 -> add Schoonebeek to Dalen
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884BEV1", 885+1279, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884BEV2", 790+1137, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884GEB3", 30+95, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884STE3", 16+49, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884VER1", 26+934, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884VER2", 18+832, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884VES1", 905+79, HDNG_v4$value)
    HDNG_v4$value <- ifelse(HDNG_v4$NAAM=="DALEN" & HDNG_v4$variable=="E884VES2", 800+72, HDNG_v4$value)
  #delete actually nonexisting municipalities 
    HDNG_v4 <- HDNG_v4[which(HDNG_v4$nonexisting==0),]
    
    
  #deal with double ACODEs on same variable
    length(which(!is.na(HDNG_v4$ACODE) & duplicated(HDNG_v4[,c("ACODE", "variable")])))
    length(which(!is.na(HDNG_v4$ACODE) & duplicated(HDNG_v4[,c("NAAM", "variable")])))
   #remove second ACODE for Onstwedde due to recoding
    HDNG_v4 <- HDNG_v4[which(!(HDNG_v4$NAAM=="ONSTWEDDE" & duplicated(HDNG_v4[,c("NAAM", "variable")]))),]
    
    
  #sort data
    HDNG_v4 <- HDNG_v4 %>% arrange(year, variable, ACODE)
    HDNG_v4 <- HDNG_v4[,c("ACODE", "NAAM", "variable", "description", "information", "sex", "year", "shapefile", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4")]
    colnames(HDNG_v4) <- c("amco", "name", "variable", "description", "information", "sex", "year", "visualisation_year", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4")
    
  #save output
    write.table(HDNG_v4, file="U:/Surfdrive/CLARIAH/HDNG/HDNG v4/HDNG_v4.txt", quote=T, sep =",", col.names=TRUE, row.names = F)
    
  
  
  #clean redundant files
    file.remove("HDNG v4/HDNG_long.txt")
    file.remove("HDNG v4/HDNG+.txt")
    file.remove("HDNG v4/HDNG+prov+missing.txt")
    file.remove("HDNG v4/HEDC_long.txt")
    file.remove("HDNG v4/missingHDNG.txt")
    file.remove("HDNG v4/provinces.txt")
    
