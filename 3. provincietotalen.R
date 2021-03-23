  #load packages
  library("lubridate") ; library("dplyr") ; library("ggplot2") ; library("reshape2") ; library("tidyr")

  rm(list=ls())
  
  #define directory
  getwd()
  setwd("C:/Surfdrive/CLARIAH/HDNG")
  getwd()
  
  #load HED
  HEDA <- read.csv("Input/HED/HEDAdata.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDB <- read.csv("Input/HED/HEDBdata.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDC <- read.csv("Input/HED/HEDCdata.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDE1 <- read.csv("Input/HED/HEDEdata1.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDE2 <- read.csv("Input/HED/HEDEdata2.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDF <- read.csv("Input/HED/HEDFdata.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDrest <- read.csv("Input/HED/HEDoverig.txt",header=T, sep="\t", stringsAsFactors=F)
  colnames(HEDC)[1] <- "databnr"
  colnames(HEDE1)[1] <- "databnr"
  colnames(HEDE2)[1] <- "databnr"
  #HDNG+
  HDNG <- read.csv("HDNG v4/HDNG+.txt", header=T, sep="\t", stringsAsFactors=F)
  #sources
  HDNGvar <- read.csv("Input/HDNG/hdng variabelen.txt", header=T, sep="\t", stringsAsFactors=F)
  HED <- read.csv("Input/HDNG/HED DOCU.txt", header=T, sep="\t", stringsAsFactors=F)
  
  #select provinces + NL
  HEDA <- HEDA[which(is.na(HEDA$cbsnr)),]
  HEDB <- HEDB[which(is.na(HEDB$cbsnr)),]
  HEDC <- HEDC[which(is.na(HEDC$cbsnr)),]
  HEDE1 <- HEDE1[which(is.na(HEDE1$cbsnr)),]
  HEDE2 <- HEDE2[which(is.na(HEDE2$cbsnr)),]
  HEDF <- HEDF[which(is.na(HEDF$cbsnr)),]
  HEDrest <- HEDrest[which(is.na(HEDrest$cbsnr)),]
  
  #delete databnr & cbsnr
  HEDA <- HEDA[,-(1:2)]
  HEDB <- HEDB[,-(1:2)]
  HEDC <- HEDC[,-(1:2)]
  HEDE1 <- HEDE1[,-(1:2)]
  HEDE2 <- HEDE2[,-(1:2)]
  HEDF <- HEDF[,-(1:2)]
  HEDrest <- HEDrest[,-(1:2)]
  
  #add CBSNR & ACODE
  HEDA$ACODE <- NA; HEDA$CBSNR <- NA
  HEDB$ACODE <- NA; HEDB$CBSNR <- NA
  HEDC$ACODE <- NA; HEDC$CBSNR <- NA
  HEDE1$ACODE <- NA; HEDE1$CBSNR <- NA
  HEDE2$ACODE <- NA; HEDE2$CBSNR <- NA
  HEDF$ACODE <- NA; HEDF$CBSNR <- NA
  HEDrest$ACODE <- NA; HEDrest$CBSNR <- NA
  
  #melt & bind
  HEDA <- melt(HEDA,id=c("CBSNR", "ACODE", "naam"))
  HEDB <- melt(HEDB,id=c("CBSNR", "ACODE", "naam"))
  HEDC <- melt(HEDC,id=c("CBSNR", "ACODE", "naam"))
  HEDE1 <- melt(HEDE1,id=c("CBSNR", "ACODE", "naam"))
  HEDE2 <- melt(HEDE2,id=c("CBSNR", "ACODE", "naam"))
  HEDF <- melt(HEDF,id=c("CBSNR", "ACODE", "naam"))
  HEDrest <- melt(HEDrest,id=c("CBSNR", "ACODE", "naam"))
  
  #HDNG of HED?
  HEDA$Citation <- "HDNG"
  HEDB$Citation <- "HDNG"
  HEDC$Citation <- "HED"
  HEDE1$Citation <- "HDNG"
  HEDE2$Citation <- "HDNG"
  HEDF$Citation <- "HDNG"
  HEDrest$Citation <- "HDNG"
  
  #bind
  prov <- rbind(HEDA, HEDB, HEDC, HEDE1, HEDE2, HEDF, HEDrest)
  prov$variable <- toupper(prov$variable)
  prov$variable <- as.character(prov$variable)
  prov$topic <- substr(prov$variable,1,1)
  
  #identify topics in first character
  prov$topic_name[substr(prov$variable,1,1)=="A"] <- "Beroepen"
  prov$topic_name[substr(prov$variable,1,1)=="B"] <- "Bedrijvigheid"
  prov$topic_name[substr(prov$variable,1,1)=="C"] <- "Godsdienst"
  prov$topic_name[substr(prov$variable,1,1)=="D"] <- "District" #eigen brouwsel
  prov$topic_name[substr(prov$variable,1,1)=="E"] <- "Bevolking"
  prov$topic_name[substr(prov$variable,1,1)=="F"] <- "Politiek"
  prov$topic_name[substr(prov$variable,1,1)=="G"] <- "Onderwijs"
  prov$topic_name[substr(prov$variable,1,1)=="H"] <- "Welvaart" #eigen brouwsel
  prov$topic_name[substr(prov$variable,1,1)=="I"] <- "Voorzieningen" #eigen brouwsel
  prov$topic_name[substr(prov$variable,1,1)=="J"] <- "Woningen" #eigen brouwsel
  prov$topic_name[substr(prov$variable,1,1)=="K"] <- "Openheid" #eigen brouwsel
  
  #identify year from character 2-4
  prov$year <- as.numeric(paste0(1,substr(prov$variable,2,4)))
  prov$year[prov$year==1000] <- NA
  
  #identify variable
  table(nchar(prov$variable))
  prov$suffix <- tolower(substr(prov$variable,5,8))
  length(which(!duplicated(prov$suffix)))
  length(which(!duplicated(prov$variable)))
  #merge variable name
  HDNGvar$topic <- toupper(HDNGvar$topic)
  prov <- merge(prov, HDNGvar, by=c("topic", "suffix"), all.x=T)
  
  #enter sources from the HED
  HED <- HED[,c(1,4:5)]
  colnames(HED)[1] <- "variable"
  prov <- merge(prov, HED, by="variable", all.x=T)
  
  #deal with suffixes
  prov$suffix[prov$suffix=="ALJ1"] <- "ALJ2"
  prov$variable[prov$variable=="E971ALJ1"] <- "E971ALJ2"
  #set suffix to upper
  prov$suffix <- toupper(prov$suffix)
  #last character
  as.data.frame(table(substr(prov$suffix,4,4)))
  prov$sex <- ifelse(substr(prov$suffix,4,4)=="1", "M",
                     ifelse(substr(prov$suffix,4,4)=="2","V", 
                            ifelse(substr(prov$suffix,4,4)=="3","M+V", NA)))
  #delete abbreviated references to "men" in variable_name
  prov$variable_name <- ifelse(substr(prov$variable_name, nchar(prov$variable_name)-2, nchar(prov$variable_name))==", M", 
                                substr(prov$variable_name, 1, nchar(prov$variable_name)-3), prov$variable_name) #variable name ends in ", M"
  prov$variable_name <- ifelse(substr(prov$variable_name, nchar(prov$variable_name)-1, nchar(prov$variable_name))==" M", 
                                substr(prov$variable_name, 1, nchar(prov$variable_name)-2), prov$variable_name) #variable name ends in " M"
  prov$variable_name <- gsub(" M ", " ", prov$variable_name)
  prov$variable_name <- gsub(" M,", " ", prov$variable_name)
  #delete abbreviated references to "women" in variable_name
  prov$variable_name <- ifelse(substr(prov$variable_name, nchar(prov$variable_name)-2, nchar(prov$variable_name))==", V", 
                                substr(prov$variable_name, 1, nchar(prov$variable_name)-3), prov$variable_name) #variable name ends in ", V"
  prov$variable_name <- ifelse(substr(prov$variable_name, nchar(prov$variable_name)-1, nchar(prov$variable_name))==" V", 
                                substr(prov$variable_name, 1, nchar(prov$variable_name)-2), prov$variable_name) #variable name ends in " V"
  prov$variable_name <- gsub(" V ", " ", prov$variable_name)
  prov$variable_name <- gsub(" V,", " ", prov$variable_name)
  
  
  #split NOTEN
  prov <- prov %>% mutate(x=NOTEN) %>% separate(x, c("remark_1", "remark_2", "remark_3", "remark_4", "remark_5"), sep=";")
  length(which(!is.na(prov$remark_5)))
  length(which(!is.na(prov$remark_4)))
  length(which(!is.na(prov$remark_3)))
  length(which(!is.na(prov$remark_2)))
  length(which(!is.na(prov$remark_1)))
  
  #order file
  prov <- prov[,c("CBSNR", "ACODE", "naam", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "BRONNEN", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")]
  colnames(prov) <- c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")
  #save long format
  write.table(prov, file="HDNG v4/provinces.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  
  
  