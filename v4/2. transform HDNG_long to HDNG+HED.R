  #load packages
  library("lubridate") ; library("dplyr") ; library("ggplot2") ; library("reshape2") ; library("tidyr")

  #clear environment
  rm(list=ls())
  
  #define directory
  getwd()
  setwd("U:/Surfdrive/CLARIAH/HDNG")
  getwd()
  
  #load datasets
  #HED, religion
  HEDC <- read.csv("Input/HED/HEDCdata.txt",header=T, sep="\t", stringsAsFactors=F)
  colnames(HEDC)[1] <- "databnr"
  #sources
  HDNGvar <- read.csv("Input/HDNG/hdng variabelen.txt", header=T, sep="\t", stringsAsFactors=F)
  HED <- read.csv("Input/HDNG/HED DOCU.txt", header=T, sep="\t", stringsAsFactors=F)
  #HDNG
  HDNG <- read.csv("HDNG v4/HDNG_long.txt", header=T, sep="\t", stringsAsFactors=F)
  
  #koppeltabel maken
  HDNG2 <- HDNG[,c("CBSNR", "ACODE", "NAAM")]
  HDNG2 <- HDNG2 %>% group_by(CBSNR, ACODE, NAAM) %>% filter(row_number()==1) %>% ungroup()
  length(which(duplicated(HDNG2$CBSNR))); length(which(duplicated(HDNG2$ACODE))); length(which(duplicated(HDNG2$NAAM)))
  
  #match cases
  HEDC2 <- merge(HEDC, HDNG2, by.x="naam", by.y="NAAM", all=T)
  length(which(!is.na(HEDC2$CBSNR))); length(which(is.na(HEDC2$CBSNR))) #13 missings: 12 provinces + country
  length(which(!is.na(HEDC2$ACODE))); length(which(is.na(HEDC2$ACODE))) #13 missings: 12 provinces + country
  
  #check consistency
  length(which(HEDC2$CBSNR==HEDC2$cbsnr)); length(which(HEDC2$CBSNR!=HEDC2$cbsnr)) #cbsnr equal in all cases
  HEDC2 <- HEDC2[,-c(2:3)] #remove cbsnr, databnr
  colnames(HEDC2)[1] <- "NAAM"
  
  #drop province/country totals
  HEDC2 <- HEDC2[which(!is.na(HEDC2$CBSNR)),]
  
  #set to long format
  long1 <- melt(HEDC2,id=c("CBSNR", "ACODE", "NAAM"))
  long1$variable <- toupper(long1$variable)
  long1$variable <- as.character(long1$variable)
  long1$topic <- substr(long1$variable,1,1)
  
  #identify topics in first character
  long1$topic_name[substr(long1$variable,1,1)=="A"] <- "Beroepen"
  long1$topic_name[substr(long1$variable,1,1)=="B"] <- "Bedrijvigheid"
  long1$topic_name[substr(long1$variable,1,1)=="C"] <- "Godsdienst"
  long1$topic_name[substr(long1$variable,1,1)=="D"] <- "District" #eigen brouwsel
  long1$topic_name[substr(long1$variable,1,1)=="E"] <- "Bevolking"
  long1$topic_name[substr(long1$variable,1,1)=="F"] <- "Politiek"
  long1$topic_name[substr(long1$variable,1,1)=="G"] <- "Onderwijs"
  long1$topic_name[substr(long1$variable,1,1)=="H"] <- "Welvaart" #eigen brouwsel
  long1$topic_name[substr(long1$variable,1,1)=="I"] <- "Voorzieningen" #eigen brouwsel
  long1$topic_name[substr(long1$variable,1,1)=="J"] <- "Woningen" #eigen brouwsel
  long1$topic_name[substr(long1$variable,1,1)=="K"] <- "Openheid" #eigen brouwsel
  
  #identify year from character 2-4
  long1$year <- as.numeric(paste0(1,substr(long1$variable,2,4)))
  long1$year[long1$year==1000] <- NA
  
  #identify variable
  table(nchar(long1$variable))
  long1$suffix <- tolower(substr(long1$variable,5,8))
  length(which(!duplicated(long1$suffix)))
  length(which(!duplicated(long1$variable)))
  #merge variable name & 
  HDNGvar$topic <- toupper(HDNGvar$topic)
  long1 <- merge(long1, HDNGvar, by=c("topic", "suffix"), all.x=T)
  
  #enter sources from the HED
  HED <- HED[,c(1,4:5)]
  colnames(HED)[1] <- "variable"
  long1 <- merge(long1, HED, by="variable", all.x=T)
  
  #deal with suffixes
  #set suffix to upper
  long1$suffix <- toupper(long1$suffix)
  #last character
  as.data.frame(table(substr(long1$suffix,4,4)))
  long1$sex <- ifelse(substr(long1$suffix,4,4)=="1", "M",
                     ifelse(substr(long1$suffix,4,4)=="2","V", 
                            ifelse(substr(long1$suffix,4,4)=="3","_T", NA)))
  #delete abbreviated references to "men" in variable_name
  long1$variable_name <- ifelse(substr(long1$variable_name, nchar(long1$variable_name)-2, nchar(long1$variable_name))==", M", 
                               substr(long1$variable_name, 1, nchar(long1$variable_name)-3), long1$variable_name) #variable name ends in ", M"
  long1$variable_name <- ifelse(substr(long1$variable_name, nchar(long1$variable_name)-1, nchar(long1$variable_name))==" M", 
                               substr(long1$variable_name, 1, nchar(long1$variable_name)-2), long1$variable_name) #variable name ends in " M"
  long1$variable_name <- gsub(" M ", " ", long1$variable_name)
  long1$variable_name <- gsub(" M,", " ", long1$variable_name)
  #delete abbreviated references to "women" in variable_name
  long1$variable_name <- ifelse(substr(long1$variable_name, nchar(long1$variable_name)-2, nchar(long1$variable_name))==", V", 
                               substr(long1$variable_name, 1, nchar(long1$variable_name)-3), long1$variable_name) #variable name ends in ", V"
  long1$variable_name <- ifelse(substr(long1$variable_name, nchar(long1$variable_name)-1, nchar(long1$variable_name))==" V", 
                               substr(long1$variable_name, 1, nchar(long1$variable_name)-2), long1$variable_name) #variable name ends in " V"
  long1$variable_name <- gsub(" V ", " ", long1$variable_name)
  long1$variable_name <- gsub(" V,", " ", long1$variable_name)
  
  
  #split NOTEN
  long1 <- long1 %>% mutate(x=NOTEN) %>% separate(x, c("remark_1", "remark_2", "remark_3", "remark_4", "remark_5"), sep=";")
  length(which(!is.na(long1$remark_5)))
  length(which(!is.na(long1$remark_4)))
  length(which(!is.na(long1$remark_3)))
  length(which(!is.na(long1$remark_2)))
  length(which(!is.na(long1$remark_1)))
  
  long1$Citation <- "HED"
  
  #order file
  long1 <- long1[,c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "BRONNEN", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")]
  colnames(long1) <- c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")
  #save long format
  write.table(long1, file="HDNG v4/HEDC_long.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  #bind long1 & hdng
  HDNG <- HDNG[!(HDNG$variable %in% long1$variable),]
  HDNGplus <- rbind(HDNG, long1)
  write.table(HDNGplus, file="HDNG v4/HDNG+.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  
  
  
