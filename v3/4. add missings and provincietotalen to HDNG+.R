  #load packages
  library("lubridate") ; library("dplyr") ; library("ggplot2") ; library("reshape2") ; library("tidyr")
  
  rm(list=ls())
  
  #setwd
  setwd("C:/Surfdrive/CLARIAH/HDNG")
  
  #load HDNG+
  HDNG <- read.csv("HDNG v3/HDNG+.txt", header=T, sep="\t", stringsAsFactors=F)
  #provincietotalen
  prov <- read.csv("HDNG v3/Provinces.txt", header=T, sep="\t", stringsAsFactors=F)
  #sources
  HDNGvar <- read.csv("Input/HDNG/hdng variabelen.txt", header=T, sep="\t", stringsAsFactors=F)
  HED <- read.csv("Input/HDNG/HED DOCU.txt", header=T, sep="\t", stringsAsFactors=F)
  
  #check whether all variables are in the HDNG
  x <- HDNG[!(HDNG$variable %in% prov$variable),]
  length(which(!duplicated(x$variable)))
  y <- prov[!(prov$variable %in% HDNG$variable),]
  length(which(!duplicated(y$variable)))
  as.data.frame(table(tolower(y$variable)))
  
  #load required HED files
  HEDE1 <- read.csv("Input/HED/HEDEdata1.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDE2 <- read.csv("Input/HED/HEDEdata2.txt",header=T, sep="\t", stringsAsFactors=F)
  HEDrest <- read.csv("Input/HED/HEDoverig.txt",header=T, sep="\t", stringsAsFactors=F)
  colnames(HEDE1)[1] <- "databnr"
  colnames(HEDE2)[1] <- "databnr"
  
  #select missing variables
  HEDE1 <- HEDE1[,c("naam","e849bed1","e849bed2","e859bev1","e859bev2","e859bev3")]
  HEDE2 <- HEDE2[,c("naam","e971hhou","e971nghh")]
  HEDrest <- HEDrest[,c("naam","d920egcb","d930egcb","d940egcc","d951egcd","d966ecsa","d971egca", "i889opha", "k947sfor")]
  
  #add CBSNR & ACODE
  HEDE1$ACODE <- NA; HEDE1$CBSNR <- NA
  HEDE2$ACODE <- NA; HEDE2$CBSNR <- NA
  HEDrest$ACODE <- NA; HEDrest$CBSNR <- NA
  
  #melt & bind
  HEDE1 <- melt(HEDE1,id=c("CBSNR", "ACODE", "naam"))
  HEDE2 <- melt(HEDE2,id=c("CBSNR", "ACODE", "naam"))
  HEDrest <- melt(HEDrest,id=c("CBSNR", "ACODE", "naam"))
  
  #bind
  missingHDNG <- rbind(HEDE1, HEDE2, HEDrest)
  missingHDNG$variable <- toupper(missingHDNG$variable)
  missingHDNG$variable <- as.character(missingHDNG$variable)
  missingHDNG$topic <- substr(missingHDNG$variable,1,1)
  
  #identify topics in first character
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="A"] <- "Beroepen"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="B"] <- "Bedrijvigheid"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="C"] <- "Godsdienst"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="D"] <- "District" #eigen brouwsel
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="E"] <- "Bevolking"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="F"] <- "Politiek"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="G"] <- "Onderwijs"
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="H"] <- "Welvaart" #eigen brouwsel
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="I"] <- "Voorzieningen" #eigen brouwsel
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="J"] <- "Woningen" #eigen brouwsel
  missingHDNG$topic_name[substr(missingHDNG$variable,1,1)=="K"] <- "Openheid" #eigen brouwsel
  
  #identify year from character 2-4
  missingHDNG$year <- as.numeric(paste0(1,substr(missingHDNG$variable,2,4)))
  missingHDNG$year[missingHDNG$year==1000] <- NA
  
  #identify variable
  table(nchar(missingHDNG$variable))
  missingHDNG$suffix <- tolower(substr(missingHDNG$variable,5,8))
  length(which(!duplicated(missingHDNG$suffix)))
  length(which(!duplicated(missingHDNG$variable)))
  #merge variable name
  HDNGvar <- HDNGvar[,c(1,3:5)]
  missingHDNG <- merge(missingHDNG, HDNGvar, by="suffix", all.x=T)
  
  #enter sources from the HED
  HED <- HED[,c(1,4:5)]
  colnames(HED)[1] <- "variable"
  missingHDNG <- merge(missingHDNG, HED, by="variable", all.x=T)
  
  #deal with suffixes
  #set suffix to upper
  missingHDNG$suffix <- toupper(missingHDNG$suffix)
  #last character
  as.data.frame(table(substr(missingHDNG$suffix,4,4)))
  missingHDNG$sex <- ifelse(substr(missingHDNG$suffix,4,4)=="1", "M",
                            ifelse(substr(missingHDNG$suffix,4,4)=="2","V", NA))
  #delete abbreviated references to "men" in variable_name
  missingHDNG$variable_name <- ifelse(substr(missingHDNG$variable_name, nchar(missingHDNG$variable_name)-2, nchar(missingHDNG$variable_name))==", M", 
                               substr(missingHDNG$variable_name, 1, nchar(missingHDNG$variable_name)-3), missingHDNG$variable_name) #variable name ends in ", M"
  missingHDNG$variable_name <- ifelse(substr(missingHDNG$variable_name, nchar(missingHDNG$variable_name)-1, nchar(missingHDNG$variable_name))==" M", 
                               substr(missingHDNG$variable_name, 1, nchar(missingHDNG$variable_name)-2), missingHDNG$variable_name) #variable name ends in " M"
  missingHDNG$variable_name <- gsub(" M ", " ", missingHDNG$variable_name)
  missingHDNG$variable_name <- gsub(" M,", " ", missingHDNG$variable_name)
  #delete abbreviated references to "women" in variable_name
  missingHDNG$variable_name <- ifelse(substr(missingHDNG$variable_name, nchar(missingHDNG$variable_name)-2, nchar(missingHDNG$variable_name))==", V", 
                               substr(missingHDNG$variable_name, 1, nchar(missingHDNG$variable_name)-3), missingHDNG$variable_name) #variable name ends in ", V"
  missingHDNG$variable_name <- ifelse(substr(missingHDNG$variable_name, nchar(missingHDNG$variable_name)-1, nchar(missingHDNG$variable_name))==" V", 
                               substr(missingHDNG$variable_name, 1, nchar(missingHDNG$variable_name)-2), missingHDNG$variable_name) #variable name ends in " V"
  missingHDNG$variable_name <- gsub(" V ", " ", missingHDNG$variable_name)
  missingHDNG$variable_name <- gsub(" V,", " ", missingHDNG$variable_name)
  
  
  #split NOTEN
  missingHDNG <- missingHDNG %>% mutate(x=NOTEN) %>% separate(x, c("remark_1", "remark_2", "remark_3", "remark_4", "remark_5"), sep=";")
  length(which(!is.na(missingHDNG$remark_5)))
  length(which(!is.na(missingHDNG$remark_4)))
  length(which(!is.na(missingHDNG$remark_3)))
  length(which(!is.na(missingHDNG$remark_2)))
  length(which(!is.na(missingHDNG$remark_1)))
  
  missingHDNG$Citation <- "HED"
  
  #order file
  missingHDNG <- missingHDNG[,c("CBSNR", "ACODE", "naam", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "BRONNEN", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")]
  colnames(missingHDNG) <- c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")
  #save long format
  write.table(missingHDNG, file="HDNG v3/missingHDNG.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  #bind long1 & hdng
  HDNGfinal <- rbind(HDNG, missingHDNG, prov)
  HDNGfinal$Citation <- NULL
  write.table(HDNGfinal, file="HDNG v3/HDNG_v3.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  #clean redundant files
  file.remove("HDNG v3/HDNG_long.txt")
  file.remove("HDNG v3/HDNG+.txt")
  file.remove("HDNG v3/HEDC_long.txt")
  file.remove("HDNG v3/missingHDNG.txt")
  file.remove("HDNG v3/provinces.txt")
  