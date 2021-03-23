  #load packages
  library("lubridate") ; library("dplyr") ; library("ggplot2") ; library("reshape2") ; library("tidyr")

  #clear environment
  rm(list=ls())
  
  #define directory
  getwd()
  setwd("C:/Surfdrive/CLARIAH/HDNG")
  getwd()
  
  #load files
  #HED
  HED <- read.csv("Input/HDNG/HED DOCU.txt", header=T, sep="\t", stringsAsFactors=F)
  #HDNG
  HDNG1 <- read.csv("Input/HDNG/HDNG1.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG2 <- read.csv("Input/HDNG/HDNG2.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG3 <- read.csv("Input/HDNG/HDNG3.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG4 <- read.csv("Input/HDNG/HDNG4.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG5 <- read.csv("Input/HDNG/HDNG5.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG6 <- read.csv("Input/HDNG/HDNG6.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG7 <- read.csv("Input/HDNG/HDNG7.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG8 <- read.csv("Input/HDNG/HDNG8.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG9 <- read.csv("Input/HDNG/HDNG9.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG10 <- read.csv("Input/HDNG/HDNG10.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNG11 <- read.csv("Input/HDNG/HDNG11.txt", quote="", header=T, sep="\t", stringsAsFactors=F)
  HDNGvar <- read.csv("Input/HDNG/hdng variabelen.txt", header=T, sep="\t", stringsAsFactors=F)

  #remove duplicates
  length(which(duplicated(HDNG1$CBSNR))); length(which(duplicated(HDNG1$NAAM)))
  HDNG1 <- HDNG1[which(!duplicated(HDNG1$CBSNR)),]
  HDNG2 <- HDNG2[which(!duplicated(HDNG2$CBSNR)),]
  HDNG3 <- HDNG3[which(!duplicated(HDNG3$CBSNR)),]
  HDNG4 <- HDNG4[which(!duplicated(HDNG4$CBSNR)),]
  HDNG5 <- HDNG5[which(!duplicated(HDNG5$CBSNR)),]
  HDNG6 <- HDNG6[which(!duplicated(HDNG6$CBSNR)),]
  HDNG7 <- HDNG7[which(!duplicated(HDNG7$CBSNR)),]
  HDNG8 <- HDNG8[which(!duplicated(HDNG8$CBSNR)),]
  HDNG9 <- HDNG9[which(!duplicated(HDNG9$CBSNR)),]
  HDNG10 <- HDNG10[which(!duplicated(HDNG10$CBSNR)),]
  HDNG11 <- HDNG11[which(!duplicated(HDNG11$CBSNR)),]
  
  #set to long format
  long1 <- melt(HDNG1,id=c("CBSNR", "ACODE", "NAAM"))
  long2 <- melt(HDNG2,id=c("CBSNR", "ACODE", "NAAM"))
  long3 <- melt(HDNG3,id=c("CBSNR", "ACODE", "NAAM"))
  long4 <- melt(HDNG4,id=c("CBSNR", "ACODE", "NAAM"))
  long5 <- melt(HDNG5,id=c("CBSNR", "ACODE", "NAAM"))
  long6 <- melt(HDNG6,id=c("CBSNR", "ACODE", "NAAM"))
  long7 <- melt(HDNG7,id=c("CBSNR", "ACODE", "NAAM"))
  long8 <- melt(HDNG8,id=c("CBSNR", "ACODE", "NAAM"))
  long9 <- melt(HDNG9,id=c("CBSNR", "ACODE", "NAAM"))
  long10 <- melt(HDNG10,id=c("CBSNR", "ACODE", "NAAM"))
  long11 <- melt(HDNG11,id=c("CBSNR", "ACODE", "NAAM"))
  
  #bind HDNG
  HDNG <- rbind(long1, long2, long3, long4, long5, long6, long7, long8, long9, long10, long11)
  str(HDNG)
  HDNG$variable <- as.character(HDNG$variable)
  HDNG$topic <- substr(HDNG$variable,1,1)
  
  #identify topics in first character
  HDNG$topic_name[substr(HDNG$variable,1,1)=="A"] <- "Beroepen"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="B"] <- "Bedrijvigheid"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="C"] <- "Godsdienst"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="D"] <- "District" #eigen brouwsel
  HDNG$topic_name[substr(HDNG$variable,1,1)=="E"] <- "Bevolking"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="F"] <- "Politiek"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="G"] <- "Onderwijs"
  HDNG$topic_name[substr(HDNG$variable,1,1)=="H"] <- "Welvaart" #eigen brouwsel
  HDNG$topic_name[substr(HDNG$variable,1,1)=="I"] <- "Voorzieningen" #eigen brouwsel
  HDNG$topic_name[substr(HDNG$variable,1,1)=="J"] <- "Woningen" #eigen brouwsel
  HDNG$topic_name[substr(HDNG$variable,1,1)=="K"] <- "Openheid" #eigen brouwsel
  
  #identify year from character 2-4
  HDNG$year <- as.numeric(paste0(1,substr(HDNG$variable,2,4)))
  HDNG$year[HDNG$year==1000] <- NA
  
  #identify variable
  table(nchar(HDNG$variable))
  HDNG$suffix <- tolower(substr(HDNG$variable,5,8))
  length(which(!duplicated(HDNG$suffix)))
  length(which(!duplicated(HDNG$variable)))
  #merge variable name
  HDNGvar$topic <- toupper(HDNGvar$topic)
  HDNG <- merge(HDNG, HDNGvar, by=c("topic", "suffix"), all.x=T)
  
  #enter sources from the HED
  HED <- HED[,c(1,4:5)]
  colnames(HED)[1] <- "variable"
  HDNG <- merge(HDNG, HED, by="variable", all.x=T) 
  
  #deal with suffixes
  #change ALJ1 (mannen) into ALJ2 (vrouwen) 
  HDNG$suffix[HDNG$suffix=="ALJ1"] <- "ALJ2"
  HDNG$variable[HDNG$variable=="E971ALJ1"] <- "E971ALJ2"
  #set suffix to upper
  HDNG$suffix <- toupper(HDNG$suffix)
  #last character
  as.data.frame(table(substr(HDNG$suffix,4,4)))
  HDNG$sex <- ifelse(substr(HDNG$suffix,4,4)=="1", "M",
                     ifelse(substr(HDNG$suffix,4,4)=="2","V", 
                            ifelse(substr(HDNG$suffix,4,4)=="3","M+V", NA)))
  #delete abbreviated references to "men" in variable_name
  HDNG$variable_name <- ifelse(substr(HDNG$variable_name, nchar(HDNG$variable_name)-2, nchar(HDNG$variable_name))==", M", 
                               substr(HDNG$variable_name, 1, nchar(HDNG$variable_name)-3), HDNG$variable_name) #variable name ends in ", M"
  HDNG$variable_name <- ifelse(substr(HDNG$variable_name, nchar(HDNG$variable_name)-1, nchar(HDNG$variable_name))==" M", 
                               substr(HDNG$variable_name, 1, nchar(HDNG$variable_name)-2), HDNG$variable_name) #variable name ends in " M"
  HDNG$variable_name <- gsub(" M ", " ", HDNG$variable_name)
  HDNG$variable_name <- gsub(" M,", " ", HDNG$variable_name)
  #delete abbreviated references to "women" in variable_name
  HDNG$variable_name <- ifelse(substr(HDNG$variable_name, nchar(HDNG$variable_name)-2, nchar(HDNG$variable_name))==", V", 
                               substr(HDNG$variable_name, 1, nchar(HDNG$variable_name)-3), HDNG$variable_name) #variable name ends in ", V"
  HDNG$variable_name <- ifelse(substr(HDNG$variable_name, nchar(HDNG$variable_name)-1, nchar(HDNG$variable_name))==" V", 
                               substr(HDNG$variable_name, 1, nchar(HDNG$variable_name)-2), HDNG$variable_name) #variable name ends in " V"
  HDNG$variable_name <- gsub(" V ", " ", HDNG$variable_name)
  HDNG$variable_name <- gsub(" V,", " ", HDNG$variable_name)
  
  
  #split NOTEN
  HDNG <- HDNG %>% mutate(x=NOTEN) %>% separate(x, c("remark_1", "remark_2", "remark_3", "remark_4", "remark_5"), sep=";")
  length(which(!is.na(HDNG$remark_5)))
  length(which(!is.na(HDNG$remark_4)))
  length(which(!is.na(HDNG$remark_3)))
  length(which(!is.na(HDNG$remark_2)))
  length(which(!is.na(HDNG$remark_1)))
  
  #add citation
  HDNG$Citation <- "HDNG"
  
  #order file
  HDNG <- HDNG[,c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "BRONNEN", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")]
  colnames(HDNG) <- c("CBSNR", "ACODE", "NAAM", "variable", "suffix", "variable_name", "description", "information", "sex", "year", "topic", "topic_name", "value", "sources", "remark_1", "remark_2", "remark_3", "remark_4", "Citation")
  #save long format
  write.table(HDNG, file="HDNG v4/HDNG_long.txt", quote=FALSE, sep ="\t", col.names=TRUE, row.names = F)
  
  
  
  

  
  
  
  