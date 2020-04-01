#NOTE even though varMap and valueMap files were created in 000_explore, import the files using the scripts because not all had info

rm(list=ls())

###############################################
###############################################
###############################################
# change information in this section only:

setwd("~/Box/Danica EuPathDB/Studies/LLINE-UP/DH_workspace/Data") 
OUTPUT <- "./Processed data/001.LLINEUP_allVars.RData"                               # SET FOLDER & FILE NAME WHERE OUTPUT WILL BE SAVED                                    
source("~/Documents/GitHub/ClinEpiWorkflow/Main/lib/R/000a_studyImport_function.R")         # SET LOCATION OF STUDY IMPORT FUNCTION

FOLDER <- "./Data from provider"     # SET FOLDER CONTAINING RAW DATA FILES FROM PROVIDER
STUDY <- "LLINEUP"                      # SET DATASET (OR STUDY)
PARTICIPANT_ID <- "not applicable"                  # set to "not applicable" to skip calculation of time varying
MISSING <- c(NA, "na", "NA", "n/a", "N/A", "", ".") # SET MISSING VALUES (NA, "na", "NA", "n/a", "N/A", "", ".")
DATE_TIME <- c("date", "dob")                       # SET REGEX FOR POSSIBLE DATE/TIME VARIABLES. NOTE: KEEP LOWERCASE
TYPE <- ".dta"                                      # SET FILE TYPE EXTENSION: 
# (".csv", ".txt", ".RData", ".sas7bdat", ".dta", ".sav", ".zsav", ".por")
# NOTE: EXTENSION MUST EXACTLY MATCH ONE OF THE SUGGESTIONS. 
# FOR EXCEL FILES --> SAVE AS .csv FILES BEFORE RUNNING SCRIPTS
# IF EXTENSION IS NOT REPRESENTED IN THE LIST, TALK TO DANICA TO UPDATE CODE

###############################################
###############################################
###############################################
# use studyImport function to get allVars, valueMap, and dataFiles

temp <- studyImport(FOLDER, TYPE, STUDY, MISSING, DATE_TIME, PARTICIPANT_ID)

allVars <- temp[[1]] 
valueMap <- temp[[2]]
dataFiles <- temp[[3]]
originalFiles <- temp[[4]]



#############################################################
# use labelled package to pull out variable labels

library(labelled)

allVars$label <- ""
for(i in unique(allVars$dataFile)){
  temp <- dataFiles[[i]]
  for(j in names(temp)){
    if(allVars$type[allVars$dataFile==i & allVars$variable==j]=="dbl+lbl"){
      if(!is.null(var_label(temp[,j]))){
        allVars$label[allVars$dataFile==i & allVars$variable==j] <- var_label(temp[,j])
      }
    }
  }
}


#############################################################
# use labelled package to pull out mapped values

values2 <- data.frame(dataFile=character(0), variable=character(0), value=character(0), mappedTerm=character(0))

for(i in names(dataFiles)){
  temp <- dataFiles[[i]]
  
  for(j in names(temp)){
    if(!is.null(val_labels(temp[,j]))){
      temp2 <- data.frame(dataFile=names(dataFiles[i]),
                          variable=rep(j, length(names(val_labels(temp[,j])))),
                          value=as.vector(unlist(val_labels(temp[j]))),
                          mappedTerm=names(val_labels(temp[,j])),
                          labeledValue=T)
      temp2$dataFile <- as.character(temp2$dataFile)
      temp2$variable <- as.character(temp2$variable)
      temp2$value <- as.character(temp2$value)
      temp2$mappedTerm <- as.character(temp2$mappedTerm)
      
      values2 <- rbind(values2, temp2)
      rm(temp2)
    }
    
    if(is.null(val_labels(temp[,j]))){
      temp2 <- data.frame(dataFile=names(dataFiles[i]),
                          variable=j,
                          value="NULL",
                          mappedTerm="NULL",
                          labeledValue=F)
      temp2$dataFile <- as.character(temp2$dataFile)
      temp2$variable <- as.character(temp2$variable)
      temp2$value <- as.character(temp2$value)
      temp2$mappedTerm <- as.character(temp2$mappedTerm)
      
      values2 <- rbind(values2, temp2)
      rm(temp2)
    }
  }
}


#############################################################
# update uniqueVar

allVars$uniqueVar <- paste(allVars$dataFile, allVars$variable, sep="::")
valueMap$uniqueVar <- paste(valueMap$dataFile, valueMap$variable, sep="::")
values2$uniqueVar <- paste(values2$dataFile, values2$variable, sep="::")


#############################################################
# update mappedTerm for continuous or > 20 categories

unique(values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values=="continuous"], "mappedTerm"])
# [1] "NULL"
values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values=="continuous"], "mappedTerm"] <- "continuous"
values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values=="continuous"], "value"] <- "continuous"

unique(values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values==">20 categories"], "mappedTerm"])
#[1] "NULL"
values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values==">20 categories"], "mappedTerm"] <- ">20 categories"
values2[values2$uniqueVar %in% valueMap$uniqueVar[valueMap$values==">20 categories"], "value"] <- ">20 categories"


#############################################################
# variables without labels in the .dta files only occupy 1 row in the values2 dataframe
# determine which rows correspond to these variables in the valueMap file, and save as missing_values
# then remove variables without labels from the values2 datafile and replace these with missing_values 

missing_values <- data.frame(uniqueVar=character(0), variable=character(0), dataFile=character(0), value=character(0), 
                             mappedTerm=character(0), labeledValue=character(0))

for(i in unique(values2$uniqueVar[values2$mappedTerm=="NULL"])){
  if(i %in% valueMap$uniqueVar==F){print(i)}
  temp <- valueMap[valueMap$uniqueVar==i, c("uniqueVar", "dataFile", "variable", "values")]
  temp$labeledValue <- F
  temp$mappedTerm <- ""
  
  missing_values <- rbind(missing_values, temp)
  print(dim(missing_values))
}

head(missing_values)
head(values2)

names(values2)[names(values2)=="value"] <- "values"

values2 <- values2[values2$uniqueVar %in% missing_values$uniqueVar==F,]
values2 <- rbind(values2, missing_values)



#############################################################
# overwrite valueMap

valueMap <- values2
valueMap$dataSet <- "LLINEUP"


#############################################################
# save workspace

head(valueMap)
dim(valueMap)
#[1] 4707 7

head(allVars)
dim(allVars)
#[1] 1260 19

save(allVars, valueMap, dataFiles, originalFiles, file=OUTPUT)
write.csv(allVars, file="./Processed data/LLINEUP_001_allVars.csv", row.names=F)
write.csv(valueMap, file="./Processed data/LLINEUP_001_valueMap.csv", row.names=F)
