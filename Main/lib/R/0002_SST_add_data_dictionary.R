addDD <- function(FOLDER, TYPE, VARIABLE, CODEBOOKDESCRIPTION, CODEBOOKVALUES, NOTESFORDL) {

#use allVariables file to generate an updated allVariables file with information
#from the data dictionary merged in (especially codebookDescription and 
#codebookValues. The new file will follow the format of the SOP and can be 
#manually edited to indicate keep/discard 
    
#LOAD LIBRARIES ###############################################################
library(tidyverse)
library(readxl)
###############################################################################

#INITIALIZE NEW COLUMNS #######################################################
newCol <- c("keepDiscard","definition","category","termType",
            "notesForOnt","notesForProvider")
allVars[,newCol] <- ""
###############################################################################

#MERGE IN DATA DICTIONARY INFORMATION #########################################
#create list of data dictionaries
if(TYPE==".txt"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataDict <- lapply(filenames, read.delim, header = T, as.is=T)
}
if(TYPE==".csv"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataDict <- lapply(filenames, read.csv, as.is=T, na.strings="")
}
if(TYPE==".xls"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataDict <- lapply(filenames, read_xls)
}


rm(filenames)

#rename variables to be merged and cut dataDict files down to those core variables
for(i in 1:length(dataDict)){
    dataDict[[i]] <- select(dataDict[[i]],variable = VARIABLE, 
                            codebookDescription = CODEBOOKDESCRIPTION,
                            codebookValues = CODEBOOKVALUES, notesForDL = NOTESFORDL)    
}

dataDict2 <- lapply(dataDict, mutate_all,as.character) #converts all columns in all files to character 
#merge data dictionary files together
dataDictToMerge <- ldply(dataDict2, data.frame)
head(dataDictToMerge)

dataDictToMerge$variable <- tolower(dataDictToMerge$variable)

#merge data dictionary with allVars file
allVars2 <- merge(allVars, dataDictToMerge, by = "variable", all = T)
###############################################################################

#ORDER COLUMNS ################################################################
allVars2 <- allVars2[c("colOrder","variable","dataFile","keepDiscard",
                       "definition","category","codebookDescription",
                       "codebookValues","termType","notesForOnt","notesForProvider",
                       "notesForDL","variable_dataFile","uniqueVar","type","example",
                       "dateTime","formatCode","values",
                       "uniqueValueCount","timeVarying","percentMissing","flag")]
###############################################################################
}