##################################################
## Title: createIDMapTemplate.R
## Script purpose: To create a boiler plate text 
## file from study's conversion. Useful for 
## studies with many datafiles                
## Date: 2022-11-15
## Author: Steph Wever Schulman
##################################################

library(tidyverse)

# LIMITATIONS - This script does not create nor check for parent enities for any of the "repeated" (ie crm, hrm, prm) entities. 

# SET FILE PATHS
convertFilePath <- "/Users/swever/Documents/GitHub/ApiCommonData/Load/ontology/Gates/crypto/doc/crypto_conversion.csv"
idMapOutputPath <- "/Users/swever/Documents/GitHub/ApiCommonData/Load/ontology/Gates/crypto/idMap.txt"


# READ IN CONVERSION FILE AND AGGREGATE ALL ENTITIES PER ALL DATAFILES
convertFile <- read_csv(convertFilePath)
convertFileKeep <- select(convertFile, c("iri", "label", "variable", "category", "repeated"))
convertFileKeep <- convertFileKeep %>%
  separate_rows(variable, sep = "[|]")
convertFileKeep$variable <- str_trim(convertFileKeep$variable)
convertFileKeep$dataFile <- sub("\\::.*", "", convertFileKeep$variable)
convertFileKeep <- convertFileKeep %>%
  distinct(dataFile, category, repeated)
convertFileKeep <- convertFileKeep[
  with(convertFileKeep, order(dataFile, category, repeated)),]

# CREATE STANDARD TEXT FOR ID COLUMN IN IDMAP
idMap <- convertFileKeep
idMap$id <- ""
idMap$id <- ifelse(idMap$category == "Community" & is.na(idMap$repeated), "{{c}}+",
                   ifelse(idMap$category == "Community" & !is.na(idMap$repeated),  "{{cr}}+",
                   ifelse(idMap$category == "Household" & is.na(idMap$repeated), "{{h}}+",
                   ifelse(idMap$category == "Household" & !is.na(idMap$repeated), "{{hr}}+",
                   ifelse(idMap$category == "Participant" & is.na(idMap$repeated), "{{p}}+",
                   ifelse(idMap$category == "Participant" & !is.na(idMap$repeated), "{{pr}}+", 
                   ifelse(idMap$category == "Sample", "{{s}}+", "")))))))

# CORRECTS CATEGORY FOR REPEATED MEASURES
idMap$category <- ifelse(idMap$id == "{{cr}}+", "community_repeated_measures",
                ifelse(idMap$id == "{{hr}}+", "household_repeated_measures",
                  ifelse (idMap$id == "{{pr}}+", "participant_repeated_measures", idMap$category)))


idMap$category <- tolower(idMap$category) 

idMap <- idMap[
  with(idMap, order(dataFile, category)),] #SORTS BY DATAFILE AND CATEGORY

colOrder <- c("dataFile", "category", "id")
idMap <- idMap[, colOrder] # REORDERS OUTPUT

#WRITE TAB FILE
write_tsv(idMap, idMapOutputPath)



