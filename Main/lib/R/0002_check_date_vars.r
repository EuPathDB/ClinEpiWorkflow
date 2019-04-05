rm(list=ls())

#############################################################
# set working directory, input, and output

setwd("~/Box/Danica EuPathDB/Studies/MAL-ED/0 to 36 months/DH_workspace/Data")             # SET WORKING DIRECTORY

source("~/Documents/SVN/ClinEpiWorkflow/trunk/Main/lib/R/000b_dateConversion_function.R")  # SET LOCATION OF DATE CONVERSION FUNCTION

INPUT  <- "./Processed data/001.MALED_0to36m_allVars.RData"                                # SET INPUT
OUTPUT <- "./Processed data/002.MALED_0to36m_dateCheck.RData"                              # SET WHERE OUTPUT WILL BE SAVED                                    


#############################################################
# load files

print(load(INPUT))


#############################################################
# hand check: view variables potentially identified as dates (type=="date?"), change type=="date" where appropriate

allVars[allVars$dateTime=="date?" & !is.na(allVars$dateTime), c("uniqueVar", "type", "example", "dateTime")]
allVars[allVars$dateTime=="date?" & !is.na(allVars$dateTime) & !is.na(allVars$example), "type"] <- "date"


#############################################################
# determine min and max values for date variables using functions in 0000_dateConversion_function

dateDF <- allVars[allVars$type=="date" & !is.na(allVars$type),]

if(length(dateDF$uniqueVar) >0){
  allVars <- dateConversion(dateDF, allVars, dataFiles)        # dateConversion() determines min, max, and value range for dates in allVars
  dataFiles <- dateConversion_DF(dateDF, allVars, dataFiles)   # dateConversion_DF() converts dates in dataFiles to as.Date format
}


#############################################################
# check by hand to see if min, max, and range (values) of date variables look OK

allVars[allVars$dateTime=="date?" & !is.na(allVars$dateTime), c("variable_dataFile", "type", "example", "dateTime", "formatCode", "minValue", "maxValue", "values")]


#############################################################
# manually check for other date variables 
# scroll through the "example" for every variable & seeing if anything looks like it may be a date

unique(allVars$example[allVars$type!="date"]) 


#############################################################
# veiw allVars information for other variables that could potentially be dates
# correct "type", "min", "max", "values" if you find other date  variables

allVars[allVars$example %in% c("1MPO30OCT13NP","1NEO06SEP13BR","1MPO09MAR13BG","1NEO26OCT13BG","1MPO05FEB13BG","1NEO24OCT13BG","1MPO11JUN12BG"),]


#############################################################
# fix valueMap so that all variables identified as dates are marked as "continuous" & clean up valueMap

valueMap[valueMap$uniqueVar %in% allVars$uniqueVar[allVars$type=="date"],"values"] <- "continuous"
valueMap <- distinct(valueMap)


#############################################################
# save workspace
save(allVars, valueMap, dataFiles, file=OUTPUT)