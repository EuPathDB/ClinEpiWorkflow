updateValueMap <- function(valueMap, allVars, OUTPUT) {
# After manual edits to allVar file, update valueMap file to only include
# variables that will be displayed. Then go through by hand to remove/add 
# valueMapping as needed

#LOAD LIBRARIES ###############################################################
library(purrr)
library(plyr)
library(dplyr)
###############################################################################

#REMOVE VARIABLES MARKED AS "DISCARD" #########################################
toKeep <- allVars$uniqueVar[!(allVars$keepDiscard == "discard")]
dim(valueMap)
valueMap <- valueMap[valueMap$uniqueVar %in% toKeep, c("variable","values","uniqueVar")]
dim(valueMap)
###############################################################################

#CHANGE NA TO CHARACTER NA ####################################################
valueMap$values[is.na(valueMap$values)] <- "NA"
is.na(valueMap$values)
###############################################################################

#REMOVE ROWS WHERE VALUES == "" ###############################################
valueMap <- valueMap[valueMap$values  != "" | is.na(valueMap$values),]
dim(valueMap)
###############################################################################

#UPDATE FORMAT AND SAVE #######################################################
valueMap$mappedTerm <- ""
valueMap$variable2 <- valueMap$uniqueVar
valueMap <- valueMap[,c("variable","variable2","values","mappedTerm")]
write.table(valueMap, OUTPUT, sep = "\t", quote = F, row.names = F)
###############################################################################

valueMap
}