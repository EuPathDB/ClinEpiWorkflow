makeVariableMap <- function(allVar, OUTPUT) {

#Remove all variables marked as "discard". Add columns for label and IRI. Remove
#columns max and min. Go through the updated file manually and fill out the 
#“label”, “IRI”, and “category” where known. Also fill out termType for all 
#variables using the following options: multifilter = multifilter category, 
#value=multifilter value, variable=maps to a variable, category=no data values. 

#REMOVE VARIABLES MARKED AS "DISCARD" #########################################
allVar2 <- allVar[!(allVar$keepDiscard == "discard"),]
###############################################################################

#INITIALIZE NEW COLUMNS #######################################################
newCol <- c("IRI", "label")
allVar2[,newCol] <- ""
###############################################################################

#ORDER COLUMNS ################################################################
allVar2 <- allVar2[c("colOrder","variable","dataFile","IRI","label",
                     "definition","category","codebookDescription",
                     "codebookValues","termType","notesForOnt",
                     "notesForProvider","notesForDL","variable_dataFile","uniqueVar",
                     "type","example","dateTime","formatCode","values",
                     "uniqueValueCount","percentMissing","flag")]
###############################################################################

#SAVE UPDATED ALLVARIABLES FILE ###############################################
write.csv(allVar2, OUTPUT, row.names = F)
###############################################################################

}
