makeVariableMap <- function(allVar, OUTPUT) {

#Removes all variables marked as "discard". Adds columns for label, IRI, 
#parentLabel, and parentIRI. Removes columns max and min. 
    
#Go through the updated file manually and fill out the “label”, “IRI”, and 
#“category” where known. Also fill out termType for all variables using the 
#following options: variable = maps to a variable, value = multifilter value,
#multifilter = multifilter category, category = no data values. 
    
#Fill out parentLabel and parentIRI for new variables that don't have an 
#existing IRI. You don't need to fill out parentLabel or parentIRI for variables
#that map to known ontology terms. Create a new row for any new parentLabels 
#where the new parentLabel is placed under the "label" column and fill out 
#parentLabel and parentIRI to help place the new category in the hierarchy.

#REMOVE VARIABLES MARKED AS "DISCARD" #########################################
varMap <- allVar[!(allVar$keepDiscard == "discard"),]
###############################################################################

#INITIALIZE NEW COLUMNS #######################################################
newCol <- c("IRI", "label", "parentIRI", "parentLabel")
varMap[,newCol] <- ""
###############################################################################

#ORDER COLUMNS ################################################################
varMap <- varMap[c("colOrder","variable","dataFile","IRI","label",
                     "definition","category","codebookDescription",
                     "codebookValues","termType","parentIRI","parentLabel","notesForOnt",
                     "notesForProvider","notesForDL","variable_dataFile","uniqueVar",
                     "type","example","dateTime","formatCode","values",
                     "uniqueValueCount","percentMissing","flag")]
###############################################################################

#SAVE UPDATED ALLVARIABLES FILE ###############################################
write.csv(varMap, OUTPUT, row.names = F)
###############################################################################

}
