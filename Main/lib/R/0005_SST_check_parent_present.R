checkParentPresent <- function(varMap, OUTPUT) {
# After manual edits to varMap file, make sure each parentLabel and parentIRI
# has its own row. Also clean up and remove NAs from final file. Remove any 
# quotations from around labels and parent labels that were copy/pasted from 
# protege

#LOAD LIBRARIES ###############################################################
library(purrr)
library(plyr)
library(dplyr)
###############################################################################

#LOAD ONTOLOGY MASTER CSV  ####################################################
knownOntTerms <- read.csv("/Users/sshahsimpson/Documents/SVN_checkout/ClinEpiDB/ontology/harmonization/clinEpi_conversion.csv", as.is = T)
knownOntTerms$IRI <- gsub("http://purl.obolibrary.org/obo/", "", knownOntTerms$IRI)
knownOntTerms$IRI <- gsub("http://www.ebi.ac.uk/efo/", "", knownOntTerms$IRI)
knownOntTerms$parentIRI <- gsub("http://purl.obolibrary.org/obo/", "", knownOntTerms$parentIRI)
knownOntTerms$parentIRI <- gsub("http://www.ebi.ac.uk/efo/", "", knownOntTerms$parentIRI)

knownOntTerms$category <- gsub("sample", "Sample", knownOntTerms$category)
knownOntTerms$category <- gsub("observation", "Observation", knownOntTerms$category)
knownOntTerms$category <- gsub("participant", "Participant", knownOntTerms$category)
knownOntTerms$category <- gsub("household", "Household", knownOntTerms$category)

uniqueOntTerms <- unique(knownOntTerms[,c("IRI", "label", "parentIRI", 
                                          "parentLabel", "category")])
###############################################################################

#REMOVE QUOTATIONS ############################################################
varMap$label <- gsub("^'", "", varMap$label)
varMap$label <- gsub("'$", "", varMap$label)
varMap$parentLabel <- gsub("^'", "", varMap$parentLabel)
varMap$parentLabel <- gsub("'$", "", varMap$parentLabel)
###############################################################################

#ADD ROW FOR EVERY CATEGORY ###################################################
for(i in 1:ncol(varMap)){
    varMap[,i] <- as.character(varMap[,i])
    varMap[,i][is.na(varMap[,i])] <- ""
} #Changes NAs to empty cells

#first check to see if there are any categories that should be added manually
#and will throw an error message
temp <- setdiff(varMap$parentLabel, varMap$label)
temp2 <- setdiff(temp,uniqueOntTerms$label)
print("Add labels below to file manually")
print(temp2)

varMap2 <- varMap
for(row in 1:nrow(varMap)) {
    temp.parentIRI <- varMap[row, "parentIRI"]
    if(!(temp.parentIRI %in% varMap2$IRI) & (temp.parentIRI != "")){
        temp.row <- data.frame(colOrder = "",
                               variable="",
                               uniqueVar="",
                               dataFile="",
                               IRI = temp.parentIRI,
                               label=uniqueOntTerms$label[uniqueOntTerms$IRI == temp.parentIRI],
                               definition="",
                               category=uniqueOntTerms$category[uniqueOntTerms$IRI == temp.parentIRI],
                               parentIRI=uniqueOntTerms$parentIRI[uniqueOntTerms$IRI == temp.parentIRI],
                               parentLabel=uniqueOntTerms$parentLabel[uniqueOntTerms$IRI == temp.parentIRI],
                               codebookDescription="",
                               codebookValues="",
                               termType="",
                               notesForOnt="",
                               notesForProvider="",
                               notesForDL="",
                               variable_dataFile="",
                               type="",
                               example="",
                               dateTime="",
                               formatCode="",
                               values="",
                               uniqueValueCount="",
                               percentMissing="",
                               flag="")
        print(temp.row[,c("label","parentLabel")])
        varMap2 <- rbind(varMap2, temp.row)
    }
}

#1) For every row, need to check if the parentIRI has it's own row where it 
#   is the IRI
#2) Then if it doesn't have it's own row, need to create a row for it and 
#   Then need to merge in the label, parentIRI, and parentLabel from the
#   OntTerms file
#3) Need to run iteratively until every parent category is represented
###############################################################################

#ORDER COLUMNS ################################################################
varMap3 <- varMap2[c("colOrder","variable","uniqueVar","dataFile","IRI","label",
                     "definition","category","parentLabel", "parentIRI",
                     "codebookDescription","codebookValues","termType",
                     "notesForOnt","notesForProvider","notesForDL","variable_dataFile",
                     "type","example","dateTime","formatCode","values",
                     "uniqueValueCount","percentMissing","flag")]
###############################################################################

#REMOVE NA ####################################################################
for(i in 1:ncol(varMap3)){
    varMap3[,i] <- as.character(varMap3[,i])
    varMap3[,i][is.na(varMap3[,i])] <- ""
} #Changes NAs to empty cells
###############################################################################

#SAVE UPDATED ALLVARIABLES FILE ###############################################
write.csv(varMap3, OUTPUT, row.names = F)
###############################################################################

varMap3
}