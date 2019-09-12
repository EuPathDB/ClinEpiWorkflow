updateVariableMap <- function(varMap, OUTPUT) {
    
# Add in parentLabel and parentIRI where known. Create a new row for each known
# category. Then go through manually to suggest parentLabel/parentIRI for new 
# variables.  

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

knownOntTermsLtd <- unique(knownOntTerms[, c("IRI", "parentIRI", "parentLabel")])
uniqueOntTerms <- unique(knownOntTerms[,c("IRI", "label", "parentIRI", 
                                          "parentLabel", "category")])
###############################################################################

#MERGE IN PARENTLABEL AND PARENTIRI FOR KNOWN IRIS ############################
varMap2 <- merge(varMap, knownOntTermsLtd, by = "IRI", all.x = T)
print(paste("Difference in rows:", nrow(varMap2) - nrow(varMap)))
#if not 0, check what got duplicated and why
varMap2$uniqueVar[duplicated(varMap2[c("uniqueVar","IRI")])]
###############################################################################

#ADD ROW FOR EVERY CATEGORY ###################################################
for(i in 1:ncol(varMap2)){
    varMap2[,i] <- as.character(varMap2[,i])
    varMap2[,i][is.na(varMap2[,i])] <- ""
} #Changes NAs to empty cells

#first check to see if there are any categories that should be added manually
#and will throw an error message
temp <- setdiff(varMap2$parentLabel, varMap2$label)
temp2 <- setdiff(temp,uniqueOntTerms$label)
print("Add labels below to file manually")
print(temp2)

varMap3 <- varMap2
for(row in 1:nrow(varMap2)) {
    temp.parentIRI <- varMap2[row, "parentIRI"]
    if(!(temp.parentIRI %in% varMap3$IRI) &  (temp.parentIRI != "")){
        temp.row <- data.frame(IRI = temp.parentIRI,
                               colOrder = "",
                               variable="",
                               dataFile="",
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
                               uniqueVar="",
                               type="",
                               example="",
                               dateTime="",
                               formatCode="",
                               values="",
                               uniqueValueCount="",
                               percentMissing="",
                               flag="")
        print(temp.row[,c("label","parentLabel")])
        varMap3 <- rbind(varMap3, temp.row)
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
varMap3 <- varMap3[c("colOrder","variable","uniqueVar","dataFile","IRI","label",
                     "definition","category","parentIRI","parentLabel", 
                     "codebookDescription","codebookValues","termType",
                     "notesForOnt","notesForProvider","notesForDL",
                     "variable_dataFile","type","example","dateTime",
                     "formatCode","values","uniqueValueCount",
                     "percentMissing","flag")]
###############################################################################

#SAVE UPDATED ALLVARIABLES FILE ###############################################
write.csv(varMap3, OUTPUT, row.names = F)
###############################################################################
}


