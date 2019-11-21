updateVariableMap <- function(varMap, OUTPUT) {
    
    # After manual edits to varMap file, this cleans up and removes NAs from  
    # final file. It removes any quotations from around labels and parent labels 
    # that were copy/pasted from protege. Finally, it removes extra columns from 
    # the final variableMap file. 
    
    # You still need to decide whether to use variable or uniqueVar going forward
    # and update the file manually accordingly.
    
    #LOAD LIBRARIES ###############################################################
    library(purrr)
    library(plyr)
    library(dplyr)
    ###############################################################################

    #REMOVE QUOTATIONS ############################################################
    varMap$label <- gsub("^'", "", varMap$label)
    varMap$label <- gsub("'$", "", varMap$label)
    varMap$parentLabel <- gsub("^'", "", varMap$parentLabel)
    varMap$parentLabel <- gsub("'$", "", varMap$parentLabel)
    ###############################################################################
    
    #ORDER COLUMNS ################################################################
    varMap2 <- varMap[c("colOrder","variable","uniqueVar","dataFile","IRI","label",
                         "definition","category","parentLabel", "parentIRI",
                         "codebookDescription","codebookValues","termType",
                         "notesForOnt")]
    ###############################################################################
    
    #REMOVE NA ####################################################################
    for(i in 1:ncol(varMap2)){
        varMap2[,i] <- as.character(varMap2[,i])
        varMap2[,i][is.na(varMap2[,i])] <- ""
    } #Changes NAs to empty cells
    ###############################################################################
    
    #SAVE UPDATED ALLVARIABLES FILE ###############################################
    write.csv(varMap2, OUTPUT, row.names = F)
    ###############################################################################
    
    varMap2
}


