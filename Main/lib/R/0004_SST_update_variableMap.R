updateVariableMap <- function(VARMAP) {
    
    # After manual edits to varMap file, this cleans up and removes NAs from  
    # final file. It removes any quotations from around labels and parent labels 
    # that were copy/pasted from protege. Finally, it removes extra columns from 
    # the final variableMap file. 
    
    # You still need to decide whether to use variable or uniqueVar going forward
    # and update the file manually accordingly.
    
    #load libraries
    library(tidyverse)
    
    varMap <- VARMAP %>% 
        mutate(label = str_remove(label,"'$")) %>%  #remove ' at end of labels
        mutate(label = str_remove(label,"^'")) %>%  #remove ' at start of labels
        mutate(parentLabel = str_remove(parentLabel,"'$")) %>%  #remove ' at end of parentLabels
        mutate(parentLabel = str_remove(parentLabel,"^'")) %>%  #remove ' at start of parentLabels
        mutate(colOrder = as.character(colOrder)) %>% #change colOrder to character
        select(-c(notesForProvider, notesForDL, type:variable_dataFile)) %>% #remove cols the ontology team doesn't need
        relocate(uniqueVar, .before = dataFile) %>% 
        replace(is.na(.), "") #remove all NAs 
}


