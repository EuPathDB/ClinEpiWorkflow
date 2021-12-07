makeVariableMap <- function(ALLVARS, OUTPUT) {

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
  
  #load libraries
  library(tidyverse)

varMap <- ALLVARS %>%
  filter(keepDiscard != "discard") %>% #remove variables marked as "discard"
  replace(is.na(.), "") %>% #remove all NAs
  mutate(IRI = "",
           label = "",
           parentIRI = "",
           parentLabel = "",
           repeated = "", 
           is_temporal = "",
           mergeKey = "",
           unitLabel = "",
           unitIRI= "",
           is_featured = "",
           hidden = "",
           scale = "",
           defaultDisplayRangeMin = "",
           defaultDisplayRangeMax = "",
           defaultBinWidth = ""
           ) %>% #initialize new columns
  select(colOrder:dataFile, IRI:label, definition:category, 
         parentIRI:parentLabel, codebookDescription:notesForDL, 
         repeated:mergeKey, dataSet, unitLabel:defaultBinWidth, type:flag,
         variable_dataFile, uniqueVar) #reorder and drop keepDiscard
  
write.csv(varMap, OUTPUT, row.names = F) #save the new variableMap file
}
