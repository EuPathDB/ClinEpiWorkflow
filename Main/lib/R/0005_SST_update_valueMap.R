updateValueMap <- function(valueMap, ALLVARS) {
# After manual edits to allVar file, update valueMap file to only include
# variables that will be displayed. Then go through by hand to remove/add 
# valueMapping as needed

#load libraries
library(purrr)
library(plyr)
library(dplyr)

toKeep <- ALLVARS %>% 
    filter(keepDiscard != "discard") #remove variables marked as "discard" from allVars

valueMap2 <- valueMap %>% 
    filter(uniqueVar %in% toKeep$uniqueVar) %>% #remove discarded variables from valueMap
    select(-c(dataSet, dataFile, variable_dataFile, file)) %>% #remove unneeded columns
    replace(is.na(.), "NA") %>% #replace NAs with character NAs
    filter(values != "") %>% #remove rows where values = "", since that doesn't need to be mapped
    mutate(mappedTerm = "",
           valueOrder = "") %>% #add columns for mappedTerm and valueOrder
    mutate(mappedTerm = case_when(
        boolean == T & values == "0" ~ "No",
        boolean == T & values == "1" ~ "Yes",
        boolean == "" ~ "")) %>% #add values of No and Yes if Danica's 
#code flagged it as boolean
    mutate(values2 = str_to_lower(values)) %>% #create lowercase values column
    distinct(uniqueVar, values2, .keep_all = T) %>% 
    select(-c(boolean, values2))  #drop the boolean and values2 column
}