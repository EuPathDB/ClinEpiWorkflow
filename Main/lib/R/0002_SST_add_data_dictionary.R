addDD <- function(ALLVARS, FOLDER, TYPE, VARIABLE, CODEBOOKDESCRIPTION, CODEBOOKVALUES, NOTESFORDL) {

#use allVariables file to generate an updated allVariables file with information
#from the data dictionary merged in, especially codebookDescription and 
#codebookValues. The new file will follow the format of the SOP and can be 
#manually edited to indicate keep/discard 
    
#load libraries
library(tidyverse)
library(readxl)

#read in and create single data dictionary file from multiple files
if(TYPE==".txt"){
    dataDictionary <- list.files(path=FOLDER, pattern=TYPE, full.names=T) %>% 
        map_dfr(read_delim)}
if(TYPE==".csv"){
    dataDictionary <- list.files(path=FOLDER, pattern=TYPE, full.names=T) %>% 
        map_dfr(read_csv)}
if(TYPE==".xls"){
    dataDictionary <- list.files(path=FOLDER, pattern=TYPE, full.names=T) %>% 
        map_dfr(read_xls)}

#rename key variables in dataDictionary, join with allVars file, and reorder
label_updates <- c(variable = VARIABLE, 
                   codebookDescription = CODEBOOKDESCRIPTION,
                   codebookValues = CODEBOOKVALUES, notesForDL = NOTESFORDL)
allvars2 <- dataDictionary %>% 
    select(all_of(label_updates)) %>% #select and rename the columns
    mutate(across(everything(), as.character)) %>%                #convert all columns to character 
    mutate(variable = tolower(variable)) %>%    #make variable all lower case 
    full_join(ALLVARS, by = "variable") %>%     #merge dataDictionary with allVars
    mutate(keepDiscard = "",
           definition = "",
           category = "",
           termType = "",
           notesForOnt = "",
           notesForProvider = "") %>%       #initialize additional columns
    select(colOrder, variable, dataFile, keepDiscard:category, 
           codebookDescription:codebookValues, termType:notesForProvider, 
           notesForDL:dataSet, variable_dataFile, type:formatCode,values:flag) #reorder variables and drop unneeded ones
}
