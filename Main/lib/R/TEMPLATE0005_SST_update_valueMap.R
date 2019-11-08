# Update valueMap file ############################################################################

# This function removes discarded variables. 

source("/Users/sshahsimpson/Documents/Git/ClinEpiWorkflow/Main/lib/R/0005_SST_update_valueMap.R") #load function
load(file = "001.PROVIDE_allVars.RData") #load the valueMap Danica's script generated
allVars <- read.csv("./R_output/PROVIDE_allVariables_v1.csv", as.is = T) #load the allVariables file that indicated which variables to keep/discard
OUTPUT <- "./R_output/PROVIDE_valueMap_v1.txt" #indicate where to save the valueMap file
valueMap2 <- updateValueMap(valueMap, allVars, OUTPUT) #run function

# Then in R may want to subset the file to look at variables marked as 
# continuous to see if you can also drop those from the file.
# Do the same check for variables marked as >20 values since these might be free 
# text fields. 
# Then go through manually to remove/add valueMapping as needed. 
# Note: variable” will have the variable name and “variable2” will have uniqueVar.
# Update the valueMap file so it has 2 columns of the same matching what you
# used in the final variableMap file.
