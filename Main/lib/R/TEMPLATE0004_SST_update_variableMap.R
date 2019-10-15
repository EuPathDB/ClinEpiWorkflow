# Update variableMap file ######################################################################

# This function cleans up and removes NAs from the file and removes quotations 
# from around labels and parentLabels that were copy/pasted from protege. It 
# will also remove all extra columns that don’t belong in the variableMap file 
# that goes to the ontology team. 


source("/Users/sshahsimpson/Documents/Git/ClinEpiWorkflow/Main/lib/R/0004_SST_update_variableMap.R") #load the function
varMap <- read.csv("./R_output/PROVIDE_variableMap_v2.csv", as.is = T) #load the variableMap file that you've been editing
OUTPUT <- "./R_output/PROVIDE_variableMap_v3.csv" #indicate where you want to save the updated file
updateVariableMap(varMap, OUTPUT) #run the function. Automatically writes the file 


# Decide whether you will be using "variable" or "uniqueVar" to represent all 
# variables going forward and update the file manually accordingly.
# If using uniqueVar, delete the variable column and rename uniqueVar to variable. 
# If using variable, delete the uniqueVar column. 
# Once finished, send to the ontology team via Git.

