#Check parentIRI/label present and finalize variableMap file ############################################################################

# After manual edits to varMap file, this function makes sure each parentLabel 
# and parentIRI has its own row. Also cleans up and remove NAs
# NOTE: Like function 4, this function references the ClinEpi conversion file
# so you will need to update the path in your copy of the function
source("/Users/sshahsimpson/Documents/General_R_files/functions/005_check_parent_present.R") #load function
varMap <- read.csv("./R_output/PROVIDE_variableMap_v4.csv", as.is = T) #load completed varMap file with 
OUTPUT <- "./R_output/PROVIDE_variableMap_v5.csv" #indicate where to save file
varMap <- checkParentPresent(varMap, OUTPUT) #run function repeatedly until the dim on varMap stop increasing


# This function will do a final clean-up and reduce the variableMap file to just
# the columns the ontology team needs
# NOTE: Like function 4, this function references the ClinEpi conversion file
# so you will need to update the path in your copy of the function
source("/Users/sshahsimpson/Documents/General_R_files/functions/006_finalize_parent_present.R")
OUTPUT <- "./R_output/PROVIDE_variableMap_v6.csv"
varMap <- finalizeParentPresent(varMap, OUTPUT) 

# After running the function, make sure termType is entered for all newly added 
# categories. Decide whether you will be using just the variable name or the 
# uniqueVar to represent all variables. 
# If using uniqueVar, delete the variable column and rename uniqueVar to variable.
# If using variable, just delete uniqueVar. 
# Once finished, send to the ontology team via Git.
