# Add parentLabel and parentIRI ######################################################################

# Adds in parentLabel and parentIRI where known. Creates a new row for each known
# category.  

# Note, the function pulls in the conversion file for the ClinEpi ontology. You 
# will have to update the path based on your own system and where you save Git 
# repo clones on your computer. 

source("/Users/sshahsimpson/Documents/General_R_files/functions/004_update_variableMap.R") #load the function
varMap <- read.csv("./R_output/PROVIDE_variableMap_v2.csv", as.is = T) #load the variableMap file that you've been editing
OUTPUT <- "./R_output/PROVIDE_variableMap_v3.csv" #indicate where you want to save the updated file
updateVariableMap(varMap, OUTPUT) #run the function. Automatically writes the file 

# Go through manually to suggest parentLabel/parentIRI for new variables. 

