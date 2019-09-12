# Make variableMap file ##############################################################################

# This function removes all variables marked as "discard" (leaves those marked
# as "keep" or left blank. It adds columns for label and IRI and removes the 
# columns max and min to make a starter variableMap file


source("/Users/sshahsimpson/Documents/General_R_files/functions/003_makeVariableMap.R") #load the function
allVar <- read.csv("./R_output/PROVIDE_allVariables_v1.csv", as.is = T) #load the allVariables file where you've marked variables to keep/discard
OUTPUT <- "./R_output/PROVIDE_variableMap_v1.csv" #indicate where to save the new variableMap file
makeVariableMap(allVar, OUTPUT) #run the function, which writes a csv file for you


# Go through the updated file manually and fill out the “label”, “IRI”, and 
# “category” where known. Also fill out termType for all variables using the 
# following options: multifilter = multifilter category, value = multifilter value,
# variable = maps to a variable, category = no data values.