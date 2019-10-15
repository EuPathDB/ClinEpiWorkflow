# Make variableMap file ##############################################################################

# This function removes all variables marked as "discard" (leaves those marked
# as "keep" or left blank. It adds columns for label, IRI, parentLabel, and 
# parentIRI and removes the columns max and min to make a starter variableMap file


source("/Users/sshahsimpson/Documents/General_R_files/functions/003_makeVariableMap.R") #load the function
allVar <- read.csv("./R_output/PROVIDE_allVariables_v1.csv", as.is = T) #load the allVariables file where you've marked variables to keep/discard
OUTPUT <- "./R_output/PROVIDE_variableMap_v1.csv" #indicate where to save the new variableMap file
makeVariableMap(allVar, OUTPUT) #run the function, which writes a csv file for you


# Go through the updated file manually and:
    # (i) For known variable - fill out the “label”, “IRI”, and “termType”
    # (ii) For new variables - fill out the “label”, “termType”, “category”, 
        # “parentLabel”, and “parentIRI”
    # (iii) Create a new row for any new multifilters/categories (new 
        # parentLabel with no matching parentIRI yet). Place the parentLabel 
        # under the “label” column and fill out “termType”, “category”, 
        # “parentLabel”, and “parentIRI” to help place the new category in the 
        # hierarchy.

# for termType: multifilter = multifilter category, value = multifilter value,
# variable = maps to a variable, category = no data values.