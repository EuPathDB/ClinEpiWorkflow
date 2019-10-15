#Add data dictionary information to allVariables file ##########################################

#Uses the allVariables file to generate an updated allVariables file with information
#from the data dictionary merged in (especially codebookDescription and 
#codebookValues. The new file will follow the format of the SOP and can be 
#manually edited to indicate keep/discard 


# Load your allVariable file made with Danica's studyImport script
load("R_output/001.PROVIDE_allVars.RData") 
# The data dictionary(ies) you want to load must be in a folder on their own. 
# They must be in csv, txt or xls format (1 tab only). Make sure they have the same
# headers in each file
FOLDER <- "R_output/dataDictionary" 
# TYPE must be ".csv",".txt", or ".xls"
TYPE <- ".csv"       
# Set the next 4 variables to match the relevant column headers from the data 
# dictionary. Names need to match how the column headers will look when loaded
# in R (e.g. spaces change to ".", etc.)
VARIABLE <- "Variable_name"
CODEBOOKDESCRIPTION <- "Variable.Description"
CODEBOOKVALUES <- "Format_Values"
NOTESFORDL <- "Missing.Values"
# Set where you want to save your updated allVariables file.
OUTPUT <- "./R_output/PROVIDE_allVariables_v1.csv"
# Load the function
source("/Users/sshahsimpson/Documents/Git/ClinEpiWorkflow/Main/lib/R/0002_SST_add_data_dictionary.R")

allVars2 <- addDD(FOLDER, TYPE, VARIABLE, CODEBOOKDESCRIPTION, CODEBOOKVALUES, NOTESFORDL)

# If multiple files contain the same variable, you will get every possible 
# permutation of variable, codebookDescription, codebookValues, etc.
# The following line of code is a crude way to clean that up.
allVars3 <- allVars2[!(duplicated(allVars2$uniqueVar)),]
# The following line will remove any rows where there was a line in the data dictionary
# that did not map to any variables in the allVariables file
allVars4 <- allVars3[is.na(allVars3$dataFile) == F,]

# Save your updated allVariables file. Then go through manually to decide where to 
# "keep" or "discard"
write.csv(allVars4, OUTPUT, row.names = F)
