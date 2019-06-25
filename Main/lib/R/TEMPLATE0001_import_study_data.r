rm(list=ls())

###############################################
###############################################
###############################################
# change information in this section only:

setwd("~/Box/Danica EuPathDB/Studies/MAL-ED/0 to 36 months/DH_workspace/Data")                  # SET WORKING DIRECTORY
OUTPUT <- "./Processed data/001.MALED_0to36m_allVars.RData"                                     # SET FOLDER & FILE NAME WHERE OUTPUT WILL BE SAVED                                    
source("~/Documents/SVN/ClinEpiWorkflow/trunk/Main/lib/R/000a_studyImport_function.R")         # SET LOCATION OF STUDY IMPORT FUNCTION

FOLDER <- "./Raw data/DCC Processed Data"           # SET FOLDER CONTAINING RAW DATA FILES FROM PROVIDER
STUDY <- "MAL-ED, 0 to 36 months"                   # SET DATASET (OR STUDY)
MISSING <- c(NA, "na", "NA", "n/a", "N/A", "", ".") # SET MISSING VALUES (NA, "na", "NA", "n/a", "N/A", "", ".")
DATE_TIME <- c("date", "dob")                       # SET REGEX FOR POSSIBLE DATE/TIME VARIABLES. NOTE: KEEP LOWERCASE
PARTICIPANT_ID <- c("pid")                          # SET HOW THE PARTICIPANT ID VARIABLE IS LABELED IN THE DATAFILES. NOTE: KEEP LOWERCASE
TYPE <- ".csv"                                      # SET FILE TYPE EXTENSION: 
                                                        # (".csv", ".txt", ".RData", ".sas7bdat", ".dta", ".sav", ".zsav", ".por")
                                                        # NOTE: EXTENSION MUST EXACTLY MATCH ONE OF THE SUGGESTIONS. 
                                                        # FOR EXCEL FILES --> SAVE AS .csv FILES BEFORE RUNNING SCRIPTS
                                                        # IF EXTENSION IS NOT REPRESENTED IN THE LIST, TALK TO DANICA TO UPDATE CODE

###############################################
###############################################
###############################################
# use studyImport function to get allVars, valueMap, and dataFiles

temp <- studyImport(FOLDER, TYPE, STUDY, MISSING, DATE_TIME, PARTICIPANT_ID)

allVars <- temp[[1]] 
valueMap <- temp[[2]]
dataFiles <- temp[[3]]
originalFiles <- temp[[4]]

#############################################################
# save workspace
save(allVars, valueMap, dataFiles, originalFiles, file=OUTPUT)