#!/usr/bin/env Rscript
library(data.table)

args <- commandArgs(TRUE)

dataDir <- args[1]
if (is.null(dataDir)) {
  stop("Error: No directory provided for shiny files to merge!")
} else {
  if (!file.exists(dataDir)) {
    stop("Error: Directory provided for shiny files does not exist!")
  }
}
baseFileName <- args[2]
#can change if we dont want to assume anything
if (is.null(baseFileName)) {
  baseFileName <- "shiny"
}

#merges all files matching dataDir/shiny_*.txt and removes any identical cols as needed

shinyFiles <- list.files(dataDir, pattern=paste0("[", baseFileName, "_*.txt]"), full.names=TRUE)

#check for prtcpnts file and start there since we merge files by 'Participant_Id'
#could also require observations file the same way if we'd like
if (any(grepl("participants", shinyFiles))) {
  prtcpnt_temp <- try(fread(shinyFiles[grepl("participants", shinyFiles)], na.strings = c("N/A", "na", "")))

 if (any(grepl("Error", prtcpnt_temp[1]))) {
   stop("Error: Participant file unreadable!")
 } else {
   prtcpnt.file <- prtcpnt_temp
   names(prtcpnt.file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
   names(prtcpnt.file)[names(prtcpnt.file) == 'SOURCE_ID'] <- 'Participant_Id'
 } 
} else {
  stop("Error: Participant file missing!")
}

masterDataTable <- prtcpnt.file
shinyFiles <- shinyFiles[!grepl("participants", shinyFiles)]

#consider using reduce rather than for loop here. may save time

for (i in 1:length(shinyFiles)) {
  file_temp <- try(fread(shinyFiles[i], na.strings = c("N/A", "na", "")))

  if (any(grepl("Error", file_temp[1]))) {
    message("Warning: ", shinyFiles[i], " unreadable... skipping.")
  } else {
    file <- file_temp
    names(file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(file)))
    names(file)[names(file) == 'SOURCE_ID'] <- 'Participant_Id'
    if (grepl("observation", file)) {
      names(file)[names(file) == 'NAME'] <- 'Observation_Id'
    }

    if (!is.null(file)) {
      if (nrow(file) > 1) {
        #remove columns that would cause duplicate entries after merge
        drop <- colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Participant_Id'
        file <- file[, !drop, with=FALSE]
        masterDataTable <- merge(masterDataTable, file, by = "Participant_Id")
      }
    }
  }
}

fwrite(masterDataTable, file.path(dataDir,"shiny_masterDataTable.txt"), sep='\t', na="NA")

