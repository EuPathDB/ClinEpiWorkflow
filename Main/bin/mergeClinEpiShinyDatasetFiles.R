#!/usr/bin/env Rscript
library(data.table)

args <- commandArgs(TRUE)

dataDir <- args[1]
if (is.null(dataDir)) {
  stop("No directory provided for shiny files to merge!")
} else {
  if (!file.exists(dataDir)) {
    stop("Directory provided for shiny files does not exist!")
  }
}
baseFileName <- args[2]
#can change if we dont want to assume anything
if (is.null(baseFileName)) {
  baseFileName <- "shiny"
}

#merges all files matching dataDir/shiny_*.txt and removes any identical cols as needed

shinyFiles <- list.files(dataDir, pattern=baseFileName, full.names=TRUE)

#check for prtcpnts file and start there since we merge files by 'Participant_Id'
#could also require observations file the same way if we'd like
if (any(grepl("participants", shinyFiles))) {
  prtcpnt_temp <- try(fread(shinyFiles[grepl("participants", shinyFiles)], na.strings = c("N/A", "na", "")))

 if (any(grepl("Error", prtcpnt_temp[1]))) {
   stop("Participant file unreadable!")
 } else {
   prtcpnt.file <- prtcpnt_temp
   names(prtcpnt.file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
   names(prtcpnt.file)[names(prtcpnt.file) == 'SOURCE_ID'] <- 'Participant_Id'
 } 
} else {
  stop("Participant file missing!")
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
    if (grepl("observation", shinyFiles[i]) | grepl("obsevation", shinyFiles[i])) {
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

#ask JohnB if he wants the ontology file in downloadDir also
metadata.temp <- try(fread(paste(dataDir, "/ontologyMetadata.txt")))

if (any(grepl("Error", metadata.temp[1]))) {
  metadata.file <- metadata.temp
  names(metadata.file) <- tolower(names(metadata.file))
  downloadDataTable <- masterDataTable
  drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
  downloadDataTable <- downloadDataTable[, !drop, with=FALSE]
  names(downloadDataTable)[!names(downloadDataTable) %in% c('Participant_Id', 'Observation_Id')] <- paste0(metadata.file$property[match(names(downloadDataTable)[!names(downloadDataTable) %in% c('Participant_Id', 'Observation_Id')], metadata.file$source_id)], " [", names(downloadDataTable)[!names(downloadDataTable) %in% c('Participant_Id', 'Observation_Id')], "]")
  fwrite(metadata.file, file.path(dataDir, "ontologyMetadata.txt"), sep = '\t', na = "NA")
  fwrite(downloadDataTable, file.path(dataDir,"shiny_downloadDir.txt"), sep='\t', na="NA")
} else {
  stop("Ontology file missing or unreadable. Cannot create download files!")
}

