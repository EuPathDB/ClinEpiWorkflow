#!/usr/bin/env Rscript
library(data.table)
library(plyr)

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

#and create shiny ontology downloadable file
metadata.temp <- fread(paste0(dataDir, "/ontologyMetadata.txt"))
dataDict.temp <- fread(paste0(dataDir, "/ontologyMapping.txt"))

if (!any(grepl("Error", metadata.temp[1]))) {
  if (!any(grepl("Error", dataDict.temp[1]))) {
    #ontology file first
    metadata.file <- metadata.temp
    dataDict.file <- dataDict.temp
    names(metadata.file) <- tolower(names(metadata.file))
    metadata.file <- merge(metadata.file, dataDict.file, by = "iri", all = TRUE)
    fwrite(metadata.file, file.path(dataDir, "shiny_downloadDir_ontologyMetadata.txt"), sep = '\t', na = "NA")
  } else {
    stop("Data Dictionary mapping file missing or unreadable. Cannot create download files!")
  }
} else {
  stop("Ontology file missing or unreadable. Cannot create download files!")
}

#merges all files matching dataDir/shiny_*.txt and removes any identical cols as needed

shinyFiles <- list.files(dataDir, pattern=baseFileName, full.names=TRUE)

#check for prtcpnts file and start there since we merge files by 'Participant_Id'
if (any(grepl("participants", shinyFiles))) {
  prtcpnt_temp <- try(fread(shinyFiles[grepl("participants", shinyFiles)], na.strings = c("N/A", "na", "")))

 if (any(grepl("Error", prtcpnt_temp[1]))) {
   stop("Participant file unreadable!")
 } else {
   prtcpnt.file <- prtcpnt_temp
   names(prtcpnt.file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
   names(prtcpnt.file)[names(prtcpnt.file) == 'SOURCE_ID'] <- 'Participant_Id'
   drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
   prtcpnt.file <- prtcpnt.file[, !drop, with=FALSE]
   masterDataTable <- prtcpnt.file
   names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id')] <- paste0(metadata.file$label[match(names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id')], metadata.file$iri)], " [", names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id')], "]")
    fwrite(prtcpnt.file, file.path(dataDir,"shiny_downloadDir_participant.txt"), sep='\t', na="NA")
 } 
} else {
  stop("Participant file missing!")
}

shinyFiles <- shinyFiles[!grepl("participants", shinyFiles)]
#if masterDataTable already exists, ignore it
shinyFiles <- shinyFiles[!grepl("masterDataTable", shinyFiles)]
shinyFiles <- shinyFiles[!grepl("downloadDir", shinyFiles)]

#consider using reduce rather than for loop here. may save time

for (i in 1:length(shinyFiles)) {
  file_temp <- try(fread(shinyFiles[i], quote="", na.strings = c("N/A", "na", "")))

  if (any(grepl("Error", file_temp[1]))) {
    message("Warning: ", shinyFiles[i], " unreadable... skipping.")
  } else {
    file <- file_temp
    names(file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(file)))
    names(file)[names(file) == 'SOURCE_ID'] <- 'Participant_Id'
    names(file)[names(file) == 'OBSERVATION_ID'] <- 'Observation_Id'
    if (grepl("observation", shinyFiles[i])) {
      names(file)[names(file) == 'NAME'] <- 'Observation_Id'
    }
    if (grepl("household", shinyFiles[i])) {
      names(file)[names(file) == 'NAME'] <- 'Household_Id'
    }

    if (!is.null(file)) {
      if (nrow(file) > 1) {
        #remove columns that would cause duplicate entries after merge
        if (grepl("samples", shinyFiles[i])) {
          keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Observation_Id')
          file <- file[, keep, with=FALSE] 
          #some obs may not have samples, so set all=T
          masterDataTable <- merge(masterDataTable, file, by = "Observation_Id", all = TRUE)
        } else {
          keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Participant_Id')
          file <- file[, keep, with=FALSE]
          if (grepl("observation", shinyFiles[i]) & (uniqueN(masterDataTable$Participant_Id) != nrow(masterDataTable))) {
            masterDataTable <- rbind.fill(masterDataTable, file)
          } else {
            masterDataTable <- merge(masterDataTable, file, by = "Participant_Id")
          } 
        }

        #also make a copy with updated col names for downloadSite dir
        drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
        file <- file[, !drop, with=FALSE]
        names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id')] <- paste0(metadata.file$label[match(names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id')], metadata.file$iri)], " [", names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id')], "]")
        fileName <- gsub("shiny_", "shiny_downloadDir_", shinyFiles[i])
        fwrite(file, fileName, sep='\t', na="NA") 

      }
    }
  }
}

fwrite(masterDataTable, file.path(dataDir,"shiny_masterDataTable.txt"), sep='\t', na="NA")



