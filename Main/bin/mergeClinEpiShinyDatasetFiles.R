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
    metadata.ws <-metadata.file
    names(metadata.ws)[names(metadata.ws) == "iri"] <- "source_id"
    names(metadata.ws)[names(metadata.ws) == "label"] <- "property"
    names(metadata.ws)[names(metadata.ws) == "parentlabel"] <- "parent"
    metadata.ws <- metadata.ws[, !'definition', with = FALSE]
    fwrite(metadata.ws, file.path(dataDir, "shiny_masterDataTable_ontologyMetadata.tab"), sep = '\t', na = "NA")
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
   #remove other participant id col
   if ("EUPATH_0000095" %in% names(prtcpnt.file)) { prtcpnt.file$EUPATH_0000095 <- NULL }
   names(prtcpnt.file)[names(prtcpnt.file) == 'HOUSEHOLD'] <- 'Household_Id'
   drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
   prtcpnt.file <- prtcpnt.file[, !drop, with=FALSE]
   prtcpnt.file <- prtcpnt.file[,which(unlist(lapply(prtcpnt.file, function(x)!all(is.na(x))))),with=F]
   masterDataTable <- prtcpnt.file
   prtcpnt.back <- prtcpnt.file
   names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')] <- paste0(metadata.file$label[match(names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], metadata.file$iri)], " [", names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], "]")
    idCols <- c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')[c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id') %in% names(prtcpnt.file)]
    otherCols <- names(prtcpnt.file)[!names(prtcpnt.file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')]
    setcolorder(prtcpnt.file, c(idCols, otherCols[order(otherCols)]))
    fwrite(prtcpnt.file, file.path(dataDir,"shiny_downloadDir_participant.txt"), sep='\t', na="NA")
 } 
} else {
  stop("Participant file missing!")
}

shinyFiles <- shinyFiles[!grepl("participants", shinyFiles)]
#if masterDataTable already exists, ignore it
shinyFiles <- shinyFiles[!grepl("masterDataTable", shinyFiles)]
shinyFiles <- shinyFiles[!grepl("downloadDir", shinyFiles)]

for (i in 1:length(shinyFiles)) {
  file_temp <- try(fread(shinyFiles[i], quote="", na.strings = c("N/A", "na", "")))

  if (any(grepl("Error", file_temp[1]))) {
    message("Warning: ", shinyFiles[i], " unreadable... skipping.")
  } else {
    message("parsing ", shinyFiles[i])
    file <- file_temp
    names(file) <-  gsub(" ", "_", gsub("\\[|\\]", "", names(file)))
    names(file)[names(file) == 'SOURCE_ID'] <- 'Participant_Id'
    names(file)[names(file) == 'OBSERVATION_ID'] <- 'Observation_Id'
    names(file)[names(file) == 'HOUSEHOLD'] <- 'Household_Id'
    if (grepl("observation", shinyFiles[i])) {
      names(file)[names(file) == 'NAME'] <- 'Observation_Id'
    }
    if (grepl("household", shinyFiles[i])) {
      names(file)[names(file) == 'NAME'] <- 'Household_Observation_Id'
      if ("EUPATH_0015467" %in% names(file)) {
        if (uniqueN(file$EUPATH_0015467, na.rm=TRUE) == 1) {
          file$Household_Observation_Id <- NULL
        }
      } else {
        file$Household_Observation_Id <- NULL
      }
    }
    if (grepl("sample", shinyFiles[i])) {
      names(file)[names(file) == 'NAME'] <- 'Sample_Id'
    }

    if (!is.null(file)) {
      if (nrow(file) > 1) { 
        #remove columns that would cause duplicate entries after merge
        if (grepl("samples", shinyFiles[i])) {
          keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Observation_Id')
          file <- file[, keep, with=FALSE] 
	  file <- file[,which(unlist(lapply(file, function(x)!all(is.na(x))))),with=F]
          #some obs may not have samples, so set all=T
          if ('Household_Id' %in% names(masterDataTable) | 'Household_Id' %in% names(file)) {
            cols <- c(names(file), 'Participant_Id', 'Household_Id')
          } else {
            cols <- c(names(file), 'Participant_Id')
          }
          masterDataTable <- merge(masterDataTable, file, by = "Observation_Id", all = TRUE)
          file <- unique(masterDataTable[, cols, with=FALSE])
          file <- file[!is.na(file$Sample_Id),]
          idCols <- c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id')[c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id') %in% names(file)]
	} else if (grepl("household", shinyFiles[i])) {
	  keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Household_Id')
	  file <- file[, keep, with=FALSE]
	  file <- file[,which(unlist(lapply(file, function(x)!all(is.na(x))))),with=F]
	  masterDataTable <- merge(masterDataTable, file, by = "Household_Id", allow.cartesian = TRUE)
          idCols <- c('Household_Observation_Id', 'Household_Id', 'Participant_Id')[c('Household_Observation_Id', 'Household_Id', 'Participant_Id') %in% names(file)]
	} else if (grepl("light", shinyFiles[i])) {
	  file$Participant_Id <- NULL
	  file <- unique(file)
          idCols <- c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id')[c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id') %in% names(file)]
        } else {
          keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Participant_Id')
          file <- file[, keep, with=FALSE]
          file <- file[,which(unlist(lapply(file, function(x)!all(is.na(x))))),with=F]
          if ('Household_Id' %in% names(masterDataTable) | 'Household_Id' %in% names(file)) {
            cols <- c(names(file), 'Household_Id')
          } else {
            cols <- names(file)
          }     
          #this basically tests for house obs 
          if (grepl("observation", shinyFiles[i]) & (uniqueN(masterDataTable$Participant_Id) != nrow(masterDataTable))) {
            temp <- merge(prtcpnt.back, file, by = "Participant_Id")
            masterDataTable <- rbind.fill(masterDataTable, temp)
            masterDataTable <- unique(masterDataTable)
          } else {
            masterDataTable <- merge(masterDataTable, file, by = "Participant_Id")
          }
	  masterDataTable <- as.data.table(masterDataTable)
          file <- unique(masterDataTable[, cols, with=FALSE])
          file <- file[!is.na(file$Observation_Id),]
          idCols <- c('Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id')[c('Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id') %in% names(file)]
        }

        #make a copy with updated col names for downloadSite dir
        drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
        file <- file[, !drop, with=FALSE]
        testDrop <- c("Household_Id", "Participant_Id", "Observation_Id")
        testFile <- file[, !testDrop, with=FALSE]
        if (length(testFile) > 0) {
          names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')] <- paste0(metadata.file$label[match(names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], metadata.file$iri)], " [", names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], "]")
#          idCols <- c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')[c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id') %in% names(file)]
	  otherCols <- names(file)[!names(file) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')]
          setcolorder(file, c(idCols, otherCols[order(otherCols)]))
          fileName <- gsub("shiny_", "shiny_downloadDir_", shinyFiles[i])
          fwrite(file, fileName, sep='\t', na="NA") 
        }
      }
    }
  }
}

fwrite(masterDataTable, file.path(dataDir,"shiny_masterDataTable.txt"), sep='\t', na="NA")
#make a copy of merged file for downloadSite
drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")
masterDataTable <- as.data.table(masterDataTable)
masterDataTable <- masterDataTable[, !drop, with=FALSE]
names(masterDataTable)[!names(masterDataTable) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')] <- paste0(metadata.file$label[match(names(masterDataTable)[!names(masterDataTable) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], metadata.file$iri)], " [", names(masterDataTable)[!names(masterDataTable) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')], "]")
idCols <- c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')[c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id') %in% names(masterDataTable)]
otherCols <- names(masterDataTable)[!names(masterDataTable) %in% c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id')]
setcolorder(masterDataTable, c(idCols, otherCols[order(otherCols)]))
fwrite(masterDataTable, file.path(dataDir,"shiny_downloadDir.txt"), sep='\t', na="NA")

