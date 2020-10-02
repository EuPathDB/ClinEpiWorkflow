library(data.table)
library(plyr)

args <- commandArgs(TRUE)

dataDir <- args[1]
baseFileName <- args[2]
if (is.null(baseFileName)) {
  baseFileName <- "shiny"
}

#and create ontology download file
metadata.file <- fread(paste0(dataDir, "/ontologyMetadata.txt"))
dataDict.file <- fread(paste0(dataDir, "/ontologyMapping.txt"))
names(metadata.file) <- tolower(names(metadata.file))
metadata.file <- merge(metadata.file, dataDict.file, by = "iri", all = TRUE)
fwrite(metadata.file, file.path(dataDir, "shiny_downloadDir_ontologyMetadata.txt"), sep = '\t', na = "NA")

shinyFiles <- list.files(dataDir, pattern=baseFileName, full.names=TRUE)
shinyFiles <- shinyFiles[!grepl("masterDataTable", shinyFiles)]
shinyFiles <- shinyFiles[!grepl("downloadDir", shinyFiles)]

####### THE HELP #######
drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")

updateColNames <- function(colNames) {
  colNames <-  gsub(" ", "_", gsub("\\[|\\]", "", colNames))
  colNames[colNames == 'SOURCE_ID'] <- 'Participant_Id'
  colNames[colNames == 'COMMUNITY_ID'] <- 'Community_Id'
  colNames[colNames == 'HOUSEHOLD'] <- 'Household_Id'

  return(colNames)
}

dropUnnecessaryCols <- function(file) {
  file <- suppressWarnings(file[, !drop, with=FALSE])
  if (exists('masterDataTable')) {
    keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Household_Id' & colnames(file) != 'Participant_Id' & colnames(file) != 'Observation_Id' & colnames(file) != 'OBI_0001508')
    file <- file[, keep, with=FALSE]
  }
  file <- file[,which(unlist(lapply(file, function(x)!all(is.na(x))))),with=F] 

  return(file)
}

makePrettyCols <- function(file, idCols) {
  names(file)[!names(file) %in% idCols] <- paste0(metadata.file$label[match(names(file)[!names(file) %in% idCols], metadata.file$iri)], " [", names(file)[!names(file) %in% idCols], "]")
  idColsPresent <- idCols[idCols %in% names(file)]
  otherCols <- names(file)[!names(file) %in% idCols]
  setcolorder(file, c(idColsPresent, otherCols[order(otherCols)]))

  return(file)
}

########################


#participants start here since its something we always have
prtcpnt.file <- fread(shinyFiles[grepl("participants", shinyFiles)], na.strings = c("N/A", "na", ""))
names(prtcpnt.file) <- updateColNames(names(prtcpnt.file))
prtcpnt.file <- dropUnnecessaryCols(prtcpnt.file)
if ("EUPATH_0000095" %in% names(prtcpnt.file)) { prtcpnt.file$EUPATH_0000095 <- NULL }
masterDataTable <- prtcpnt.file
prtcpnt.back <- prtcpnt.file
## human readable, sorted columns names and print 
idCols <- c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id', 'Collection_Id')
prtcpnt.file <- makePrettyCols(prtcpnt.file, idCols)
fwrite(prtcpnt.file, file.path(dataDir,"shiny_downloadDir_participants.txt"), sep='\t', na="NA")


#households
if (any(grepl("household", shinyFiles))) {
  household.file <- fread(shinyFiles[grepl("household", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(household.file) > 0) {
    names(household.file) <- updateColNames(names(household.file)) 
    names(household.file)[names(household.file) == 'NAME'] <- 'Household_Observation_Id'
    names(household.file)[names(household.file) == 'EUPATH_0044122'] <- 'OBI_0001508'
    household.file <- dropUnnecessaryCols(household.file)
    if (!all(names(household.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
      if ("EUPATH_0015467" %in% names(household.file)) {
        if (uniqueN(household.file$EUPATH_0015467, na.rm=TRUE) == 1) {
          file$Household_Observation_Id <- NULL
        }
      }
      if (nrow(household.file) == uniqueN(household.file$Household_Id)) {
        household.file$Household_Observation_Id <- NULL
      }
      ## allow.cartesian bc multiple house obs per prtcpnt, multiple prtcpnts per house
      masterDataTable <- merge(masterDataTable, household.file, by = 'Household_Id', allow.cartesian = TRUE)
      idCols <- c('Household_Observation_Id', 'Household_Id', 'Participant_Id', 'Observation_Id', 'Sample_Id', 'Collection_Id')
      household.file <- makePrettyCols(household.file, idCols)
      fwrite(household.file, file.path(dataDir,"shiny_downloadDir_households.txt"), sep='\t', na="NA")
    }
  }
}

#community
if (any(grepl("community", shinyFiles))) {
  community.file <- fread(shinyFiles[grepl("community", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(community.file) > 0) {
    names(community.file) <- updateColNames(names(community.file))
    community.file <- dropUnnecessaryCols(community.file)
    names(community.file)[names(community.file) == 'EUPATH_0035016'] <- 'OBI_0001508'
    if (!all(names(community.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
      mergeByCols <- 'Household_Id'
      if ('OBI_0001508' %in% names(community.file)) {
        mergeByCols <- c('Household_Id', 'OBI_0001508')
      }
      masterDataTable <- merge(masterDataTable, community.file, by = mergeByCols)
      idCols <- c('Community_Id', 'Household_Observation_Id', 'Household_Id', 'Participant_Id', 'Observation_Id', 'Sample_Id', 'Collection_Id')
      community.file <- makePrettyCols(community.file, idCols)
      fwrite(community.file, file.path(dataDir,"shiny_downloadDir_community.txt"), sep='\t', na="NA")
    }
  }
}

#observations
if (any(grepl("observation", shinyFiles))) {
  obs.file <- fread(shinyFiles[grepl("observation", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(obs.file) > 0) {
    names(obs.file) <- updateColNames(names(obs.file))
    names(obs.file)[names(obs.file) == 'NAME'] <- 'Observation_Id'
    obs.file <- dropUnnecessaryCols(obs.file)
    if (!all(names(obs.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
      ##this basically tests for house obs in master table
      if ('Household_Id' %in% names(masterDataTable)) {
        cols <- c(names(obs.file), 'Household_Id')
      } else {
        cols <- names(obs.file)
      }
      mergeByCols <- 'Participant_Id'
      if ('OBI_0001508' %in% names(obs.file)) {
        mergeByCols <- c('Participant_Id', 'OBI_0001508')
      }
      if (uniqueN(masterDataTable$Participant_Id) != nrow(masterDataTable)) {
        temp <- merge(prtcpnt.back, obs.file, by = "Participant_Id")
        masterDataTable <- rbind.fill(masterDataTable, temp)
        masterDataTable <- unique(masterDataTable)
      } else {
        masterDataTable <- merge(masterDataTable, obs.file, by = mergeByCols)
      }
      masterDataTable <- as.data.table(masterDataTable)
      obs.file <- unique(masterDataTable[, cols, with=FALSE])
      obs.file <- obs.file[!is.na(obs.file$Observation_Id),]
      idCols <- c('Observation_Id', 'Participant_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id', 'Collection_Id')
      obs.file <- makePrettyCols(obs.file, idCols)
      fwrite(obs.file, 'shiny_downloadDir_observations.txt', sep='\t', na="NA")
    }
  }
}

#samples
if (any(grepl("sample", shinyFiles))) {
  sample.file <- fread(shinyFiles[grepl("sample", shinyFiles)], quote="", na.strings = c("N/A", "na", ""))
  if (nrow(sample.file) > 0) {
    names(sample.file) <- updateColNames(names(sample.file))
    names(sample.file)[names(sample.file) == 'OBSERVATION_ID'] <- 'Observation_Id'
    names(sample.file)[names(sample.file) == 'NAME'] <- 'Sample_Id'
    sample.file <- dropUnnecessaryCols(sample.file)
    if (!all(names(sample.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) { 
      ##some obs may not have samples, so set all=T
      if ('Household_Id' %in% names(masterDataTable) | 'Household_Id' %in% names(sample.file)) {
        cols <- c(names(sample.file), 'Participant_Id', 'Household_Id')
      } else {
        cols <- c(names(sample.file), 'Participant_Id')
      }

      masterDataTable <- merge(masterDataTable, sample.file, by = "Observation_Id", all = TRUE)
      sample.file <- unique(masterDataTable[, cols, with=FALSE])
      sample.file <- sample.file[!is.na(sample.file$Sample_Id),]
      idCols <- c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id', 'Collection_Id')
      sample.file <- makePrettyCols(sample.file, idCols)
      fwrite(sample.file, 'shiny_downloadDir_samples.txt', sep='\t', na="NA")
    }
  }
}


#entomology
if (any(grepl("ento", shinyFiles))) {
  ento.file <- fread(shinyFiles[grepl("ento", shinyFiles)], quote="", na.strings = c("N/A", "na", ""))
  if (nrow(ento.file) > 0) {
    names(ento.file) <- updateColNames(names(ento.file))
    names(ento.file)[names(ento.file) == 'OBSERVATION_ID'] <- 'Observation_Id'
    names(ento.file)[names(ento.file) == 'NAME'] <- 'Collection_Id'
    ento.file <- dropUnnecessaryCols(ento.file)
    ento.file$Participant_Id <- NULL
    ento.file <- unique(ento.file)
    if (!all(names(ento.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) { 
      idCols <- c('Collection_Id', 'Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 'Household_Id')
      ento.file <- makePrettyCols(ento.file, idCols)
      fwrite(ento.file, 'shiny_downloadDir_entomology.txt', sep='\t', na="NA")
    }
  }
}


#combined file
masterDataTable <- as.data.table(masterDataTable)
masterDataTable <- suppressWarnings(masterDataTable[, !drop, with=FALSE])
idCols <- c('Participant_Id', 'Observation_Id', 'Household_Id', 'Household_Observation_Id', 'Sample_Id', 'Collection_Id')
masterDataTable <- makePrettyCols(masterDataTable, idCols)
fwrite(masterDataTable, file.path(dataDir,"shiny_downloadDir.txt"), sep='\t', na="NA")
