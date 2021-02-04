rm(list=ls())
setwd("~/Box/Danica EuPathDB/Studies/WASH-B/Bangladesh/Primary outcomes/DH_workspace/Data/Downloaded data/Bulk downloads")

library(data.table)
library(plyr)

# args <- commandArgs(TRUE)
# 
# dataDir <- args[1]
# baseFileName <- args[2]
# if (is.null(baseFileName)) {
#   baseFileName <- "shiny"
# }

dataDir <- "./2020_02_02/EuPathDB_workflows_ClinEpiDB_4.0_data"

#and create ontology download file
metadata.file <- fread(paste0(dataDir, "/ontologyMetadata.txt"))
dataDict.file <- fread(paste0(dataDir, "/ontologyMapping.txt"))
names(metadata.file) <- tolower(names(metadata.file))
names(dataDict.file) <- tolower(names(dataDict.file))
metadata.file <- merge(metadata.file, dataDict.file, by = "iri", all = TRUE)
fwrite(metadata.file, file.path(dataDir, "shiny_downloadDir_ontologyMetadata.txt"), sep = '\t', na = "NA")

# shinyFiles <- list.files(dataDir, pattern=baseFileName, full.names=TRUE)
shinyFiles <- list.files(path="./2020_02_02/EuPathDB_workflows_ClinEpiDB_4.0_data", pattern="shiny", full.names=T)
shinyFiles <- shinyFiles[!grepl("masterDataTable", shinyFiles)]
shinyFiles <- shinyFiles[!grepl("downloadDir", shinyFiles)]


####### THE HELP #######
drop <- c("PAN_ID", "NAME", "DESCRIPTION", "PAN_TYPE_ID", "PAN_TYPE")

updateColNames <- function(colNames) {
  colNames <-  gsub(" ", "_", gsub("\\[|\\]", "", colNames))
  colNames[colNames == 'SOURCE_ID'] <- 'Participant_Id'
  colNames[colNames == 'COMMUNITY_ID'] <- 'Community_Id'
  colNames[colNames == 'COMMUNITY_OBSERVATION_ID'] <- 'Community_Observation_Id'
  colNames[colNames == 'HOUSEHOLD'] <- 'Household_Id'
  colNames[colNames == 'HOUSEHOLD_OBSERVATION_ID'] <- 'Household_Observation_Id'
  
  return(colNames)
}

dropUnnecessaryCols <- function(file) {
  file <- suppressWarnings(file[, !drop, with=FALSE])
  if (exists('masterDataTable')) {
    keep <- !(colnames(file) %in% colnames(masterDataTable) & colnames(file) != 'Household_Id' & colnames(file) != 'Participant_Id' & colnames(file) != 'Observation_Id' & colnames(file) != 'OBI_0001508' & colnames(file) != 'Community_Id')
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
if ("EUPATH_0000095" %in% names(prtcpnt.file)) { prtcpnt.file$EUPATH_0000095 <- NULL }# if Participant ID has been assigned an IRI, drop it
prtcpnt.file$Participant_Id = as.character(prtcpnt.file$Participant_Id);
masterDataTable <- prtcpnt.file
prtcpnt.back <- prtcpnt.file
## human readable, sorted columns names and print 
idCols <- c('Participant_Id', 'Observation_Id', 'Community_Id', 'Community_Observation_Id', 'Household_Id', 
            'Household_Observation_Id', 'Sample_Id', 'Collection_Id')
prtcpnt.file <- makePrettyCols(prtcpnt.file, idCols)
fwrite(prtcpnt.file, file.path(dataDir,"shiny_downloadDir_participants.txt"), sep='\t', na="NA")

dim(masterDataTable)
#[1] 11549 12
dim(prtcpnt.file)
#[1] 11549 12


#households 
#Household study timepoint = EUPATH_0044122
#Household observation type = EUPATH_0015467)
if (any(grepl("household", shinyFiles))) {
  household.file <- fread(shinyFiles[grepl("household", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(household.file) > 0) {
    names(household.file) <- updateColNames(names(household.file)) 
    names(household.file)[names(household.file) == 'NAME'] <- 'Household_Observation_Id'
    household.file <- dropUnnecessaryCols(household.file)
    if (!all(names(household.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
      if ("EUPATH_0015467" %in% names(household.file)) { 
        if (uniqueN(household.file$EUPATH_0015467, na.rm=TRUE) == 1) {
          household.file$Household_Observation_Id <- NULL
        }
      }
      if (nrow(household.file) == uniqueN(household.file$Household_Id)) {
        household.file$Household_Observation_Id <- NULL
      }
##### ADDED THIS CODE ######
      if (nrow(household.file) != uniqueN(household.file$Household_Id)) {
        non_long <- NULL
        temp <- data.frame(household.file)
        for(i in names(temp)){
          if(length(temp[,i][is.na(temp$EUPATH_0044122) & !is.na(temp[,i])])>0){
            non_long <- append(i,non_long)
          }
        }
        
        non_long <- non_long[non_long %in% idCols==F] #these are the non-longitudinal household variables
        temp <- unique(temp[is.na(temp$EUPATH_0044122),c("Household_Id", non_long)])
        
        household.file <- merge(household.file[, c(names(temp)[names(temp)!="Household_Id"]):=NULL], 
                                temp, by="Household_Id", all.x=T, all.y=T)
        
        household.file <- household.file[!is.na(household.file$'EUPATH_0044122'),]
      }
      
####################      
      mergeByCols <- 'Household_Id' # participants have it, so masterDataTable has it
      if('EUPATH_0044122' %in% names(household.file)) { 
        household.file$'mergeByTimepoint' <- household.file$'EUPATH_0044122'
        if('mergeByTimepoint' %in% names(masterDataTable)){ # which it will not be, so why bother?
          mergeByCols <- c('Household_Id', 'mergeByTimepoint')
        }
      }
      ## allow.cartesian bc multiple house obs per prtcpnt, multiple prtcpnts per house
      print("Merging household");
      masterDataTable <- merge(masterDataTable, household.file, by = mergeByCols, all.x=T, all.y=T)#allow.cartesian = TRUE)
      idCols <- c('Household_Observation_Id', 'Household_Id', 'Community_Id', 'Community_Observation_Id', 
                  'Participant_Id', 'Observation_Id', 'Sample_Id', 'Collection_Id')
     if('mergeByTimepoint' %in% names(household.file)){
        household.file$'mergeByTimepoint' <- NULL
      }
      household.back <- household.file
      household.file <- makePrettyCols(household.file, idCols)
      fwrite(household.file, file.path(dataDir,"shiny_downloadDir_households.txt"), sep='\t', na="NA")
    }
  }
}
non_long_hh <- non_long




####sanity check section#####
dim(masterDataTable)
#[1] 22715 59
names(prtcpnt.back)[names(prtcpnt.back) %in% names(masterDataTable)==F]
sort(names(masterDataTable))

length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Participant_Id)])
#[1] 0

masterDataTable[is.na(masterDataTable$'ENVO_00000004'),]
table(masterDataTable$'ENVO_00000004', useNA="ifany")
####sanity check section#####




#community 
#Community study timepoint = EUPATH_0035016
if (any(grepl("community", shinyFiles))) {
  community.file <- fread(shinyFiles[grepl("community", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(community.file) > 0) {
    names(community.file) <- updateColNames(names(community.file))
    community.file <- dropUnnecessaryCols(community.file)
    community.file <- unique(community.file)
    if (!all(names(community.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
    
    ##### ADDED THIS CODE ######
    if (nrow(community.file) == uniqueN(community.file$Community_Id)) {
      community.file$Community_Observation_Id <- NULL
    }
    
    if (nrow(community.file) != uniqueN(community.file$Community_Id)) {
      non_long <- NULL
      temp <- data.frame(community.file)
      for(i in names(temp)){
        if(length(temp[,i][is.na(temp$EUPATH_0035016) & !is.na(temp[,i])])>0){
          non_long <- append(i,non_long)
        }
      }
      
      non_long <- non_long[non_long %in% idCols==F] #these are the non-longitudinal community variables
      temp <- unique(temp[is.na(temp$EUPATH_0035016),c("Community_Id", non_long)])
      
      community.file <- merge(community.file[, c(names(temp)[names(temp)!="Community_Id"]):=NULL], 
                              temp, by="Community_Id", all.x=T, all.y=T)
      
      community.file <- community.file[!is.na(community.file$'EUPATH_0035016'),]
    }
    ####################    
    
    mergeByCols <- 'Community_Id' # if communities exist, then households exist and they are in masterDataTable and have Community_Id
    if ('EUPATH_0035016' %in% names(community.file)) { 
      community.file$'mergeByTimepoint' <- community.file$'EUPATH_0035016'
      if('mergeByTimepoint' %in% names(masterDataTable)){
        mergeByCols <- c('Community_Id', 'mergeByTimepoint')
      }
    }
    ## allow.cartesian bc multiple community obs per household, multiple houses per community
    print("Merging community");
    masterDataTable <- merge(masterDataTable, community.file, by = mergeByCols, all.x=T, all.y=T)#allow.cartesian = TRUE)
    idCols <- c('Community_Id', 'Community_Observation_Id', 'Household_Observation_Id', 'Household_Id', 
                'Participant_Id', 'Observation_Id', 'Sample_Id', 'Collection_Id')
    if( 'mergeByTimepoint' %in% names(community.file)){
      community.file$'mergeByTimepoint' <- NULL
    }
    community.back <- community.file
    community.file <- makePrettyCols(community.file, idCols)
    fwrite(community.file, file.path(dataDir,"shiny_downloadDir_community.txt"), sep='\t', na="NA")
    }
  }
}
non_long_c <- non_long



####sanity check section#####
dim(masterDataTable)
#[1] 22715 95
names(prtcpnt.back)[names(prtcpnt.back) %in% names(masterDataTable)==F]
names(household.back)[names(household.back) %in% names(masterDataTable)==F]
sort(names(masterDataTable))

length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Participant_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Community_Id)])
#[1] 0

masterDataTable[is.na(masterDataTable$'ENVO_00000004'),]
table(masterDataTable$'ENVO_00000004', useNA="ifany")

length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Observation_Id)])
#[1]0
length(masterDataTable$Household_Id[is.na(masterDataTable$Community_Observation_Id)])
#[1]0
####sanity check section#####




#observations 
#Study timepoint = OBI_0001508
if (any(grepl("observation", shinyFiles))) {
  obs.file <- fread(shinyFiles[grepl("observation", shinyFiles)], na.strings = c("N/A", "na", ""))
  if (nrow(obs.file) > 0) {
    names(obs.file) <- updateColNames(names(obs.file))
    names(obs.file)[names(obs.file) == 'NAME'] <- 'Observation_Id'
    obs.file <- dropUnnecessaryCols(obs.file)
    obs.file$Participant_Id = as.character( obs.file$Participant_Id, TRUE);
    if (!all(names(obs.file) %in% c("Household_Id", "Participant_Id", "Observation_Id"))) {
      # ##this basically tests for house obs in master table
      # if ('Household_Id' %in% names(masterDataTable)) {
      #   cols <- c(names(obs.file), 'Household_Id')
      # } else {
      #   cols <- names(obs.file)
      # }
      mergeByCols <- 'Participant_Id'
      if ('OBI_0001508' %in% names(obs.file)) {
        obs.file$'mergeByTimepoint' <- obs.file$'OBI_0001508'
        if('mergeByTimepoint' %in% names(masterDataTable)){
          mergeByCols <- c('Participant_Id', 'mergeByTimepoint')
        }
      }
      print("Merging observations");
      if (uniqueN(masterDataTable$Participant_Id) < nrow(masterDataTable)) { #need to map participant info into every observation
        myVector <- c("Household_Id", "Community_Id", non_long_c, non_long_hh)
        prtcpnt.back <- merge(prtcpnt.back, 
                              unique(masterDataTable[, ..myVector]), 
                              by="Household_Id", 
                              all.x=T, all.y=T)
        temp <- merge(prtcpnt.back, obs.file, by = "Participant_Id", all.x=T, all.y=T)
        masterDataTable <- merge(masterDataTable, temp, 
                                 by=unique(c(mergeByCols, names(temp)[names(temp) %in% names(masterDataTable)])), 
                                 all.x=T, all.y=T)
        masterDataTable <- unique(masterDataTable)
      } 
      if (uniqueN(masterDataTable$Participant_Id) == nrow(masterDataTable)) { #no need to backfill participant data to every observation
        masterDataTable <- merge(masterDataTable, obs.file, by = mergeByCols, all.x=T, all.y=T)
      } 
      masterDataTable <- as.data.table(masterDataTable)
      if ('Household_Id' %in% names(masterDataTable)) {
        cols <- c(names(obs.file), 'Household_Id')
      } else {
        cols <- names(obs.file)
      }
      if ('Community_Id' %in% names(masterDataTable)) {
        cols <- c(cols, 'Community_Id')
      } else {
        cols <- cols
      }
      obs.file <- unique(masterDataTable[, cols, with=FALSE])
      obs.file <- obs.file[!is.na(obs.file$Observation_Id),]
      idCols <- c('Observation_Id', 'Participant_Id', 'Community_Id', 'Community_Observation_Id', 
                  'Household_Id', 'Household_Observation_Id', 'Sample_Id', 'Collection_Id')
      # remove the timepoint merging column before writing
      if( 'mergeByTimepoint' %in% names(obs.file)){
        obs.file$'mergeByTimepoint' <- NULL
      }
      obs.file <- makePrettyCols(obs.file, idCols)
      fwrite(obs.file, file.path(dataDir, 'shiny_downloadDir_observations.txt'), sep='\t', na="NA")
    }
  }
}



####sanity check section#####
dim(masterDataTable)
#[1] 32217 148
names(prtcpnt.back)[names(prtcpnt.back) %in% names(masterDataTable)==F]
names(household.back)[names(household.back) %in% names(masterDataTable)==F]
names(community.back)[names(community.back) %in% names(masterDataTable)==F]
sort(names(masterDataTable))

length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Participant_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Community_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Observation_Id)])
#[1] 7182

table(masterDataTable$'ENVO_00000004', useNA="ifany")
table(masterDataTable$'EUPATH_0035014', useNA="ifany")
length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Observation_Id)])
#[1]9502
length(masterDataTable$Household_Id[is.na(masterDataTable$Community_Observation_Id)])
#[1]9502

temp <- data.frame(masterDataTable[is.na(masterDataTable$Observation_Id),])
head(temp[,c("Community_Id","Community_Observation_Id","Household_Observation_Id","Household_Id",
             "Participant_Id","Observation_Id", 'PATO_0000047','ENVO_00000004','EUPATH_0035013')])

temp <- data.frame(masterDataTable[, c("Community_Id","Household_Id", "EUPATH_0035016", "EUPATH_0044122", "OBI_0001508")])
temp[temp$EUPATH_0035016 != temp$EUPATH_0044122 & !is.na(temp$EUPATH_0035016) & !is.na(temp$EUPATH_0044122),]
temp[temp$EUPATH_0035016 != temp$OBI_0001508 & !is.na(temp$EUPATH_0035016) & !is.na(temp$OBI_0001508),]
temp[temp$OBI_0001508 != temp$EUPATH_0044122 & !is.na(temp$OBI_0001508) & !is.na(temp$EUPATH_0044122),]

table(masterDataTable$'ENVO_00000004', useNA="ifany")
table(masterDataTable$'PATO_0000047', useNA="ifany")
table(masterDataTable$'EUPATH_0035013', useNA="ifany")

masterDataTable[is.na(masterDataTable$'PATO_0000047'),]
table(masterDataTable$'EUPATH_0035128'[is.na(masterDataTable$'PATO_0000047')],useNA = "ifany")
#Never surveyed 
#2073 
####sanity check section#####




#samples
if (any(grepl("sample", shinyFiles))) {
  sample.file <- fread(shinyFiles[grepl("sample", shinyFiles)], quote="", na.strings = c("N/A", "na", ""))
  if (nrow(sample.file) > 0) {
    names(sample.file) <- updateColNames(names(sample.file))
    names(sample.file)[names(sample.file) == 'OBSERVATION_ID'] <- 'Observation_Id'
    names(sample.file)[names(sample.file) == 'NAME'] <- 'Sample_Id'
    sample.file <- dropUnnecessaryCols(sample.file)      
    sample.file <- dropUnnecessaryCols(sample.file)
    sample.file <- unique(sample.file)
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
      idCols <- c('Sample_Id', 'Observation_Id', 'Participant_Id', 'Community_Id', 'Community_Observation_Id', 
                  'Household_Observation_Id', 'Household_Id', 'Collection_Id')
      sample.file <- makePrettyCols(sample.file, idCols)
      fwrite(sample.file, file.path(dataDir, 'shiny_downloadDir_samples.txt'), sep='\t', na="NA")
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
      idCols <- c('Collection_Id', 'Sample_Id', 'Observation_Id', 'Participant_Id', 'Household_Observation_Id', 
                  'Household_Id', 'Community_Id', 'Community_Observation_Id')
      ento.file <- makePrettyCols(ento.file, idCols)
      fwrite(ento.file, file.path(dataDir, 'shiny_downloadDir_entomology.txt'), sep='\t', na="NA")
    }
  }
}


#combined file
if('mergeByTimepoint' %in% names(masterDataTable)) {
  names(masterDataTable)[names(masterDataTable)=='mergeByTimepoint'] <- "Timepoint"
}
masterDataTable <- as.data.table(masterDataTable)
masterDataTable <- suppressWarnings(masterDataTable[, !drop, with=FALSE])
idCols <- c('Community_Id', 'Household_Id', 'Participant_Id', 'Timepoint', 'Community_Observation_Id', 
            'Household_Observation_Id', 'Observation_Id','Sample_Id', 'Collection_Id')
masterDataTable <- makePrettyCols(masterDataTable, idCols)
fwrite(masterDataTable, file.path(dataDir,"shiny_downloadDir.txt"), sep='\t', na="NA")


####sanity check section#####
dim(masterDataTable)
#[1] 32217 148
names(prtcpnt.file)[names(prtcpnt.file) %in% names(masterDataTable)==F]
names(household.file)[names(household.file) %in% names(masterDataTable)==F]
names(community.file)[names(community.file) %in% names(masterDataTable)==F]
names(obs.file)[names(obs.file) %in% names(masterDataTable)==F]
names(masterDataTable)

length(masterDataTable$Household_Id[is.na(masterDataTable$Household_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Participant_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Community_Id)])
#[1] 0
length(masterDataTable$Household_Id[is.na(masterDataTable$Observation_Id)])
#[1] 7182

table(masterDataTable$'Country', useNA="ifany")
table(masterDataTable$'Compound study arm', useNA="ifany")
table(masterDataTable$'Sex', useNA="ifany")
table(masterDataTable$'Target child never surveyed flag'[is.na(masterDataTable$'Sex')], useNA="ifany")
####sanity check section#####