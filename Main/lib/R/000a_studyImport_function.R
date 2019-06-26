studyImport <- function(FOLDER, TYPE, STUDY, MISSING, DATE_TIME, PARTICIPANT_ID) {
  
  ###############################################
  # load libraries
  
  require(purrr)
  require(plyr)
  require(dplyr)
  require(haven)
  
  Sys.setlocale('LC_ALL','C') 
  
  
  ###############################################
  # import all data files into a named list
  
  if(TYPE==".csv"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read.csv, as.is=T)
  }
  if(TYPE==".txt"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read.delim, header = T, as.is=T)
  }
  if(TYPE==".RData"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, load)
  }
  if(TYPE==".sas7bdat"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read_sas)
  }
  if(TYPE==".dta"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read_dta)
  }
  if(TYPE==".sav" | TYPE==".zsav" | TYPE==".por"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read_spss)
  }
  if(TYPE==".dat"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles <- lapply(filenames, read.csv, as.is=T)
  }
  
  names(dataFiles) <- gsub("^.+./", "", filenames)
  
  
  ###############################################
  # clean up data by removing any values that start or end with spaces (variables with structure = "chr" only)
  
  for(i in 1:length(dataFiles)){
    for(j in 1:length(dataFiles[[i]])){
      print(paste(i, j, sep=", "))
      if (class(dataFiles[[i]][,j])=="character"){
        dataFiles[[i]][,j][grep(" ", dataFiles[[i]][,j])] <- gsub(" +$", "", gsub("^ +", "", dataFiles[[i]][,j][grep(" ", dataFiles[[i]][,j])]))
      }
    }
  }
  
  
  ###############################################
  # create variableMap dataframes for each data file
  
  variableMaps <- list()
  
  for(i in 1:length(dataFiles)){
    print(paste(i, j, sep=", "))
    variableMaps[[paste("var", names(dataFiles[i]), sep="_")]] <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles[i]))), tolower(names(dataFiles[[i]])), sep="::"),
                                                                             dataSet=STUDY,
                                                                             variable=tolower(names(dataFiles[[i]])),
                                                                             dataFile=tolower(gsub("[.].+$", "", names(dataFiles[i]))),
                                                                             variable_dataFile=names(dataFiles[[i]]),
                                                                             file=names(dataFiles[i]),
                                                                             colOrder=c(1:ncol(dataFiles[[i]])),
                                                                             type=map_chr(dataFiles[[i]], type_sum),
                                                                             example="",
                                                                             stringsAsFactors=F)
  }
  
  
  ###############################################
  # create "example" column in the variableMap dataframes that pastes in the first non-missing value for each variable. this will help assess the structure of the data
  
  for(i in 1:length(dataFiles)){
    for(j in 1:length(dataFiles[[i]])){
      print(paste(i, j, sep=", "))
      variableMaps[[paste("var", names(dataFiles[i]), sep="_")]][j,"example"] <- dataFiles[[i]][,j][dataFiles[[i]][,j] %in% MISSING==F][1]
    }
  }
  rm(i, j)
  
  
  #############################################################
  #merge variable mapping codebook files together
  
  allVars <- ldply(variableMaps, data.frame)
  allVars <- allVars[,names(allVars) != ".id"]
  
  rm(variableMaps)
  
  
  #############################################################
  # create "dateTime" and fill with "date?" if variable name matches the regex defined above --> these will have to be checked by hand, but are a good first pass
  
  allVars$dateTime <- NA
  
  for(i in DATE_TIME){
    allVars[grep(i, allVars$variable),"dateTime"] <- "date?"
  }
  
  
  #############################################################
  # using each variable's value "example", see which match format codes for date variables. If so, mark dateTime == "date?" 
  # CHEAT SHEET FOR REGEX: 
  # \\d = ANY ONE DIGIT FROM 0-9 
  # \\D = ANY ONE CHARACTER THAT IS NOT A DIGIT
  # CHEAT SHEET FOR DATES: 
  # %d = day as a number (0-31)	
  # %a = abbreviated weekday (Mon)
  # %A = unabbreviated weekday (Monday)
  # %m = month (00-12)	
  # %b = abbreviated month (Jan)
  # %B = unabbreviated month (January)
  # %y = 2-digit year (07)
  # %Y = 4-digit year (2007)
  
  allVars$formatCode <- NA
  allVars[grep("^\\d\\d\\D\\D\\D\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d%b%Y"
  allVars[grep("^\\d\\d/\\D\\D\\D/\\d\\d$", allVars$example), "formatCode"] <- "%d/%b/%y"
  allVars[grep("^^\\d\\d-\\D\\D\\D-\\d\\d$", allVars$example), "formatCode"] <- "%d-%b-%y"
  allVars[grep("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", allVars$example), "formatCode"] <- "%Y-%m-%d"
  allVars[grep("^\\d\\d-\\D\\D\\D-\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d-%b-%Y"
  allVars[grep("^\\d\\d-\\D\\D\\D-\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d-%b-%Y"
  allVars[grep("^^\\d\\d\\D\\D\\D\\d\\d:", allVars$example), "formatCode"] <- "%d%b%y:%h:%m:%s"
  allVars[grep("^\\d\\d/\\d\\d/\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d/%m/%Y"
  allVars[grep("^\\d/\\d\\d/\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d/%m/%Y"
  allVars[grep("^\\d\\d/\\D\\D\\D/\\d\\d\\d\\d$", allVars$example), "formatCode"] <- "%d/%b/%Y"
  
  allVars[!is.na(allVars$formatCode), "dateTime"] <- "date?"
  
  
  ###############################################
  # determine minValue, maxValue, values, uniqueValueCount, percentMissing
  
  allVars[,c("minValue", "maxValue", "values", "uniqueValueCount", "percentMissing")] <- NA
  
  for (i in 1:length(dataFiles)){
    for (j in unique(names(dataFiles[[i]]))){
      print(paste(i, j, sep=", "))
      all.values <- as.vector(unlist(dataFiles[[i]][j]))
      temp.values <- sort(unique(as.vector(unlist(dataFiles[[i]][j]))), na.last=T)
      
      # fill out information for variables that have numeric or integer values
      if(allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"type"] %in% c("dbl", "int", "date")){
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"minValue"] <- min(temp.values[temp.values!="" & !is.na(temp.values)])
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"maxValue"] <- max(temp.values[temp.values!="" & !is.na(temp.values)])
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"uniqueValueCount"] <- length(unique(all.values[all.values!="" & !is.na(all.values)]))
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"percentMissing"] <- length(all.values[all.values %in% MISSING])/length(all.values)
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"values"] <- 
          paste(allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"minValue"], allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"maxValue"], sep=" to ")
      }
      
      # fill out information for variables that have character or logical values
      if(allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i], "type"] %in% c("chr", "lgl")){
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"minValue"] <- "n/a"
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"maxValue"] <- "n/a"
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"uniqueValueCount"] <- length(unique(all.values[all.values!="" & !is.na(all.values)]))
        allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"percentMissing"] <- length(all.values[all.values %in% MISSING])/length(all.values)
        if (length(temp.values[!is.na(temp.values)]) > 10){
          allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"values"] <- ">10 unique values"
        } 
        if (length(temp.values[!is.na(temp.values)]) > 0 & length(temp.values) <= 10){
          allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"values"] <- paste(temp.values, collapse=", ")
        } 
        if (length(temp.values[!is.na(temp.values)]) == 0){
          allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles)[i],"values"] <- "n/a"
        }
      }
      print(names(dataFiles[i]))
    }
  }
  rm(i, j, temp.values, all.values)
  
  #############################################################
  # add "timeVarying" column to allVars
  
  allVars$timeVarying <- NA
  
  for(i in allVars$uniqueVar){
    print(i)
    variable <- unique(allVars$variable[allVars$uniqueVar==i])
    dataFile <- unique(allVars$file[allVars$uniqueVar==i])
    
    if(variable %in% tolower(names(dataFiles[[dataFile]]))){
      temp <- dataFiles[[dataFile]]
      names(temp) <- tolower(names(temp))
      temp2 <- temp[,c(PARTICIPANT_ID, variable)]
      temp2 <- distinct(temp2[!is.na(temp2[,variable]),]) 
      
      if(length(temp2[,variable])>0){
        temp3 <- data.frame(table(temp2[,PARTICIPANT_ID]))
        
        if(max(temp3$Freq)==1){
          allVars$timeVarying[allVars$uniqueVar==i] <- "once"
        }
        if(max(temp3$Freq)>1){
          allVars$timeVarying[allVars$uniqueVar==i] <- "repeated"
        }
      }
      
      if(length(temp2[,variable])==0){
        allVars$timeVarying[allVars$uniqueVar==i] <- "no data"
      }
    }
  }
  
  rm(temp, temp2, temp3, i)
  
  #############################################################
  # create "flag" and flag variables that are duplicated or missing 100% of data
  
  allVars$flag <- NA
  for(i in names(dataFiles)){
    d <- dataFiles[[i]]
    if(length(d[,1])==0){
      print(i)
      allVars$flag[allVars$file==i] <- "file has no data"
    }
  }
  
  allVars[allVars$variable %in% unique(allVars$variable[duplicated(allVars$variable)]) & allVars$flag !="file has no data", "flag"]
  table(allVars$flag, useNA="ifany")
  
  allVars[allVars$variable %in% unique(allVars$variable[duplicated(allVars$variable)]), "flag"] <- gsub(", NA", "", paste("duplicated variable", 
                                                                                                                          allVars[allVars$variable %in% unique(allVars$variable[duplicated(allVars$variable)]), "flag"], 
                                                                                                                          sep=", "))
  allVars[allVars$percentMissing==1 & allVars$flag %in% c("file has no data", "duplicated variable, file has no data")==F, "flag"] <- "no data"
  
  
  ###############################################
  # PREP for value map: if TYPE==".csv" or if TYPE=".txt" re-import data for creating value map 
  # values entered as "NA" versus no value entered (BLANK) need to be differentiated
  ### blank cells in the .csv are now <NA> --> these won't need to be added to the value map file
  ### "NA" cells in the .csv are now "NA" -->  these need to be added to the value map file & mapped to :::UNDEF::: to remove or given a different mapped term
  
  if(TYPE==".csv"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles2 <- lapply(filenames, read.csv, as.is=T, na.strings="")
  }
  if(TYPE==".txt"){
    filenames <- list.files(path=FOLDER, pattern=TYPE, full.names=T)
    dataFiles2 <- lapply(filenames, read.delim, header=T, as.is=T, na.strings="")
  }
  if (exists("dataFiles2") && is.list(get("dataFiles2"))){
    names(dataFiles2) <- gsub("^.+./", "", filenames)
  } else {
    dataFiles2 <- dataFiles
  }
  
  names(dataFiles2) <- gsub("^.+./", "", filenames)
  
  
  ###############################################
  # create list of all possible values (responses) for each variable
  
  valueMap <- data.frame(uniqueVar=character(), dataSet=character(), variable=character(), dataFile=character(), values=character())
  
  for (i in 1:length(dataFiles2)){
    for (j in unique(names(dataFiles2[[i]]))){
      print(paste(i, j, sep=", "))
      all.values <- as.vector(unlist(dataFiles2[[i]][j]))
      temp.values <- sort(unique(as.vector(unlist(dataFiles2[[i]][j]))), na.last=T)
      
      ####### VALUEMAP
      
      #for allVars with no data --> don't add a row to the data file
      if(length(temp.values[!is.na(temp.values)])==0){
        # temp.df <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
        #                       dataSet=STUDY,
        #                       variable=tolower(j), 
        #                       dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
        #                       variable_dataFile=j,
        #                       file=names(dataFiles2[i]),
        #                       values="no data", 
        #                       stringsAsFactors=F) 
      }
      
      #for categorical allVars where type == chr or lgl and <20 unique values
      if(length(temp.values[!is.na(temp.values)])>0 & 
         length(temp.values[!is.na(temp.values)])<=20 & 
         allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles2)[i], "type"] %in% c("chr", "lgl")){
        temp.df <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                              dataSet=STUDY,
                              variable=tolower(j), 
                              dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                              variable_dataFile=j,
                              file=names(dataFiles2[i]),
                              values=temp.values[!is.na(temp.values)], 
                              stringsAsFactors=F) 
      }
      
      #for categorical allVars where type == chr or lgl (screen out variables with >20 values, as these are likely IDs (PID, sampleID, etc))
      if(length(temp.values[!is.na(temp.values)])>20 & 
         allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles2)[i], "type"] %in% c("chr", "lgl")){
        temp.df <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                              dataSet=STUDY,
                              variable=tolower(j), 
                              dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                              variable_dataFile=j,
                              file=names(dataFiles2[i]),
                              values=">20 categories", 
                              stringsAsFactors=F) 
        if("NA" %in% temp.values[!is.na(temp.values)]){
          temp.df2 <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                                 dataSet=STUDY,
                                 variable=tolower(j), 
                                 dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                                 variable_dataFile=j,
                                 file=names(dataFiles2[i]),
                                 values="NA", 
                                 stringsAsFactors=F)
          temp.df <- rbind(temp.df, temp.df2)
          rm(temp.df2)
        }
      }
      
      #for categorical allVars where length of values <= 10 and type == dbl or int or date
      if(length(temp.values[!is.na(temp.values)])>0 & length(temp.values[!is.na(temp.values)])<=10 & 
         allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles2)[i], "type"] %in% c("int", "dbl", "date")){ 
        temp.df <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                              dataSet=STUDY,
                              variable=tolower(j), 
                              dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                              variable_dataFile=j,
                              file=names(dataFiles2[i]),
                              values=temp.values[!is.na(temp.values)], 
                              stringsAsFactors=F) 
      }
      
      #for continuous allVars where length of values > 10 and type == dbl or int
      if(length(temp.values[!is.na(temp.values)])>10 & 
         allVars[!is.na(allVars$variable_dataFile) & allVars$variable_dataFile==j & allVars$file==names(dataFiles2)[i], "type"] %in% c("int", "dbl", "date")){ 
        temp.df <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                              dataSet=STUDY,
                              variable=tolower(j), 
                              dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                              variable_dataFile=j,
                              file=names(dataFiles2[i]),
                              values="continuous", 
                              stringsAsFactors=F)
        if("NA" %in% temp.values[!is.na(temp.values)]){
          temp.df2 <- data.frame(uniqueVar=paste(tolower(gsub("[.].+$", "", names(dataFiles2[i]))), tolower(j), sep="::"),
                                 dataSet=STUDY,
                                 variable=tolower(j), 
                                 dataFile=tolower(gsub("[.].+$", "", names(dataFiles2[i]))),
                                 variable_dataFile=j,
                                 file=names(dataFiles2[i]),
                                 values="NA", 
                                 stringsAsFactors=F)
          temp.df <- rbind(temp.df, temp.df2)
          rm(temp.df2)
        }
      }
      if (exists("temp.df") && is.data.frame(get("temp.df"))){
        valueMap <- rbind(valueMap, temp.df)
        print(names(dataFiles2[i]))
        print(dim(valueMap))
        rm(temp.df, temp.values, all.values)
      }
    }
  }
  rm(i, j)
  
  
  #############################################################
  # demark boolean variables (ie, those with only values of 0, 1 and NA)
  
  valueMap$boolean <- ""
  for(i in unique(valueMap$uniqueVar)){
    if(paste(sort(valueMap$values[valueMap$uniqueVar==i]), collapse=", ") %in% c("0, 1", "0, 1, NA")){
      valueMap$boolean[valueMap$uniqueVar==i] <- T
    }
  }
  
  
  #############################################################
  # convert <NA> and "NA" into "" and <NA> to match the original data file for .csv and .txt files
  
  if(TYPE==".txt" | TYPE==".csv"){
    for(j in names(dataFiles2)){
      for(i in 1:ncol(dataFiles2[[j]])){
        dataFiles2[[j]][,i][is.na(dataFiles2[[j]][,i])] <- ""
        dataFiles2[[j]][,i][dataFiles2[[j]][,i]=="NA"] <- NA
      }
    }
  }
  
  #############################################################
  # shorten names of each dataFile in the list to match uniqueVar after the pipe
  
  names(dataFiles2) <- tolower(gsub("[.].+$", "", gsub("^.+./", "", filenames)))
  names(dataFiles) <- tolower(gsub("[.].+$", "", gsub("^.+./", "", filenames)))
  
  
  #############################################################
  # make column names in dataFiles lowercase
  
  for(i in unique(names(dataFiles2))){
    names(dataFiles2[[i]]) <- tolower(names(dataFiles2[[i]]))
  }
  
  for(i in unique(names(dataFiles))){
    names(dataFiles[[i]]) <- tolower(names(dataFiles[[i]]))
  }
  
  
  #############################################################
  # return results of the function as a list of allVars, valueMap, and dataFiles
  
  originalFiles <- dataFiles2
  list(allVars, valueMap, dataFiles, originalFiles)
}
  