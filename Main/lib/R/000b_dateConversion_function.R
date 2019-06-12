dateConversion <- function(dateDF, allVarsDF, dataFilesDF) {
  
  for (i in unique(names(dataFilesDF))){
    vars <- dateDF$variable[dateDF$dataFile==i]
    
    for (j in vars){
      if (!is.na(allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]) & 
          allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]=="%d%b%y:%h:%m:%s"){
        formatCode <- allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]
        temp.values <- unique(as.vector(unlist(dataFilesDF[[i]][j])))
        temp.values <- gsub(":00:00:00", "", temp.values)
        temp.values <- sort(as.Date(temp.values, format="%d%b%y"), na.last=T)
        
        if(length(temp.values[!is.na(temp.values)])>0){
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "minValue"] <- 
            as.character(as.Date(min(temp.values[!is.na(temp.values)])))
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "maxValue"] <- 
            as.character(as.Date(max(temp.values[!is.na(temp.values)])))
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "values"] <- 
            paste(allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"minValue"], 
                  allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"maxValue"], sep=" to ")
        }
      }
      
      if (!is.na(allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]) & 
          allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]!="%d%b%y:%h:%m:%s"){
        formatCode <- allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]
        temp.values <- sort(as.Date(unique(as.vector(unlist(dataFilesDF[[i]][j]))), format=formatCode), na.last=T)

        if(length(temp.values[!is.na(temp.values)])>0){
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "minValue"] <- 
            as.character(as.Date(min(temp.values[!is.na(temp.values)])))
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "maxValue"] <- 
            as.character(as.Date(max(temp.values[!is.na(temp.values)])))
          allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "values"] <- 
            paste(allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"minValue"], 
                  allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"maxValue"], sep=" to ")
        }
      }
    }
  }
  return(allVarsDF)
}


dateConversion_DF <- function(dateDF, allVarsDF, dataFilesDF) {
  
  for (i in unique(names(dataFilesDF))){
    vars <- dateDF$variable[dateDF$dataFile==i]
    
    for (j in vars){
      formatCode <- allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]
      temp <- data.frame(values=gsub(":00:00:00", "", as.vector(unlist(dataFilesDF[[i]][j]))),
                         dates=as.Date(gsub(":00:00:00", "", as.vector(unlist(dataFilesDF[[i]][j]))), format=formatCode))
      temp$values <- as.character(temp$values)
      
      if(length(temp[is.na(temp$dates) & !is.na(temp$values) & temp$values!="","dates"])>0){
        temp$dates <- as.character(temp$dates)
        temp[is.na(temp$dates) & !is.na(temp$values) & temp$values!="","dates"] <- temp[is.na(temp$dates) & !is.na(temp$values) & temp$values!="","values"]
        temp[is.na(temp$dates) & !is.na(temp$values) & temp$values=="","dates"] <- ""
      }
      
      dataFilesDF[[i]][j] <- temp$dates
    }
  }
  
  return(dataFilesDF)
}
  
  