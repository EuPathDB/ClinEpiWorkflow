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
        
        allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "minValue"] <- 
          as.character(as.Date(min(temp.values[!is.na(temp.values)])))
        allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "maxValue"] <- 
          as.character(as.Date(max(temp.values[!is.na(temp.values)])))
        allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i, "values"] <- 
          paste(allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"minValue"], 
                allVarsDF[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i,"maxValue"], sep=" to ")
      }
      
      if (!is.na(allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]) & 
          allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]!="%d%b%y:%h:%m:%s"){
        formatCode <- allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]
        temp.values <- sort(as.Date(unique(as.vector(unlist(dataFilesDF[[i]][j]))), format=formatCode), na.last=T)
        
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
  
  return(allVarsDF)
}


dateConversion_DF <- function(dateDF, allVarsDF, dataFilesDF) {
  
  for (i in unique(names(dataFilesDF))){
    vars <- dateDF$variable[dateDF$dataFile==i]
    
    for (j in vars){
      if (!is.na(allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]) & 
          allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]=="%d%b%y:%h:%m:%s"){
        temp.values <- as.vector(unlist(dataFilesDF[[i]][j]))
        temp.values <- gsub(":00:00:00", "", temp.values)
        dataFilesDF[[i]][j] <- as.Date(temp.values, format="%d%b%y")
      }
      
      if (!is.na(allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]) & 
          allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]!="%d%b%y:%h:%m:%s"){
        formatCode <- allVarsDF$formatCode[!is.na(allVarsDF$variable) & allVarsDF$variable==j & allVarsDF$dataFile==i]
        dataFilesDF[[i]][j] <- as.Date(as.vector(unlist(dataFilesDF[[i]][j])), format=formatCode)
      }
    }
  }
  
  return(dataFilesDF)
}
  
  