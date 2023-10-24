# The ontology conversion file "variable" condenses all unique "variable" terms (column headers from data files) 
# that map to the same IRI for a study into 1 row.
######## (1 row per unique IRI & multiple variable column headers per row).

# The create_conversion() function was created to take the variable map file (which has multiple rows per unique IRI 
# & 1 row per 1 variable column header) and create a conversion file so that each IRI is given ONLY a single row. 
######## (ONE row per unique IRI, with variable and dataFile columns collapsed).

# To run the script:
######## (A) import the variable map file with labels & IRIs (and parent lables & IRIs) filled out
######## ######### NOTE: EVERY ROW IN THE VARIABLE MAP FILE MUST HAVE A LABEL
######## ######### NOTE: IRIs / definitions / all other annotation properties only need to be filled out for 1 row
######## (B) conversion_file <- create_conversion(variable_map)



create_conversion <- function(varMap_file) {
  
  # sort varMap by datafile then by colOrder
  varMap_file$colOrder <- as.numeric(varMap_file$colOrder)
  varMap_file <- varMap_file[order(varMap_file$dataFile, varMap_file$colOrder),]
  
  # set up the dataframe for the conversion file
  conversion <- data.frame(colOrder=integer(),
                           variable=character(),
                           dataFile=character(),
                           IRI=character(),
                           label=character(),
                           definition=character(),
                           category=character(),
                           displayOrder=character(),
                           parentLabel=character(),
                           parentIRI=character(),
                           termType=character(),
                           repeated=character(),
                           is_temporal=character(),
                           mergeKey=character(),
                           dataSet=character(),
                           unitLabel=character(),
                           unitIRI=character(),
                           is_featured=character(),
                           hidden=character(),
                           scale=character(),
                           defaultDisplayRangeMin=double(),
                           defaultDisplayRangeMax=double(),
                           defaultBinWidth=double(),       
                           forceStringType=character(),
                           uniqueVar=character())
  
  # fill in the conversion file
  for(i in unique(varMap_file$label)){
    temp <- conversion[1,]
    temp[,1:ncol(temp)] <- ""
    
    temp$label <- i
    
    for(j in names(temp[names(temp)!="label"])){
      temp[,j] <- paste(unique(varMap_file[varMap_file$label==i,j][varMap_file[varMap_file$label==i,j]!=""]), collapse=" | ")
    }
    
    conversion <- rbind(conversion, temp)
    rm(temp)
  }
  
  return(conversion)
}

