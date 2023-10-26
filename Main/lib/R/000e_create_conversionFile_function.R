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
######## (B) conversion <- create_conversion(varMap)

# AFTER CREATING THE CONVERSION FILE: Things that will need updating after running the script to make the conversion file:
######## (A) termType = “category” for all parent category rows… update to “multifilter” as needed
######## (B) Only the entities and immediate parent categories are added as rows. 
######## ######## If there is additional structure (ie, another category between the immediate parent category and the 
######## ######## entity), this will need to be manually added 
######## (C) any differences in annotation properties will be separated with a pipe. 
######## ######## after creating the conversion file with the script, open it in excel and use conditional highlighting
######## ######## to find every cell with a pipe and correct annotation properties as necessary



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
  
  if (length(unique(conversion$dataSet[conversion$dataSet != "" & !is.na(conversion$dataSet)]))==1){
    conversion$dataSet <- unique(conversion$dataSet[conversion$dataSet != "" & !is.na(conversion$dataSet)])
  }

  # add rows for parent categories
  
  categories <- conversion[1:length(unique(conversion$parentLabel)),]
  for(i in names(categories)){
    categories[,names(categories)==i] <- ""
  }
  
  categories$label <- unique(conversion$parentLabel)
  categories$dataSet <- paste(unique(conversion$dataSet), collapse=" | ")
  categories$termType <- "category"
  
  for(i in unique(categories$label)){
    categories$IRI[categories$label==i] <- paste(unique(conversion$parentIRI[conversion$parentLabel==i]), collapse=" | ")
    categories$category[categories$label==i] <- paste(unique(conversion$category[conversion$parentLabel==i]), collapse=" | ")
  }
  
  conversion <- rbind(categories, conversion)
  rm(categories)
  
  entities <- conversion[1:length(unique(conversion$category[conversion$category %in% conversion$category[grep("[|]", conversion$category)]==F])),]
  for(i in names(entities)){
    entities[,names(entities)==i] <- ""
  }
  
  entities$label <- unique(unique(conversion$category[conversion$category %in% conversion$category[grep("[|]", conversion$category)]==F]))
  entities$dataSet <- paste(unique(conversion$dataSet), collapse=" | ")
  entities$termType <- "category"
  entities$category <- entities$label
  
  entities$IRI[entities$label=="Community"] <- "http://purl.obolibrary.org/obo/EUPATH_0035127"
  entities$IRI[entities$label=="Household"] <- "http://purl.obolibrary.org/obo/PCO_0000024"
  entities$IRI[entities$label=="Participant"] <- "http://purl.obolibrary.org/obo/EUPATH_0000096"
  entities$IRI[entities$label=="Sample"] <- "http://purl.obolibrary.org/obo/EUPATH_0000609"
  
  for(i in unique(conversion$label)){
    if(i %in% entities$label){
      conversion <- conversion[conversion$label !=i,]
    }
  }
  
  conversion <- rbind(entities, conversion)
  rm(entities)
  
  row.names(conversion) <- 1:nrow(conversion)
  
  return(conversion)
}

