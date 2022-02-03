# The split_uniqueStudy() function was created to rearrange conversion files so that each unique dataset/IRI combo is given its own row. 
######## (multiple rows per unique IRI).

# To run the script:
######## (A) import the clinepi.owl conversion file that was created by the ontology ("conversion_file")
######## (B) conversion_file <- split_uniqueStudy(conversion_file)

split_uniqueStudy <- function(conversion_file) {
  
  require(stringr)
  
  conversion_file$UID <- paste(conversion_file$dataSet, conversion_file$IRI, sep="_")
  conversion_file$study <- conversion_file$dataSet
  
  temp <- conversion_file[grep("[|]", conversion_file$dataSet),]
  conversion_file <- conversion_file[conversion_file$UID %in% temp$UID==F,]
  
  for(i in unique(temp$UID)){
    print(dim(conversion_file))
    new <- temp[temp$UID==i,]
    new <- new[rep(seq_len(nrow(new)), each=(str_count(i, "[|]")+1)),]
    for(j in 1:str_count(i, "[|]")+1){
      new[j,"study"] <- unlist(strsplit(new$dataSet[new$UID==i], " [|] "))[j]
    }
    conversion_file <- rbind(conversion_file, new)
  }
  
  conversion_file$dataSet <- conversion_file$study
  conversion_file <- conversion_file[,names(conversion_file) != "study"]
  conversion_file <- conversion_file[,names(conversion_file) != "UID"]
  
  conversion_file$dataSet <- gsub(" [|].+$", "", conversion_file$dataSet)
  
  return(conversion_file)
}