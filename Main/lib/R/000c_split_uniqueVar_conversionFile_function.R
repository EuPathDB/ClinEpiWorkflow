# The ontology conversion file "variable" condenses all uniqueVars that map to the same IRI for a study into 1 row.
######## (1 row per unique IRI & multiple uniqueVar per row).

# The split_uniqueVar() function was created to rearrange this conversion file so that each uniqueVar is given its own row. 
######## (multiple rows per unique IRI & 1 row per 1 uniqueVar).

# To run the script:
######## (A) import the conversion file that was created by the ontology team for your study ("conversion_file")
######## (B) conversion_file <- split_uniqueVar(conversion_file)

split_uniqueVar <- function(conversion_file) {
  
  require(stringr)
  
  conversion_file$uniqueVar <- conversion_file$variable

  temp <- conversion_file[grep("[|]", conversion_file$variable),]
  conversion_file <- conversion_file[conversion_file$variable %in% temp==F,]
  
  for(i in unique(temp$variable)){
    print(dim(conversion_file))
    new <- temp[temp$variable==i,]
    new <- new[rep(seq_len(nrow(new)), each=str_count(i, "::")),]
    for(j in 1:str_count(i, "::")){
      new[j,"uniqueVar"] <- unlist(strsplit(i, " [|] "))[j]
    }
    conversion_file <- rbind(conversion_file, new)
  }
  
  return(conversion_file)
}