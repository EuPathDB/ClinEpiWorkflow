pacman::p_load(
    tidyverse,
    plyr,
    readxl)
dataDictionary<-function(path){
  read_excel_allsheets <- function(path, tibble = FALSE) {
    sheets <- readxl::excel_sheets(path)
    x <- lapply(sheets, function(X) readxl::read_excel(path, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x}
  dc<-rbind.fill(read_excel_allsheets(path))%>%
     add_column(Missing_values=NA,
                Format_values=NA) 
}

