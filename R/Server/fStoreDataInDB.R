fStoreDataInDB <- function(DF, DBTable, DBCon, Settings) {
  
  if (Settings$ProdMode) {
    
    DBCon %>%
      RMariaDB::dbAppendTable(name = DBTable, value = DF)
    
  } else {
    
    print(DBTable)
    
  }
  
}
