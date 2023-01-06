fActiveWorkoutTable <- function(User, DBCon, Settings) {
  
  DBCon %>%
    dplyr::tbl(Settings$DBTableName$Active) %>%
    dplyr::filter(UID == User) %>%
    dplyr::collect()
  
}
