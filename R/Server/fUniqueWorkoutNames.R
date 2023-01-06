fUniqueWorkoutNames <- function(User, DBCon, Settings) {
  
  DBCon %>%
    dplyr::tbl(Settings$DBTableName$Types) %>%
    dplyr::filter(UID == User) %>%
    dplyr::distinct(WorkoutType) %>%
    dplyr::collect()
  
}
