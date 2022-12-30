#' Check if active workout exists in table
#' @return FALSE scalar if no active workout, dataframe of WorkoutType and DateTime if exists.
fCheckForActiveWorkout <- function(User, DBCon, Settings) {
  
  # Load active workout table
  ActiveWorkouts <- DBCon %>%
    dplyr::tbl(Settings$DBTableName$Active) %>%
    dplyr::filter(UID == User) %>%
    dplyr::collect()
  
  if (nrow(ActiveWorkouts) == 0) {
    
    # No active workouts
    return(FALSE)
    
  } else {
    
    # Active workout detected - return information
    ActiveWorkoutSummary <- ActiveWorkouts %>%
      dplyr::left_join(y = DBCon %>%
                         dplyr::tbl(Settings$DBTableName$Sessions) %>%
                         dplyr::filter(UID == User & SessionID %in% ActiveWorkouts$SessionID) %>%
                         dplyr::select(SessionID, DateTime) %>%
                         dplyr::collect(),
                       by = "SessionID") %>%
      dplyr::distinct(WorkoutType, DateTime) %>%
      dplyr::mutate(DateTime = format(DateTime, format = "%y-%m-%d %H:%M"))
    
    return(ActiveWorkoutSummary)
    
  }
  
}
