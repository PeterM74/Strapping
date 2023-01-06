fCreateNewWorkoutTableFormat <- function(input, UserID, ExerciseCount, Settings) {
  
  purrr::map_dfr(seq(ExerciseCount),
                 function(ExcCount, input) {
                   
                   CurrentMatrix <- input[[paste0("ExerciseMatrix", ExcCount)]] %>%
                     fRemoveEmptyRowFromMatrix()
                   
                   tibble::tibble(Activity = input[[paste0("ExerciseName", ExcCount)]],
                                  Notes = input[[paste0("ExerciseInfo", ExcCount)]],
                                  Set = CurrentMatrix %>%
                                    nrow() %>% seq(),
                                  SetDetail = CurrentMatrix %>%
                                    {paste(.[,1], .[,2], sep = ";")})
                   
                 }, input = input)  %>%
    
    dplyr::mutate(UID = UserID,
                  WorkoutType = input$NewWorkoutName) %>%
    dplyr::select(UID, WorkoutType, Activity, Set, SetDetail, Notes)
  
}
