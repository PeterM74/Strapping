fGetSettings <- function() {
  
  Settings <- list()
  
  # Project information -----
  Settings$ProdMode <- FALSE  # Ensure ProdMode set to TRUE when deploying - this doesn't write to table and prints locally
  
  # DB information -----
  ## Currently stored in Google Cloud SQL services
  load("Data/Credentials.RData")
  Settings$DBName <- "Strapping"
  Settings$DBUsername <- Credentials$Username
  Settings$DBPassword <- Credentials$Password
  Settings$DBHost <- "34.134.41.122"
  Settings$DBTableName <- list(
    "Active" = "ActiveWorkout",
    "History" = "WorkoutHistory",
    "Sessions" = "WorkoutSessions",
    "Types" = "WorkoutTypes"
  )
  rm(Credentials)
  
  
  
  # Aesthetic information -----
  Settings$ColourTheme <- "#673ab7"
  Settings$ColourThemeLight <- "#e1d8f1"
  Settings$TableColNames <- c("Rep", "Kg/Cal", "Complete")
  
  
  
  # Other metadata -----
  ## Anytime Fitness locations for tracking purposes.
  ## Numeric vector of length 3: Long, Lat, acceptable distance between points for geoloc in m
  ## TODO: Dist calc for later: geosphere::distm(x = matrix(c(k$AFLocations$Long, k$AFLocations$Lat),
  ##        byrow = FALSE, ncol = 2), y = rev(c(-33.67405556834981, 150.92179497401867)), fun = distHaversine)
  Settings$AFLocations = tibble::tribble(
      ~Centre, ~Long, ~Lat, ~AcceptableDist,
      "Rouse Hill North", 150.9201, -33.67351, 1000,
      "Riverstone", 150.8603, -33.67693, 1500,
      "St Leonards", 151.1952, -33.82288, 1250,
      "Rouse Hill Town Centre", 150.9265, -33.68969, 1000,
      "Marsden Park", 150.8421, -33.72165, 1500,
      "Richmond", 150.7513, -33.59600, 1500
  )
  
  return(Settings)
  
}
