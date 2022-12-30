fBuildDBConnection <- function(Settings) {
  
  RMariaDB::dbConnect(RMariaDB::MariaDB(),
                      dbname = Settings$DBName,
                      username = Settings$DBUsername,
                      password = Settings$DBPassword,
                      host = Settings$DBHost)
  
}