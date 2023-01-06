fRemoveEmptyRowFromMatrix <- function(Matrix) {
  
  Matrix[!(rowSums(Matrix == "") == ncol(Matrix)),]
  
}
