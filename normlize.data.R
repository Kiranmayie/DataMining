normlize.data <- function(data) {
  output <- matrix(data = NA ,
                   nrow = nrow(data),
                   ncol = ncol(data))
  for (j in 1:ncol(data)) {
    for (i in 1:nrow(data)) {
      x[, i] <- as.numeric(as.character( x[, i] ))
      x[, j] <- as.numeric(as.character( x[, j] ))
      output[i, j] <-
        (x[i, j] - min(x[, j], na.rm = TRUE)) / (max(x[, j], na.rm = TRUE) -
                                                   min(x[, j], na.rm = TRUE))
    }
  }
  return(output)
}
