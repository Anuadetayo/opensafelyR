read_opensafely <- function(filepath) {
  if (grepl("\\.gz$", filepath)) {
    data <- read.csv(gzfile(filepath))
  } else {
    data <- read.csv(filepath)
  }
  return(data)
}