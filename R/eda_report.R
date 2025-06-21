#' EDA Report
#'
#' Generates a basic exploratory data analysis for a health dataset.
#'
#' @param data A data frame
#' @export
#' @import ggplot2

eda_report <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  
  cat("📊 Summary Statistics\n")
  print(summary_opensafely(data))
  cat("\n\n")
  
  # Numeric variables
  num_vars <- sapply(data, is.numeric)
  cat("📉 Histograms (numeric variables)\n")
  for (col in names(data)[num_vars]) {
    print(ggplot(data, aes_string(col)) + 
            geom_histogram(fill = "#2c7fb8", bins = 30, color = "white") + 
            labs(title = paste("Histogram of", col)))
  }
  
  # Categorical variables
  cat("📋 Bar Plots (categorical variables)\n")
  cat_vars <- sapply(data, function(x) is.character(x) || is.factor(x))
  for (col in names(data)[cat_vars]) {
    print(ggplot(data, aes_string(col)) + 
            geom_bar(fill = "#41ab5d") +
            labs(title = paste("Bar Plot of", col)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  }
  
  # Correlation matrix
  if (sum(num_vars) >= 2) {
    cat("📈 Correlation Matrix\n")
    print(GGally::ggcorr(data[, num_vars], label = TRUE))
  }
  
  # Missing data summary
  cat("🧩 Missing Data Summary\n")
  missings <- colSums(is.na(data))
  print(missings[missings > 0])
}
