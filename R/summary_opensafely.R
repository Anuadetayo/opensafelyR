#' Print full summary of health dataset in console
#'
#' @param data A data frame
#' @export
summary_opensafely <- function(data) {
  require(dplyr)
  require(e1071)
  
  numeric_cols <- sapply(data, is.numeric)
  categorical_cols <- sapply(data, is.character)
  
  cat("📊 Summary of Numeric Variables\n")
  cat("-----------------------------\n\n")
  
  for (col in names(data)[numeric_cols]) {
    x <- data[[col]]
    cat("🔹", col, "\n")
    cat("   Mean:", mean(x, na.rm = TRUE), "\n")
    cat("   Median:", median(x, na.rm = TRUE), "\n")
    cat("   Mode:", as.numeric(names(sort(table(x), decreasing = TRUE))[1]), "\n")
    cat("   SD:", sd(x, na.rm = TRUE), "\n")
    cat("   Variance:", var(x, na.rm = TRUE), "\n")
    cat("   Range:", paste(range(x, na.rm = TRUE), collapse = " - "), "\n")
    cat("   IQR:", IQR(x, na.rm = TRUE), "\n")
    cat("   Skewness:", e1071::skewness(x, na.rm = TRUE), "\n")
    cat("   Kurtosis:", e1071::kurtosis(x, na.rm = TRUE), "\n")
    cat("   Missing:", sum(is.na(x)), "(", round(mean(is.na(x)) * 100, 2), "%)\n")
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
    cat("   Outliers:", outliers, "\n\n")
  }
  
  cat("\n📋 Summary of Categorical Variables\n")
  cat("----------------------------------\n\n")
  
  for (col in names(data)[categorical_cols]) {
    x <- data[[col]]
    cat("🔹", col, "\n")
    freq <- table(x, useNA = "ifany")
    prop <- round(prop.table(freq) * 100, 2)
    print(data.frame(Value = names(freq), Count = as.vector(freq), Percent = prop))
    cat("   Missing:", sum(is.na(x)), "(", round(mean(is.na(x)) * 100, 2), "%)\n\n")
  }
  
  if (sum(numeric_cols) > 1) {
    cat("\n🔗 Correlation Matrix\n")
    cat("---------------------\n\n")
    print(round(cor(data[, numeric_cols], use = "pairwise.complete.obs"), 2))
  } else {
    cat("\n(No numeric correlation matrix - only one numeric column present.)\n")
  }
  
  invisible(NULL)
}
