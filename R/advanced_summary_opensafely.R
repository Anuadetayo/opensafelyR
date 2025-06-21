#' Advanced Statistical Summary for OpenSAFELY-like Datasets
#'
#' @param data A data frame
#' @return A list with detailed summaries
#' @export
advanced_summary_opensafely <- function(data) {
  require(dplyr)
  require(psych)
  require(e1071)
  
  numeric_cols <- sapply(data, is.numeric)
  categorical_cols <- sapply(data, is.character)
  
  summary_list <- list()
  
  # Central tendency, dispersion, shape
  summary_list$numerical_summary <- lapply(data[, numeric_cols], function(x) {
    stats <- list(
      Mean = mean(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      Mode = as.numeric(names(sort(table(x), decreasing = TRUE))[1]),
      SD = sd(x, na.rm = TRUE),
      Variance = var(x, na.rm = TRUE),
      Range = range(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE),
      Skewness = e1071::skewness(x, na.rm = TRUE),
      Kurtosis = e1071::kurtosis(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Q2 = quantile(x, 0.5, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Missing = sum(is.na(x)),
      Missing_Percent = round(mean(is.na(x)) * 100, 2)
    )
    return(stats)
  })
  
  # Categorical summaries
  summary_list$categorical_summary <- lapply(data[, categorical_cols], function(x) {
    tbl <- table(x, useNA = "ifany")
    prop <- prop.table(tbl)
    list(
      Count = tbl,
      Proportion = round(100 * prop, 2)
    )
  })
  
  # Outlier detection (IQR method)
  summary_list$outliers <- lapply(data[, numeric_cols], function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    outlier_count <- sum(x < lower | x > upper, na.rm = TRUE)
    return(outlier_count)
  })
  
  # Correlation matrix
  if (sum(numeric_cols) > 1) {
    summary_list$correlation <- cor(data[, numeric_cols], use = "pairwise.complete.obs", method = "pearson")
  }
  
  return(summary_list)
}
