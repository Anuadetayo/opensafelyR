my_summary <- function(df) {
  cat("Dataset Dimensions:\n")
  cat(nrow(df), "rows,", ncol(df), "columns\n\n")
  
  for (col in names(df)) {
    col_data <- df[[col]]
    col_type <- class(col_data)
    missing <- sum(is.na(col_data))
    missing_pct <- round(100 * missing / length(col_data), 2)
    
    cat("Column:", col, "\n")
    cat(" Type:", col_type, "\n")
    
    if (is.numeric(col_data)) {
      cat(" Mean:", round(mean(col_data, na.rm = TRUE), 2), "\n")
      cat(" Median:", round(median(col_data, na.rm = TRUE), 2), "\n")
      cat(" Min:", round(min(col_data, na.rm = TRUE), 2), "\n")
      cat(" Max:", round(max(col_data, na.rm = TRUE), 2), "\n")
    } else if (is.factor(col_data) || is.character(col_data)) {
      cat(" Unique values:", length(unique(col_data)), "\n")
    }
    
    cat(" Missing values:", missing, "(", missing_pct, "% )\n")
    cat("----\n")
  }
}
