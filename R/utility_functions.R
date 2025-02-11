# General utility functions


#' Seconds to minutes and seconds
#'
#' @description Pretty print convert seconds to minutes and seconds
pretty_print_seconds <- function(x) {
  x <- round(x, 0)
  if(x < 60) {
    etime <- paste0(x, " seconds")
  } else {
    mins <- x / 60
    mins <- round(mins, 0)
    secs <- x %% 60
    etime <- paste0(mins, " minutes ", secs, " seconds")
  }
  return(etime)
}

convert_k_columns <- function(df) {
  # Find all column names that start with "k_"
  k_cols <- grep("^k_", names(df), value = TRUE)
  # Loop over the identified columns and convert them
  for (col in k_cols) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
  return(df)
}
