
############ Go_mergeTab function ############

Go_mergeTab <- function(pattern, file_path){
  
  # Add input files
  path <- file_path
  
  filenames <- list.files(path, pattern = pattern);filenames
  sample.names <- sapply(strsplit(filenames, pattern), `[`, 1) ;sample.names
  filenames <- list.files(path, pattern = pattern);filenames
  
  cat(sprintf("Files location: %s\n", path))
  cat("=======================================================================\n")
  cat("Merged files:\n")
  cat(sprintf("%s\n", filenames))
  
  # Add input files
  df <- {}
  for (sn in sample.names) {
    file <- file.path(path, paste0(sn, pattern))
    df1 <- read.csv(file, row.names = NULL, check.names = FALSE)
    df <- rbind(df, df1)
  }
  return(df)
}

