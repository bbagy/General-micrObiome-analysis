
############ Go_emptyMap function ############

Go_emptyMap <- function(psIN, project){
  
  # Out dir
  map_path <- file.path("3_map")
  if(!file_test("-d", map_path)) { dir.create(map_path) }
  
  # Empty map table
  SampleID <- sample_names(psIN)
  StudyID <- sample_names(psIN)
  emptyMap <- data.frame(SampleID, StudyID)
  cat(sprintf("Empty map data is saved in %s.\n", map_path))
  cat("                                                       \n")
  write.csv(emptyMap, quote = FALSE, col.names = NA, row.names = FALSE,
            file = sprintf("%s/emptyMap.%s.%s.csv", map_path, project, format(Sys.Date(), "%y%m%d")))
  
  # Empty metadata table
  column.names <- c("StudyID", "Variation1", "Variation2", "etc")
  col.count <- length(column.names)
  analysis <- c("type",	"baseline",	"Go_barchart", "Go_overview", "Go_box",	"Go_linear", "Go_clme", "Go_reg", "Go_bdiv",	"Go_perm", "Go_mirkat", "Go_deseq2", "Go_ancombc", "Go_lmem", "Confounder")
  row.count <- length(analysis)
  
  emptyMetadata <- data.frame(matrix(ncol = col.count, nrow = row.count))
  colnames(emptyMetadata) <- column.names
  rownames(emptyMetadata) <- analysis
  
  for(an in analysis){
    if(an == "type"){
      emptyMetadata[c(an), ] <- c("", "factor", "numeric", "factor")    # Temporary; later to be manually changed in the file.
    } else if(an == "baseline"){
      emptyMetadata[c(an), ] <- c("", "control", "before", "male")      # Temporary; later to be manually changed in the file.
    } else {
      emptyMetadata[c(an), ] <- c("no", "no", "yes", "yes")             # Temporary; later to be manually changed in the file.
    }
  }
  
  cat(sprintf("Empty metadata is saved in %s.\n", map_path))
  cat("                                                       \n")
  write.csv(emptyMetadata, quote = FALSE, col.names = NA, row.names = TRUE,
            file = sprintf("%s/emptyMetadata.%s.%s.csv", map_path, project, format(Sys.Date(), "%y%m%d")))
} 

