
############ Go_adiv function ############

Go_adiv <- function(psIN, project, alpha_metrics){
  
  # Out dir
  Go_path(project, pdf = FALSE, table = TRUE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_tab <- file.path(sprintf("%s/table", out))
  #if(!file_test("-d", out_tab)) { dir.create(out_tab) }
  out_adiv <- file.path(sprintf("%s_%s/table/adiv", project, format(Sys.Date(), "%y%m%d")))
  if(!file_test("-d", out_adiv)) { dir.create(out_adiv) }
  
  # adiv table
  mapping.sel <- data.frame(sample_data(psIN))
  adiv <- estimate_richness(psIN, measures = alpha_metrics)
  rownames(adiv) <- gsub("^X", "", rownames(adiv))
  adiv$SampleID <- rownames(adiv)
  rownames(adiv) <- rownames(mapping.sel)
  adiv <- merge(adiv, mapping.sel, by = "row.names")
  rownames(adiv) <- adiv$SampleID
  cat(sprintf("adiv table is saved in %s.\n", out_adiv))
  cat("                                                       \n")
  write.csv(adiv, quote = FALSE, col.names = NA, 
            file = sprintf("%s/adiv.%s.%s.csv", out_adiv, project, format(Sys.Date(), "%y%m%d")))
  
  return(adiv)
} 

