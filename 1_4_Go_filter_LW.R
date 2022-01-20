
############ Go_filter function ############

Go_filter <- function(psIN, cutoff){
  
  # Remove those with 0 ASVs (reads)
  psIN.prune = prune_samples(sample_sums(psIN) > 0, psIN);psIN.prune
  
  x <- sample_sums(psIN.prune) > 0
  cat("#--  Removing column(s) with 0 sum reads   --#\n")
  cat(sprintf("#--  %s column(s) are removed   --#\n", length(x[x == FALSE])))
  cat("\n")
  
  # "relabun" short for relative abundance. The transform_sample_counts() function transforms the sample counts of a taxa abundance matrix according to a function of our interest provided by us.
  phylo_relabun <- transform_sample_counts(physeq = psIN.prune, fun = function(x) x / sum(x))
  phylo_filter_out <- filter_taxa(physeq = phylo_relabun, flist = function(x) mean(x) < cutoff, prune = TRUE) # cutoff = 0.00005
  rmtaxa <- taxa_names(phylo_filter_out)
  alltaxa <- taxa_names(phylo_relabun)
  myTaxa <- alltaxa[alltaxa %notin% rmtaxa]
  phylo_relabun_filtered <- prune_taxa(taxa = myTaxa, x = phylo_relabun)
  ps_filtered <- prune_taxa(taxa = myTaxa, x = psIN.prune)
  
  cat("#--  Before filter  --#\n")
  print(psIN)
  cat("\n")
  cat("#--  After filter   --#\n")
  print(ps_filtered)
  
  #??? prune_taxa(taxa = myTaxa, x = psIN)
  
  # Out dir
  #out <- file.path("2_rds") 
  #if(!file_test("-d", out)) dir.create(out)
  #saveRDS(ps_filtered, sprintf("%s/ps_filtered.%s.(%s).%s.rds", out, project, cutoff, format(Sys.Date(), "%y%m%d")))
  return(ps_filtered)
  #cat("\n")
  #print(sprintf("ps_filtered is saved as 2_rds/ps_filtered.%s.(%s).%s.rds", project, cutoff, format(Sys.Date(), "%y%m%d")))
  
}

