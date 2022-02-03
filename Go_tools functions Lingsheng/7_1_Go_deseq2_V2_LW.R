
############ Go_deseq2_V2 function ############

Go_deseq2_V2 <- function(psIN, metaData, project, order, type, filter, taxanames, data_type, adjust, des, name, alpha = 0.05){
  
  # Out dir
  Go_path(project, pdf = FALSE, table = TRUE)
  
  #out <- file.path(sprintf("%s_%s",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) dir.create(out)
  #out_path <- file.path(sprintf("%s_%s/table",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out_path)) dir.create(out_path)
  out_deseq2 <- file.path(sprintf("%s_%s/table/deseq2", project, format(Sys.Date(), "%y%m%d"))) 
  if(!file_test("-d", out_deseq2)) { dir.create(out_deseq2) }
  out_deseq2.Tab <- file.path(sprintf("%s/tab", out_deseq2)) 
  if(!file_test("-d", out_deseq2.Tab)) { dir.create(out_deseq2.Tab) }
  out_deseq2.ps <- file.path(sprintf("%s/ps", out_deseq2))
  if(!file_test("-d", out_deseq2.ps)) { dir.create(out_deseq2.ps) }
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # map 정리
  mapping <- data.frame(sample_data(psIN))
  sel <- intersect(rownames(metadata), colnames(mapping)); head(sel, "3")
  metadata.sel <- metadata[sel, , drop = FALSE];head(metadata.sel)
  mapping.sel <- mapping[rownames(mapping), sel, drop = FALSE];head(mapping.sel)
  dim(mapping.sel)
  
  if(!is.null(taxanames)){ psIN <- aggregate_taxa(psIN, taxanames) }
  else { psIN <- psIN }
  
  
  # 최근 버전 for unstrafied (20210112 확인)
  if(type == "function"){
    # remove colume sum 0 and psIN 재구성(20201027)
    a <- data.frame(otu_table(psIN))*10000
    a.ceiling <- ceiling(a[-c(99), ])
    b <- a.ceiling[, -which(numcolwise(sum)(a.ceiling) < 1)]
    if (length(b) == 0){
      OTU.sta <- otu_table(a, taxa_are_rows = TRUE);head(OTU.sta)
      colnames(OTU.sta) <- gsub("X", "", colnames(OTU.sta))
      otu_table(psIN) <- OTU.sta
    } else if(length(b) > 1){
      OTU.sta <- otu_table(b, taxa_are_rows = TRUE);head(OTU.sta)
      colnames(OTU.sta) <- gsub("X", "", colnames(OTU.sta))
      otu_table(psIN) <-  OTU.sta
    }
  } else if(type %in% c("taxanomy", "bacmet")){
    psIN <- psIN
  } #else if(type == "bacmet"){
  #psIN <- psIN
  #}
  
  
  # Start
  res <- {}
  for (mvar in rownames(subset(metadata.sel, tolower(Go_deseq2) == "yes"))) {
    if (length(unique(mapping.sel[, mvar])) == 1) { next }
    
    # Remove NA
    mapping.sel <- data.frame(sample_data(psIN))
    mapping.sel[mapping.sel == ""] <- "NA"
    mapping.sel.no.na <- mapping.sel[!is.na(mapping.sel[,mvar]), ]
    na.count <- length(mapping.sel.no.na)
    psIN.no.na <- prune_samples(rownames(mapping.sel[!is.na(mapping.sel[,mvar]), ]), psIN)
    mapping.sel.no.na.rem <- data.frame(sample_data(psIN.no.na))
    if (length(unique(mapping.sel.no.na.rem[, mvar])) == 1) { next }
    
    if (length(des) == 1) {
      print(sprintf("##-- %s-%s (total without NA: %s/%s) --##", des, mvar, dim(mapping.sel.no.na.rem)[1], dim(mapping.sel)[1]))
    } else {
      print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(mapping.sel.no.na.rem)[1], dim(mapping.sel)[1]))
    }
    
    if (length(mapping.sel.no.na.rem[,mvar]) < 4){
      next
      print(sprintf("%s is removed because length(%s) less than 4", mvar, length(mapping.sel.no.na.rem[,mvar])))
    }
    
    # Integer control
    if (class(mapping.sel.no.na.rem[, mvar]) == "character"){
      mapping.sel.no.na.rem[, mvar] <- factor(mapping.sel.no.na.rem[, mvar])
      sample_data(psIN.no.na) <- mapping.sel.no.na.rem
    }
    if (class(mapping.sel.no.na.rem[, mvar]) %in% c("integer", "numeric")){
      mapping.sel.no.na.rem[, mvar] <- factor(mapping.sel.no.na.rem[, mvar])
      sample_data(psIN.no.na) <- mapping.sel.no.na.rem
    }
    
    # Combination
    mapping.sel[, mvar] <- factor(mapping.sel[, mvar], levels = orders)
    
    mapping.sel[, mvar] <- factor(mapping.sel[, mvar])
    cbn <- combn(x = levels(mapping.sel[, mvar]), m = 2)
    
    my_comparisons <- {}
    for(i in 1:ncol(cbn)){
      x <- cbn[, i]
      my_comparisons[[i]] <- x
    };my_comparisons
    
    
    # Subset sample by combination
    for(i in 1:length(my_comparisons)){
      print(my_comparisons[i])
      combination <- unlist(my_comparisons[i]);combination
      basline <-combination[1]
      smvar <- combination[2]
      
      mapping.sel.cb <- subset(mapping.sel, mapping.sel[[mvar]] %in% c(basline, smvar)) # phyloseq subset does not work.
      psIN.cb <- psIN.no.na
      sample_data(psIN.cb) <- mapping.sel.cb
      
      psIN.cb <- Go_filter(psIN.cb, cutoff = filter) #0.00005
      
      #-- DESeq2 for phyloseq --#
      gm_mean <- function(x, na.rm = TRUE){
        exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
      }
      
      if (length(adjust) >= 1) {
        form <- as.formula(sprintf("~ %s + %s", mvar, paste(setdiff(adjust, "SampleType"), collapse = "+")))
        print(form)
        dds <- phyloseq_to_deseq2(psIN.cb, form)
      } else {
        dds <- phyloseq_to_deseq2(psIN.cb, as.formula(sprintf("~ %s", mvar)))
        print(sprintf("~ %s", mvar))
      }
      
      geoMeans <- apply(counts(dds), 1, gm_mean)
      dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
      dds <- estimateDispersions(dds)
      vst <- getVarianceStabilizedData(dds)
      dds <- DESeq(dds, fitType = "local")
      resultsNames(dds)
      
      # Calculation
      print("pass2")
      tmp <- results(dds, contrast = c(mvar, smvar, basline))
      tmp$taxa <- unlist(lapply(rownames(tmp), function(x) {
        tmp <- unlist(strsplit(x, ";"))
        tmp[length(tmp)]
      }))
      
      tmp$dir <- ifelse(tmp$padj < alpha, ifelse(sign(tmp$log2FoldChange) == 1, "up", "down"), "NS")
      tmp$mvar <- mvar
      tmp$basline <- basline
      tmp$smvar <- smvar
      if (length(des) == 1) { tmp$des <- des }
      
      #-- Give taxa name --#
      res <- cbind(as(tmp, "data.frame"), as(tax_table(psIN)[rownames(tmp), ], "matrix"))
      print("pass3")
      
      if(type == "taxonomy"){
        taxaRanks <- c("Kingdon", "Phylum", "Class", "Order", "Family", "Genus", "Species")
        for(t in 2:length(taxaRanks)){
          
          if (!is.null(taxanames)) {
            if (taxanames == taxaRanks[t-1]){ break }
          }
          
          res[, taxaRanks[t]] == "NA"
          res[, taxaRanks[t]] <- as.character(res[, taxaRanks[t]])
          res[, taxaRanks[t]][is.na(res[, taxaRanks[t]])] <- "__"
          
          for(i in 1:length(res[,taxaRanks[t]])){
            if (res[,taxaRanks[t]][i] == "s__" || res[,taxaRanks[t]][i] == "g__" || res[,taxaRanks[t]][i] == "f__" || res[,taxaRanks[t]][i] == "o__" || res[,taxaRanks[t]][i] == "c__"|| res[,taxaRanks[t]][i] == "p__"|| res[,taxaRanks[t]][i] == "__"){
              res[,taxaRanks[t]][i] <- ""
            }
          }
        }
        
        print("pass4")
        res$TaxaName <- paste(res$Phylum, "", res$Class, "", res$Order, "", res$Family, "", res$Genus, "", res$Species)
        #res$ShortName <- paste(res$Phylum,res$Family," ",res$Genus," ",res$Species)
        
        if (!is.null(taxanames)) {
          
          if (tolower(data_type) == "dada2") {
            if(taxanames == "Species"){
              res$ShortName <- paste(res$Genus, "", res$Species)
            } else {
              res$ShortName <- paste(res[, taxanames], "", res$Species)
            }
          } else if (tolower(data_type) == "nephele") {
            res$ShortName <- paste(res[, taxanames], "", res$Species)
          } else if (tolower(data_type) == "other") {
            res$ShortName <- paste(res[, taxanames])
          }
          
        } else {
          if (tolower(data_type) == "dada2") {
            res$ShortName <- paste(res$Genus, "", res$Species)
          } else if (tolower(data_type) == "nephele") {
            res$ShortName <- paste(res$Genus, "", res$Species)
          } else if (tolower(data_type) == "other") {
            res$ShortName <- paste(res$Species)
          }
        }
        
        
        # Use last taxa name
        for(taxa in c("Family", "Order", "Class", "Phylum")){
          for(i in 1:length(res[, taxa])){
            if (res$ShortName[i] != "  "){ next }
            else if (res$ShortName[i] == "  " & res[, taxa][i] != ""){
              res$ShortName[i] <- paste(res[,taxa][i])
            }
          }
        }
      }
      
      else if(type == "function"){
        for(taxa in c("KO", "KO.des", "Path", "Path.des")){
          res[, taxa] == "NA"
          res[, taxa] <- as.character(res[, taxa])
          res[, taxa][is.na(res[, taxa])] <- "__"
          for(i in 1:length(res[, taxa])){
            if (res[,taxa][i] == "s__" || res[,taxa][i] == "g__" || res[,taxa][i] == "f__" || res[,taxa][i] == "o__" || res[,taxa][i] == "c__"|| res[,taxa][i] == "p__"|| res[,taxa][i] == "__"){
              res[,taxa][i] <- ""
            }
          }
        }
        print("pass4")
        res$KOName <- paste(res$Path, "", res$KO)
        res$ShortName <- paste(res$Path.des, "", res$KO.des)
        
        # Use last taxa name
        for(taxa in c("KO", "KO.des", "Path", "Path.des")){
          for(i in 1:length(res[, taxa])){
            if (res$ShortName[i] != "  "){ next }
            else if (res$ShortName[i] == "  " & res[,taxa][i] != ""){
              res$ShortName[i] <- paste(res[,taxa][i])
            }
          }
        }
      }
      
      else if(type == "bacmet"){
        for(taxa in c("Gene",	"Organism",	"Compound",	"NCBI_annotation")){
          res[, taxa] == "NA"
          res[, taxa]<- as.character(res[, taxa])
          res[, taxa][is.na(res[, taxa])] <- "__"
          for(i in 1:length(res[, taxa])){
            if (res[,taxa][i] == "s__" || res[,taxa][i] == "g__" || res[,taxa][i] == "f__" || res[,taxa][i] == "o__" || res[,taxa][i] == "c__"|| res[,taxa][i] == "p__"|| res[,taxa][i] == "__"){
              res[,taxa][i] <- ""
            }
          }
        }
        print("pass4")
        res$TaxaName <- paste(res$Compound, "", res$Gene, "", res$Organism)
        res$ShortName <- paste(res$Compound, "", res$Gene, "", res$Organism)
      }
      
      
      
      #--- Give simple name to res---#
      headers <- vector(dim(res)[2], mode = "character")
      for (i in 1:dim(res)[1]) {
        headers[i] <- paste("ASV", i, sep = "_")
      }
      res$taxa <- headers
      print("pass5")
      #-- Create table --#
      res <- as.data.frame(res)
      res$padj <- p.adjust(res$pvalue, method = "fdr")
      res$dir <- ifelse(res$padj < alpha, ifelse(sign(res$log2FoldChange) == 1, "up", "down"), "NS")
      
      
      # Get ps object only significant taxa 
      res.sel <- subset(res, !(res$dir  == "NS"));dim(res.sel)[1]
      taxa_sig <- rownames(res.sel)[1:dim(res.sel)[1]]; summary(taxa_sig)
      
      if(dim(res.sel)[1] == 0){ ps.taxa.sig <- psIN.cb }
      else {
        ps.taxa.sig <- prune_taxa(taxa_sig, psIN.cb)
        print(ps.taxa.sig)
      }
      
      
      # Output file
      write.csv(res, quote = FALSE, col.names = NA,
                file = sprintf("%s/(%s.vs.%s).Sig%s.%s.%s%s%s%s.csv", out_deseq2.Tab, baseline, smvar, dim(res.sel)[1], mvar,
                               ifelse(is.null(des), "", paste(des, ".", sep = "")),
                               ifelse(is.null(name), "", paste(name, ".", sep = "")),
                               ifelse(is.null(taxanames), "", paste(taxanames, ".", sep = "")), project))
      saveRDS(ps.taxa.sig, sprintf("%s/(%s.vs.%s).Sig%s.%s.%s%s%s%s.rds", out_deseq2.Tab, baseline, smvar, dim(res.sel)[1], mvar,
                                   ifelse(is.null(des), "", paste(des, ".", sep = "")),
                                   ifelse(is.null(name), "", paste(name, ".", sep = "")),
                                   ifelse(is.null(taxanames), "", paste(taxanames, ".", sep = "")), project))
      
      
    }
  }
}


