
############ Go_colbarchart function ############

Go_colbarchart <- function(psIN, metaData, project, taxRanks, data_type, x_label, facet, legend, orders, cutoff, name, ncol, height, width,plotCols, plotRows){
  if(!is.null(dev.list())) dev.off()
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) dir.create(out)
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) dir.create(out_pdf)
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  
  # Logic for out file - merged the 4 possible scenarios (2 facet x 2 name).
  pdf(sprintf("%s_%s/pdf/3_barchart.%s.%s%s(%s).%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(facet), "", paste(facet, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), cutoff, format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  ranks <- taxRanks
  taxaname <- ranks
  
  # order by bdiv
  ordi <- ordinate(psIN , method = "PCoA", distance = "bray")
  ordering.pc1 <- names(sort(ordi$vectors[, "Axis.1"]))
  mapping.sel <- data.frame(sample_data(psIN))
  
  plotlist <- list()
  
  for(i in 1:length(taxaname)){
    # dada2 or nephele
    if (tolower(data_type) == "dada2") {
      otu.filt <- as.data.frame(t(otu_table(psIN)))
    }
    else if (tolower(data_type) %in% c("nephele", "other")) {
      otu.filt <- as.data.frame(otu_table(psIN))
    }
    
    # continue
    otu.filt[, taxaname[i]] <- getTaxonomy(otus = rownames(otu.filt), tax_tab = tax_table(psIN), taxRanks = taxRanks, level = taxaname[i])
    otu.filt$PhylumCol <- getTaxonomy(otus = rownames(otu.filt), tax_tab = tax_table(psIN), taxRanks = taxRanks, level="Phylum")
    
    if (dim(otu.filt)[2] == 2){ next }
    
    agg <- aggregate(as.formula(sprintf(". ~ %s + PhylumCol", taxaname[i])), otu.filt, sum, na.action = na.pass)
    genera <- agg[, taxaname[i]]
    PhylumCol <- agg$PhylumCol
    agg[, taxaname[i]] <- NULL
    agg$PhylumCol <- NULL
    
    agg <- normalizeByCols(agg)
    inds_to_grey <- which(rowMeans(agg) < cutoff)
    genera[inds_to_grey] <- "[1_#Other]"
    agg[,taxaname[i]] <- genera
    agg$PhylumCol <- PhylumCol 
    
    if (taxaname[i] == "Phylum"){ agg$Phylum <- genera }
    
    df <- melt(agg, variable.name = "SampleID")
    
    # Add StduyID
    df2 <- aggregate(as.formula(sprintf("value ~ %s + PhylumCol + SampleID" , taxaname[i])), df, sum)
    df2$SampleID <- as.character(df2$SampleID)
    df2$SampleIDfactor <- factor(df2$SampleID, levels = ordering.pc1)
    df.SampleIDstr <- unique(df2[, c("SampleID", "SampleIDfactor")]);head(df.SampleIDstr)
    
    #mapping.sel[df2$SampleID, "StudyID"]
    
    # Add groups
    for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
      df.SampleIDstr$Group <- as.character(mapping.sel[df.SampleIDstr$SampleID, mvar])
      df2[, mvar] <- mapping.sel[df2$SampleID, mvar]
      
      # Order
      if (length(orders) >= 1) { df2[, mvar] <- factor(df2[, mvar], levels = orders) }
      else { df2[, mvar] <- factor(df2[, mvar]) }
    }
    
    # Adding facet to groups
    if (length(facet) == 1) {
      for (fa in facet){
        df.SampleIDstr$Group <- as.character(mapping.sel[df.SampleIDstr$SampleID, fa])
        df2[, fa] <- mapping.sel[df2$SampleID, fa]
      }
    }
    
    if (x_label %in% c("SampleID", "SampleIDfactor")){ df2 <- df2 }
    else if (length(x_label) >= 1){
      df2[, x_label] <- mapping.sel[df2$SampleID, x_label]
      df2[, x_label] <- factor(df2[, x_label], levels = orders)
    }  
    
    print(1)
    
    #------------------------#
    # ---  Color table   --- #
    #------------------------#
    agg$PhylumCol <- PhylumCol 
    agg[,taxaname[i]] <- genera
    
    #-------- remove other from taxa table --------#
    TaxaTab <- agg[order(agg[,taxaname[i]], decreasing = TRUE), ]
    cdf <- data.frame(subset(TaxaTab, select = c("PhylumCol", taxaname[i])))
    cdf.sel <- subset(cdf, cdf[, taxaname[i]] != "[1_#Other]");dim(cdf.sel)[1]
    
    # 몇개인지 결정후 Phylum 으로 정리
    N <- dim(cdf.sel)[1]
    cdf.sel <- cdf.sel[order(cdf.sel$PhylumCol, decreasing = FALSE), ]
    cdf.sel <- data.frame(as.character(cdf.sel$PhylumCol[1:N]), as.character(cdf.sel[, taxaname[i]][1:N]))
    colnames(cdf.sel) <- c("PhylumCol", taxaname[i])
    #cdf.sel[ ,c("Kingdom","Class", "Order", "Family","Genus")] <- list(NULL)
    
    cdf.sel[,taxaname[i]] <- gsub("p__", "", gsub("c__", "", gsub("o__", "", gsub("f__", "", gsub("g__", "", gsub("s__", "", cdf.sel[, taxaname[i]]))))))
    
    # Save species name
    taxaName <- cdf.sel[, taxaname[i]]
    cdf.sel[, taxaname[i]] <- NULL
    
    # -----  create color table   ---- #
    coltab <- Go_color(cdf = cdf.sel, taxaName = taxaName)
    
    # hsv code
    #print(coltab$color_table)
    #coltab$color_table$Phylum
    # color code
    #print(coltab$coloring)
    
    print(2)
    # pdf size height = 5, width = 9
    if (legend == "bottom"){
      if (N < 30) { col <- 5 }
    } else if (legend == "right") {
      if (N < 18) { col <- 1 }
      else if (N > 19 & N < 35) { col <- 2 }
      else if (N > 36) { col <- 3 }
    }
    
    # plot
    # df2 <- df2[order(df2$value, decreasing=T),]
    print(3)
    level <- unique(df2[, taxaname[i]])
    #facet <- "SampleType"
    #mvar <- "TreatmentGroup"
    df2[, facet] <- factor(df2[, facet], levels = orders)
    if (length(facet) == 1) {
      for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
        if (class(ncol) == "numeric") { ncol <- ncol }
        else if(length(unique(df2[, mvar])) >= 1){
          ncol <- length(unique(df2[, mvar])) * length(unique(df2[, facet]))
        }
        if (facet == mvar) { next }
        
        df2[, facet] <- factor(df2[, facet], levels = orders)
        print(4)
        
        p <- ggplot(df2, aes_string(x = x_label, y = "value", fill = factor(df2[, taxaname[i]], levels=level), order = taxaname[i])) +
          geom_bar(stat = "identity", position = "stack") +
          theme_classic() +
          theme(legend.position = legend, legend.text = element_text(size = 8)) +
          theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
          guides(fill = guide_legend(ncol = col)) +
          guides(col = guide_legend(ncol = col)) +
          ylim(c(-.1, 1.01)) +
          scale_fill_manual(values = coltab$coloring) +
          facet_wrap(as.formula(sprintf("~ %s + %s", paste(setdiff(facet, "SampleType"), collapse = "+"), mvar)), scales = "free_x", ncol = ncol) +
          labs(y = "Relative abundance") +
          labs(fill = taxaname[i])
        
        if (length(name) == 1) {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s-%s (cut off < %s)", mvar, name, cutoff))
        }
        else {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s (cut off < %s)", mvar, cutoff))
        }
        
        print(p)
        #plotlist[[length(plotlist)+1]] <- p
      }
    } else if (length(facet) != "NULL") {
      for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
        if (class(ncol) == "numeric") { ncol <- ncol }
        else if(length(unique(df2[, mvar])) >= 1){
          ncol <- length(unique(df2[, mvar]))
        }
        
        p <- ggplot(df2, aes_string(x = x_label, y = "value", fill = factor(df2[, taxaname[i]], levels = level), order = taxaname[i])) +
          geom_bar(stat = "identity", position = "stack") +
          theme_classic() +
          theme(legend.position = legend, legend.text = element_text(size = 8)) +
          theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
          guides(fill = guide_legend(ncol = col)) +
          guides(col = guide_legend(ncol = col)) +
          ylim(c(-.1, 1.01)) +
          scale_fill_manual(values = coltab$coloring) +
          facet_wrap(as.formula(sprintf("~ %s", mvar)), scales = "free_x", ncol = ncol) +
          labs(y = "Relative abundance") +
          labs(fill = taxaname[i])
        
        if (length(name) == 1) {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s-%s (cut off < %s)", mvar, name, cutoff))
        }
        else {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s (cut off < %s)", mvar, cutoff))
        }
        #plotlist[[length(plotlist)+1]] <- p
        print(p)
      }
    }
  }
  #multiplot(plotlist=plotlist, cols=plotCols, rows=plotRows)
  dev.off()
}

