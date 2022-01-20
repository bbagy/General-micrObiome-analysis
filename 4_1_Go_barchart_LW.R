
############ Go_barchart function ############

Go_barchart <- function(psIN, metaData, project, taxanames, data_type, simple = FALSE, x_label = "SampleIDfactor", facet = NULL,
                        legend = "bottom", orders, cutoff = 0.005, name = NULL, ncol = 11, height, width, plotCols, plotRows){
  
  if(!is.null(dev.list())) { dev.off() }
  if(simple %notin% c(TRUE, FALSE)) { stop("Please input 'simple = TRUE' or 'simple = FALSE'.") }
  
  colorset = "Set1" # Dark1 Set1 Paired
  #taxanames <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
  taxRanks <- taxanames
  
  # Out dir
  Go_path(project, pdf = TRUE, table = TRUE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  #out_tab <- file.path(sprintf("%s/table", out)) 
  #if(!file_test("-d", out_tab)) { dir.create(out_tab) }
  out_taxa <- file.path(sprintf("%s_%s/table/taxa", project, format(Sys.Date(), "%y%m%d"))) 
  if(!file_test("-d", out_taxa)) { dir.create(out_taxa) }
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # Logic for out file - merged the 4 possible scenarios (2 facet x 2 name).
  pdf(sprintf("%s_%s/pdf/3_barchart.%s.%s%s(%s).%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(facet), "", paste(facet, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), cutoff, format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  # Order by bdiv
  ordi <- ordinate(psIN, method = "PCoA", distance = "bray")
  ordering.pc1 <- names(sort(ordi$vectors[, "Axis.1"]))     # Axis.1 is the most accurate among 50 "axes"
  mapping.sel <- data.frame(sample_data(psIN))
  
  plotlist <- list()     # Initialize a list for plots
  
  
  for(i in 1:length(taxanames)){
    # data_type of dada2 or nephele
    if(tolower(data_type) == "dada2"){
      otu.filt <- as.data.frame(t(otu_table(psIN)))
    }
    else if(tolower(data_type) %in% c("nephele", "other")){
      otu.filt <- as.data.frame(otu_table(psIN))
    }
    
    # continue
    otu.filt[, taxanames[i]] <- getTaxonomy(otus = rownames(otu.filt), tax_tab = tax_table(psIN), taxRanks = taxRanks, level = taxanames[i])
    
    if(dim(otu.filt)[2] == 2){ next }
    agg <- aggregate(as.formula(sprintf(". ~ %s" , taxanames[i])), otu.filt, sum, na.action = na.pass)
    
    if(taxanames[i] == "Species"){ agg <- agg[grepl("NA NA", agg$Species) == FALSE, ] }
    
    genera <- agg[, taxanames[i]]
    agg <- agg[, -1]
    agg <- normalizeByCols(agg)
    inds_to_grey <- which(rowMeans(agg) < cutoff)
    genera[inds_to_grey] <- "[1_#Other]"
    agg[,taxanames[i]] <- genera
    #saving table
    agg_other_out <- subset(agg, agg[,taxanames[i]] != "[1_#Other]")
    write.csv(agg_other_out, quote = FALSE, col.names = NA, file = sprintf("%s/%s.taxa_abundance.(%s).%s.%s.csv", out_taxa, project, cutoff, taxanames[i], format(Sys.Date(), "%y%m%d")))
    
    df <- melt(agg, variable = "SampleID")
    
    # Add StduyID
    df2 <- aggregate(as.formula(sprintf("value ~ %s + SampleID" , taxanames[i])), df, sum)
    df2$SampleID <- as.character(df2$SampleID)
    df2$SampleIDfactor <- factor(df2$SampleID, levels=ordering.pc1)
    df.SampleIDstr <- unique(df2[, c("SampleID", "SampleIDfactor")]);head(df.SampleIDstr)
    #mapping.sel[df2$SampleID, "StudyID"]
    
    # Add groups
    for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
      df.SampleIDstr$Group <- as.character(mapping.sel[df.SampleIDstr$SampleID, mvar])
      df2[,mvar] <- mapping.sel[df2$SampleID, mvar]
      
      # Order
      if(length(orders) >= 1) { df2[,mvar] <- factor(df2[,mvar], levels = orders) }
      else { df2[,mvar] <- factor(df2[,mvar]) }
    }
    
    # Adding facet to groups
    if(!is.null(facet)) {
      for (fa in facet){
        df.SampleIDstr$Group <- as.character(mapping.sel[df.SampleIDstr$SampleID, fa])
        df2[, fa] <- mapping.sel[df2$SampleID, fa]
        df2[, fa] <- factor(df2[, fa], levels = orders)
      }
    }
    
    if (x_label %in% c("SampleID", "SampleIDfactor")){ df2 <- df2 }
    else if(length(x_label) >= 1){
      df2[, x_label] <- mapping.sel[df2$SampleID, x_label]
      df2[, x_label] <- factor(df2[, x_label], levels = orders)
    }
    
    print(1)
    # Color
    colourCount = length(unique(df2[, taxanames[i]]));colourCount
    getPalette = colorRampPalette(brewer.pal(9, colorset))
    
    # pdf size height = 5, width = 9
    
    if(legend == "bottom"){
      if (colourCount < 30){ coln <- 4 }
      else if(colourCount > 30){ coln <- 5 }
    }
    else if(legend == "right"){
      if(colourCount < 18){ coln <- 1 }
      else if(colourCount > 19 & colourCount < 35){ coln <- 2 }
      else if(colourCount > 36){ coln <- 3 }
    }
    
    # plot
    # df2 <- df2[order(df2$value, decreasing=T),]
    print(2)
    
    p <- ggplot(data = df2, aes_string(x = x_label, y = "value", fill = taxanames[i], order = taxanames[i])) +
      geom_bar(stat = "identity", position = "stack") +
      theme_classic() +
      labs(fill = NULL) +
      theme(legend.position = legend, legend.text = element_text(size = 8)) +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
      guides(fill = guide_legend(ncol = coln))  + #guides(col = guide_legend(ncol = coln)) + 
      ylim(c(-.1, 1.01)) +
      scale_fill_manual(values = getPalette(colourCount)) +
      labs(y = "Relative abundance")
    
    if(!is.null(facet)){
      for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
        if (class(ncol) == "numeric") { ncol <- ncol }
        else if(length(unique(df2[,mvar])) >= 1){
          ncol <- length(unique(df2[, mvar])) * length(unique(df2[, facet]))
        }
        
        if(facet == mvar) { next }
        df2[, facet] <- factor(df2[, facet], levels = orders)
        
        print(sprintf("Facet by %s-%s", mvar, facet))
        p <- p + facet_wrap(as.formula(sprintf("~ %s + %s", paste(setdiff(facet, "SampleType"), collapse = "+"), mvar)), scales = "free_x", ncol = ncol) 
        
        if(!is.null(name)) {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s-%s (cut off < %s)", mvar, name, cutoff))
        }
        else {
          p = p + ggtitle(sprintf("Taxa barplots overall of %s (cut off < %s)", mvar, cutoff))
        }
        print(p)
      }
    } else if(is.null(facet) & simple == FALSE){
      for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
        if(class(ncol) == "numeric"){ ncol <- ncol }
        else if(length(unique(df2[,mvar])) >= 1){
          ncol <- length(unique(df2[, mvar]))
        }
        print("B")
        print(sprintf("Facet by %s", mvar))
        p <- p + facet_wrap(as.formula(sprintf("~ %s" ,mvar)), scales = "free_x", ncol = ncol)  
        
        if (length(name) == 1) {
          p = p + ggtitle(sprintf("%s barplots overall of %s-%s (cut off < %s)", taxanames[i], mvar, name, cutoff))
        }
        else {
          p = p + ggtitle(sprintf("%s barplots overall of %s (cut off < %s)", taxanames[i], mvar, cutoff))
        }
        #plotlist[[length(plotlist)+1]] <- p
        print(p)
      }
    } else if(is.null(facet) & simple == TRUE) {
      for (mvar in rownames(subset(metadata, tolower(Go_barchart) == "yes"))) {
        if (class(ncol) == "numeric") { ncol <- ncol }
        else if(length(unique(df2[, mvar])) >= 1){
          ncol <- length(unique(df2[, mvar]))
        }
        print("C")
        print("Simpe plot")
        p <- p
        
        if (!is.null(name)) {
          p = p + ggtitle(sprintf("%s barplots overall of %s-%s (cut off < %s)", taxanames[i], mvar, name, cutoff))
        }
        else {
          p = p + ggtitle(sprintf("%s barplots overall of %s (cut off < %s)", taxanames[i], mvar, cutoff))
        }
        #plotlist[[length(plotlist)+1]] <- p
        print(p)
      }
    }
  }
  dev.off()
}

