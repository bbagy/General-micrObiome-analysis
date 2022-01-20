
############ Go_bdiv function ############

Go_bdiv <- function(psIN, metaData, project, orders, distance_metrics, plot = "PCoA", shapes = NULL, ID = NULL, ellipse = TRUE, facet = NULL, name = NULL, height, width){
  
  if(!is.null(dev.list())) { dev.off() }
  #colorset = "Dark2" # Dark1 Set1 Paired
  Tableau10 = c("#1170aa", "#fc7d0b", "#76B7B2", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BABOAC") 
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) dir.create(out)
  #out_path <- file.path(sprintf("%s_%s/pdf",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out_path)) dir.create(out_path)
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # Logic for out file - merged the 4 possible scenarios (2 facet x 2 name).
  pdf(sprintf("%s_%s/pdf/6_ordi.%s.%s%s(%s).%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(facet), "", paste(facet, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), cutoff, format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  plotlist <- list()
  
  for (mvar in rownames(subset(metadata, tolower(Go_bdiv) == "yes"))) {
    mapping <- data.frame(sample_data(psIN))
    
    if (metadata[mvar, "type"] == "factor") {
      mapping[, mvar] <- factor(mapping[, mvar])
    } else if (metadata[mvar, "type"] == "numeric") {
      mapping[, mvar] <- as.numeric(as.character(mapping[, mvar]))
    }
    
    sample_data(psIN) <- mapping
    
    if (length(facet) >= 1){
      if (facet == mvar){ next }
    } else {}
    
    if (length(shapes) >= 1){
      if (shapes == mvar){ next }
    } else {}
    
    
    for(distance_metric in distance_metrics){
      # Remove na
      mapping.sel <- data.frame(sample_data(psIN))
      mapping.sel[mapping.sel == ""] <- "NA"
      mapping.sel.na <- mapping.sel[!is.na(mapping.sel[, mvar]), ]
      na.count <- length(mapping.sel.na)
      psIN.na <- prune_samples(rownames(mapping.sel[!is.na(mapping.sel[, mvar]), ]), psIN)
      mapping.sel.na.rem <- data.frame(sample_data(psIN.na))
      
      if (!is.null(facet)) {
        print(sprintf("##-- %s-%s (total without NA: %s/%s) --##", facet, mvar, dim(mapping.sel.na.rem)[1], dim(mapping.sel)[1]))
      } else {
        print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(mapping.sel.na.rem)[1], dim(mapping.sel)[1]))
      }
      
      ## Fix factor and numeric
      if (metadata[mvar, "type"] == "factor") {
        mapping.sel.na.rem[, mvar] <- factor(mapping.sel.na.rem[, mvar])
        sample_data(psIN.na) <- mapping.sel.na.rem
      } else if (metadata[mvar, "type"] == "numeric") { next }
      
      
      ord_meths <- plot   # plot %in% c("DCA", "CCA", "RDA", "DPCoA", "NMDS", "PCoA")
      plist <- llply(as.list(ord_meths), function(i, psIN.na, distance_metric){
        ordi <- ordinate(psIN.na, method = i, distance = distance_metric)
        plot_ordination(psIN.na, ordi, type = "samples", color = mvar)
      }, psIN.na, distance_metric)
      
      names(plist) <- ord_meths
      
      pdataframe <- ldply(plist, function(x){
        df = x$data[, 1:2]
        colnames(df) = c("Axis_1", "Axis_2")
        return(cbind(df, x$data))
      })
      
      names(pdataframe)[1] <- "method"
      pdataframe[, facet] <- factor(pdataframe[, facet], levels = orders)
      pdataframe[, mvar] <- factor(pdataframe[, mvar], levels = orders)
      
      # Plots
      p <- ggplot(data = pdataframe, aes_string("Axis_1", "Axis_2", color = mvar))
      
      if (!is.null(shapes)) {
        pdataframe[, shapes] <- factor(pdataframe[, shapes], levels = orders)
        p <- p + geom_point(aes_string(shape = shapes), size = 1.5, alpha = 3) +
          scale_shape_manual(values = c(1, 16, 8, 0, 15, 2, 17, 11, 10, 12, 3, 4, 5, 6, 7, 8, 9, 13, 14)) 
      } else {
        p <- p + geom_point(size = 1.5, alpha = 3)
      }
      
      p <- p + ggtitle(sprintf("%s (%s)", mvar, distance_metric)) +  # Deleted the same in the if/else loop just above
        facet_wrap(~ method, scales = "free") +
        theme_bw() +
        theme(strip.background = element_blank()) +    # open(1), cross(10), closed(2)
        scale_color_manual(values = Tableau10) +
        theme(legend.position = "bottom", legend.title = element_blank(), legend.justification = "left", legend.box = "vertical",
              legend.box.margin = ggplot2::margin(0, 0, 0, -1, "cm"))
      
      # ID argument
      if (!is.null(ID)) {
        p <- p + geom_text_repel(aes_string(label = ID), size = 2)
      } else { p <- p }
      
      # ellipse argument
      if (ellipse) {
        p <- p + stat_ellipse(type = "norm", linetype = 2) 
      } else { p <- p }
      
      
      if (!is.null(facet)) {
        ncol <- length(unique(mapping.sel.na.rem[, facet]))
        p <- p + facet_wrap(as.formula(sprintf("~ %s", facet)), scales = "free_x", ncol = ncol)
      }
      else { p <- p }
      #plotlist[[length(plotlist)+1]] <- p
      print(p)
    }
  }
  
  dev.off()
}


