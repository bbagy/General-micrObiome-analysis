
############ Go_clme function ############

Go_clme <- function(psIN, metaData, project, paired, node, decreasing, height, timepoint, ID, orders, xangle, name, width, plotCols, plotRows){
  
  # Descriptions: Subgroup to the variation to be analyzed
  # paired: patient or same person ID
  # node: Look at the overall pattern of the node and set the node at the highest time point
  # decreasing: Determine whether the pattern is increasing or decreasing in decreasing pattern, determining whether decreasing = true and false, mean and median
  
  if(!is.null(dev.list())) { dev.off() }
  
  alpha_metrics <- c("Chao1", "Shannon")
  # colorset = "Dark2" # Dark1 Set1 Paired
  Tableau10 = c("#1170aa", "#fc7d0b", "#76B7B2", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BABOAC") 
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  pdf(sprintf("%s_%s/pdf/5_clme.%s.%s(%s.%s).%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(name), "", paste(name, ".", sep = "")), node, decreasing, format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  
  # adiv
  adiv <- estimate_richness(psIN, measures = alpha_metrics)
  mapping <- data.frame(sample_data(psIN))
  rownames(adiv) <- gsub("^X", "", rownames(adiv))
  adiv$SampleID <- rownames(adiv)
  rownames(adiv) <- rownames(mapping)
  adiv <- merge(adiv, mapping, by = "row.names")
  rownames(adiv) <- adiv$SampleID
  
  if (length(orders) >= 1) {
    adiv[, timepoint] <- factor(adiv[, timepoint], levels = orders)
  }
  
  # clme
  cons <- list(order = "umbrella", node = node, decreasing = decreasing) 
  print(cons)
  # Look at the overall pattern and set the node at the highest time point.
  # Determine whether the pattern is increasing or decreasing, determining whether decreasing = true and false, determining as mean and median.
  
  
  plotlist <- list()
  
  for (mvar in rownames(subset(metadata, tolower(Go_clme) == "yes"))) {
    print(mvar)
    if (length(unique(adiv[,mvar])) < 2){ next }
    
    # Remove NA
    adiv[, mvar] <- data.frame(adiv[, mvar]);adiv[, mvar]
    #adiv[, mvar][adiv[, mvar] == ""] <- "NA";adiv[, mvar]
    adiv[, mvar] <- as.factor(adiv[, mvar]);adiv[, mvar]
    adiv.no.na <- subset(adiv, adiv[, mvar] != "NA");adiv.no.na[, mvar]    # NA deletion using "subset"
    adiv <- adiv.no.na
    
    if (mvar == timepoint){
      for (am in alpha_metrics){
        form <- as.formula(sprintf("%s ~ %s + (1|%s)", am, timepoint, paired))
        
        clme.mod <- clme(form, data = adiv, constraints = cons, seed = 2, nsim = 1000)
        clme.sum <- summary(clme.mod, seed = 2)
        clme.globalp <- function(model) {
          label <- substitute(
            italic(p) == globalp,
            list(globalp <- model$p.value)
          )
          as.character(as.expression(format(globalp, nsmall = 3))) 
        }
        clme.globalp <- paste("CLME P=", clme.globalp(clme.sum))
        
        # Plot
        p <- ggplot(data = adiv, aes_string(x = timepoint, y = am, color = timepoint, group = paired)) +
          geom_line(color = "grey") +
          geom_point(size = 1.25) + 
          xlab(timepoint) + 
          ylab(sprintf("%s Index\n", am)) +
          ggtitle(sprintf("%s \n (%s) ", mvar, clme.globalp)) +
          scale_color_manual(values = Tableau10) +
          theme_bw() +
          theme(strip.background = element_blank()) +
          theme(title = element_text(size = 8), axis.text.x = element_text(angle = xangle, hjust = 1, vjust = 0.5)) +
          theme(legend.position = "NONE")
        
        if (length(ID) == 1) {
          p = p + geom_text_repel(aes_string(label = ID), size = 2)
        }
        plotlist[[length(plotlist) + 1]] <- p
      }
      
    } else {
      for (des in unique(adiv[, mvar])){
        if(dim(subset(adiv, adiv[, mvar] == des))[1] < 3){ next }
        if(timepoint == mvar){ next }
        print(des)
        for (am in alpha_metrics){
          form <- as.formula(sprintf("%s ~ %s + (1|%s)", am, timepoint, paired))
          
          clme.mod <- clme(form, data = adiv[adiv[, mvar] == des,], constraints = cons, seed = 2, nsim = 1000)
          clme.sum <- summary(clme.mod, seed = 2)
          clme.globalp <- function(model) {
            label <- substitute(
              italic(p) == globalp,
              list(globalp <- model$p.value)
            )
            as.character(as.expression(format(globalp, nsmall = 3))) 
          }
          clme.globalp <- paste("CLME P=", clme.globalp(clme.sum))
          
          # Plot
          p <- ggplot(data = adiv[adiv[, mvar] == des,], aes_string(x = timepoint, y = am, color = timepoint, group = paired)) +
            geom_line(color = "grey") +
            geom_point(size = 1.25) +
            xlab(timepoint) +
            ylab(sprintf("%s Index\n", am)) +
            ggtitle(sprintf("%s-%s \n (%s) ", mvar, des, clme.globalp)) +
            scale_color_brewer(palette = Tableau10) +
            theme_bw() +
            theme(title = element_text(size = 8), axis.text.x = element_text(angle = xangle, hjust = 1, vjust = 0.5)) +
            theme(legend.position = "NONE")
          
          if (length(ID) == 1) {
            p = p + geom_text_repel(aes_string(label = ID), size = 2)
          }
          plotlist[[length(plotlist) + 1]] <- p
        }
      }
    }
    
  }
  multiplot(plotlist=plotlist, cols=plotCols, rows=plotRows)
  dev.off()
}

