
############ Go_dualYplot function ############

Go_dualYplot <- function(df, TaxTab, metaData, project, orders = NULL, Box, Line1, Line2 = NULL, title = NULL, name = NULL, xanlgle = 90, height, width){
  
  if(!is.null(dev.list())) { dev.off() }
  # plot color
  # colorset = "Dark2" # Dark2 Set1 Paired
  Tableau10 = c("#1170aa", "#fc7d0b", "#76B7B2", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BABOAC") 
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out))
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  
  set.seed(151) 
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # Out file
  pdf(sprintf("%s_%s/pdf/4_dualYplot.%s.%s%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(name), "", paste(name, ".", sep = "")), format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  ## Fix factor and numeric
  df$etc <- NULL
  df2 <- read.csv(TaxTab, header = TRUE, row.names = 1, check.names = FALSE);head(df2)
  rownames(df2) <- df2$Species
  df2$Species <- NULL
  rownames(df2) <- gsub(" ", "_", rownames(df2));rownames(df2)
  df2 <- as.data.frame(t(df2))
  
  
  # Make sure the variable type is correct.
  for (var in rownames(subset(metadata, tolower(Go_box) == "yes"))) {
    print(var)
    if (metadata[var, "type"] == "factor") {
      df[, var] <- factor(df[, var])
    } else if (metadata[var, "type"] == "numeric") {
      df[, var] <- as.numeric(as.character(df[, var]))
    }
  }
  
  # Plot
  for (mvar in rownames(subset(metadata, tolower(Go_box) == "yes"))) {
    if (length(unique(df[, mvar])) < 2){ next }
    
    # Merge adiv and taxa table
    adiv <- merge(df, df2, by = "row.names");head(adiv)
    # NA remove
    adiv[, mvar] <- as.character(adiv[, mvar]);adiv[, mvar]
    adiv[, mvar][adiv[, mvar] == ""] <- "NA";adiv[, mvar]
    adiv.no.na <- subset(adiv, adiv[, mvar] != "NA");adiv.no.na[, mvar]  # NA deletion using "subset"
    adiv.no.na[, mvar] <- as.factor(adiv.no.na[, mvar]);adiv.no.na[, mvar]  
    
    print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(adiv.no.na)[1], dim(adiv)[1]))
    
    if (length(unique(adiv.no.na[, mvar])) == 1) { next }
    summary.adiv.no.na <- summary(adiv.no.na[, mvar])
    
    # Re-order
    if (length(orders) >= 1) {
      adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar], levels = orders)
    } else {
      adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar])
    }
    
    #===============================#
    # Visualization for Dual Y axis #
    #===============================#
    
    # For Line1
    mean.line1 <- aggregate(adiv.no.na[, Line1], list(adiv.no.na[, mvar]), FUN = mean)
    colnames(mean.line1) <- c(mvar, Line1);mean.line1
    mean.line1[, Line1] <- mean.line1[, Line1]*10
    
    p <- ggplot(data = adiv.no.na) +
      geom_boxplot(aes(x = !!sym(mvar), y = !!sym(Box), colour = !!sym(mvar)), outlier.shape = NA, show.legend = FALSE) +
      theme_bw() +
      theme(strip.background = element_blank()) +
      #theme_ipsum() +
      theme(text = element_text(size = 9), axis.text.x = element_text(angle = xanlgle, hjust = 1, vjust = 0.5)) +
      # theme(legend.position="none") +
      scale_color_manual(NULL, values = Tableau10) 
    
    p1 <- p + geom_line(data = mean.line1, aes(x = !!sym(mvar), y = !!sym(Line1), group = 1, linetype = ""),
                        inherit.aes = FALSE, color = "#FF9DA7", size = 1) + 
      scale_linetype_manual(NULL, labels = Line1, values = 1) 
    
    
    # For Line2
    if (!is.null(Line2)){
      mean.line1 <- aggregate(adiv.no.na[, Line1], list(adiv.no.na[, mvar]), FUN = mean)
      colnames(mean.line1) <- c(mvar, Line1);mean.line1
      mean.line1[, Line1] <- mean.line1[, Line1]*10
      
      mean.line2 <- aggregate(adiv.no.na[, Line2], list(adiv.no.na[, mvar]), FUN = mean)
      colnames(mean.line2) <- c(mvar, Line2);mean.line1
      mean.line2[, Line2] <- mean.line2[, Line2]*10
      
      mean.line <- merge(mean.line1, mean.line2, by = mvar);head(mean.line)
      mean.line.melt <- melt(mean.line)
      
      p1 <- p + geom_line(data = mean.line.melt, aes(x = !!sym(mvar), y = value, group = variable, color = variable), 
                          inherit.aes = FALSE, size = 1) + 
        scale_linetype_manual(NULL, values = 1)
    }
    
    p1 <- p1 + scale_y_continuous(sec.axis = sec_axis(~.*10, name="Relative abundance (%)")) 
    
    if (!is.null(title)) { p1 <- p1 + ggtitle(title) }
    else{ p1 <- p1 + ggtitle(mvar) }
    print(p1)
  }
  
  dev.off()
}

