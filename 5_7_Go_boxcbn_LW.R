
############ Go_boxcbn function ############

Go_boxcbn <- function(df, metaData, project, orders = NULL, outcomes, statistics = TRUE, parametric = FALSE, star = FALSE, title = NULL, facet = NULL, paired = NULL, name = NULL, xanlgle = 90, combination, height, width, plotCols, plotRows){
  
  if(!is.null(dev.list())) { dev.off() }
  # plot color
  # colorset = "Dark2" # Dark2 Set1 Paired
  Tableau10 = c("#1170aa", "#fc7d0b", "#76B7B2", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BABOAC") 
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) dir.create(out)
  #out_path <- file.path(sprintf("%s_%s/pdf",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out_path)) dir.create(out_path)
  
  set.seed(151) 
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  ## Fix factor and numeric
  df$etc <- NULL
  for (var in rownames(subset(metadata, tolower(Go_box) == "yes"))) {
    print(var)
    if (metadata[var, "type"] == "factor") {
      df[, var] <- factor(df[, var])
    } else if (metadata[var, "type"] == "numeric") {
      df[, var] <- as.numeric(as.character(df[, var]))
    }
  }
  
  # Plot
  plotlist <- list()
  for (mvar in rownames(subset(metadata, tolower(Go_box) == "yes"))) {
    if (length(unique(df[, mvar])) < 2){ next }
    
    if (length(facet) >= 1){
      if (facet == mvar){ next }
    } else {}
    
    # Remove NA
    adiv <- data.frame(df)
    #adiv[, mvar] <- as.character(adiv[, mvar]);adiv[,mvar]
    #adiv[,mvar][adiv[,mvar]==""] <- "NA";adiv[,mvar]
    adiv.no.na <- subset(adiv, adiv[,mvar] != "NA");adiv.no.na[,mvar]  # NA deletion using "subset"
    adiv.no.na[, mvar] <- as.factor(adiv.no.na[, mvar]);adiv.no.na[, mvar]  
    print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(adiv.no.na)[1], dim(adiv)[1]))
    
    if (length(unique(adiv.no.na[,mvar])) == 1) { next }
    
    summary.adiv.no.na <- summary(adiv.no.na[,mvar])
    
    # For group combination
    #adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar], levels = orders); adiv.no.na[, mvar]
    #adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar]); adiv.no.na[, mvar]
    if (length(orders) >= 1) {
      adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar], levels = orders)
    } else {
      adiv.no.na[, mvar] <- factor(adiv.no.na[, mvar])
    }
    
    group.cbn <- combn(x = levels(adiv.no.na[,mvar]), m = combination)
    print(count(group.cbn))
    
    group_comparisons <- {}
    for(i in 1:ncol(group.cbn)){
      x <- group.cbn[,i]
      group_comparisons[[i]] <- x
    };group_comparisons
    
    
    print(1)
    for(i in 1:length(group_comparisons)){
      print(group_comparisons[i])
      group.combination <- unlist(group_comparisons[i]);group.combination
      
      if(combination == 2){
        basline <- group.combination[1]
        smvar <- group.combination[2]
        adiv.cbn <- subset(adiv.no.na, adiv.no.na[, mvar] %in% c(basline, smvar)) 
      } else if(combination == 3){
        basline <- group.combination[1]
        smvar1 <- group.combination[2]
        smvar2 <- group.combination[3]
        adiv.cbn <- subset(adiv.no.na, adiv.no.na[, mvar] %in% c(basline, smvar1, smvar2)) 
      } else if(combination == 4){
        basline <- group.combination[1]
        smvar1 <- group.combination[2]
        smvar2 <- group.combination[3]
        smvar3 <- group.combination[4]
        adiv.cbn <- subset(adiv.no.na, adiv.no.na[, mvar] %in% c(basline, smvar1, smvar2, smvar3)) 
      } else {
        print("Combination should be 2, 3, and 4 only.")
        break
      }
      
      unique(adiv.cbn[, mvar])
      
      # Make a combination for stat
      adiv.cbn[, mvar] <- factor(adiv.cbn[, mvar])
      cbn <- combn(x = levels(adiv.cbn[, mvar]), m = 2)
      
      my_comparisons <- {}
      for(i in 1:ncol(cbn)){
        x <- cbn[, i]
        my_comparisons[[i]] <- x
      };my_comparisons
      
      # Check statistics method
      for(oc in outcomes){
        if (statistics){
          if (parametric){ testmethod <- "t.test" }
          else { testmethod <- "wilcox.test" }
        }
        
        # Re-order
        if (length(orders) >= 1) {
          adiv.cbn[,mvar] <- factor(adiv.cbn[,mvar], levels = orders)
        } else {
          adiv.cbn[,mvar] <- factor(adiv.cbn[,mvar])
        }
        
        # Remove NA for facet
        if (length(facet) >= 1) {
          for (fc in facet){
            adiv.cbn[, fc] <- as.character(adiv.cbn[, fc]);adiv.cbn[, fc]
            adiv.cbn[, fc][adiv.cbn[, fc] == ""] <- "NA"
            adiv.cbn.sel <- adiv.cbn[!is.na(adiv.cbn[, fc]), ]
            adiv.cbn <- adiv.cbn.sel 
            # Facet or not
            adiv.cbn[, fc] <- factor(adiv.cbn[, fc], levels = orders)
          }
        }
        
        p1 <- ggplot(adiv.cbn, aes_string(x = mvar, y = oc, colour = mvar)) +
          labs(y = oc, x = NULL) +
          theme_bw() +
          theme(strip.background = element_blank()) +
          theme(text = element_text(size = 9), axis.text.x = element_text(angle = xanlgle, hjust = 1, vjust = 0.5)) +
          # scale_color_brewer(palette=colorset)
          scale_color_manual(values = Tableau10)
        
        # Close an image
        if (!is.null(title)) { p1 <- p1 + ggtitle(title) }
        else { p1 <- p1 + ggtitle(mvar) }
        
        if (statistics){
          if (!star) {
            p1 <- p1 + stat_compare_means(method = testmethod, label = "p.format", comparisons = my_comparisons, size = 2)
          }  else if (star) {
            p1 <- p1 + stat_compare_means(method = testmethod, label = "p.signif", comparisons = my_comparisons, hide.ns = TRUE, size = 3)
          }
        } else if(!statistics){ p1 <- p1 }
        
        # Plot design
        if (height*width <= 6){
          dot.size = 0.7
          box.tickness = 0.3
        } else if (height*width > 6 & height*width < 10){
          dot.size = 1
          box.tickness = 0.4
        } else {
          dot.size = 1.5
          box.tickness = 0.5
        }
        
        # Paired plot type
        if (!is.null(paired)) {
          #p1 = p1 + geom_point(size = 1) 
          p1 <- p1 + geom_line(aes_string(group = paired), color = "grey50", size = 0.3) +
            geom_point(aes_string(shape = paired)) +
            scale_shape_manual(values = c(1, 16, 8, 0, 15, 2, 17, 11, 10, 12, 3, 4, 5, 6, 7, 8, 9, 13, 14)) +
            guides(color = FALSE, size = FALSE) +
            theme(legend.title = element_blank(), legend.position = "bottom", legend.justification = "left",                                       legend.box.margin = ggplot2::margin(0,0,0,-1,"cm")) 
        } else {
          p1 <- p1 + geom_boxplot(aes_string(colour = mvar), outlier.shape = NA, lwd = box.tickness) +
            theme(legend.position = "none")
          
          if (dim(adiv.cbn)[1] < 500){
            p1 <- p1 + geom_jitter(aes_string(colour = mvar), shape = 16, alpha = 0.8, size = dot.size,
                                   position = position_jitter(0.2)) # alpha=0.3
          } else { p1 <- p1 }
        }
        
        # Facet
        if (length(facet) >= 1) {
          facetCol <- length(unique(adiv[, facet]))
          p1 <- p1 + facet_wrap(as.formula(sprintf("~ %s", paste(setdiff(facet, "SocpleType"), collapse = "+"))), scales = "free_x", ncol = facetCol)
        } else { p1 <- p1 }
        
        plotlist[[length(plotlist) + 1]] <- p1 
      }
    }
    
    
    # Out file
    pdf(sprintf("%s_%s/pdf/boxCbn.%s.%s%s(cbn=%s).%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(facet), "", paste(facet, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), combination, format(Sys.Date(), "%y%m%d")), height = height, width = width)
    
    multiplot(plotlist = plotlist, cols = plotCols, rows = plotRows)
    dev.off()
  }
}

