
############ Go_linear function ############

Go_linear <- function(df, metaData, project, outcomes, maingroup, orders, name = NULL, height, width, plotCols, plotRows){
  
  if(!is.null(dev.list())) { dev.off() }
  
  colorset = "Tableau10" # Dark1 Set1 Dark2 Tableau10
  Tableau10 = c("#1170aa", "#fc7d0b", "#76B7B2", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BABOAC") 
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # Out file
  pdf(sprintf("%s_%s/pdf/4_linear.%s.%s%s%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(maingroup), "", paste(maingroup, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  my.formula <- y ~ x
  my.method <- "lm"
  
  # Plot
  plotlist <- list()
  
  for (mvar in rownames(subset(metadata, tolower(Go_linear) == "yes" & type == "numeric"))) {
    if (mvar == maingroup){ next }
    
    # Remove NA
    df[, mvar] <- as.numeric(as.character(df[[mvar]]))   # Make it numeric
    #df[, mvar][df[, mvar] == ""] <- "NA";df[, mvar]
    #df[, mvar] <- as.numeric(df[, mvar]);df[, mvar]
    df.no.na <- subset(df, df[, mvar] != "NA");df.no.na[, mvar]  # NA deletion using "subset"
    
    print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(df.no.na)[1], dim(df)[1]))
    if (length(unique(df.no.na[, mvar])) == 1) { next }
    summary.df.no.na <- summary(df.no.na[, mvar])
    
    # Remove NA in the maingroup
    if (!is.null(maingroup)) {
      df.no.na[, maingroup] <- as.character(df.no.na[, maingroup]);df.no.na[,maingroup]
      df.no.na[,maingroup][df.no.na[,maingroup]==""] <- "NA";df.no.na[,maingroup]
      df.no.na[, maingroup]<- as.factor(df.no.na[, maingroup]);df.no.na[, maingroup]
      df.no.na.na <- subset(df.no.na, df.no.na[, maingroup] != "NA");df.no.na.na[, maingroup]
      df.no.na.na[, maingroup] <- factor(df.no.na.na[, maingroup], levels = orders);df.no.na.na[, maingroup]
    }
    
    for(i in 1:length(outcomes)){
      # mvar may not include "Chao1" or "Shannon" because their types may be "" instead of "numeric"
      if (outcomes[i] == mvar | outcomes[i] == "Chao1" & mvar == "Shannon" | outcomes[i] == "Shannon" & mvar == "Chao1") {
        print(sprintf("Stop function bacause out was %s and mvar was %s", outcomes[i], mvar))
        next
      }
      
      print(outcomes[i])
      
      if (!is.null(maingroup)) {
        p <- ggplot(data = df.no.na.na, aes_string(x = mvar, y = outcomes[i], group = maingroup, color = maingroup, linetype = maingroup))
      } else {
        p <- ggplot(data = df.no.na, aes_string(x = mvar, y = outcomes[i]))
      }
      
      p <- p + theme_classic() +
        geom_point(size = 0.5) + 
        # scale_colour_brewer(palette = colorset) + 
        scale_color_manual(values = Tableau10) +
        geom_smooth(method = my.method, formula = my.formula, linetype = "solid", fill = "lightgrey", se = T, size = 0.5) + 
        ggtitle(sprintf("%s with %s", mvar, outcomes[i])) +
        labs(x = NULL) +
        theme(title = element_text(size = 10),
              axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain")) +
        #stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 3) +
        stat_fit_glance(method.args = list(formula = my.formula), method = my.method, 
                        #geom = 'text', the formula is not arranged to one side, and a number is attached to the line.
                        aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g', 
                                            stat(r.squared), stat(p.value))),
                        parse = TRUE, size = 3)
      
      plotlist[[length(plotlist) + 1]] <- p
    }
  }
  
  multiplot(plotlist = plotlist, cols = plotCols, rows = plotRows)
  dev.off()
}

