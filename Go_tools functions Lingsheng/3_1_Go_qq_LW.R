
############ Go_qq function ############

Go_qq <- function(psIN, project, alpha_metrics, name, height, width){
  
  if(!is.null(dev.list())) { dev.off() }
  
  # Out dir
  Go_path(project, pdf = TRUE, table = FALSE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  
  # Logic of out file
  pdf(sprintf("%s_%s/pdf/2_QQ.%s.%s%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(is.null(name), "", paste(name, ".", sep = "")), format(Sys.Date(), "%y%m%d")), height = height, width = width)
  
  # 1st adiv table
  mapping.sel <- data.frame(sample_data(psIN))
  adiv <- estimate_richness(psIN, measures = alpha_metrics)
  rownames(adiv) <- gsub("^X", "", rownames(adiv))
  adiv$SampleID <- rownames(adiv)
  rownames(adiv) <- rownames(mapping.sel)
  adiv <- merge(adiv, mapping.sel, by = "row.names")
  rownames(adiv) <- adiv$SampleID
  adiv$ShannonLn <- log(adiv$Shannon)
  # Show last column name; in this case "ShannonLn"
  rev(names(adiv))[1]
  
  #----------- QQ plot and histogram -----------#
  par(mfrow = c(3,2))    # format c(nr, nc). Subsequent figures will be drawn in an nr-by-nc array on the device by rows (mfrow)
  mes <- c(alpha_metrics, rev(names(adiv))[1])
  for (am in mes){
    test <- shapiro.test(adiv[, am])
    hist(adiv[, am], freq = FALSE, xlab = am, main = sprintf("Histogram of %s (%s)", project, am), cex.main = 1) 
    lines(density(adiv[, am])) 
    rug(adiv[, am])
    adiv.inf <- adiv[!is.infinite(adiv[, am]), ]    # Remove infinite numeric values.
    qqnorm(adiv.inf[, am], main = sprintf("Normal Q-Q Plot (%s p = %.2g)", "Shapiro", test$p.value), cex.main = 1)
    qqline(adiv.inf[, am])
    print(sprintf("%s %s Shapiro test (p = %.2g)", project, am, test$p.value))
  }
  dev.off()
}

