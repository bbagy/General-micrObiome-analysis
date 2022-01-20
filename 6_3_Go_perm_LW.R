

############ Go_perm function ############

#' Mar 07 2020
#' adjsted 기능을 추가 하였다.
#' 분석 할때 마다 수치가 조금 변하는 것을 수정 하였다.set.seed(1)
#' dm를 따로 분리 하여 시간을 단축 하였고, dm를 다른 방법으로 분석 할수 있게 되었다.
#' Go_perm()

Go_perm <- function(psIN, metaData, project, distance, distance_metrics, adjust = NULL, des, name = NULL){
  
  # Out dir
  Go_path(project, pdf = FALSE, table = TRUE)
  
  #out <- file.path(sprintf("%s_%s",project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) dir.create(out)
  #out_path <- file.path(sprintf("%s/table",out)) 
  #if(!file_test("-d", out_path)) dir.create(out_path)
  out_perm <- file.path(sprintf("%s_%s/table/perm", project, format(Sys.Date(), "%y%m%d"))) 
  if(!file_test("-d", out_perm)) { dir.create(out_perm) }
  out_distance <- file.path(sprintf("%s_%s/table/distance", project, format(Sys.Date(), "%y%m%d"))) 
  if(!file_test("-d", out_distance)) { dir.create(out_distance) }
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadata <- as.data.frame(t(metadataInput))
  
  # Run
  if (!is.null(des)) {
    # Uni
    print(sprintf("#--- Running Paired-PERMANOVA (%s) ---#", des))
  } else {
    print("#--- Running Paired-PERMANOVA  ---#")
  }
  
  set.seed(1)
  mapping.sel <-data.frame(sample_data(psIN))
  res.pair <-{}
  
  # Run
  for (mvar in rownames(subset(metadata, tolower(Go_perm) == "yes"))) {
    mapping.sel.na <- mapping.sel[!is.na(mapping.sel[, mvar]), ]
    if (length(unique(mapping.sel.na[, mvar])) == 1){
      cat(sprintf("there is no group campare to %s\n", unique(mapping.sel[, mvar])))
      next
    }
    
    for (distance_metric in distance_metrics) {
      psIN.sel <- prune_samples(rownames(mapping.sel[!is.na(mapping.sel.na[, mvar]), ]), psIN)
      
      ## Fix factor and numeric
      if (metadata[mvar, "type"] == "factor") {
        mapping.sel.na[,mvar] <- factor(mapping.sel.na[,mvar])
        sample_data(psIN.sel) <- mapping.sel.na
      } else if (metadata[mvar, "type"] == "numeric") { next }
      
      distance <- Go_dist(psIN = psIN.sel, project = project, distance_metrics = distance_metric)
      
      # pairwise.adonis2
      # pair.ado <- pairwise.adonis2(x=as.dist(distance[[distance_metric]]), factors = mapping.sel.na[,mvar], map=mapping.sel.na, adjust=adjust, mvar=mvar)
      
      x <- as.dist(distance[[distance_metric]])
      factors <- mapping.sel.na[, mvar]
      map <- mapping.sel.na
      
      co <- combn(unique(as.character(map[, mvar])), 2)
      R2 <- c()
      p.value <- c()
      F.Model <- c()
      pairs <- c()
      SumsOfSqs <- c()
      Df <- c()
      
      for(elem in 1:ncol(co)){
        x1 = as.matrix(x)[factors %in% c(as.character(co[1, elem]), as.character(co[2, elem])),
                          factors %in% c(as.character(co[1, elem]), as.character(co[2, elem]))]
        # Run
        map.pair <- subset(map, map[,mvar] %in% c(co[1, elem],co[2, elem]))
        
        if (count(map.pair[, mvar])[1, 2] <= 2 | count(map.pair[, mvar])[2, 2] <= 2){ next }
        
        if (!is.null(adjust)) {
          form <- as.formula(sprintf("x1 ~ %s + %s", mvar, paste(setdiff(adjust, "SampleType"), collapse = "+")))
          print(form)
        } else {
          form <- as.formula(sprintf("x1 ~ %s", mvar))
          print(form)
        }
        
        ad <- adonis2(form, data = map.pair, permutations = 999, by = "terms")     # "terms" "margin" NULL
        
        pairs <- c(pairs, paste(co[1, elem],'vs', co[2, elem]));
        Df <- c(Df, ad[1, 1])
        SumsOfSqs <- c(SumsOfSqs, ad[1, 2])
        R2 <- c(R2, ad[1, 3])
        F.Model <- c(F.Model, ad[1, 4]);
        p.value <- c(p.value, ad[1, 5])
      }
      
      pairw.res <- data.frame(pairs, Df, SumsOfSqs, R2, F.Model, p.value)
      class(pairw.res) <- c("pwadonis", "data.frame")
      
      # End adonis end
      tmp <- as.data.frame(pairw.res)
      tmp$distance_metric <- distance_metric
      tmp$mvar <- mvar
      tmp$adjusted <- paste(setdiff(adjust, "SampleType"), collapse = "+")
      res.pair <- rbind(res.pair, tmp)
    }
  }
  
  res.pair$padj <- p.adjust(res.pair$p.value, method = "bonferroni")
  res.pair <- res.pair[, c("pairs", "Df", "SumsOfSqs", "R2", "F.Model", "p.value", "padj", "distance_metric", "mvar", "adjusted")]
  
  # Output file
  write.csv(res.pair, quote = FALSE, col.names = NA,
            file = sprintf("%s/pair_permanova.%s%s.%s%s%s.csv", out_perm, ifelse(is.null(adjust), "", "adjusted."), project, ifelse(is.null(des), "", paste(des, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = ""))))
  
  return(res.pair)
}

