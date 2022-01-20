
############ Go_regression function ############

Go_regression <- function(df, metaData, interaction = FALSE, project, orders, outcomes, pvalue = 0.05, des, name = NULL){
  
  # Out dir
  Go_path(project, pdf = FALSE, table = TRUE)
  
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_tab <- file.path(sprintf("%s/table", out))
  #if(!file_test("-d", out_tab)) { dir.create(out_tab) }
  out_reg <- file.path(sprintf("%s_%s/table/regression", project, format(Sys.Date(), "%y%m%d"))) 
  if(!file_test("-d", out_reg)) { dir.create(out_reg) }
  
  # Metadata
  metadataInput <- read.csv(file = metaData, header = TRUE, as.is = TRUE, row.names = 1, check.names = FALSE)
  metadataInput$Chao1 <- c("numeric", rep(c(""), times = dim(metadataInput)[1]-1))
  metadataInput$Shannon <- c("numeric", rep(c(""), times = dim(metadataInput)[1]-1))
  write.csv(metadataInput, quote = FALSE, col.names = NA,  row.names = T, file = metaData)
  metadata <- as.data.frame(t(metadataInput))
  
  # Data control
  # Fix column types
  adiv <- data.frame(df)
  
  for (mvar in rownames(subset(metadata, tolower(Go_reg) == "yes" | tolower(Confounder) == "yes"))) {
    if (metadata[mvar, "type"] == "factor") {
      adiv[, mvar] <- factor(adiv[, mvar])
      if (!(is.na(metadata[mvar, "baseline"])) && metadata[mvar, "baseline"] != "") {
        adiv[, mvar] <- relevel(adiv[, mvar], metadata[mvar, "baseline"])
      }
    } else if (metadata[mvar, "type"] == "numeric") {
      adiv[, mvar] <- as.numeric(as.character(adiv[[mvar]]))
    } else if (metadata[mvar, "type"] == "date") {
      adiv[, mvar] <- as.Date(sprintf("%06d", adiv[, mvar]), format = "%m%d%y")
      adiv[, mvar] <- factor(as.character(adiv[, mvar]), levels = as.character(unique(sort(adiv[, mvar]))))
    }
  }
  
  
  #----------------------------------------------------#
  #--------------    regression model     -------------#
  #----------------------------------------------------#
  
  set.seed(1)
  
  for (outcome in outcomes){
    # Make sure the variable type is correct.
    if (metadata[outcome, "type"] == "factor") {
      adiv[, outcome] <- factor(adiv[, outcome])
      if (!(is.na(metadata[outcome, "baseline"])) && metadata[outcome, "baseline"] != "") {
        adiv[, outcome] <- relevel(adiv[, outcome], metadata[outcome, "baseline"])
      }
    } else if (metadata[outcome, "type"] == "numeric") {
      adiv[, outcome] <- as.numeric(adiv[, outcome])
    } else if (metadata[outcome, "type"] == "date") {
      adiv[, outcome] <- as.Date(sprintf("%06d", adiv[, outcome]), format = "%m%d%y")
      adiv[, outcome] <- factor(as.character(adiv[, outcome]), levels = as.character(unique(sort(adiv[, outcome]))))
    }
    
    res <- {}
    for (mvar in rownames(subset(metadata, tolower(Go_reg) == "yes"))) {
      if (outcome == mvar | outcome == "Chao1" & mvar == "Shannon" | outcome == "Shannon" & mvar == "Chao1") { next }
      
      # Remove NA
      #adiv[, mvar] <- as.character(adiv[[mvar]]);adiv[, mvar]
      #adiv[,mvar][adiv[,mvar]==""] <- "NA";adiv[,mvar]
      #adiv[,mvar]<- as.factor(adiv[,mvar]);adiv[,mvar]
      # adiv.na <- adiv[!(is.na(adiv[,mvar])), ];adiv.na[,mvar] 틀린건 없는 거 같은데 지워지지 않는다. 
      adiv.no.na <- subset(adiv, adiv[, mvar] != "NA");adiv.no.na[, mvar]  # subset 를 사용한 NA 삭제
      print(sprintf("##-- %s (total without NA: %s/%s) --##", mvar, dim(adiv.no.na)[1], dim(adiv)[1]))
      
      if (length(unique(adiv.no.na[, mvar])) == 1) { next }
      
      # Column cleanup
      # Make sure the variable type is correct.
      if (metadata[mvar, "type"] == "factor") {
        adiv[, mvar] <- factor(adiv[, mvar])
        if (metadata[mvar, "baseline"] != "") {
          adiv[, mvar] <- relevel(adiv[, mvar], metadata[mvar, "baseline"])
        } 
      } else if (metadata[mvar, "type"] == "numeric") {
        adiv[, mvar] <- as.numeric(as.character(adiv[, mvar]))
      } else if (metadata[mvar, "type"] == "date") {
        adiv[, mvar] <- as.Date(sprintf("%06d", adiv[, mvar]), format = "%m%d%y")
        adiv[, mvar] <- factor(as.character(adiv[, mvar]), levels = as.character(unique(sort(adiv[, mvar]))))
      }
      
      if (length(rownames(subset(metadata, tolower(Confounder) == "yes"))) >= 1){
        regConfounder <- rownames(subset(metadata, tolower(Confounder) == "yes"))[mvar != rownames(subset(metadata, tolower(Confounder) == "yes"))]
        if (interaction == TRUE){
          mvar.regConfounder <- paste(mvar, "*", regConfounder)
          form <- as.formula(sprintf("%s ~ %s + %s + %s", outcome, mvar, paste(setdiff(regConfounder, "SampleType"), collapse = "+"), paste(setdiff(mvar.regConfounder, "SampleType"), collapse = "+")));form
          #form <- as.formula(sprintf("%s ~ %s", outcome,paste(setdiff(mvar.regConfounder, "SampleType"), collapse="+")));form
        } else {
          form <- as.formula(sprintf("%s ~ %s + %s", outcome, mvar, paste(setdiff(regConfounder, "SampleType"), collapse = "+")))
        }
        print(form)
        print(1)
      } else {
        form <- as.formula(sprintf("%s ~ %s", outcome, mvar))
        print(form)
        print(3)
      }
      
      if (class(adiv[, outcome]) == "numeric"){
        mod <- lm(form, adiv)  # lm or glm or lmer
        m <- "lm"
      } else if (class(adiv[, outcome]) == "factor"){
        mod <- glm(form, adiv[adiv[, outcome] %in% levels(adiv[, outcome]), ], family = binomial(link = 'logit'))
        m <- "glm"
      }
      
      # Out for the model
      coef <- as.data.frame(summary(mod)$coefficients)
      coef <- coef[setdiff(rownames(coef), "(Intercept)"), ,drop = FALSE]
      colnames(coef) <- c("Estimate", "SE", "t", "pval")
      if (dim(coef)[1] == 0){ next }
      
      # Out for the confidence interval 
      conf <- as.data.frame(confint(mod))
      conf <- conf[setdiff(rownames(conf), "(Intercept)"), , drop = FALSE]
      conf.na <- na.omit(conf)
      
      coef$`2.5 %` <- conf.na$`2.5 %`
      coef$`97.5 %` <- conf.na$`97.5 %`
      
      coef$outcome <- outcome
      coef$mvar <- mvar
      coef$model <- m
      
      if (length(rownames(subset(metadata, tolower(Confounder) == "yes"))) >= 1){
        if (interaction == T){
          # coef$multi<- sprintf("%s + %s", paste(setdiff(regConfounder, "SampleType"), collapse="+"), paste(setdiff(mvar.regConfounder, "SampleType"), collapse="+"))
          # ㅇ It shows the same result as the formula below.
          coef$multi<-paste(setdiff(mvar.regConfounder, "SampleType"), collapse = "+")
          type <-"multi_interaction"
        } else {
          coef$multi <- paste(setdiff(rownames(subset(metadata, tolower(Confounder) == "yes")), "SampleType"), collapse = "+")
          type <-"multi"
        }
      } else { type <-"uni" }
      
      res <- rbind(res, coef)
    }
    
    if (length(des) == 1) { res$des <- des }
    res$padj <- p.adjust(res$pval, method = "fdr")
    #res <- res[order(res$time_point),]
    res$comp <- factor(rownames(res), levels = rownames(res))
    res$dir <- ifelse(res$pval < pvalue, ifelse(sign(res$Estimate) == 1, "up", "down"), "NS")
    
    write.csv(res, quote = FALSE, col.names = NA,
              file = sprintf("%s/regression_%s.%s.%s%s%s.%s.csv", out_reg, project, outcome, ifelse(is.null(des), "", paste(des, ".", sep = "")), ifelse(is.null(name), "", paste(name, ".", sep = "")), type, format(Sys.Date(), "%y%m%d")))
    
  }
}

