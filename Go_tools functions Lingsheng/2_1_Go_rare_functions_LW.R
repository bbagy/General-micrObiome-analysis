

############ Go_rare function ############

#' Make a rarefaction curve using ggplot2
#' @param physeq_object A phyloseq class object, from which abundance data are extracted
#' @param step Step Size for sample size in rarefaction curves
#' @param label Default `NULL`. Character string. The name of the variable to map to text labels on the plot. Similar to color option but for plotting text.
#' @param color Default `NULL`. Character string. The name of the variable to map to the colors in the plot. This can be a sample variables among the set returned by sample_variables(physeq_object) or taxonomic rank, among the set returned by rank_names(physeq_object)
#' @param plot default `TRUE`. Logical. Should the graph be plotted
#' @param parallel default `FALSE`. Logical. Should rarefaction be parallelized
#' @param se default `TRUE`. Logical. Should standard errors be calculated.
#' @examples
#' good_taxon_table <- data.frame(sum.taxonomy = c("a;b;c;d;f;u", "p;q;r;s;t;u"),
#' site_1 = c(0,1), site_2 = c(10, 20))
#' good_maps <- data.frame(site = c("site_1", "site_2"),
#' season = c("wet", "dry"), host = c("oak", "sage"))
#' physeq_object <- convert_anacapa_to_phyloseq(good_taxon_table, good_maps)
#' ggrare(physeq_object, step = 20, se = TRUE)
#' @export

Go_rare <- function(physeq_object, step = 10, label = NULL, color = NULL, xlimit, plot = TRUE, parallel = FALSE, se = TRUE) {
  
  if(label %notin% c(TRUE, FALSE)) { stop("Please input 'label = TRUE' or 'label = FALSE'.") }
  
  Go_path(project, pdf = TRUE, table = FALSE)
  #out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d"))) 
  #if(!file_test("-d", out)) { dir.create(out) }
  #out_pdf <- file.path(sprintf("%s/pdf", out)) 
  #if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
  
  x <- methods::as(phyloseq::otu_table(physeq_object), "matrix")   # As a matrix, it may be easier to get transposed later.
  if(phyloseq::taxa_are_rows(physeq_object)) { x <- t(x) }         # Taxa/seq should be on the columns
  
  
  ## This script is adapted from vegan `rarecurve` function
  tot <- Matrix::rowSums(x)
  S <- Matrix::rowSums(x > 0)
  nr <- nrow(x)
  
  rarefun <- function(i) {
    cat(sprintf("Rarefying sample %s\n", rownames(x)[i]))   # rownames(x) returns the sample names of the phyloseq object.
    n <- seq(1, tot[i], by = step)
    if(n[length(n)] != tot[i]) { n <- c(n, tot[i]) }
    
    y <- vegan::rarefy(x[i, , drop = FALSE], sample = n, se = se)   # y is a matrix
    
    if(nrow(y) != 1) {
      rownames(y) <- c(".S", ".se")
      return(data.frame(t(y), Size = n, Sample = rownames(x)[i]))    # "Size" and "Sample" are newly created; like mutate().
    } else {
      return(data.frame(.S = y[1, ], Size = n, Sample = rownames(x)[i]))
    }
  }
  
  if(parallel) {
    out_rare <- parallel::mclapply(x = seq_len(nr), FUN = rarefun, mc.preschedule = FALSE)
  } else {
    out_rare <- lapply(X = seq_len(nr), FUN = rarefun)     # lapply() function returns a list object
  }
  
  df <- do.call(what = rbind, args = out_rare)    # Combine the results in the list "out_rare" into one data frame.
  
  # Get sample data
  if(!is.null(phyloseq::sample_data(physeq_object, errorIfNULL = FALSE))) {
    sdf <- methods::as(phyloseq::sample_data(physeq_object), "data.frame")    # sdf: sample data frame
    sdf$Sample <- rownames(sdf)
    data <- merge(df, sdf, by = "Sample")
    labels <- data.frame(x = tot, y = S, Sample = rownames(x))
    labels <- merge(labels, sdf, by = "Sample")
  }
  
  # Add any custom-supplied plot-mapped variables
  if(length(color) > 1) {
    data$color <- color
    names(data)[names(data) == "color"] <- deparse(substitute(color))
    color <- deparse(substitute(color))
  }
  
  #if(length(label) > 1) {
  #  labels$label <- label
  #  names(labels)[names(labels) == "label"] <- deparse(substitute(label))
  #  label <- deparse(substitute(label))
  #}
  
  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_line(ggplot2::aes_string(x = "Size", y = ".S", group = "Sample", color = color)) +
    ggplot2::labs(x = "Sequence Sample Size", y = "Species Richness") +
    ggplot2::xlim(NA, xlimit) +
    theme_classic()
  
  if(label) {
    p <-  p + directlabels::geom_dl(ggplot2::aes_string(x = "Size", y = ".S", label = "Sample"), method = list("last.points", cex = 0.5))  # cex is the font size of the labels
  }
  
  ## Add standard error if available
  if(se) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(x = "Size", y = ".S", ymin = ".S - .se", ymax = ".S + .se", color = NULL, fill = color), alpha = 0.2)
  }
  
  if(plot) { plot(p) }
  invisible(p)
  
  ggsave(plot = p, filename = sprintf("%s_%s/pdf/1_rarefaction1.%s.%s%s%s.pdf", project, format(Sys.Date(), "%y%m%d"), project, ifelse(label, "label.", ""), ifelse(se, "se.", ""), format(Sys.Date(), "%y%m%d")), dpi = 300, height = 5, width = 10)
  
}




calRare <- function(psdata, measures, depths, parallel = FALSE) {
  require('plyr') # ldply
  require('reshape2') # melt
  require('doParallel')
  
  # Set parallel options if required
  if (parallel) {
    paropts  <- list(.packages = c("phyloseq", "reshape2"))
  } else {
    paropts  <- NULL
  }
  
  estimate_rarified_richness <- function(psdata, measures, depth) {
    if(max(sample_sums(psdata)) < depth) { return() }
    
    psdata <- prune_samples(sample_sums(psdata) >= depth, psdata)
    rarified_psdata <- rarefy_even_depth(psdata, depth, verbose = FALSE)
    alpha_diversity <- estimate_richness(rarified_psdata, measures = measures)
    
    # as.matrix forces the use of melt.array, which includes the Sample names (rownames)
    molten_alpha_diversity <- melt(as.matrix(alpha_diversity), varnames = c('Sample', 'Measure'), value.name = 'Alpha_diversity')
    molten_alpha_diversity
  }
  
  names(depths) <- depths # this enables automatic addition of the Depth to the output by ldply
  rarefaction_curve_data <- ldply(depths, estimate_rarified_richness, psdata = psdata, measures = measures, .id = 'Depth', .progress = ifelse(interactive() && ! parallel, 'text', 'none'), .parallel=parallel, .paropts=paropts)
  
  # convert Depth from factor to numeric
  rarefaction_curve_data$Depth <- as.numeric(levels(rarefaction_curve_data$Depth))[rarefaction_curve_data$Depth]
  
  rarefaction_curve_data
}



Go_rare2 <- function(psIN, project, alpha_metrics, color, group, xlimit, plot = TRUE, parallel = FALSE, se = TRUE) {
  ps.rare <- calRare(psIN, alpha_metrics, rep(c(1, 10, 100, 1000, 1:100 * 10000), each = 10))
  summary(ps.rare)
  ps.summary <- ddply(ps.rare, c('Depth', 'Sample', 'Measure'), summarise, 
                      Alpha_diversity_mean = mean(Alpha_diversity), Alpha_diversity_sd = sd(Alpha_diversity))
  
  ps.summary$Sample <-  gsub("X", "", ps.summary$Sample); ps.summary$Sample
  ps.summ.verbose <- data.frame(merge(ps.summary, data.frame(sample_data(psIN)),by.x = 'Sample', by.y = 'row.names'))
  
  sample_variables(psIN)
  ps.summ.verbose$Measure
  
  
  # Plot
  plotlist <- list()
  for (am in alpha_metrics){
    ps.summ.verbose.sel <- subset(ps.summ.verbose, Measure == am)
    p <- ggplot(ps.summ.verbose.sel, mapping = aes_string(x = "Depth", y = "Alpha_diversity_mean", 
                                                          ymin = "Alpha_diversity_mean - Alpha_diversity_sd", 
                                                          ymax = "Alpha_diversity_mean + Alpha_diversity_sd", 
                                                          colour = color, group = group))+
      xlim(NA, xlimit) + theme_light() + geom_line() + #geom_pointrange(size = 0.1) +
      theme(legend.position="right", legend.text=element_text(size=8))+ guides(col = guide_legend(ncol = 2)) +
      ggtitle(sprintf("%s-Rarefaction curve", am )) + labs(x = "Sequence Sample Size", y = am)
    # + facet_wrap(facets = ~ StudyID, scales = 'free_y')
    #print(p)
    #plotlist[[length(plotlist)+1]] <- p 
  }
  if (plot) { plot(p) }
  invisible(p)
  #multiplot(plotlist=plotlist, cols=cols, rows=rows)
  
}

