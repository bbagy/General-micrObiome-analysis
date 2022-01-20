
############ Install packages by bioconductor ############

# Install package and reads library is combined

`%notin%` <- Negate(`%in%`)

# version 1
if (!requireNamespace(package = "BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

bioconductors <- c("ANCOMBC", "dada2", "DESeq2", "dplyr", "ggpubr", "ggfortify", "ggpmisc", "illuminaio", "msa", "phyloseq", "rstatix", "useful", "DECIPHER", "microbiome")

for (bioconductor in bioconductors){
  if(bioconductor %notin% installed.packages()){
    library(BiocManager)
    BiocManager::install(bioconductor)
  } else {
    library(bioconductor, character.only = TRUE)
  }
}

############ Install packages ############

packages <- c("ape", "car", "cluster", "CLME", "compositions", "cowplot", "crayon", "caret", "colorspace",
              "digest", "data.table", "devtools", "directlabels", "doParallel", "ellipse", "emmeans", "e1071",
              "gplots", "ggplot2", "grid", "gridExtra", "ggrepel", "Hmisc", "huge", "irlba", "igraph", "irr",
              "lme4", "lmerTest", "lubridate", "Matrix", "magrittr", "MASS", "missForest", "nlme",
              "phangorn", "plot3D", "pheatmap", "pkgconfig", "plyr", "parallel", "pscl", "plotly",
              "rfUtilities", "rlang", "randomForest", "readxl", "RColorBrewer", "ROCR", "reshape", "reshape2",
              "stringi", "S4Vectors", "ShortRead", "tidyverse", "vegan", "VGAM")
# version 1
#for (pack in packs){install.packages(sprintf("%s",pack))}
# version 2 (better version)
for (package in packages){
  if(package %notin% installed.packages()){
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}


############ Introduction ############

cat(blue("#--------------------------------------------------------------# \n"))
cat(blue("#------       General analysis Of microbiome (Go)        ------# \n"))
cat(blue("#------    Quick statistics and visualization tools      ------# \n"))
cat(blue("#--------------------------------------------------------------# \n"))
cat(red("                                      Version: Go_tools.2.7.3 \n"))
cat("                                          Written by Heekuk & Lingsheng \n")
cat(yellow("All the required packages were installed.\n"))
cat(yellow("All the required packages were loaded.\n"))
cat(blue("#--------------------------------------------------------------# \n"))


