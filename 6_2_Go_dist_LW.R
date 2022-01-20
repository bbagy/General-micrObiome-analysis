
############ Go_dist function ############

Go_dist <- function(psIN, project, distance_metrics){
  
  # Run distance
  dm <- list()
  for (distance_metric in distance_metrics) {
    dm[[length(dm) + 1]] <- phyloseq::distance(psIN, method = distance_metric)
  }
  
  names(dm) <- distance_metrics
  class(dm)
  
  return(dm)
}

