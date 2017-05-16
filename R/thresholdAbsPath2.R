thresholdAbsSPath2 = function (Sabs) {
  
  labelsPath <- list()
  valThres <- Sabs[upper.tri(Sabs)]
  orderValue <- valThres[order(valThres)]
  for (lam in 1:length(orderValue)) {
    lambdaR <- orderValue[lam]
    E <- Sabs
    E[Sabs > lambdaR] <- 1
    E[Sabs < lambdaR] <- 0
    E[Sabs == lambdaR] <- 0
    goutput <- graph.adjacency(E, mode = "undirected", weighted = NULL)
    labelsPath[[lam]] <- clusters(goutput)$membership
  }
  return(list(partitionList = unique(labelsPath), lambdaPath = unique(orderValue)))
}