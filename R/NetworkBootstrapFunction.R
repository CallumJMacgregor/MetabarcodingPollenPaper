bootstrap_network <- function(x,y, repeats) {
  networks <- swap.web(repeats,x)
  data.list <- lapply(networks, networklevel, 
                      weighted = TRUE,
                      index = c("linkage density",
                                "weighted connectance",
                                "connectance",
                                "generality",
                                "vulnerability",
                                "H2",
                                "robustness"))
  data <- data.frame(do.call("cbind",data.list))
  means <- data.frame(rowMeans(data))
  CIs <- data.frame(t(apply(as.matrix(data), 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})))
  results <- cbind(y, means,CIs)
  colnames(results) <- c("True value","Bootstrap mean","LCI","UCI")
  print("N.b. edge sums and network connectance are held constant for each permutation")
  return(results)
}





bootstrap_robustness <- function(x, repeats) {
data <- data.frame(c("robustness.HL,robustness.LL"))
  for (i in 1:repeats){
  robust <- data.frame(networklevel(x, weighted=TRUE, index = "robustness"))
  data <- cbind(data,robust)
  }  
  mean <- data.frame(rowMeans(data[1,2:length(data)]))
  lbound <- repeats*0.025
  ubound <- repeats*0.975
  robust.HL <- as.matrix(data[1,2:length(data)])
  LCI <- sort(robust.HL)[lbound]
  UCI <- sort(robust.HL)[ubound]
  results <- cbind(mean,LCI,UCI)
  colnames(results) <- c("Bootstrap mean","LCI","UCI")
  return(results)
}


