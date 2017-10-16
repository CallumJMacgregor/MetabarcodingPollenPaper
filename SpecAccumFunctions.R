sacplot <- function(x,cols=1) {
  p <- c("vegan")
  packages <- p[!(p %in% installed.packages()[,"Package"])]
  if(length(packages)) {
    stop("Install required packages - ",packages)
  } else {
      lapply(p, require, character.only = TRUE)
  }
  if (nrow(x)>1) {                   # checks if more than threshold number of individuals were sampled
    x <- x[,-c(1:cols)]
  sac <- specaccum(x)
  plot(sac, ci.type="polygon",ci.col="yellow")
  } else {
  warning("No accumulation as only 1 individual sampled")
}}


samplebased <- function(x,cols) {
  p <- c("vegan")
  packages <- p[!(p %in% installed.packages()[,"Package"])]
  if(length(packages)) {
    stop("Install required packages - ",packages)
  } else {
    lapply(p, require, character.only = TRUE)
  }
  x <- x[,-c(1:cols)]
  SR <- estimateR(x)
  return(SR)
}


sitebased <- function(x,cols) {
  p <- c("vegan")
  packages <- p[!(p %in% installed.packages()[,"Package"])]
  if(length(packages)) {
    stop("Install required packages - ",packages)
  } else {
    lapply(p, require, character.only = TRUE)
  }
  x <- x[,-c(1:cols)]
  SR <- t(specpool(x))
  return(SR)
}





interaction.complete <- function(x,cols=0,threshold=0) {
  p <- c("vegan")                                  # list of necessary packages
  packages <- p[!(p %in% installed.packages()[,"Package"])]      # checks each is installed
  
  if(length(packages)) {
    stop("Install required packages - ",packages)                # returns an error saying which packages are missing
    
  } else {
    lapply(p, require, character.only = TRUE)                    # loads up packages if all are installed
  }
  
  if (nrow(x)>threshold) {                   # checks if more than threshold number of individuals were sampled
    x <- x[,-c(1:cols)]
    SR <- t(specpool(x))
    
    
    return(SR)                                        # outputs the data
    
  } else {                                               # if only one or two insect species sampled...
    
    warning("Not enough individuals sampled")         
    
  }
}