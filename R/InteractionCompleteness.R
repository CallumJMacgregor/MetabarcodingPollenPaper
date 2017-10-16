species.interaction.completeness <- function(x,cols=1,threshold=1) {
    require("vegan")                                                      # loads up full vegan package as not all necessary functions are bipartite dependencies

  if (nrow(x)>=threshold) {                                               # checks if threshold number of individuals were sampled
    x <- x[,-c(1:cols)]                                                   # removes the first column and any additional descriptive columns as specified
    SR <- t(specpool(x))                                                  # estimates interaction richness based on accumulation of interactions across all individuals
    
    return(SR)                                                            # outputs the data
    
  } else {                                                                # if fewer than the threshold number of  insect species sampled...
    
    warning("Not enough individuals sampled")                             # returns a warning (this is important in the full-network interaction completeness function)
    
  }
}



interaction.completeness <- function(x, cols=1, threshold=1, species.col="Species") {
  dframes <- split(x, list(x[,eval(species.col)]))                                                    # this creates a list of smaller dframes, one for each species
  inter.complete <- lapply(dframes, species.interaction.completeness, cols=cols, threshold=threshold) # estimate interaction richness for each species
  ICmerge <- do.call("cbind", inter.complete)                                                         # merge the data with one species per column
  colnames(ICmerge) <- names(inter.complete)                                                          # assign the species names to each column
  ICmerge <- data.frame(t(ICmerge))                                                                   # make it a dataframe, transposing to put species into rows
  ICinter <- ICmerge[ which( ! ICmerge$Species %in% "Not enough individuals sampled") , ]             # retain only species which were sampled to the threshold
  ICinter$chao <- as.numeric(as.character(ICinter$chao))                                              # make sure several numeric variables are in the right format
  ICinter$Species <- as.numeric(as.character(ICinter$Species))
  ICinter$n <- as.numeric(as.character(ICinter$n))
  ICinter$chao <- ifelse(ICinter$Species==0,0,ICinter$chao)                                           # set the chao column to 0 for species with no observed interactions
  ICinter$completeness <- (ICinter$Species*100)/ICinter$chao                                          # calculate the interaction completeness for each species
  ICgood <- ICinter[ which( ! ICinter$completeness %in% "NaN") , ]                                    # retain only species with observed interactions
  completeness <- weighted.mean(ICgood$completeness, ICgood$chao, na.rm=T)                            # calculate the network completeness, sensu Macgregor et al. 2017
  travgood <- ICgood[ which(ICgood$n >= 10), ]                                                        # retain only species sampled to Traveset's threshold of 10 individuals
  trav.completeness <- mean(travgood$completeness)                                                    # calculate the network completeness, sensu Traveset et al. 2015
  indivs <- nrow(x)                                                                                   # record the total number of individuals in the network
  species <- nrow(ICinter)                                                                            # record the number of species meeting the minimum sampling threshold
  results <- data.frame(cbind(indivs,species,completeness,trav.completeness))                         # bind these together in a dataframe
  colnames(results) <- c("Individuals","Species","Interaction.completeness","IC.sensu.Traveset")      # label items in the dataframe
  
  return(results)
  
}
