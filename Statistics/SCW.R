SCw1 <- function(x, estimator="Chao1", cols=0) {
  require("vegan")
  
  if (cols!=0){
    x <- x[,-c(1:cols)]
  }
  
  inter.complete <- estimateR(x)                                                                      # estimate interaction richness for each species
  IC <- data.frame(t(inter.complete))                                                                 # put values into a dataframe, transposing species into rows
  IC$S.obs <- as.numeric(as.character(IC$S.obs))                                                      # make sure necessary numeric variables are in the right format
  IC$S.chao1 <- as.numeric(as.character(IC$S.chao1))
  IC$S.ACE <- as.numeric(as.character(IC$S.ACE))
  IC$S.ACE <- ifelse((IC$S.ACE %in% "NaN"),IC$S.obs,IC$S.ACE)
  results <- data.frame(nrow(IC))                                                                     # record the number of species meeting the minimum sampling threshold
  colnames(results) <- "Species"
  
  if ("Chao1" %in% estimator){                                                                        # if user asks for Chao1 estimator...
    IC$completeness <- (IC$S.obs*100)/IC$S.chao1                                                      # calculate the interaction completeness for each species
    ICgood <- IC[ which( ! IC$completeness %in% "NaN") , ]                                            # retain only species with observed interactions
    completeness <- weighted.mean(ICgood$completeness, ICgood$S.chao1, na.rm=T)                       # calculate the network completeness, sensu Macgregor et al. 2017
    results$Chao1.completeness <- completeness                                                        # bind these together in a dataframe with the name of the estimator used
  }
  
  if ("ACE" %in% estimator){                                                                          # if user asks for ACE estimator...
    IC$completeness <- (IC$S.obs*100)/IC$S.ACE                                                        # calculate the interaction completeness for each species
    ICgood <- IC[ which( ! IC$completeness %in% "NaN") , ]                                            # retain only species with observed interactions
    completeness <- weighted.mean(ICgood$completeness, ICgood$S.ACE, na.rm=T)                         # calculate the network completeness, sensu Macgregor et al. 2017
    results$ACE.completeness <- completeness                                                          # bind these together in a dataframe with the name of the estimator used
  }  
  
  if ((! "Chao1" %in% estimator) & (! "ACE" %in% estimator)){                                         # if user asks for neither estimator...
    print("No valid estimator selected - choose Chao1 and/or ACE")
  }
  
  return(results)
    
}
  
  



species.interaction.completeness <- function(x,cols=1,threshold=1) {      # a function that intercomp2 depends on. Not intended for direct calling.
    require("vegan")                                                      # loads up full vegan package as not all necessary functions are bipartite dependencies

  if (nrow(x)>=threshold) {                                               # checks if threshold number of individuals were sampled
    x <- x[,-c(1:cols)]                                                   # removes the first column and any additional descriptive columns as specified
    SR <- t(specpool(x))                                                  # estimates interaction richness based on accumulation of interactions across all individuals
    
    return(SR)                                                            # outputs the data
    
  } else {                                                                # if fewer than the threshold number of  insect species sampled...
    
    warning("Not enough individuals sampled")                             # returns a warning (this is important in the full-network interaction completeness function)
    
  }
}



SCw2 <- function(x, cols=1, threshold=1, species.col="Species", estimator="Chao2", Traveset=FALSE) {
  if (missing(cols)) {
    factors <- numeric()
    for (y in (2:length(x))){
      if (!is.numeric(x[,y])){
        factors <- append(factors,y)
      }
    }
    if (length(factors)==0) {
      print("Argument `cols` left as default (1); check this is correct or sampling completeness may be estimated incorrectly!")
    } else {
      cat("Columns ",factors," are not numeric; use the 'cols' argument to ignore extra variables or sampling completeness will be estimated incorrectly.\n")  
    }
  }
  dframes <- split(x, list(x[,eval(species.col)]))                                                    # this creates a list of smaller dframes, one for each species
  inter.complete <- lapply(dframes, species.interaction.completeness, cols=cols, threshold=threshold) # estimate interaction richness for each species
  ICmerge <- do.call("cbind", inter.complete)                                                         # merge the data with one species per column
  colnames(ICmerge) <- names(inter.complete)                                                          # assign the species names to each column
  ICmerge <- data.frame(t(ICmerge))                                                                   # make it a dataframe, transposing to put species into rows
  ICinter <- ICmerge[ which( ! ICmerge$Species %in% "Not enough individuals sampled") , ]             # retain only species which were sampled to the threshold
  ICinter$chao <- as.numeric(as.character(ICinter$chao))                                              # make sure necessary numeric variables are in the right format
  ICinter$Species <- as.numeric(as.character(ICinter$Species))
  ICinter$n <- as.numeric(as.character(ICinter$n))
  ICinter$jack1 <- as.numeric(as.character(ICinter$jack1))
  ICinter$jack2 <- as.numeric(as.character(ICinter$jack2))
  ICinter$boot <- as.numeric(as.character(ICinter$boot))
  ICgood <- ICinter[ which(ICinter$Species!=0) , ]                                                    # retain only species with observed interactions
  indivs <- sum(ICinter$n)                                                                            # record the total number of individuals of species meeting the minimum sampling threshold
  species <- nrow(ICinter)                                                                            # record the number of species meeting the minimum sampling threshold
  results <- data.frame(cbind(indivs,species))                                                        # bind these together in a dataframe
  colnames(results) <- c("Individuals","Species")                                                     # label items in the dataframe

  if (Traveset){                                                                                      # if user asks for Traveset approach...
    travgood <- ICgood[ which(ICgood$n >= 10), ]                                                      # retain only species sampled to Traveset's threshold of 10 individuals
    travgood$completeness <- (travgood$Species*100)/travgood$chao                                     # calculate the interaction completeness for each species
    trav.completeness <- mean(travgood$completeness)                                                  # calculate the network completeness, sensu Traveset et al. 2015
    results$IC.sensu.Traveset <- trav.completeness
  }
    
  if ("Chao2" %in% estimator){                                                                        # if user asks for Chao2 estimator...
    ICgood$completeness <- (ICgood$Species*100)/ICgood$chao                                           # calculate the interaction completeness for each species
    chao.completeness <- weighted.mean(ICgood$completeness, ICgood$chao, na.rm=T)                     # calculate the weighted network completeness, sensu Macgregor et al. 2017
    results$Chao2.completeness <- chao.completeness                                                   # bind this to the results dataframe
  }
  
  if ("Jack1" %in% estimator){                                                                        # if user asks for first-order jackknife estimator...
    ICgood$completeness <- (ICgood$Species*100)/ICgood$jack1                                          # calculate the interaction completeness for each species
    jack1.completeness <- weighted.mean(ICgood$completeness, ICgood$jack1, na.rm=T)                   # calculate the weighted network completeness, sensu Macgregor et al. 2017
    results$Jack1.completeness <- jack1.completeness                                                  # bind this to the results dataframe
  }
  
  if ("Jack2" %in% estimator){                                                                        # if user asks for second-order jackknife estimator...
    ICgood$completeness <- (ICgood$Species*100)/ICgood$jack2                                          # calculate the interaction completeness for each species
    jack2.completeness <- weighted.mean(ICgood$completeness, ICgood$jack2, na.rm=T)                   # calculate the weighted network completeness, sensu Macgregor et al. 2017
    results$Jack2.completeness <- jack2.completeness                                                  # bind this to the results dataframe
  }
  
  if ("Boot" %in% estimator){                                                                         # if user asks for bootstrap estimator...
    ICgood$completeness <- (ICgood$Species*100)/ICgood$boot                                           # calculate the interaction completeness for each species
    boot.completeness <- weighted.mean(ICgood$completeness, ICgood$boot, na.rm=T)                     # calculate the weighted network completeness, sensu Macgregor et al. 2017
    results$Boot.completeness <- boot.completeness                                                    # bind this to the results dataframe
  }
  
  if ((! "Chao2" %in% estimator) & (! "Jack1" %in% estimator) & (! "Jack2" %in% estimator) & (! "Boot" %in% estimator)){
                                                                                                      # if user asks for none of the above estimators...
    print("No valid estimator selected - choose any combination of Chao2, Jack1, Jack2, Boot")
  }
  
  
  return(results)
}
