effectsMultiway <- function(nrOfFactors, means, cellSD, interactions = "yes"){
  # werkt tot en met 4-way ANOVA
  
  sz <- dim(means)
  grandMean <- mean(means)
  
  mainEffects <- matrix(nrow = nrOfFactors, ncol = 2)
  colnames(mainEffects) <- c("Maineffect", "df")
  
  interactionEffects <- array(dim = c(nrOfFactors-1,nrOfFactors-1))
  rownames(interactionEffects) <- paste("Factor", 1:(nrOfFactors-1))
  colnames(interactionEffects) <- paste("Factor", 2:nrOfFactors)
  interactionDf <- interactionEffects
  extraVar <- 0
  
  for(i in 1:nrOfFactors){# 1-way or more
    
    factoriMeans <- apply(means,i,mean)
    mainEffects[i,1:2] <- c(sum((factoriMeans-grandMean)^2)/sz[i], sz[i]-1)
    
    if(i < nrOfFactors){# 2-way or more
      for(j in (i+1):nrOfFactors){
        factorjMeans <- apply(means, j, mean)
        interactionijMeans <- apply(means, c(i,j), mean)
        differences  <- sweep(interactionijMeans+grandMean, 1, factoriMeans, FUN="-")
        differences  <- sweep(differences, 2, factorjMeans, FUN="-")

        interactionEffects[i,j-1] <- sum(differences^2)/sz[i]/sz[j]
        interactionDf[i,j-1] <- (sz[i]-1)*(sz[j]-1)
        
        if(j < nrOfFactors){# 3-way or more
          for(k in (j+1):nrOfFactors){
            
            factorkMeans <- apply(means, k, mean)
            interactionikMeans <- apply(means, c(i,k), mean)
            interactionjkMeans <- apply(means, c(j,k), mean)
            interactionijkMeans <- apply(means, c(i,j,k), mean)
            
            differences <- sweep(interactionijkMeans-grandMean, 1, factoriMeans, FUN="+")
            differences <- sweep(differences, 2, factorjMeans, FUN="+")
            differences <- sweep(differences, 3, factorkMeans, FUN="+")
            
            differences <- sweep(differences, c(1,2), interactionijMeans, FUN="-")
            differences <- sweep(differences, c(1,3), interactionikMeans, FUN="-")
            differences <- sweep(differences, c(2,3), interactionjkMeans, FUN="-")
            
            extraVar <- extraVar + sum(differences)^2/sz[i]/sz[j]/sz[k]
            
            if(k < nrOfFactors){ #4-way
              for(l in (k+1):nrOfFactors){
                factorlMeans <- apply(means, l, mean)
                interactionilMeans <- apply(means, c(i,l), mean)
                interactionjlMeans <- apply(means, c(j,l), mean)
                interactionklMeans <- apply(means, c(k,l), mean)
                
                interactionijlMeans <- apply(means, c(i,j,l), mean)
                interactioniklMeans <- apply(means, c(i,k,l), mean)
                interactionjklMeans <- apply(means, c(j,k,l), mean)
                
                interactionijklMeans <- apply(means, c(i,j,k,l), mean)
                
                differences <- sweep(interactionijklMeans+grandMean, 1, factoriMeans, FUN="-")
                differences <- sweep(differences, 2, factorjMeans, FUN="-")
                differences <- sweep(differences, 3, factorkMeans, FUN="-")
                differences <- sweep(differences, 4, factorlMeans, FUN="-")
                
                differences <- sweep(differences, c(1,2), interactionijMeans, FUN="+")
                differences <- sweep(differences, c(1,3), interactionikMeans, FUN="+")
                differences <- sweep(differences, c(2,3), interactionjkMeans, FUN="+")
                differences <- sweep(differences, c(1,4), interactionilMeans, FUN="+")
                differences <- sweep(differences, c(2,4), interactionjlMeans, FUN="+")
                differences <- sweep(differences, c(3,4), interactionklMeans, FUN="+")
                
                differences <- sweep(differences, c(1,2,3), interactionijkMeans, FUN="-")
                differences <- sweep(differences, c(1,3,4), interactioniklMeans, FUN="-")
                differences <- sweep(differences, c(2,3,4), interactionjklMeans, FUN="-")
                differences <- sweep(differences, c(1,2,4), interactionijlMeans, FUN="-")
                
                extraVar <- extraVar + sum(differences)^2/sz[i]/sz[j]/sz[k]/sz[l]
              }
            }
          }
        }
      }
    }
  }
  
   
  if(interactions == "yes"){
    errorVar = cellSD^2 + extraVar
    interactionEffects <- interactionEffects/(interactionEffects+errorVar)
  }else{
    errorVar = cellSD^2 + sum(interactionEffects, na.rm = TRUE) + extraVar
  }
  
  mainEffects[,1] <-  mainEffects[,1]/(mainEffects[,1]+errorVar)
  FmainEffects <- (mainEffects[,1]/(1-mainEffects[,1]))^0.5
  FinteractionEffects <- (interactionEffects/(1-interactionEffects))^0.5
  
  if(interactions == "yes"){
    results <- list(FmainEffects = FmainEffects, mainEffects = mainEffects, FinteractionEffects = FinteractionEffects,
                    interactionEffects = interactionEffects, interactionDf = interactionDf)
  }else{
    results <- list(FmainEffects = FmainEffects, mainEffects = mainEffects)
  }
  
    return(results)
}

effectsRepeatedMeasures <- function(means, cellSD, correlation, interactions = "yes"){
  # De gepaarde metingen gaan over de kolommen, ongepaard over rijen, e.g
  
  #              Dag 1    Dag 15     Dag 30 (gepaard)
  # Jonge muizen
  # Oude muizen  
  # (ongepaard)
  
  sz <- dim(means)
  grandMean <- mean(means)
  errorVar  <- cellSD^2*(1 - 1/sz[2] - correlation*(sz[2]-1)/sz[2])
  extraVar <- 0
  
  if(interactions == "yes"){
    effects   <- matrix(nrow = 3, ncol = 3)
    rownames(effects) <- c("Between effect", "Within effect", "Within-Between")
    colnames(effects) <- c("f","eta^2", "df")
  }else{
    effects   <- matrix(nrow = 2, ncol = 3)
    rownames(effects) <- c("Between effect", "Within effect")
    colnames(effects) <- c("f","eta^2", "df")
  }
  
  betweenMeans <- apply(means-grandMean, 1, mean)
  withinMeans  <- apply(means-grandMean, 2, mean)
  explVar <- sweep(means-grandMean,1,betweenMeans, FUN = "-")
  explVar <- sum(sweep(explVar,2, withinMeans, FUN = "-")^2)/sz[1]/sz[2]
  if(interactions == "yes"){
    effects[3,2] <- explVar/(explVar+errorVar)
    effects[3,3] <- (sz[1]-1)*(sz[2]-1)
    effects[3,1] <- (effects[3,2]/(1-effects[3,2]))^0.5
  }else{
    extraVar <- explVar
  }
  
  explVar      <- sum(betweenMeans^2)/sz[1]
  effects[1,2] <- explVar/(explVar+cellSD^2+extraVar)
  effects[1,3] <- sz[1]-1
  effects[1,1] <- (effects[1,2]/(1-effects[1,2]))^0.5
  
  explVar      <- sum(withinMeans^2)/sz[2]
  effects[2,2] <- explVar/(explVar+errorVar+extraVar)
  effects[2,3] <- sz[2]-1
  effects[2,1] <- (effects[2,2]/(1-effects[2,2]))^0.5
  
  return(effects)
}

## Test ----

# means <- matrix(c(0,0,2,2),nrow = 2)
# nrOfFactors <- 2
# cellSD <- 4

# results <- effectsMultiway(nrOfFactors, means, cellSD)

# means2 <- matrix(1:6, nrow = 2, ncol = 3)
# cellSD <- 2
# correlation <- 0.3

# results2 <- effectsRepeatedMeasures(means2, cellSD, correlation)
