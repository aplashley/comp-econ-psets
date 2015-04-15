
  
library(RANN)
library(FNN)
library(ggplot2)
library(reshape2)

library(foreach)  
library(doParallel)  
library(parallel)

require(foreach)
require(doParallel)
require(parallel)
require(ggplot2)

numCores <- detectCores()  
cl <- makeCluster(numCores)  
registerDoParallel(cl)

testv = 10000
## Max on number of times function runs

testRacialPreferenceTable <- matrix(1:15, ncol = 5, nrow = 3)
testRacialPreferenceTable[1,] <- c("R",1, 20, 2, 1)
testRacialPreferenceTable[2,] <- c("G", 0, 10, 2, 1)
testRacialPreferenceTable[3,] <- c("B", -1, 10, 2, 1)
colnames(testRacialPreferenceTable) <- c("Color", "Value", "Pop.", "Test Pool Size", "Racial Threshold")
print(testRacialPreferenceTable)

nR <- as.numeric(testRacialPreferenceTable[1,"Pop."])
nG <- as.numeric(testRacialPreferenceTable[2,"Pop."])
nB <- as.numeric(testRacialPreferenceTable[3,"Pop."])

n <- sum(nR + nG + nB)
## Find total population from summing each racial population

inputs <- testRacialPreferenceTable

stop.val <- .8
happy_counter <- 0


Schelling <- function(racialPreferenceTable = testRacialPreferenceTable, cyclemax = testv){
  set.seed(20016)
  library(ggplot2)
  LocationTable <- matrix(ncol = 3)
  ## Initalizing table for initial neighborhood coordinates
  
  for (i in 1:nR){
    x <- runif(1, min=0, max=1)
    ## Generate random X coordinate between 0 and 1 for point
    y <- runif(1, min=0, max=1)
    ## Generate random Y coordinate between 0 and 1 for point
    R = c(1,x,y)
    ## Create vector with point coordinates, labeling point as red
    LocationTable <- rbind(LocationTable, R)
    ## Add red point to table of all neighborhood coordinates
  }
  
  for (i in 1:nG){
    x <- runif(1, min=0, max=1)
    y <- runif(1, min=0, max=1)
    
    G = c(0,x,y)
    
    LocationTable <- rbind(LocationTable, G)
  }
  
  for (i in 1:nB){
    x <- runif(1, min=0, max=1)
    y <- runif(1, min=0, max=1)
    B = c(-1,x,y)
    
    LocationTable <- rbind(LocationTable, B)
  }
  
  LocationTable <- LocationTable[-1,]
  Count <- c(1:nrow(LocationTable))
  ## Create column counting number of points or people
  
  Happy <- c(rep(0, nrow(LocationTable)))
  ## Create column to keep track of if person is happy
  
  Testpool <- c(rep(0, nrow(LocationTable)))
  ## Create column for indvidual's testpool
  
  Threshold <- c(rep(0, nrow(LocationTable)))
  ## Create column for indvidual's threshold
  
  LocationTable <- cbind(Count, LocationTable, Happy, Testpool, Threshold)
  ## Add columns to Location Table
  
  
  p <- qplot(x = LocationTable[,3], y = LocationTable [,4], col = ifelse(LocationTable[,2] < -0.5, "red", ifelse(LocationTable[,2] < 0.5, "green", "blue"))) + theme(legend.position = "none")
  
  print(p)
  
  testpoolR <- as.numeric(racialPreferenceTable[1,4])
  ## Pull m value for given race
  thresholdR <- as.numeric(racialPreferenceTable[1,5])
  ##Pull j value for given race
  
  testpoolG <- as.numeric(racialPreferenceTable[2,4])
  thresholdG <- as.numeric(racialPreferenceTable[2,5])
  
  testpoolB <- as.numeric(racialPreferenceTable[3,4])
  thresholdB <- as.numeric(racialPreferenceTable[3,5])
  
  for (individual in 1:nrow(LocationTable)){
    
    own_race <- LocationTable[individual,2]
    
    if(own_race == 1){
      ##If the point is red...
      
      testpool <- testpoolR
      ## Pull m value for individual given race
      threshold <- thresholdR
      ##Pull j value for indvidual given race
      
      LocationTable[individual,6] <- testpool
      LocationTable[individual,7] <- threshold
      
    }
    
    if(own_race == 0){
      ##If the point is green...
      
      testpool <- testpoolG
      threshold <- thresholdG
      
      LocationTable[individual,6] <- testpool
      LocationTable[individual,7] <- threshold
      
    }
    
    if(own_race == -1){
      ##If the point is blue...
      
      testpool <- testpoolB
      threshold <- thresholdB
      
      LocationTable[individual,6] <- testpool
      LocationTable[individual,7] <- threshold
      
    } 
  }
  
  print(LocationTable)
  
  maxtestnumb <- max(testpoolR, testpoolG, testpoolB)
  #Finding max testpool value so we can create neighborlist outside loop
  
  LoopUnhappyLocationTable <- LocationTable
  
  justXYtable = LocationTable[,3:4]
  #Make seperate table with just X & Y coordinate for
  #nearest neighbor function
  
  neighborList <- get.knn(data = justXYtable, k = maxtestnumb)$nn.index
  ## Create matrix of m closest neighbors for each point
  
  print(neighborList)
  
  bad_neighbors <- 0
  good_neighbors <- 0
  
  ##Initialize value for total number of neighbors evaluate
  
  cycles <- 0
  
  
  while (((happy_counter/n) < stop.val) & (cycles < cyclemax)){
    
    NumUnhappy <- nrow(LoopUnhappyLocationTable)
    
    cycles <- cycles + 1  
    happy_counter<- sum(LocationTable[,5])
    
    for (individual in (1:NumUnhappy)){
      ##For a point in the location table...
      
      for (neighbor in 1:(LocationTable[individual,6])){
        ## For each closest neighbor of the given point
        
        neighborList <- neighborList[,1:LocationTable[individual,6]]
        ##Get rid of extraneous neighbors who are ranked lower than
        ## k closest
        
        a_neighbor <- neighborList[individual,neighbor]
        ## Find numerical value of neighboor in Location matrix
        
        a_neighbors_race <- LocationTable[a_neighbor,1]
        ## Find neighbor's race
        
        while ((bad_neighbors + good_neighbors) < (LocationTable[individual,6])){
          
          good_neighbors <- 0
          bad_neighbors <- 0
          
          if (a_neighbors_race == own_race){
            good_neighbors <- goodneighbors + 1
            
            if ((good_neighbors + bad_neighbors) == (LocationTable[individual,6])){
              LocationTable[individual,5] = 1
              LoopUnhappyLocationTable = LoopUnhappyLocationTable[-individual,]
            }
          }
          
          if (a_neighbors_race != own_race){
            
            bad_neighbors <- bad_neighbors + 1
            ## If a neighbor's race is different from individual's, 
            ## increase number of bad neighbors
            
            if (bad_neighbors > (LocationTable[individual,7])){
              ##If the number of bad neighbors exceeds threshold...
              
              new_x <- runif(1, min=0, max=1)
              new_y <- runif(1, min=0, max=1)
              
              LocationTable[individual,3] <- new_x
              LocationTable[individual,4] <- new_y
              LoopUnhappyLocationTable[individual,3] <- new_x
              LoopUnhappyLocationTable[individual,4] <- new_y
              
            } 
          }
        }
      }
    }
    
    p <- qplot(x = LocationTable[,3], y = LocationTable [,4], col = ifelse(LocationTable[,2] < -0.5, "red", ifelse(LocationTable[,2] < 0.5, "green", "blue"))) + theme(legend.position = "none")
    
    if (cycles %% 5 == 0) {print(p)} 
  }
  
  return(cycles)
  print(p)
  
}

Schelling(testRacialPreferenceTable)

# print(system.time(Schelling(testRacialPreferenceTable)))