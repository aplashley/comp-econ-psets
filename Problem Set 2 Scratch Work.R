racialPreferenceTable <- matrix(1:12, ncol = 4, nrow = 3)
colnames(racialPreferenceTable) <- c("Color", "Percent of Total Population", "Closest Neighbors Test Pool Size", "Closest Neighbors Racial Threshold")

x <- c("R", .5, 5, 2)
y <- c("G", .25, 5, 2)
z <- c("B", .25, 5, 2)

racialPreferenceTable[1,] <- x
racialPreferenceTable[2,] <- y
racialPreferenceTable[3,] <- z
racialPreferenceTable[,1] <- c(50,25,25)

library(ggplot2)

Schelling <- function(racialPreferenceTable = testRacialPreferenceTable){
  set.seed(20016)
  LocationTable <- matrix(ncol = 3)
  
  for (i in 1:nR){
    x <- runif(1, min=0, max=1)
    y <- runif(1, min=0, max=1)
    currentpointR = c(1,x,y)
    
    LocationTable <- rbind(LocationTable, currentpointR)
    
  }
  
  for (i in 1:nG){
    x <- runif(1, min=0, max=1)
    y <- runif(1, min=0, max=1)
    currentpointG = c(0,x,y)
    
    LocationTable <- rbind(LocationTable, currentpointG)
  }
  
  for (i in 1:nB){
    x <- runif(1, min=0, max=1)
    y <- runif(1, min=0, max=1)
    currentpointB = c(-1,x,y)
    
    LocationTable <- rbind(LocationTable, currentpointB)
  }
  
 return(LocationTable) 
}

qplot(x = LocationTable[,2], y = LocationTable [,3], col = ifelse(LocationTable[,1] > .1, "red", ifelse(LocationTable[,1] < -.1, "blue", "green"))) + 

