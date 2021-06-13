

.libPaths("C:/Users/jamesmartin/R_LIBS")
setwd("R:/Ryan_Lab/JamesM/GIS_final")
par(mar = c(0,0,0,0))

#________________________________________________
#                   ~ set up ~

require('rgdal')

# read in network distances
# this is a data frame where each row is a tract
# and each column is a location 
dist <- read.csv("dist.csv")

# load huaqillas tracts, contains pop data
huaq <- readOGR("shapes/huaq.shp")


#________________________________________________
#               ~ format data ~

# sort the tracts by DPA number
huaq <- huaq[order(huaq@data$DPA_SECTOR),]

dist <- dist[,2:6]

# add the tract DPA and population to 
# the distances for calcuations 
dist$DPA_SECTOR <- huaq@data$DPA_SECTOR
dist$POP        <- huaq@data$POPTOTAL


#________________________________________________
#                ~ calculate Vj ~

# divide block pop by distance and sum for 
# respective hospitals 

Vj <- c()

for( i in 1:5){
  
  rez <- c()
  for(j in 1:nrow(dist)){
    rez[j] <- dist$POP[j]/dist[j, i]
  }
  
  Vj[i] <- sum(rez)
}

data.frame(names(dist[1:5]), Vj)

#________________________________________________
#               ~ generate services ~

# divide block pop by distance and sum for 
# respective hospitals 
library('gtools')

# generate a matrix of every combination of 0 to 30
#x <- seq(0,100,10)
x <- seq(0,30,2)
services <- permutations(n= length(x),r=5,v=x,repeats.allowed=T)

# if they dont sum to 100 delete them
sumz <- c()
for(i in 1:nrow(services)){
  sumz[i] <- sum(services[i,]) == 30
}

services <- services[sumz,]

# get service in minutes per month
services <- services*40*4*60

#________________________________________________
#             ~ maximize sum access ~

access_stats <- vector(mode = "list", length = 3)

for (h in 1:nrow(services)){
  
  #beds <- services[h,]
  access <- c()
  
  for(i in 1:nrow(dist)){
    rez <- c()
    
    for(j in 1:5){
      rez[j] <- services[h,][j]/(Vj[j] * dist[i,j] )
    }
    
    access[i] <- sum(rez)
  }
  
  access_stats[[1]][h] <- mean(access)
  access_stats[[2]][h] <- sd(access)
  access_stats[[3]][h] <- sum(access)
    
  
}  # 21 to 24


# top three optimizations based on 
services[which(access_stats[[1]] == 
          sort(access_stats[[1]], decreasing = T)[1]),]
services[which(access_stats[[1]] == 
          sort(access_stats[[1]], decreasing = T)[2]),]
services[which(access_stats[[1]] == 
          sort(access_stats[[1]], decreasing = T)[3]),]


services[which(access_stats[[1]] == sort(access_stats[[1]], 
                                         decreasing = T)[1]),]
# most equal distribution 
services[which(access_stats[[2]] == sort(access_stats[[2]])[1]),]

services[which(access_stats[[3]] == sort(access_stats[[3]], 
                                         decreasing = T)[1]),]


#________________________________________________
#                 ~ use thresolds ~

library(BAMMtools)
#thresolds <- getJenksBreaks(access, 10)

thresolds <- c(2, 4, 6, 8, 10, 12, 14)

access_thresold_Ai <- vector(mode = "list", length = 7)
access_thresold    <- vector(mode = "list", length = 7)
access_thresold_weighted <- vector(mode = "list", length = 7)

for (h in 1:nrow(services)){
  
 # beds <- services[h,]
  access <- c()
  
  for(i in 1:nrow(dist)){
    rez <- c()
    
    for(j in 1:5){
      rez[j] <- services[h,][j]/(Vj[j] * dist[i,j] )
    }
    
    access[i] <- sum(rez)
  }
  
  # get summed access
  for(i in 1:7){
    access_thresold_Ai[[i]][h] <-      sum(access)
  }
  
  # unweighted access just count the tracts
  for(i in 1:7){
    access_thresold[[i]][h] <- sum(sapply(access, function(x)
                ifelse( x >= thresolds[i], 1, 0)))
  }
  
  # weight the tracts by population 
  for(i in 1:7){
    access_thresold_weighted[[i]][h] <- sum(sapply(1:length(access),  
                              function(x)
      ifelse( access[x] >= thresolds[i], 
              dist$POP[x], 
              0)))
  }
  
}  # 11:32 11:35

# population protected under best distribution
max(access_thresold_weighted[[1]])
max(access_thresold_weighted[[2]])
max(access_thresold_weighted[[3]])
max(access_thresold_weighted[[4]])
max(access_thresold_weighted[[5]])

which.min(
sapply(1:nrow(services), function(x) sd(services[x,]))
)
# equal distribution
services[2322,]

# 
access_thresold[[1]][2322]
access_thresold[[2]][2322]
access_thresold[[3]][2322]

access_thresold_weighted[[1]][2322]
access_thresold_weighted[[2]][2322]
access_thresold_weighted[[3]][2322]


access_thresold_rez <- vector(mode = "list", length = 7)

for(i in 1:7){
  
  passes <- which(
    access_thresold_weighted[[i]] == max(access_thresold_weighted[[i]]))
  
 best <-  passes[which.max(
   sapply(passes, function(x) access_thresold_Ai[[i]][x])) ]
  
 access_thresold_rez[[i]] <- services[best,]
}



#________________________________________________
#       ~ rerun and get results for best  ~

final_Ai <- vector(mode = "list", length = 7)

for(h in 1:7){
  best_services <- access_thresold_rez[[h]]

  access_j <- c()

  for(i in 1:nrow(dist)){
    rez <- c()
  
    for(j in 1:5){
      rez[j] <- best_services[j]/(Vj[j] * dist[i,j] )
    }
  
    access_j[i] <- sum(rez)
  }
  final_Ai[[h]] <- access_j
}



huaq@data$DPA_SECTOR2 <- dist$DPA_SECTOR
huaq@data$access      <- access
huaq@data$binational  <- dist$binational
huaq@data$nov18       <- dist$nov18
huaq@data$la_paz      <- dist$la_paz
huaq@data$haultalco   <- dist$haultalco
huaq@data$mercedes    <- dist$mercedes

# first thresold also simple case best 
huaq@data$thresold1 <- final_Ai[[1]]
huaq@data$thresold2 <- final_Ai[[2]]
huaq@data$thresold3 <- final_Ai[[3]]
huaq@data$thresold4 <- final_Ai[[4]]
huaq@data$thresold5 <- final_Ai[[5]]
huaq@data$thresold6 <- final_Ai[[6]]
huaq@data$thresold7 <- final_Ai[[7]]



writeOGR(huaq,'./shapes','access_thres', driver="ESRI Shapefile")












