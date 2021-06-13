

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

# add the tract DPA and population to 
# the distances for calcuations 
dist$DPA_SECTOR <- huaq@data$DPA_SECTOR
dist$POP        <- huaq@data$POPTOTAL

names(dist)

#________________________________________________
#                ~ calculate Vj ~

# divide block pop by distance and sum for 
# respective hospitals 

Vj <- c()

for( i in 2:6){
  
  rez <- c()
  for(j in 1:nrow(dist)){
    rez[j] <- dist$POP[j]/dist[j, i]
  }
  
  Vj[i-1] <- sum(rez)
}


#________________________________________________
#                ~ calculate Ai ~

# estimate accessability for each tract 

# assume equal distribution of service
# 960 hours a month
# 57600 minutes
services <- c(57600, 57600, 57600, 57600, 57600)

access <- c()

for(i in 1:nrow(dist)){
  rez <- c()
  
  for(j in 1:5){
    rez[j] <- services[j]/(Vj[j] * dist[i,j+1] )
  }
  # access for ech tract
  access[i] <- sum(rez)
}

# summed access for entire city 
sum(access)

# mean access for entire city 
mean(access)

access*60

#________________________________________________
#                ~ write results ~

huaq@data$DPA_SECTOR2 <- dist$DPA_SECTOR
huaq@data$access      <- access
huaq@data$binational  <- dist$binational
huaq@data$nov18       <- dist$nov18
huaq@data$la_paz      <- dist$la_paz
huaq@data$haultalco   <- dist$haultalco
huaq@data$mercedes    <- dist$mercedes

  
writeOGR(huaq,'./shapes','access', driver="ESRI Shapefile")





