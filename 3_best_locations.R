

.libPaths("C:/Users/jamesmartin/R_LIBS")
setwd("R:/Ryan_Lab/JamesM/GIS_final")
par(mar = c(0,0,0,0))

require('rgdal')

# import distances
dist <- read.csv("dist.csv")

# import huaquillas tracts
huaq <- readOGR("shapes/huaq.shp")

# reorder based on DPA, decending
huaq <- huaq[order(huaq@data$DPA_SECTOR),]

# add population and DPA
dist$DPA_SECTOR <- huaq@data$DPA_SECTOR
dist$POP        <- huaq@data$POPTOTAL

# create a list of distance matrices
dists <- vector(mode = "list", length = 54)

# we include one grid point in each 
for(i in 1:54){
  
  dists[[i]] <- cbind(
    dist[2:6],
    dist[i + 6],
    dist[61:62])
}

#---------------------- run for all grids -------------------

sumz <- c()
meanz <- c()
sdz <- c()


for(h in 1:length(dists)){


# divide block pop by distance and sum for respective hospitals 
  Vj <- c()

  for( i in 1:6){
  
    rez <- c()
    for(j in 1:nrow(dists[[h]])){
      rez[j] <- dist$POP[j]/dists[[h]][j, i]
    }
  
    Vj[i] <- sum(rez)
  }



# estimate accessability for each tract 

 # beds <- c(100/6, 100/6, 100/6, 100/6, 100/6, 100/6)
  beds <- c(48000, 48000, 48000, 48000, 48000, 48000)

  access_j <- c()

  for(i in 1:nrow(dists[[h]])){
    rez <- c()
  
    for(j in 1:6){
      rez[j] <- beds[j]/(Vj[j] * dists[[h]][i,j] )
    }
  
    access_j[i] <- sum(rez)
  }

  sumz[h] <- sum(access_j)
  meanz[h] <- mean(access_j)
  sdz[h]   <- sd(access_j)

}


summary(meanz)



write.csv(data.frame(sumz, meanz, sdz),
          "shapes/grid_rez.csv")


#-------------------- re run for best grid


best_grid <- which.max(meanz)

Vj <- c()

for( i in 1:6){
  
  rez <- c()
  for(j in 1:nrow(dists[[best_grid ]])){
    rez[j] <- dist$POP[j]/dists[[best_grid ]][j, i]
  }
  
  Vj[i] <- sum(rez)
}



# estimate accessability for each tract 
beds <- c(48000, 48000, 48000, 48000, 48000, 48000)

access_j <- c()

for(i in 1:nrow(dists[[best_grid ]])){
  rez <- c()
  
  for(j in 1:6){
    rez[j] <- beds[j]/(Vj[j] * dists[[best_grid]][i,j] )
  }
  
  access_j[i] <- sum(rez)
}




huaq@data$DPA_SECTOR2 <- dists[[best_grid]]$DPA_SECTOR
huaq@data$access      <- access_j
huaq@data$binational  <- dists[[best_grid]]$binational
huaq@data$nov18       <- dists[[best_grid]]$nov18
huaq@data$la_paz      <- dists[[best_grid]]$la_paz
huaq@data$haultalco   <- dists[[best_grid]]$haultalco
huaq@data$mercedes    <- dists[[best_grid]]$mercedes
huaq@data$grid_18     <- dists[[best_grid]]$grid_18

  
writeOGR(huaq,'./shapes','access_grid', driver="ESRI Shapefile")





