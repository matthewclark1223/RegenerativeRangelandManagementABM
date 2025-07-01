library(raster)
library(spdep)
library(gstat)

MakeAutoCorMat<-function(dummyMatrix,target_I){

r <- dummyMatrix
# Create grid of coordinate
xy <- as.data.frame(coordinates(r))

# Create a gstat object with a variogram model
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=TRUE, beta=0.5,
                 model=vgm(psill=1, model="Exp", range=0.01), nmax=20)

# Simulate one realization
sim <- predict(g.dummy, newdata=xy, nsim=1)

# Add simulated values to raster
r[] <- sim$sim1

# Convert raster to matrix and then to neighbors list
r_mat <- matrix(r[], nrow=nrow(r), byrow=TRUE)
nb <- cell2nb(nrow(r), ncol(r), type="queen")
lw <- nb2listw(nb, style="W")

# Calculate Moran's I
current_I<-moran.test(as.vector(r_mat), lw)$estimate[[1]]
#current_I<-0

tolerance <- 0.025
range_val <- 0.01

while (abs(current_I - target_I) > tolerance) {
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=TRUE, beta=0,
                   model=vgm(psill=1, model="Exp", range=range_val), nmax=20)
  sim <- predict(g.dummy, newdata=xy, nsim=1)
  r[] <- sim$sim1
  r_mat <- matrix(r[], nrow=nrow(r), byrow=TRUE)
  lw <- nb2listw(nb, style="W")
  current_I <- moran.test(as.vector(r_mat), lw)$estimate[1]
  print(paste("Range:", range_val, "Moran's I:", current_I))
  range_val <- range_val + 0.05 * sign(target_I - current_I)
}

return(r)
}
