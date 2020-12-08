
# spatial interpolation to create continuous surface
###########################################
#IDW Interpolation (IDP value based on iterations that minimize RMSE)
P.idw <- gstat::idw(Elevation ~ 1, elev, newdata=grd, idp=4.9)
r.idw       <- raster(P.idw)
r.m.idw     <- mask(r.idw, VRI)

idw.4.9 <- tm_shape(r.m.idw) + 
  tm_raster(n=6,palette = "YlOrBr",
            title="Predicted elevation (m) with \n observed elevation points in black") + 
  tm_shape(elev) + tm_dots(size=0.2, n=6, palette="YlOrBr") +
  tm_legend (position=c("left", "bottom"))
idw.4.9


###########################################
# Leave-one-out validation routine
IDW.out <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out[i] <- idw(Elevation ~ 1, elev[-i,], elev[i,], idp=4.9)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ elev$Elevation, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ elev$Elevation), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - elev$Elevation)^2) / length(elev$Elevation))


############################################
# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.
# Create the interpolated surface
img <- gstat::idw(Elevation~1, elev, newdata=grd, idp=4.9)
n   <- length(elev)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)


# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(Elevation~1, elev[-i,], newdata=grd, idp=4.9)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Southern California
r <- raster(img.sig, layer="v")
r.m <- mask(r, VRI)

# Plot the map
tm_shape(r.m) + tm_raster(n=6,title="95% Confidence interval (m)", palette = "-YlOrBr") +
  tm_shape(elev) +
  tm_dots(col="Elevation (m)", palette = "Blues",
          size=0.2, 
          breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, Inf), 
          style = "fixed") + 
  tm_legend(position=c("left", "bottom"))

###########################################
