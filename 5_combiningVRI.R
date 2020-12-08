#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
#Convert your interpolation into a raster and map it:
r <- raster(r.idw)

SurfaceMap <- tm_shape(r.idw) + 
  tm_raster(n=6,palette = "YlOrBr", title="Elevation (m)") + 
  tm_shape(elev) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
SurfaceMap

#If you have too many cells, 
#you can reduce the number by aggregating values
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)

#Extract average elev for each polygon
vriClean$Elevation <- extract(r.idw, vriClean, fun = mean)[,1]
