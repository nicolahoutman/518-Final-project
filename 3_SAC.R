vri.nb <- poly2nb(vriClean)
vri.net <- nb2lines(vri.nb, coords=coordinates(vriClean))
crs(vri.net) <- crs(vriClean)

#Map the network
GMI_network <-tm_shape(vriClean) + tm_borders(col='lightgrey') +   
  tm_shape(vri.net) + tm_lines(col='red')            

#Create a weights matrix 
vri.lw <- nb2listw(vri.nb, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)

########################
#Complete the global Moran's I test and calculate if it there is significant autocorrelation

mi <- moran.test(vriClean$Stand_StemBio, vri.lw, zero.policy = TRUE)
mi 

## Calculate range of expected Moran's I 
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vri.lw)


## Extract results of the global Moran's I test
mI <- (mi$estimate[[1]])
eI <- (mi$estimate[[2]])
var <- (mi$estimate[[3]])

## Calculate the Z score for the global Moran's I test
z<-((mI-eI)/sqrt(var))


############################################### 
# Preform the local Moran's I test on the variable, 
lisa.test <- localmoran(vriClean$Stand_StemBio, vri.lw, zero.policy = TRUE)
lisa.test


## Isolate the results from the local Moran's I test, and join them back to the associated polygons
vriClean$Ii <- lisa.test[,1]
vriClean$E.Ii<- lisa.test[,2]
vriClean$Var.Ii<- lisa.test[,3]
vriClean$Z.Ii<- lisa.test[,4]
vriClean$P<- lisa.test[,5]


#############################################

# Map the LISA statistic

## Create meaningful breaks data breaks so we can interpret if a polygon is has significant spatial autocorreltation 
### the absolute value of these z scores represent the following p-values in a two tailed test. 
#### 3.023 = p (0.01)
#### 2.141 = p (0.025)
#### 1.96 = p (0.05)
#### 1.645 = p (0.1)


##### Listing the z scores in both the positive and the negative will also provide information if a feature has positive or negative spatial autocorrelation
##### 300 was used in the breaks to make sure no data was excluded from the calculation by using the function "range" on the z scores
breaks_SIG<- c(-300, -3.023, -2.241, -1.96, -1.645, 1.645, 1.96, 2.241, 3.023, 300)

map_lisa<- tm_shape(vriClean)+
  tm_polygons(col= "Z.Ii",
              title = "Confidence (%) in SAC",
              breaks = c(-300, -3.023, -2.241, -1.96, -1.645, 1.645, 1.96, 2.241, 3.023, 300),
              style= "fixed",
              palette = "-RdBu", n=9,
              labels=c("Disimilar >99%", "Disimilar 97.5%-99%", "Disimilar 95%-97.5%", "Disimilar 90-95% ", "Random", "Similar 90-95%", "Similar 95%-97.5%", "Similar 97.5%-99%", "Similar C>99%"))+
  tm_legend(legend.outside=TRUE)+
  tm_scale_bar()

map_lisa
tmap_mode("plot")
########################
#NOT SHOWN IN REPORT
#Create a plot to visualize the spatial autocorrelation of each location relative to each other
moran.plot(vriClean$Stand_StemBio, vri.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Stand biomass (tonnes/ha)", 
           ylab="Spaitally lagged stand biomass (neighbours values)", quiet=NULL)

########################

