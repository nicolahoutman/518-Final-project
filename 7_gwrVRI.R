####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
VRI.no0.coords <- sp::coordinates(VRI.no0)
#Observe the result:
head(VRI.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
VRI.no0$X <- VRI.no0.coords[,1]
VRI.no0$Y <- VRI.no0.coords[,2]

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation, 
                        data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation, 
                data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

# test to see if fixed bandwidth provides a better AICc
###Determine the bandwidth for GWR: this will take a while
GWRbandwidth_fixed <- gwr.sel(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation, 
                              data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y),adapt=F) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model_fixed = gwr(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation, 
                      data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                      bandwidth = GWRbandwidth_fixed, hatmatrix=TRUE, se.fit=TRUE) 
#Print the results of the model
gwr.model_fixed

# Calculate the difference in AICc values (adaptive-fixed)
59357.53 -59125.98 

#Look at the results in detail
results<-as.data.frame(gwr.model_fixed$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
VRI.no0$localr <- results$localR2
head(VRI.no0)
hist(VRI.no0$localr)
mean(VRI.no0$localr)
max(VRI.no0$localr)
min(VRI.no0$localr)
# calculate % postitve r^2 values
View(VRI.no0@data)
100-(16/4887*100)
r2neg <-  VRI.no0[which(VRI.no0$localr < 0), ]

tmap_mode("plot")
#Create choropleth map of r-square values
map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              style = "fixed",
              breaks= c(-Inf, -0.1,0,0.1,0.2,0.4,0.6, 0.8, Inf),
              palette = "-RdBu", n = 6)+
tm_shape(r2neg) +
  tm_polygons(title = "-R2 values",
              palette = "blueviolet")
map_r2


hist(VRI.no0$coeff, breaks=30)
mean(VRI.no0$coeff)
max(VRI.no0$coeff)
min(VRI.no0$coeff)


map_r2
 
#Time for more magic. Let's map the coefficients
VRI.no0$coeff <- results$VRI.no0.Elevation
#Create choropleth map of the coefficients
map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "GWR coefficients",
              style = "fixed",
              breaks = c(-Inf, -3, -1, -0.5, 0, 0.5, 1, 3, Inf),
              palette = "-RdBu", n = 6)
map_coef




hist(VRI.no0$coeff, breaks=30)
max(VRI.no0$coeff)
min(VRI.no0$coeff)
mean(VRI.no0$coeff)
median(VRI.no0$coeff)
map_res <- tm_shape(VRI.no0) +
  tm_polygons(col = "gwr_residuals",
              title = "GWR residuals",
              style = "fixed",
              breaks = c(-Inf, -200, -100, -50, 0, 50, 100, 200, Inf),
              palette = "-RdBu", n = 6)
map_res

##################################
#Analyze residuals of GWR to determine if there is SAC of residuals 

shapiro.test(VRI.no0$gwr_residuals)$p.value

## add residuals back to data
# column gwr.e contains the residuals (https://gis.stackexchange.com/questions/241127/how-to-plot-output-from-gwr-in-r)

VRI.no0$gwr_residuals <- results$gwr.e

mi.gwr <- moran.test(VRI.no0$gwr_residuals, vri.lw, zero.policy = TRUE)
mi.gwr 

# Range will be the same as before 
## Extract results of the global Moran's I test
mI.gwr <- (mi.gwr$estimate[[1]])
eI.gwr <- (mi.gwr$estimate[[2]])
var.gwr <- (mi.gwr$estimate[[3]])

## Calculate the Z score for the global Moran's I test
z.gwr<-((mI.gwr-eI.gwr)/sqrt(var.gwr))

###################################
#look at the spatial distribution of the residuals
#####################################
lisa.test <- localmoran(VRI.no0$gwr_residuals, vri.lw, zero.policy = TRUE)
lisa.test

## Isolate the results from the local Moran's I test, and join them back to the associated polygons
VRI.no0$Ii <- lisa.test[,1]
VRI.no0$E.Ii<- lisa.test[,2]
VRI.no0$Var.Ii<- lisa.test[,3]
VRI.no0$Z.Ii<- lisa.test[,4]
VRI.no0$P<- lisa.test[,5]


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

map_lisa<- tm_shape(VRI.no0)+
  tm_polygons(col= "Z.Ii",
              title = "Confidence (%) in \n SAC of OLS \nand GWR residuals",
              breaks = c(-300, -3.023, -2.241, -1.96, -1.645, 1.645, 1.96, 2.241, 3.023, 300),
              style= "fixed",
              palette = "-RdBu", n=9,
              labels=c("Disimilar >99%", "Disimilar 97.5%-99%", "Disimilar 95%-97.5%", "Disimilar 90-95% ", "Random", "Similar 90-95%", "Similar 95%-97.5%", "Similar 97.5%-99%", "Similar C>99%"))+
  tm_legend(legend.outside= TRUE)+
  tm_scale_bar()

map_lisa
tmap_mode("plot")