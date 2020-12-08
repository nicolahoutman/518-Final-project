######Linear Regression##########
#Let's say your dataset with both Elev and Height are stored in a dataset called VRI.
#Plot Height and Elev from the VRI dataset you created
plot(vriClean$Stand_StemBio ~ vriClean$Elevation)

#Notice that there are a lot of 0's in this dataset.
VRI.no0 <-  vriClean[which(vriClean$Stand_StemBio > 0), ]

#Now plot the data again
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation)

#Get the summary of the results
summary(lm.model)

#Calculate the second order AIC to make model comparable
AIC(lm.model, k=2)

#Add the regression model to the plot you created
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elevation, xlab="Elevation (m)", ylab = "Stand stem biomass (tonnes/ha)")
abline(lm.model, col = "red", lwd=3)
text(450, 800, "Y=260.370 - 0.261X, R2=0.1268",col="red")


#add the fitted values to your spatialpolygon dataframe
VRI.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
VRI.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(VRI.no0@data)
min(VRI.no0$residuals)
max(VRI.no0$residuals)
hist(VRI.no0$residuals, breaks=50)
mean(VRI.no0$residuals)
shapiro.test(VRI.no0$residuals)$p.value

tmap_mode("plot")
#Now, create choropleth map of residuals
map_resid <- tm_shape(VRI.no0) +
  tm_polygons(col = "residuals",
              title = "OLS Residuals",
              breaks = c(-Inf, -200, -100, -50, 0, 50, 100, 200, Inf), 
              style = "fixed", 
              palette = "-RdBu", n = 6)


map_resid
##################################################

########################
#Complete the global Moran's I test and calculate if there is significant SAR in the OLS regression residuals 
# use the same network as before 
mi.r <- moran.test(VRI.no0$residuals, vri.lw, zero.policy = TRUE)
mi.r 

# Range will be the same as before 
## Extract results of the global Moran's I test
mI.r <- (mi.r$estimate[[1]])
eI.r <- (mi.r$estimate[[2]])
var.r <- (mi.r$estimate[[3]])

## Calculate the Z score for the global Moran's I test
z.r<-((mI.r-eI.r)/sqrt(var.r))

###################################
#look at the spatial distribution of the residuals
#####################################
lisa.test <- localmoran(VRI.no0$residuals, vri.lw, zero.policy = TRUE)
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
              title = "Confidence (%) in \n SAC of OLS residuals",
              breaks = c(-300, -3.023, -2.241, -1.96, -1.645, 1.645, 1.96, 2.241, 3.023, 300),
              style= "fixed",
              palette = "-RdBu", n=9,
              labels=c("Disimilar >99%", "Disimilar 97.5%-99%", "Disimilar 95%-97.5%", "Disimilar 90-95% ", "Random", "Similar 90-95%", "Similar 95%-97.5%", "Similar 97.5%-99%", "Similar C>99%"))+
  tm_legend(legend.outside= TRUE)+
  tm_scale_bar()

map_lisa
tmap_mode("plot")
########################


