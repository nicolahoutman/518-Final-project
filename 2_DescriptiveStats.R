#Calculate descriptive stats for stand biomass and elevation (within the study area)
# Range
max(elev_subset$Elevation)
min(elev_subset$Elevation)

max(vriClean$Stand_StemBio)
min(vriClean$Stand_StemBio)

#Mean
D.MEAN.bio<-mean(vriClean$Stand_StemBio)
D.MEAN.el<-mean(elev_subset$Elevation)

#Standard Deviation
D.SD.bio<-sd(vriClean$Stand_StemBio)
D.SD.el<-sd(elev_subset$Elevation)

#Mode
D.MODE.bio <- as.numeric(names(sort(table(vriClean$Stand_StemBio), decreasing = TRUE))[1])
D.MODE.el<- as.numeric(names(sort(table(elev_subset$Elevation), decreasing = TRUE))[1])

#Median
D.MED.bio <- median(vriClean$Stand_StemBio, na.rm = TRUE)
D.MED.el<-median(elev_subset$Elevation, na.rm = TRUE)

#Skewness
D.SKEW.bio <- skewness(vriClean$Stand_StemBio, na.rm = TRUE)[1]
D.SKEW.el<-skewness(elev_subset$Elevation, na.rm = TRUE)[1]

#Kurtosis
D.KURT.bio <- kurtosis(vriClean$Stand_StemBio, na.rm = TRUE)[1]
D.KURT.el<-kurtosis(elev_subset$Elevation, na.rm=TRUE)[1]

#CoV
D.COV.bio <- (D.SD.bio / D.MEAN.bio) * 100
D.COV.el<- (D.SD.el/D.MEAN.el)*100

#Normal distribution test
normBIO_PVAL <- shapiro.test(vriClean$Stand_StemBio)$p.value
normEL_PVaL <- shapiro.test(elev_subset$Elevation)$p.value

#set significant figures
D.MEAN.bio  <-round(D.MEAN.bio, 3)
D.MED.bio   <-round(D.MED.bio, 3)
D.MODE.bio  <-round(D.MODE.bio, 3)
D.SD.bio    <-round(D.SD.bio, 3)
D.SKEW.bio  <-round(D.SKEW.bio, 3)
D.KURT.bio  <-round(D.KURT.bio, 3)
D.COV.bio   <-round(D.COV.bio, 3)
normBIO_PVAL  <-signif(normBIO_PVAL, 3)

D.MEAN.el  <-round(D.MEAN.el, 3)
D.MED.el   <-round(D.MED.el, 3)
D.MODE.el  <-round(D.MODE.el, 3)
D.SD.el    <-round(D.SD.el, 3)
D.SKEW.el  <-round(D.SKEW.el, 3)
D.KURT.el  <-round(D.KURT.el, 3)
D.COV.el   <-round(D.COV.el, 3)
normEL_PVaL<-signif(normEL_PVaL, 3)

#Create and Print histograms
png("Elevation_SA.png")
hist(elev_subset$Elevation,  breaks = 30, main = "Elevation samples", xlab = "Elevation (m)") 
dev.off()

png("Stand_bio_Histogram.png")
hist(vriClean$Stand_StemBio, breaks = 30, main = "Forest Stand biomass", xlab = "Stand biomass (tonnes/ha)") 
dev.off()



