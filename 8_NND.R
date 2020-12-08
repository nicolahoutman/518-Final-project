#Point pattern analysis of elevation samples that fall within the VRI data

#find coordinates of elevation sample locations
coords <- coordinates(elev_subset)

#join coordinates to shape file
elev_subset$X <- coords[,1]
elev_subset$Y<- coords[,2]


#intersect the two datasets (original file had n=300, now n=239)
VRI_ELEV <- raster::intersect(elev, vriClean)
VRI_ELEVATION<-intersect(elev, vriClean)

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
#no duplicates existed in our data 
zd <- zerodist(elev_subset)

kma.ext <- as.matrix(extent(VRI))
kma.ext2 <- kma.ext
kma.ext2[1,1] <- kma.ext[1,1] 
kma.ext2[1,2] <- kma.ext[1,2] 
kma.ext2[2,1] <- kma.ext[2,1] 
kma.ext2[2,2] <- kma.ext[2,2] 

#observation window
window <- as.owin(list(xrange = kma.ext2[1,], yrange = kma.ext2[2,]))

#create ppp oject from spatstat
kma.ppp <- ppp(x = elev_subset$X, y = elev_subset$Y, window = window)

##Nearest Neighbour Distance
###NEAREST NEIGHBOUR
nearestNeighbour <- (nndist(kma.ppp)/1000)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

##Change the column name to "Distance km"
colnames(nearestNeighbour) = "Distance km"


##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour

#Calculate study area in Km^2 (VRI has all polygons withing GVWSA)

studyArea <- (gArea(VRI)/1000000)

# N is the number of elevation points within the GVWSA
N<-234

pointDensity <- N/studyArea

r.nnd = 1/(2*(sqrt(pointDensity)))

colnames(nearestNeighbour) <- c("NND")
d.nnd = (sum(nearestNeighbour$NND)/N)

R = d.nnd/r.nnd

SE.NND <- 0.26136/(sqrt(N*pointDensity))

z = (d.nnd-r.nnd)/SE.NND

NNDD= (1.07453/sqrt(pointDensity))

pointDensity
r.nnd
d.nnd
SE.NND
z
NNDD

