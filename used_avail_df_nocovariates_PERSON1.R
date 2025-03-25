##Code containing wolfhyt data and kernel home ranges for ind. pack 

##Load packages-----------------------------------------------------------------

packages <- c("ks", "here", "plotrix", "lattice", "adehabitatHR", "maptools", "mapview", "ggplot2","colorRamps", "sf", "terra", "tmap", "stars", "dplyr")

#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#run function to install packages
ipak(packages)

##Load in Wolfyht data----------------------------------------------------------

wolfyht<-st_read(here::here("Data","wolfyht.shp"))

#view data
head(wolfyht)

class(wolfyht)

crs(wolfyht,proj = TRUE) # note this is a UTM projected map system. 

str(wolfyht)


# Note that there are two fields, Easting and Northing which are the X and Y coordinates in UTM zone 11.  We will use these to map it for each PackID
# base plot of wolf packs by color with legend
base::plot(wolfyht$EASTING,wolfyht$NORTHING,col=c("red","blue")[wolfyht$PackID],ylab="Northing",xlab="Easting")
legend(555000,5742500,unique(wolfyht$Pack),col=c("blue","red"),pch=1) 


##home range analysis-------------------------------------------------------

##RED DEER ANALYSIS---------------------------------------------------------

#extract basics for X Y locations in new data frame

rd.data<-wolfyht[wolfyht$Pack=="Red Deer",]
x<-rd.data$EASTING
y<-rd.data$NORTHING
xy<-cbind(x,y)
class(xy)

rd <- data.frame(as.character(rd.data$NAME))
coordinates(rd) <- xy
crs(rd) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

class(rd)

# fix wolves with too few locations 

#note error that one animal does not have at least 5 locations
table(rd.data$NAME)

#looks like 4 of the wolves do not have enough locations

#remove these individuals with too few of locations
names(rd)<-"NAME"
rd<-rd[rd@data$NAME!="69" & rd@data$NAME!="81" & rd@data$NAME!="82" & rd@data$NAME!="84",]
#remove unused NAME levels
rd@data$NAME<-factor(rd@data$NAME)


#Kernel Density Estimate home ranges 

#calculate 99% KDE for Red Deer wolf pack

red.deerUD <- kernelUD(rd, grid=30, extent=0.5, same4all=TRUE) # reference grid
image(red.deerUD)

#get polygons for home ranges
homerangeRD <- getverticeshr(red.deerUD)
as.data.frame(homerangeRD)

class(homerangeRD)

plot(homerangeRD, col=2:4)

#Estimate UD in raster mode
red.deerud <- getvolumeUD(red.deerUD) 
red.deerud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(red.deerud[[1]]) #for first wolf only
title("Red Deer Wolf UD") 
xyzv <- as.image.SpatialGridDataFrame(red.deerud[[1]]) 
contour(xyzv, add=TRUE)

#Store the volume under UD 

fud <- red.deerud[[1]] #for first wolf only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(red.deerud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)


##BOW VALLEY ANALYSIS-----------------------------------------------------------

#create bv data frames


graphics.off()
#calculate 99% KDE for Bow Valley wolf pack
bow.valleyUD <- kernelUD(bv, grid=30, extent=0.1, same4all=TRUE) # reference grid
image(bow.valleyUD)


#get polygons for home ranges
homerangeBV <- getverticeshr(bow.valleyUD)
as.data.frame(homerangeBV)

class(homerangeBV)

plot(homerangeBV, col=2:4)


#Estimate UD in raster mode
bow.valleyud <- getvolumeUD(bow.valleyUD) 
bow.valleyud


## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(bow.valleyud[[1]])
title("Bow Valley Pack UD") 
xyzv <- as.image.SpatialGridDataFrame(bow.valleyud[[1]]) 
contour(xyzv, add=TRUE)


fud <- bow.valleyud[[1]]
## store the value of the volume under 95% UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(bow.valleyud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)


##BOTH WOLF PACKS---------------------------------------------------------------

#calculate 99% KDE for both wolf packs
allUD <- kernelUD(all, grid=30, extent=0.5, same4all=TRUE) # reference grid
image(allUD)

homerangeALL <- getverticeshr(allUD)
as.data.frame(homerangeALL)

class(homerangeALL)

plot(homerangeALL, col=2:3)

allud <- getvolumeUD(allUD) 
allud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(allud[[1]]) #for first wolf only
title("Output of getvolumeUD") 
xyzv <- as.image.SpatialGridDataFrame(allud[[1]]) 
contour(xyzv, add=TRUE)


fud <- allud[[1]] #for first wolf pack only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(allud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)

##Sampling Availability within Home Range----------------------------------------

#subset polygons by wolf pack
red.deerPOLY<-homerangeALL[homerangeALL@data$id=="Red Deer",]
bow.valleyPOLY<-homerangeALL[homerangeALL@data$id=="Bow valley",]

#generate 1000 points from Red Deer wolf pack KDE polygon
rd.avail<-spsample(red.deerPOLY, 1000, "random")
plot(rd.avail)

#generate 1000 points from Bow valley wolf pack KDE polygon
bv.avail<-spsample(bow.valleyPOLY, 1000, "random")
plot(bv.avail)

#Plot sample availability of both packs 
plot(wolfyht$EASTING,wolfyht$NORTHING, col=c("red","blue")[wolfyht$PackID],ylab="Northing",xlab="Easting")
legend(555000,5742500,unique(wolfyht$Pack),col=c("blue","red"),pch=1)
plot(bv.avail, add=TRUE)
plot(rd.avail, add=TRUE)

##Create data frame of Used/Available Locations---------------------------------

#create df for used locations per pack

rdused_noCOV <- rd
rdused_noCOV$pack <- c("Red Deer")

bvused_noCOV <- bv
bvused_noCOV$pack <- c("Bow Valley")

wolfused_noCOV <- merge(rdused_noCOV, bvused_noCOV, all.x = TRUE, all.y = TRUE)
str(wolfused_noCOV)


head(wolfused_noCOV)


#add new column for 1 = used, 0 = avail
wolfused_noCOV$used <- 1
  
  
#Create df for available data per pack 

rdavail <- as.data.frame(rd.avail)
rdavail$pack <- c("Red Deer")

#repeat for Bow Valley pack
bvavail <- as.data.frame(bv.avail)
bvavail$pack <- c("Bow Valley")

## merge the two availability samples together
wolfavail_noCOV <- rbind(rdavail, bvavail)

## and for next week, lets add a new column for a 1=used 0 = avail
wolfavail_noCOV$used <- 0

##combine df's for used and available 
wolfkde_noCOV <- rbind(wolfused_noCOV, wolfavail_noCOV)
str(wolfkde_noCOV)
  

  
  
  
  
