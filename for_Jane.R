##load required libraries
library(crawl)   #to fit Kalman filter models
library(trip)    #to prepare GPS data
library(maptools)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)

# read chinstrap data, updated 0700 6/1/20 
both <- read.csv("chinstrap_data.csv")

#read in newest SGSSI shapefile
Seamask<-readOGR("Seamask.shp")
# It takes a long time to plot the whole map
#plot(Seamask,axes=T)
# crop to SSI (this gives a warning but it works)
e<-extent(450000,750000, -600000, 0)
SSI<-crop(Seamask,e)
plot(SSI,axes=T)


# check what it's projected as
crs(SSI)
proj4string(SSI)<-CRS("+init=epsg:3762 +proj=lcc +lat_1=-54 +lat_2=-54.75 +lat_0=-55 +lon_0=-37 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0")
#then reproject to Lambert azimuthal equal area
SSI_laea<-spTransform(SSI, CRS=CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m"))
plot(SSI_laea, axes = T)
# reproject as WGS84 long lat for use later on
SSI_WGS84<-spTransform(SSI_laea, CRS="+proj=longlat +ellps=WGS84")

#######################################################################
###                 Prepare GPS data for analysis                   ###
#######################################################################

#format times
both$Time <- as.POSIXct(strptime(both$DATETIME, "%Y-%m-%d %H:%M:%S"), "GMT")
head(both)

#change times in both files to hours since first fix
both$Time2<-as.numeric(difftime(both$Time, min(both$Time), units="hours"))
head(both)

## remove completely-duplicated rows
both<- both[!duplicated(both), ]
colnames(both)[names(both)=="LC"]<-"Argos_loc_class"
head(both)

#######################################################################
###                       Pick a penguin                            ###
#######################################################################


## we want to do the crawl stuff for each bird at a time so first split to just one bird (one PTT tag number)
unique(both$PTT)

pick_a_penguin <- function(){
  x <- readline("Pick a PTT number: ")  
  return(x)
}

## enter the PTT number on the command line
penguin <- pick_a_penguin()

# just select one ID here
x1<-both[both$PTT == penguin, ]
head(x1)


#######################################################################
###                      Add error classes                          ###
#######################################################################


# This is where it will be different for gls data/. I used PTT data which have error classes, 
# sometimes it only works if we remove B class fixes but check it still covers same range and extent as full data set
# For this individual, there are no B classes so I commented out these 
summary(x1$Argos_loc_class)
#x1<-x1[x1$Argos_loc_class!="B",]

# View the data points
par(mar=c(5,5,1,1))
plot(x1$LON,x1$LAT)
plot(SSI_WGS84, col="NA", add=T)
