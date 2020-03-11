
setwd("~/Dropbox/South_Sandwich_2020/CHPE_tracking_analyses")

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
# replot as WGS84 long lat for use later on
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

## enter the PTT number on the command line
pick_a_penguin <- function(){
  x <- readline("Pick a PTT number: ")  
  return(x)
}

penguin <- pick_a_penguin()

# just select one ID here
x1<-both[both$PTT == penguin, ]

#x1<-both[both$PTT==196697,]
#x1<-both[both$PTT==196701,]
#x1<-both[both$PTT==196707,]
#x1<-both[both$PTT==196716,]
#x1 <- both[both$PTT==196713,]
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

# This bit won't work for now as the data and map are not in the same projection yet
#plot(SSIreproj, add=T)
#plot(SSIreproj, xlim=c(-28,-24), ylim=c(-59,-56))

# Just take the first 40 points as an approximate of a trip 
#x1<-x1[1:40,]
# or leave it commented out to do all of them
plot(x1$LON,x1$LAT)

#plot(x1a$LON,x1a$LAT,ylim=c(-61,-60.4),xlim=c(-46,-44),pch=20,col="red")
#plot(SO, col="gray", add=T)  #add map, we don't have a map atm
#if there are lods of points around land, it soemtimes doesnt work, so it might be worth cutting these off


x1 <- x1[order(x1$Time), ]

## fudge duplicated times
x1$Time<- adjust.duplicateTimes(x1$Time, x1$PTT)
table(x1$Argos_loc_class)

#make the location classes a factor
# this is the original command
#x1$Argos_loc_class <- factor(x1$Argos_loc_class,  
#                             levels=c("0","1","2","3", "A")) 
# This is how I think it should be based on the fact that 3 has the least error and 0 the most
x1$Argos_loc_class <- factor(x1$Argos_loc_class,  
                             levels=c("3","2","1","0", "A")) 

#x1$Argos_loc_class <- factor(x1$Argos_loc_class,  
#                 levels=c("0","1","2","3", "A","B")) 


###########################################################################################
##            Apply McConnell speed filter in trip package to remove duff fixes           #
###########################################################################################

# make a new data frame
x2<-data.frame(lat=x1$LAT,lon=x1$LON,Date=x1$Time,id=x1$PTT)

#Change it into class SpatialPointsDataFrame 
coordinates(x2) <- c("lon","lat")

#create trip object
tr <- trip(x2,c("Date","id"))

#McConnell Speed filter; ignore coordinates warning as data are lonlat
x1$Filter <- speedfilter(tr, max.speed = 8)

#remove filtered coordinates
x1<-subset(x1,x1$Filter==TRUE)
head(x1)

#create dataframe with only the required data
xy<-data.frame(longitude = x1$LON, latitude = x1$LAT, time = x1$Time2, id = x1$PTT,
               Argos_loc_class = x1$Argos_loc_class)
head(xy)
tail(xy)

xy$time<- adjust.duplicateTimes(xy$time, xy$id)

#################################################################################
##                           Reproject the fixes                                #
#################################################################################

###crawl doesnt work on lat and long data, it needs to be projected. 
# for me I used lambert azimuthal equal area

coordinates(xy) <- ~longitude + latitude

#first tell it that it is in wgs1984
proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84")
# then reproject it to laea
# just centered on long and lat of Saunders for now, maybe change to UTM zones - check this
xy <- spTransform(xy, CRS = CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m"))
# we can now plot it with our laea projection of the map
plot(xy, axes=T)
plot(SSI_laea, add=T)

##### KEEP READING ABOUT ERRORS (PRIORS) AND INITIAL CONDITIONS
# https://jmlondon.github.io/crawl-workshop/crawl-practical.html#determining-your-model-parameters

#################################################################################
######                         Initial params                                ####
#################################################################################

# Initiate with the first co-ordinate, which should be at the colony
#initial = list( 
#  a=c(coordinates(xy)[1,1],0,0, 
#      coordinates(xy)[1,2],0,0), 
#  P=diag(c(10000^2,5400^2,5400^2,10000^2,5400^2,5400^2))
  # Me: where do these numbers come from??? Vicky: i still dont know! but it seems to work
 

# From the pragmatic guide to crawling and modified for our data structure
initial = list(a = c(coordinates(xy)[1,1], 0,
                     coordinates(xy)[1,2], 0),
               P = diag(c(10 ^ 2, 10 ^ 2,
                10 ^ 2, 10 ^ 2)))


#################################################################################
######                   Add location error params                           ####
#################################################################################


# The next thing is to add the location error estimates which are known for the different classes
#These are the location error estimates already known for ARGOS data (google argos location errors). SO here we are fixing the parameters for the error
#classes that we already know (we are assuming the vairance is the same in the x and y directions, which is actually not true)
# and then repeat na the number of parameter for which you are estimating values (in this case 6). 
# fixPar = c(log(250), log(500), log(1500), rep(NA,6))
# displayPar( mov.model=~1, err.model=list(x=~Argos_loc_class-1), drift = TRUE,
#            data=xy,fixPar=fixPar)

# Use this if you keep B errors since there is one more variance parameter needed
#fixPar = c(log(250), log(500), log(1500), rep(NA,7)) 
#displayPar( mov.model=~1, err.model=list(x=~Argos_loc_class-1), drift = TRUE, 
#            data=xy,fixPar=fixPar) 


# you can constrain the parameter options here. in this case I have constrained the error of location clsses a and b to log(1500)as that is the lowest accuracy of the other classes
#constr=list( 
#  lower=c(rep(log(1500),3), rep(-Inf,4)), 
#  upper=rep(Inf,7) 
#) 
# AGAIN IF i DROP B THERE IS ONE LESS PARAMETER TO ESTIMATE
# constr=list( 
#   lower=c(rep(log(1500),2), rep(-Inf,4)), 
#   upper=rep(Inf,6) 
# ) 

### From the pragmatic guide:

# The second option is to provide a prior distribution for each of the location quality classes. 
# The crawl::crwMLE() function accepts a function for the ‘prior’ argument. In this example, 
# we provide a normal distribution of the log-transformed error. The standard error of 0.2

# Previous documentation and examples that described a setup for ‘crawl’ often suggested users implement a 
# mixed approach by providing both fixed values and constraints to optimize the fit and increase the model’s 
# ability to converge with limited/challenging data. We now suggest users rely on prior distributions to 
# achieve a similar intent but provide the model more flexibility. Users should feel free to explore various 
# distributions and approaches for describing the priors (e.g. laplace, log-normal) based on their data and 
# research questions.

# In addition to prior distributions for the location quality classes, we can also provide a prior distribution 
# for the beta parameter. We suggest a normal distribution with a mean of -4 and a standard deviation of 2. 
# This encourages the model to fit a smoother track unless the data warrant a rougher, more Brownian, path.

prior <-  function(p) { 
  dnorm(p[1], log(250), 0.2 , log = TRUE) + # class 3
  dnorm(p[2], log(500), 0.2 , log = TRUE) + # class 2
  dnorm(p[3], log(1500), 0.2, log = TRUE) + # class 1
  dnorm(p[4], log(2500), 0.4 , log = TRUE) + # class 0
  dnorm(p[5], log(2500), 0.4 , log = TRUE) + # class A
  # skip p[6] as we won't provide a prior for sigma
  dnorm(p[7], -4, 2, log = TRUE)
}


if(exists("fit")){rm(fit)} 

fit1 <- crwMLE( 
  mov.model = ~1, 
  err.model=list(x=~Argos_loc_class-1), 
  drift=T, 
  data=xy, 
  Time.name="time",  #method="L-BFGS-B",
  initial.state=initial, 
  #fixPar=fixPar, constr=constr,
  prior=prior, 
  control=list(trace=1, REPORT=1) 
) 
#fit2
fit1 

#make new data to predict into, at regular time intervals
#predTime <- seq(ceiling(min(xy$time)), floor(max(xy$time)), 1/12) #create time intervals: 5 mins here
predTime <- seq(min(xy$time), max(xy$time), 1/12) #create time intervals: 5 mins here, Vicky prefers not to round using the ceiling and floor functions

predObj <- crwPredict(object.crwFit=fit1, predTime, speedEst=TRUE, flat=TRUE) #simulate locations
head(predObj)
#plot map of best fit track (red line) and locations at times of original fixes (blur dots)
par(mfrow=c(1,1))
#crwPredictPlot(predObj)
# doens't work, don't know why
#plot(SSI_laea, col="gray", add=T)  #add map
#THERE ARE WAYS TO SIMULATE MULTIPLE LINES INT H ORIGIANL CODE FROM NORM (DOCUMENTS/CRAWL/RCODE)BUT DONT THINK ITS NECESSARY
#head(predObj)
#remove observed locations and create dataframes with columns of interest
bf_track <- subset(predObj, predObj$locType=="p")
bf_track <- data.frame(Time = predTime, Lon = bf_track$mu.x, Lat = bf_track$mu.y)
plot(predObj$mu.x[predObj$locType == "p"], predObj$mu.y[predObj$locType == "p"])
#head(bf_track)
#plot(bf_track$mu.x,bf_track$mu.y)


#plot tracks (best-fit in red and raw in grey) as line
plot(bf_track$Lon, bf_track$Lat, cex=0.5, pch=16, col="blue", axes=T, xlab="Lon", ylab="Lat")
lines(bf_track$Lon, bf_track$Lat, col="red")
plot(SSI_laea, col="NA", add=T)  #add map
# add the original tracking data
proj4string(tr)<-CRS("+proj=longlat +ellps=WGS84")
#then reproject it to laea
tr<-spTransform(tr,CRS=CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m"))
lines(tr,col="gray")#this shows if you just join the dots I think

#to get a sensible time stamp, you need to know the lowest time value from the data frame where you changed it to numeric, 
#then put that as the origin

# get origin time
# NEED TO CHANGE ORIGIN TO FIRST VALUE FOR EACH BIRD
#t.gmt <- as.POSIXct(3600 * (bf_track$Time ), origin = '2020-01-06 18:31:45', tz="GMT") 
#head(t.gmt)
t.gmt <- as.POSIXct(3600 * (bf_track$Time ), origin = x1$Time[1], tz="GMT") 
head(t.gmt)
bf_track$Time2 <- as.POSIXct(format(t.gmt),"%Y-%m-%d %H:%M:%OS",tz= "GMT") 
head(bf_track)

# write to csv
write.csv(bf_track, paste0(penguin, "_track.csv", sep = ""), row.names = FALSE)

# save plot to jpg
jpeg(file = paste0(penguin, "_track.jpg", sep = ""), 
     height = 4, 
     width = 6,
     units = "in",
     res = 72)
par(mar=c(5,5,1,1))
plot(bf_track$Lon, 
     bf_track$Lat, 
     cex=0.5, 
     pch=16, 
     col="blue", 
     axes=T, 
     xlab="Lon", 
     ylab="Lat")
lines(bf_track$Lon, 
      bf_track$Lat, 
      col="red")
plot(SSI_laea, 
     col="NA", 
     add=T)  
lines(tr,
      col="gray")
dev.off()


# I can't get rid of the loopy bits on 196702
# I tried using fixPar with constraints
# Using priors resulted in NAs the whole time

# old
# write.csv(bf_track,paste("196701_track.csv",sep=""),row.names=F)
# write.csv(bf_track,paste("196707_track.csv",sep=""),row.names=F)
# write.csv(bf_track,paste("196716_track.csv",sep=""),row.names=F)
# write.csv(bf_track,paste("196713_track.csv",sep=""),row.names=F) # correct time
# Check which ones have the correct times 



# sessionInfo()

# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.3

# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] ggplot2_3.2.1  raster_3.0-12  rgdal_1.4-8    maptools_0.9-9 sp_1.3-2       trip_1.6.0     crawl_2.2.1   
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_0.2.5      purrr_0.3.3           splines_3.6.1         lattice_0.20-38       geodist_0.0.3        
# [6] colorspace_1.4-1      spatstat.utils_1.15-0 htmltools_0.4.0       mgcv_1.8-28           rlang_0.4.2          
# [11] spatstat.data_1.4-3   later_1.0.0           pillar_1.4.2          withr_2.1.2           spatstat_1.63-0      
# [16] foreign_0.8-71        glue_1.3.1            lifecycle_0.1.0       rgeos_0.5-2           munsell_0.5.0        
# [21] gtable_0.3.0          mvtnorm_1.0-12        codetools_0.2-16      labeling_0.3          fastmap_1.0.1        
# [26] httpuv_1.5.2          Rcpp_1.0.3            xtable_1.8-4          tensor_1.5            promises_1.1.0       
# [31] scales_1.1.0          abind_1.4-5           farver_2.0.1          mime_0.7              deldir_0.1-25        
# [36] digest_0.6.23         dplyr_0.8.3           shiny_1.4.0           polyclip_1.10-0       grid_3.6.1           
# [41] tools_3.6.1           magrittr_1.5          goftest_1.2-2         lazyeval_0.2.2        tibble_2.1.3         
# [46] crayon_1.3.4          pkgconfig_2.0.3       MASS_7.3-51.4         Matrix_1.2-17         assertthat_0.2.1     
# [51] R6_2.4.1              rpart_4.1-15          nlme_3.1-140          compiler_3.6.1       
