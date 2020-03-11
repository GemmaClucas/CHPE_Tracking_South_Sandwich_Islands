
##load required libraries
library(crawl)   #to fit Kalman filter models
library(trip)    #to prepare GPS data
library(maptools)
library(sp)
library(rgdal)

#read example data
both<-read.csv("adels_chins_all.csv")

# read chinstrap data update 0700 6/1/20 --- we put it in a new script!!
#both<-read.csv("chinstrap_data.csv")

# Don't need this section for now
#read in south orkney shapefile
SO<-readShapePoly("C:/Users/vicrwi/Documents/bas 4th april/3rd paper/gis/Land_Project.shp")

#SO<-readShapePoly("C:/Users/vicrwi/Documents/3rd paper/gis/Land_Project.shp")
plot(SO,xlim=c(-54,-27),ylim=c(-76,-54))

#REPROJECT so
#first tell it that it is in wgs1984
proj4string(SO)<-CRS("+proj=longlat +ellps=WGS84")
#then reprojext to Lambert azimuthal equal area
SOreproj<-spTransform(SO,CRS=CRS("+proj=laea +lon_0=-40 +lat_0=-90 +units=m"))

#######################################################################
### Prepare GPS data for analysis         #############################
#######################################################################

#format times
both$Time<-as.POSIXct(strptime(both$Date, "%d-%b-%Y %H:%M:%S"), "GMT")


head(both)
#change times in both files to hours since first GPS fix
both$Time2<-as.numeric(difftime(both$Time,min(both$Time),units="hours"))
## remove completely-duplicated rows
both<- both[!duplicated(both), ]
colnames(both)[names(both)=="Loc.Class"]<-"Argos_loc_class"

## we want to do the crawl stuff for each trip at a time so first split to just one trip
unique(both$Dep.Id)

# just select one ID here
x1<-both[both$Dep.Id==1248,]

x1<-both[both$Dep.Id==1299,]
#head(x1)

#This is where it will be different for gls data/. I used PTT data which have error classes, 
#sometimes it only works if we remove b class fixes but check it still covers same range and extent as full data set
x1<-x1[x1$Argos_loc_class!="B",]
plot(x1$Lon1,x1$Lat1)

plot(x1$Lon1,x1$Lat1,ylim=c(-61,-60.4),xlim=c(-46,-44),pch=20,col="red")
plot(SO, col="gray", add=T)  #add map
#if there are lods of points around land, it soemtimes doesnt work, so it might be worth cutting these off


x1 <- x1[order(x1$Time), ]

## fudge duplicated times
x1$Time<- adjust.duplicateTimes(x1$Time, x1$Dep.Id)

#make the location classes a factor
x1$Argos_loc_class <- factor(x1$Argos_loc_class,  
                            levels=c("0","1","2","3", "A")) 
#x1$Argos_loc_class <- factor(x1$Argos_loc_class,  
#                 levels=c("0","1","2","3", "A","B")) 
#####################################################################
##Apply McConnell speed filter in trip package to remove duff fixes #
#####################################################################
x2<-data.frame(lat=x1$Lat1,lon=x1$Lon1,Date=x1$Time,id=x1$Dep.Id,dry=x1$Land)

#Create coordinate variable
coordinates(x2) <- c("lon","lat")

#create trip object
tr <- trip(x2,c("Date","id"))

#McConnell Speed filter; ignore coordinates warning as data are lonlat
x1$Filter <- speedfilter(tr, max.speed = 8)

#remove filtered coordinates
x1<-subset(x1,x1$Filter==TRUE)
head(x1)
#create dataframe with only the required data
xy<-data.frame(longitude=x1$Lon1,latitude=x1$Lat1,time=x1$Time2,id=x1$Dep.Id,
               Argos_loc_class=x1$Argos_loc_class,dry=x1$Land)
head(xy)
tail(xy)

xy$time<- adjust.duplicateTimes(xy$time, xy$id)

###crawl doesnt work on lat and long data, it needs to be projected. for me I used lambert azimuthal equal area, centered at -40 -90 but you might want a different projection

coordinates(xy)<-~longitude+latitude

#first tell it that it is in wgs1984
proj4string(xy)<-CRS("+proj=longlat +ellps=WGS84")
#then reproject it to laea
# THIS WILL NEED TO BE CHANGED -> should be ok to just center on long and lat of Saunders or maybe change to UTM zones - check this
xy<-spTransform(xy,CRS=CRS("+proj=laea +lon_0=-40 +lat_0=-90 +units=m"))


initial = list( 
  a=c(coordinates(xy)[1,1],0,0, 
      coordinates(xy)[1,2],0,0), 
  P=diag(c(10000^2,5400^2,5400^2,10000^2,5400^2,5400^2))  # where do these numbers come from??? i still dont know! but it seems to work
) 


fixPar = c(log(250), log(500), log(1500), rep(NA,7)) 
#These are the location error estimates already known for ARGOS data (google argos location errors). SO here we are fixing the parameters for the error
#classes taht we already know (eg class 3 errors <250 m ) 
  #and then repeat na the number of parameter for which you are estimating values (in this case 7). 
displayPar( mov.model=~1, err.model=list(x=~Argos_loc_class-1), drift = TRUE, 
           data=xy,fixPar=fixPar) 

#IF I DROP LOC CLASS "B" THERE IS ONE FEWER PARAM TO ESTIMATE
fixPar = c(log(250), log(500), log(1500), rep(NA,6)) #I think these are the location estimates already known for ARGOS data (google argos location errors)
displayPar( mov.model=~1, err.model=list(x=~Argos_loc_class-1), drift = TRUE, 
            data=xy,fixPar=fixPar) 



#IF I GET MESSAGE ABOUT FIXPAR IS NOT THERIGHT LENGTH IT IS because we are trying to estimate the location error for the fixes with error classes
#A or B, which dont have enough messages to estimate the accuracy (for argos loc class 0-3 an accuracy will have been estimated so we dont need to estimate one
#so as the northern fur sea example didnt have any B loc classes, it is estimateing a different number of parameters. so we have one more parameter toestimate
#(class b), so need one more paprameter in the model.

#you can constrain the parameter options here. in this case I have constrained the error of location clsses a and b to log(1500)as that is the lowest accuracy of the other classes
# I should read up on these error classes!
constr=list( 
  lower=c(rep(log(1500),3), rep(-Inf,4)), 
  upper=rep(Inf,7) 
) 
#AGAIN IF i DROP B THERE IS ONE LESS PARAMETER TO ESTIMATE
constr=list( 
  lower=c(rep(log(1500),2), rep(-Inf,4)), 
  upper=rep(Inf,6) 
) 

ln.prior = function(theta){-abs(theta[4]+3)/0.5} 
#you can set priors, but i didnt actually use this as I didnt understand where the numbers came from (this was from an example in the literature)

if(exists("fit")){rm(fit)} 

set.seed(321) 
fit1 <- crwMLE( 
  mov.model=~1, err.model=list(x=~Argos_loc_class-1), drift=T, 
  data=xy, Time.name="time",  #method="L-BFGS-B",
  initial.state=initial, fixPar=fixPar, constr=constr,#prior=ln.prior,#,#  
  control=list(trace=1, REPORT=1) 
) 
#fit2
fit1 

#make new data to predict into, at regular time intervals
predTime <- seq(ceiling(min(xy$time)), floor(max(xy$time)), 1/12) #create time intervals: 5 mins here
predTime <- seq(min(xy$time), max(xy$time), 1/12) #create time intervals: 5 mins here, Vicky prefers not to round using the ceiling and floor functions

predObj <- crwPredict(object.crwFit=fit1, predTime, speedEst=TRUE, flat=TRUE) #simulate locations
#plot map of best fit track (red line) and locations at times of original fixes (blur dots)
par(mfrow=c(1,1))
crwPredictPlot(predObj, 'map')
plot(SOreproj, col="gray", add=T)  #add map
#THERE ARE WAYS TO SIMULATE MULTIPLE LINES INT H ORIGIANL CODE FROM NORM (DOCUMENTS/CRAWL/RCODE)BUT DONT THINK ITS NECESSARY
#head(predObj)
#remove observed locations and create dataframes with columns of interest
bf_track<-subset(predObj,predObj$locType=="p")
bf_track<-data.frame(Time=predTime,Lon=bf_track$mu.x,Lat=bf_track$mu.y)
plot(predObj$mu.x[predObj$locType=="p"],predObj$mu.y[predObj$locType=="p"])
#head(bf_track)
#plot(bf_track$mu.x,bf_track$mu.y)


#plot tracks (best-fit in red and raw in grey) as line
plot(bf_track$Lon,bf_track$Lat,cex=0.5,pch=16,col="blue",axes=T,xlab="Lon",ylab="Lat")
lines(bf_track$Lon,bf_track$Lat,col="red")
plot(SOreproj, col="NA", add=T)  #add map
#plot(SO)
#project tr
###tryy reprojecting to laea
#coordinates(tr)<-~longitude+latitude
#first tell it that it is in wgs1984
proj4string(tr)<-CRS("+proj=longlat +ellps=WGS84")
#then reproject it to laea
tr<-spTransform(tr,CRS=CRS("+proj=laea +lon_0=-40 +lat_0=-90 +units=m"))
lines(tr,col="gray")#this shows if you just join the dots I think

#to get a sensible time stamp you need to know the lowest time value form the data frame where you changed it to numeric, 
#then put that aws the origin
t.gmt <- as.POSIXct(3600 * (bf_track$Time ), origin = '2012-01-09 16:27:20') 
bf_track$Time2 <- as.POSIXct(format(t.gmt),"%d-%b-%Y %H:%M:%S",timezone= "GMT")
head(bf_track)

write.csv(bf_track,paste("C:/Users/vicrwi/Documents/bas 4th april/3rd paper/crawl outputs drift/adelies/1248_track.csv",sep=""),row.names=F)





