
# Then remove land from these points and split into trips
library(maptools)
library(sf)
library(patchwork)
library(ggspatial)
library(purrr)

# List files to later run in loop - update this later
#birds<-list.files("C:/Users/vicrwi/Documents/Signy2013/Tracking data/raw/reformated")
#birds<-unlist(strsplit(birds,"[.]"))
#birds<-birds[nchar(birds)>3]

#head(birds)
#birds[21]
#k<-birds[2]
# we need to read in the new csv crawl outputs, but we need to convert tehm to shapefiles with the same projection as the maps
#for (k in birds){
#  r1b1<-read.csv(paste("C:/Users/vicrwi/Documents/Signy2013/Tracking data/predicted without raw/",k,".drift.predtrack.csv",sep=""))
#  head(r1b1)
  #first project teh track, so need to make it spatial
  #make into spatial points object
  #trac<-r1b1
  
trac_laea<-bf_track
coordinates(trac_laea)<-~Lon+Lat
  
  # it's already in projected format
  proj4string(trac_laea)<-CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m")
  #then reproject it to laea
  #traclaea<-spTransform(trac,CRS=CRS("+proj=laea +lon_0=-40 +lat_0=-90 +units=m"))
  #then plot the land
  #plot(ork)
  par(mar=c(1,1,0,0))
  plot(SSI_laea, xlim=c(-30000,20000), ylim=c(-5000, 60000), axes=T)
  #then add the track
  plot(trac_laea,cex=0.1,col="dodgerblue",add=T)#,cex=0.1,col="dodgerblue")
  
  # # zoom in on the colony
  # plot(SSI_laea, xlim=c(-25000,-20000), ylim=c(18000, 27000), axes=T)
  # #then add the track
  # plot(trac_laea,cex=0.1,col="dodgerblue",add=T)#,cex=0.1,col="dodgerblue")
  # 
  
  ###################################################################################
  ### Add buffer around island and remove points that fall within the boundaries  ###
  ###################################################################################
  
  crs(trac_laea)
  #inside.park <- !is.na(over(trac_laea, as(SSI_laea, "SpatialPolygons")))
  # create 1000m buffer around island
  SSI_laea_buffer <- buffer(SSI_laea, width=-3000)
  inside.park <- !is.na(over(trac_laea, SSI_laea_buffer))
  # check by plotting
  plot(SSI_laea, xlim=c(-30000,20000), ylim=c(-5000, 60000), axes=T)
  plot(trac_laea,cex=0.1,col="dodgerblue",add=T)#,cex=0.1,col="dodgerblue")
  points(trac_laea[!inside.park, ], pch=1, col="gray")
  points(trac_laea[inside.park, ], pch=16, col="red")
  r1b1noland<-trac_laea[inside.park, ]
  #PROJECT IT BACK INTO WGS84
  r1b1noland1<-spTransform(r1b1noland,CRS=CRS("+proj=longlat +ellps=WGS84"))
  plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  plot(r1b1noland1,add=T)
  
  
  ##write shapefiles for the tracks with no land
  #writeOGR(r1b1noland1,dsn="/Users/gemmaclucas/Dropbox/South_Sandwich_2020/CHPE_tracking_analyses/",layer=paste0(penguin, "_noland_WGS84"),driver="ESRI Shapefile")
  
  ###################################################################################
  ###                           Split into trips                                  ###
  ###################################################################################
  
  
  #write a function for getting values from row above
  rowShift <- function(x, shiftLen = 1L) {
    rr <- (1L + shiftLen):(length(x) + shiftLen)
    rr[rr<1] <- NA
    return(x[rr])
  }
  
  # make a column of whether its at sea
  all<-trac_laea
  all$sea <- !is.na(over(trac_laea, SSI_laea_buffer))
  head(all)
  # # Check whether the sea column is working with a couple of quick plots
  all <- spTransform(all,CRS=CRS("+proj=longlat +ellps=WGS84"))
  plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  plot(all, add=T)
  plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  plot(all[all$sea == TRUE,], add=T,)
  
  # now, keep just the points at sea i.e. where sea == TRUE
  all <- all[all$sea == TRUE,]
  # calculate the cumulative lag from time zero to the current time
  all$lag1<-rowShift(all$Time,-1)
  # calculate differences between each lag (we can use these differences to distinguish the end of one trip and the start of the next as there will be a long lag)
  all$diff1<-all$Time-all$lag1
  head(all,100)
  # put TRUE in all$Start_trip column where the difference between the previous point and this one is greater than 30 mins
  # i.e. it spent 30 mins on land
  all$Start_trip <- all$diff1 >= 0.5
  # change the "NA" at the beginning of the first trip to "TRUE"
  all$Start_trip[1] <- TRUE
  # now there are eighteen Start_trips, this sounds about right
  
  # In order to plot the final trip, I need to fudge the final line of all and give it Start_Trip == TRUE
  # So that I can use Start_row_indexes to plot this final trip
  all[length(all), "Start_trip"] <- TRUE
  
  # Store row numbers of each Start_trip in a list which we will use to split by trip
  Start_row_indexes <- as.list(which(all$Start_trip == TRUE))
  
  # # Plot all trips (using ggplot)
  # ggplot() +
  #   ggspatial::layer_spatial(data = SSI_WGS84) +
  #   xlim(c(-27,-25.6)) +
  #   ylim(c(-58.2,-57.4)) +
  #   ggspatial::layer_spatial(data = all)
  # 
  
  # # Use row indexes to separate trips and then plot (standard plotting)
  # # Trip 1
  # plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  # plot(all[c(Start_row_indexes[[1]]:Start_row_indexes[[2]]-1), ], add=T)
  # # Trip 2
  # plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  # plot(all[c(Start_row_indexes[[2]]:Start_row_indexes[[3]]-1), ], add=T)
  # # Trip 3
  # plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  # plot(all[c(Start_row_indexes[[3]]:Start_row_indexes[[4]]-1), ], add=T)
  # # Trip 4
  # plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  # plot(all[c(Start_row_indexes[[4]]:Start_row_indexes[[5]]-1), ], add=T)
  # # Trip 5
  # plot(SSI_WGS84, xlim=c(-26.6,-25.6), ylim=c(-58,-57.6))
  # plot(all[c(Start_row_indexes[[5]]:Start_row_indexes[[6]]-1), ], add=T)
  
  # But I want to plot all 17 trips at once
  
  # Make a function for plotting
  trip_plot_fun = function(x) {
    ggplot() +
      ggspatial::layer_spatial(data = SSI_WGS84) +
      xlim(c(-27,-25.6)) +
      ylim(c(-58.2,-57.4)) +
      ggspatial::layer_spatial(data = all[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1), ])
  }
  
  
  
  seq <- c(1:(length(Start_row_indexes) -1))
  # This map() function  takes the numbers in seq and applies them to the trip_plot_fun() in turn
  plots = purrr::map(seq, ~trip_plot_fun(.x))
  
  # plot with patchwork
    plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + 
    plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]] +
    plots[[9]] + plots[[10]] + plots[[11]] + plots[[12]] +
    plots[[13]] + plots[[14]] + plots[[15]] + plots[[16]] 
  
  # 6 bad trip
  # 8 bad trip
  # 13 bad trip
  # 15 bad trip
  # 18 two trips in one - make buffer bigger - 3km seems to sort out problems with splitting trips
    
   ### Need to figure out how to handle the final trip since it doesn't have a Start_trip_row to finish on
    ## add the row index of the final row to the start row indexes?
  #########################################################################################################

  

  
  ##### We got up to this point when I was at BAS but I don't know that I need much of what is below anymore
  
  
  
  #FIRST GET RID OF BOTH IF MADE IN PREVIOUS RUNS
  if (exists ("both")){remove(both)}
  ####now try and keep unfinished trips over 1 hr
  #get the last row of the data which moved between lnand and sea
  en<-ok[nrow(ok),]
  #if the trip ended at sea
  if ( en$land == 0 ){
    #take the last row of change from land to sea (ie when it left the colony), and bind it with the last row of all points
    #ie when the battery  ran out
    lastpoint<-all[nrow(all),]
    both<-rbind(en[1:8],lastpoint)
    #now check that the trip was over an hr by taking the time difference between the points, and if it was, bind it to ok
    both$lag<-rowShift(both$Time,-1)
    both$diff<-both$Time-both$lag
    both$Trip<-(both$diff) > 1
    ok<-rbind(ok, both)
  }
  
  
  #identify whihc line is the end of the trip, then loop through the file to 
  end<-which(ok$Trip==TRUE & ok$land==1)
  
  trips<-data.frame()
  if (length(end) > 0){
    #the length of end files is the number of trips, whereas teh value is the row which is the end of the trip
    for (ii in 1:length(end)){
      #so first subset teh end list to get the first row
      a<-end[ii]
      #then select the row which has the end of the trip, and the row before
      bb<-ok[c(a-1,a),]
      #then give it a trip number 
      bb$TR<-ii
      #and bind it to a data frame so we should end up with two rows of trip 1, two rows of trip2 etc
      trips<-rbind(trips,bb)
      
    }#end of for ii in end loop
  }#end of if end >0 loop
  
  if (exists ("both")){
    start<-which(both$Trip==TRUE & both$land==0)
    if (length(start > 0)){
      for (kk in 1:length(start)){
        #so first subset teh start list to get the first row
        a<-start[kk]
        #then select the row which has the end of the trip, and the row before
        bb<-both[c(a-1,a),]
        
        #then give it a trip number 
        if(length(trips)>0){
          bb$TR<-(trips$TR[nrow(trips)])+kk
        }
        
        if(length(trips)==0){bb$TR<-kk}
        #and bind it to a data frame so we should end up with two rows of trip 1, two rows of trip2 etc
        trips<-rbind(trips,bb)
      }#end of 1: length start
    }#end of length start
  }#end of if exists both
  
  if(length(trips)>0){
    trips<-trips[,c(1:6,12)]
    
    #then merge the ok file with the original data file
    m<-merge(all,trips,by="Time",all.x=T)
    #change the nas from trip number to 0
    m$TR[ is.na(m$TR) ] <- 0
    
    
    #then run a loop to identify the trips
    e<-data.frame()
    #first set a value for t, this will be zero, as its the trip number from teh first gps reading which will usually still be in the colony
    t<-m$TR[1] 
    
    #then loop through the file
    for (i  in 1:nrow(m)){
      #subset each row individually
      d<-m[i,] 
      #if the number of the trip for that row is higher than the stored t then change the value of t
      if (d$TR> t){
        t<-d$TR
      }
      #then assign a trip number to the row and bind it back together
      d$trip1<-t
      e<-rbind(e,d)
    }
    
    head(e)
    #then subset it so we just get the bits not on land
    f<-e[e$land.x==0,]
    f<-f[,c(1:6,15)]
    head(f)
    # f$trip1<-1
    
    #sometimes we get a row of trip zero left over, so get rid of this
    
    f<-f[f$trip1!=0,]
    write.csv(f,paste("C:/Users/vicrwi/Documents/Signy2013/Tracking data/trips/",k,"trips.csv",sep=""))
    
    #then seperate into trips
    g<-unique(f$trip1)
    for (l in g){
      h<-subset(f,f$trip1==l)
      write.csv(h,paste("C:/Users/vicrwi/Documents/Signy2013/Tracking data/sep trips/",k,"t",l,".csv",sep=""))
    }
    
  }}











##################################################################################


####