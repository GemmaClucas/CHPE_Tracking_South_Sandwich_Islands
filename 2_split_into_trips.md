Split into trips
================
Gemma Clucas
4th January 2021

``` r
library(maptools)
library(sf)
library(patchwork)
library(ggspatial)
library(purrr)
library(rgdal)
library(raster)
library(plyr)
library(tidyverse)
library(sp)
library(ggplot2)
library(knitr)
library(spdplyr)
options(scipen=999)
```

### Load map and plot

``` r
Seamask<-readOGR("Seamask.shp")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/gemmaclucas/GitHub/CHPE_Tracking_South_Sandwich_Islands/Seamask.shp", layer: "Seamask"
    ## with 1 features
    ## It has 1 fields

``` r
#SSI <- crop(Seamask, c(450000, 750000, -600000, -100000)) # the original values I used here were cropping the end of the tracks when I filtered for the points off land, so I increased the extent of this base map to prevent that
SSI <- crop(Seamask, c(0, 1000000, -1000000, -100000))
```

    ## Warning in RGEOSUnaryPredFunc(spgeom, byid, "rgeos_isvalid"): Ring Self-
    ## intersection at or near point 77954.359424359995 26605.230663620001

    ## x[i, ] is invalid

    ## Warning in rgeos::gIntersection(x[i, ], y, byid = TRUE, drop_lower_td = TRUE):
    ## Invalid objects found; consider using set_RGEOS_CheckValidity(2L)

``` r
#Re-project to Lambert Azimuthal Equal Area
SSI_laea<-spTransform(SSI, CRS=CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m"))

# convert to dataframe for use with ggplot2
SSI_laea@data$id = rownames(SSI_laea@data)
SSI_laea.points = fortify(SSI_laea, region="id")
SSI_laea.df = plyr::join(SSI_laea.points, SSI_laea@data, by="id")

# filter out only the polygons for the islands
SSI_laea.df <- SSI_laea.df %>% filter(hole == TRUE)

# plot
SSI_laea.df %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill="grey") +
  geom_path(color="grey") +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"))
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Function for plotting the penguin track onto the map

``` r
plot_track <- function(x) {
  # plot
  ggplot() + 
    geom_polygon(data = SSI_laea.df, aes(x = long, y = lat, group = group), fill="grey50") +
    geom_path(data = SSI_laea.df, aes(x = long, y = lat, group = group), color="grey50") +
    coord_equal() +
    geom_point(data = x, aes(x = LON, y = LAT)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "aliceblue"),
          legend.title = element_blank()) 
}
```

### Function to remove points that are over land

This also removes points within 500m of land.

``` r
# first create 500m buffer around island
SSI_laea_buffer <- buffer(SSI_laea, width=-500)

remove_points_on_land <- function(track) {
  # make the track spatial points df
  coordinates(track) <- ~LON + LAT
  # tell it that it's projected in LAEA
  proj4string(track) <- CRS("+proj=laea +lon_0=-26 +lat_0=-58 +units=m")
  # add new column to track object identifying whether the track is off the island
  track$off_island <- !is.na(over(track, SSI_laea_buffer))
  # filter the points for just those that are off the island
  track %>%
    filter(off_island == TRUE)
}
```

### Split into trips

First create a function for offsetting the values in a column by 1 row
(or more if you change `shiftLen`). This is needed to later calculate
the lag time between each point.

``` r
# function for offsetting values by 1 row
rowShift <- function(x, shiftLen = 1L) {
    rr <- (1L + shiftLen):(length(x) + shiftLen)
    rr[rr<1] <- NA
    return(x[rr])
}
```

Then we calculate the lag time between each point. If there is a lag
longer than 5 minutes (0.08333 hours), then this is when the bird was on
land (technically inside the buffer zone) and so we can use this to
split the track into separate foraging trips.

I still want to play around with the best lag time to use (30 mins
maybe?), in case the bird was foraging or hanging out just at the edge
of the buffer zone.

Each time there is a lag greater than x minutes, we get a `Start_trip =
TRUE` in the `at_sea` spatial object, otherwise `Start_trip = FALSE`.

In order to plot the final trip, I need to fudge the final line of
`at_sea` and give it `Start_Trip == TRUE` so that I can use
Start\_row\_indexes to plot this final trip.

``` r
split_into_trips <- function(at_sea) {
  # offset the values in Time_since by 1 row 
  at_sea2 <- at_sea %>% mutate(lag1 = rowShift(Time_since, -1), 
                               # calculate differences between Time_since and each lag 
                               diff1 = Time_since - lag1,
                               # put track$Start_trip == TRUE where the diff1 is greater than x mins
                               # Start_trip = diff1 >= 0.5) # 30 minutes on land
                               Start_trip = diff1 >= 0.16) # 10 minutes on land
                               # Start_trip = diff1 >= 0.25) # 15 minutes on land
  # change the "NA" at the beginning of the first trip to "TRUE"
  at_sea2$Start_trip[1] <- TRUE
  # change the final value of Start_trip to TRUE to signal this is actually the end of the last trip
  at_sea2[length(at_sea2), "Start_trip"] <- TRUE
  return(at_sea2)
}
```

### Function for plotting the trips

This function takes a sequence of numbers corresponding to the number of
`Start_trip == TRUE` in the object `at_sea` to split the dataframe up
into trips and plot each indiviudally. The function will resize the map
according to the min and max longitude and latitude for each trip. This
is not idea but will do for now.

``` r
plot_trip <- function(x) {
  ggplot() +
    # plot the map first
    geom_polygon(data = SSI_laea.df, aes(x = long, y = lat, group = group), fill="grey50") +
    geom_path(data = SSI_laea.df, aes(x = long, y = lat, group = group), color="grey50") +
    coord_equal() +
    # add the points. This uses the trip number (x) to subset the dataframe by trip.
    geom_point(data = data.frame(at_sea[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1), ]), aes(x = LON, y = LAT)) +
    theme_bw() +
    # add headings for trip number and start/stop time
    labs(title = paste0("Trip ", x),
         subtitle = paste0("Trip start: ",
                            data.frame(at_sea[Start_row_indexes[[x]], ]) %>% select(Time_absolute),
                            ", Trip end: ",
                            data.frame(at_sea[Start_row_indexes[[x+1]]-1, ]) %>% select(Time_absolute))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "aliceblue"),
          legend.title = element_blank()) +
    # size according to the dimensions of the trip - this is not ideal
    coord_fixed(ratio = 1,
                xlim = c(data.frame(at_sea[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1),]) %>% select(LON) %>% min(),
                         data.frame(at_sea[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1),]) %>% select(LON) %>% max()),
                ylim = c(data.frame(at_sea[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1),]) %>% select(LAT) %>% min(),
                         data.frame(at_sea[c(Start_row_indexes[[x]]:Start_row_indexes[[x+1]]-1),]) %>% select(LAT) %>% max()),
                expand = TRUE,
                clip = "on")
}
```

### Penguin - 196697

Load crawled
track.

``` r
predObj <- read.csv("predicted_tracks/196697_track.csv", stringsAsFactors = FALSE) 

# select the useful columns and rename
track <- predObj %>%  
  select(Ptt, Time_absolute, Time_since, mu.x, mu.y) %>% 
  rename(LON = mu.x, LAT = mu.y)
```

Plot the raw track

``` r
plot_track(track)
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Remove points on land and replot to make sure nothing has gone wrong.

``` r
at_sea <- remove_points_on_land(track)

at_sea %>% 
    data.frame() %>% 
    plot_track(.) 
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Split into trips.

``` r
at_sea <- split_into_trips(at_sea)
head(at_sea) # check that the table starts with Start_trip = TRUE
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island   lag1   diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>       <dbl>   <dbl> <lgl>     
    ## 1 196697 2020-01-06 17:14:00      0.4   TRUE       NA     NA      TRUE      
    ## 2 196697 2020-01-06 17:19:00      0.483 TRUE        0.4    0.0833 FALSE     
    ## 3 196697 2020-01-06 17:24:00      0.567 TRUE        0.483  0.0833 FALSE     
    ## 4 196697 2020-01-06 17:29:00      0.65  TRUE        0.567  0.0833 FALSE     
    ## 5 196697 2020-01-06 17:34:00      0.733 TRUE        0.65   0.0833 FALSE     
    ## 6 196697 2020-01-06 17:39:00      0.817 TRUE        0.733  0.0833 FALSE

``` r
tail(at_sea) # check that the table ends with Start_trip = TRUE
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1  diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>  <dbl> <lgl>     
    ## 1 196697 2020-02-26 20:09:00      1227. TRUE       1227. 0.0833 FALSE     
    ## 2 196697 2020-02-26 20:14:00      1227. TRUE       1227. 0.0833 FALSE     
    ## 3 196697 2020-02-26 20:19:00      1227. TRUE       1227. 0.0833 FALSE     
    ## 4 196697 2020-02-26 20:24:00      1228. TRUE       1227. 0.0833 FALSE     
    ## 5 196697 2020-02-26 20:29:00      1228. TRUE       1228. 0.0833 FALSE     
    ## 6 196697 2020-02-26 20:34:00      1228. TRUE       1228. 0.0833 TRUE

Store the row numbers of each `Start_trip == TRUE` in a list which we
will use to split by trip

``` r
Start_row_indexes <- as.list(which(at_sea$Start_trip == TRUE))
```

Plot the trips

``` r
seq <- c(1:(length(Start_row_indexes) -1))
plots = purrr::map(seq, ~plot_trip(.x))

invisible(lapply(plots, print))
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-7.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-8.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-9.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-10.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-11.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-12.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-13.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-14.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-15.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-16.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-12-17.png)<!-- -->

``` r
# plot with patchwork
#    plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + 
#    plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]] +
#    plots[[9]] + plots[[10]] + plots[[11]] + plots[[12]] +
#    plots[[13]] + plots[[14]] + plots[[15]] 
```

### Penguin - 196698

``` r
predObj <- read.csv("predicted_tracks/196698_track.csv", stringsAsFactors = FALSE) 

# select the useful columns and rename
track <- predObj %>%  
  select(Ptt, Time_absolute, Time_since, mu.x, mu.y) %>% 
  rename(LON = mu.x, LAT = mu.y)

plot_track(track)
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# remove points on land
at_sea <- remove_points_on_land(track)

# plot to make sure it worked
at_sea %>% 
    data.frame() %>% 
    plot_track(.) 
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
# split into trips
at_sea <- split_into_trips(at_sea)
head(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1   diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>   <dbl> <lgl>     
    ## 1 196698 2020-01-08 03:57:00       35.1 TRUE        NA   NA      TRUE      
    ## 2 196698 2020-01-08 04:02:00       35.2 TRUE        35.1  0.0833 FALSE     
    ## 3 196698 2020-01-08 04:07:00       35.3 TRUE        35.2  0.0833 FALSE     
    ## 4 196698 2020-01-08 04:12:00       35.4 TRUE        35.3  0.0833 FALSE     
    ## 5 196698 2020-01-08 04:17:00       35.4 TRUE        35.4  0.0833 FALSE     
    ## 6 196698 2020-01-08 04:22:00       35.5 TRUE        35.4  0.0833 FALSE

``` r
tail(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1  diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>  <dbl> <lgl>     
    ## 1 196698 2020-02-26 20:17:00      1227. TRUE       1227. 0.0833 FALSE     
    ## 2 196698 2020-02-26 20:22:00      1228. TRUE       1227. 0.0833 FALSE     
    ## 3 196698 2020-02-26 20:27:00      1228. TRUE       1228. 0.0833 FALSE     
    ## 4 196698 2020-02-26 20:32:00      1228. TRUE       1228. 0.0833 FALSE     
    ## 5 196698 2020-02-26 20:33:00      1228. TRUE       1228. 0.0167 FALSE     
    ## 6 196698 2020-02-26 20:37:00      1228. TRUE       1228. 0.0667 TRUE

``` r
# store row indexes for the start of each trip
Start_row_indexes <- as.list(which(at_sea$Start_trip == TRUE))
```

Plot each trip.

``` r
seq <- c(1:(length(Start_row_indexes) -1))
plots = purrr::map(seq, ~plot_trip(.x))

invisible(lapply(plots, print))
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-16.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-14-17.png)<!-- -->

**Why is this last trip being cut into two? Is there a gap in the data
for some reason? Come back to
this**

### Penguin - 196699

``` r
predObj <- read.csv("predicted_tracks/196699_track.csv", stringsAsFactors = FALSE) 

# select the useful columns and rename
track <- predObj %>%  
  select(Ptt, Time_absolute, Time_since, mu.x, mu.y) %>% 
  rename(LON = mu.x, LAT = mu.y)

plot_track(track)
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
at_sea <- remove_points_on_land(track)

# plot to make sure it worked
at_sea %>% 
    data.frame() %>% 
    plot_track(.) 
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
at_sea <- split_into_trips(at_sea)
head(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1   diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>   <dbl> <lgl>     
    ## 1 196699 2020-01-07 19:43:00       26.9 TRUE        NA   NA      TRUE      
    ## 2 196699 2020-01-07 19:48:00       27.0 TRUE        26.9  0.0833 FALSE     
    ## 3 196699 2020-01-07 19:53:00       27.0 TRUE        27.0  0.0833 FALSE     
    ## 4 196699 2020-01-07 19:58:00       27.1 TRUE        27.0  0.0833 FALSE     
    ## 5 196699 2020-01-07 20:03:00       27.2 TRUE        27.1  0.0833 FALSE     
    ## 6 196699 2020-01-07 20:08:00       27.3 TRUE        27.2  0.0833 FALSE

``` r
tail(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1  diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>  <dbl> <lgl>     
    ## 1 196699 2020-02-12 11:28:00       883. TRUE        883. 0.0833 FALSE     
    ## 2 196699 2020-02-12 11:33:00       883. TRUE        883. 0.0833 FALSE     
    ## 3 196699 2020-02-12 11:38:00       883. TRUE        883. 0.0833 FALSE     
    ## 4 196699 2020-02-12 11:43:00       883. TRUE        883. 0.0833 FALSE     
    ## 5 196699 2020-02-12 11:48:00       883. TRUE        883. 0.0833 FALSE     
    ## 6 196699 2020-02-12 11:52:00       883. TRUE        883. 0.0667 TRUE

``` r
Start_row_indexes <- as.list(which(at_sea$Start_trip == TRUE))
```

Plot each trip.

``` r
seq <- c(1:(length(Start_row_indexes) -1))
plots = purrr::map(seq, ~plot_trip(.x))

invisible(lapply(plots, print))
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-7.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-8.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-9.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-10.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-11.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-12.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-13.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-14.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-15.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-16.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-17.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-18.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-19.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-16-20.png)<!-- -->

### Penguin - 196707

``` r
predObj <- read.csv("predicted_tracks/196707_track.csv", stringsAsFactors = FALSE) 

# select the useful columns and rename
track <- predObj %>%  
  select(Ptt, Time_absolute, Time_since, mu.x, mu.y) %>% 
  rename(LON = mu.x, LAT = mu.y)

plot_track(track)
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
at_sea <- remove_points_on_land(track)

# plot to make sure it worked
at_sea %>% 
    data.frame() %>% 
    plot_track(.) 
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
at_sea <- split_into_trips(at_sea)
head(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1   diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>   <dbl> <lgl>     
    ## 1 196707 2020-01-07 06:56:00       14.1 TRUE        NA   NA      TRUE      
    ## 2 196707 2020-01-07 07:01:00       14.2 TRUE        14.1  0.0833 FALSE     
    ## 3 196707 2020-01-07 07:06:00       14.3 TRUE        14.2  0.0833 FALSE     
    ## 4 196707 2020-01-07 07:11:00       14.4 TRUE        14.3  0.0833 FALSE     
    ## 5 196707 2020-01-07 07:16:00       14.4 TRUE        14.4  0.0833 FALSE     
    ## 6 196707 2020-01-07 07:21:00       14.5 TRUE        14.4  0.0833 FALSE

``` r
tail(at_sea)
```

    ## # A tibble: 6 x 7
    ##      Ptt Time_absolute       Time_since off_island  lag1  diff1 Start_trip
    ##    <int> <chr>                    <dbl> <lgl>      <dbl>  <dbl> <lgl>     
    ## 1 196707 2020-03-13 20:11:00      1611. TRUE       1611. 0.0833 FALSE     
    ## 2 196707 2020-03-13 20:16:00      1611. TRUE       1611. 0.0833 FALSE     
    ## 3 196707 2020-03-13 20:21:00      1612. TRUE       1611. 0.0833 FALSE     
    ## 4 196707 2020-03-13 20:22:00      1612. TRUE       1612. 0.0167 FALSE     
    ## 5 196707 2020-03-13 20:26:00      1612. TRUE       1612. 0.0667 FALSE     
    ## 6 196707 2020-03-13 20:31:00      1612. TRUE       1612. 0.0833 TRUE

``` r
Start_row_indexes <- as.list(which(at_sea$Start_trip == TRUE))
```

Plot each trip.

``` r
seq <- c(1:(length(Start_row_indexes) -1))
plots = purrr::map(seq, ~plot_trip(.x))

invisible(lapply(plots, print))
```

![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-8.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-9.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-10.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-11.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-12.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-13.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-14.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-15.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-16.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-17.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-18.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-19.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-20.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-21.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-22.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-23.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-24.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-25.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-26.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-27.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-28.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-29.png)<!-- -->![](2_split_into_trips_files/figure-gfm/unnamed-chunk-18-30.png)<!-- -->

## Questions

1.  Is 30 minutes on land a good cut-off or are short trips being
    missed? I should think about this later after sorting out question
    2.

2.  How do I deal with the fact that when the bird is most likely on
    land, the crawled tracks often show it moving slowly away from the
    colony? Should I remove parts of the track when the speed is very
    slow? How would I calculate the speed of the bird?

3.  Is it ok to delete some of the short trips which are clearly just
    the result of an inaccurate fix? Is there a rule of thumb to follow
    for keeping trips that look real (e.g. the have some wiggles in
    them?) and deleting the bad stuff?
