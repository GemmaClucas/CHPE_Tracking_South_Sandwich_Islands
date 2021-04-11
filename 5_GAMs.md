5\_GAMs
================
Gemma Clucas
3/24/2021

Read in my
data

``` r
data <- read.csv(file = "PresBackgroundLocationsWithEnvironmentalVariables.csv", stringsAsFactors = FALSE)

birds <- unique(data$Ptt)

names(data)
```

    ##  [1] "LON"           "LAT"           "Ptt"           "Trip"         
    ##  [5] "locType"       "Time_absolute" "depth"         "colonydist"   
    ##  [9] "shelfdist"     "slope"         "SST"           "Height"       
    ## [13] "NorthVelocity" "EastVelocity"  "chlorA"        "pres"

Run the GAMs varying the covariate and the number of knots from 3 to 5.
This holds out the real and background data for each Ptt in turn, and
then calculates the mean AUC, sensitivity, and specificty from all 20
runs.

``` r
# IRL
# var <- c("depth", "colonydist", "slope", "SST", "Height", "NorthVelocity", "EastVelocity", "chlorA")
# knots <- c(3,4,5)

# Quick knit
var <- c("colonydist")
knots <- c(3)

for (l in var) {
  for (i in knots) {
    for (j in birds){
      TRAIN <- data[data$Ptt!=j,]  # use all groups except 1 in training data
      TEST<- data[data$Ptt==j,]  # keep the other group for testing data
      # run gam with one variable, k is number of knots
      # have to define the formula as a string first, then convert to formula and pass to GAM
      form <- as.formula(paste("pres ~ s(", l, ", k = i)"))
      GAM <- gam(form, data=TRAIN, bs="cs", family=binomial, select=TRUE, method='GCV.Cp')  
      TEST$GAM_pred <- as.numeric(predict(GAM, type="response", newdata=TEST)) # predict into the test data frame
      roc1 <- roc(TEST$pres, TEST$GAM_pred) # create a roc curve from the test data
      AUCEVAL <- as.numeric(roc1[9]) # get the auc value
      #also get specificity and sensitivity from the best point on the ROC curve
      co1 <- pROC::coords(roc1, x = "best", best.method = "closest.topleft", ret=c("specificity","sensitivity")) 
      spec1 <- as.numeric(co1[1])
      sen1 <- as.numeric(co1[2])
      eva <- as.data.frame(rbind(AUCEVAL,spec1,sen1))
      names(eva) <- j
      eva
      if(exists("allevals")){
        allevals <- cbind(allevals, eva)
      }else {
        allevals <- eva
      }
    }
    # after running through all 20 birds
    # calculate mean AUC, sensitiviy, and specificity for this number of knots
    allevals$mean <- rowMeans(allevals)
    # save as a csv
    write.csv(allevals, file = paste("GAMs/", l, "_k", i, ".csv", sep = "" ))
    # save just the mean, with the variable names to an object
    assign(paste(l, i, sep = "_k"), 
           as_tibble(cbind(group = names(allevals), t(allevals))) %>%
             filter(group == "mean") %>% 
             select(-group) %>% 
             mutate(var = l, k = i) )
    # remove allevals before starting the next run, since now the number of knots will change
    rm(allevals)
  }
}
```

Combine the outputs from the GAMs run with just one covariate, to find
the one with the best AUC.

``` r
# IRL
# dplyr::bind_rows(colonydist_k3, colonydist_k4, colonydist_k5,
#                  depth_k3, depth_k4, depth_k5,
#                  slope_k3, slope_k4, slope_k5,
#                  SST_k3, SST_k4, SST_k5,
#                  Height_k3, Height_k4, Height_k5,
#                  chlorA_k3, chlorA_k4, chlorA_k5,
#                  NorthVelocity_k3, NorthVelocity_k4, NorthVelocity_k5,
#                  EastVelocity_k3, EastVelocity_k4, EastVelocity_k5) %>%
#   select(var, k, AUCEVAL, spec1, sen1) %>% 
#   rename(covariate = var, knots = k,
#          AUC = AUCEVAL, Sensitivity = sen1, Specificity = spec1) %>% 
#   arrange(desc(AUC))

# Quick knit
colonydist_k3 %>% 
  rename(covariate = var, knots = k,
         AUC = AUCEVAL, Sensitivity = sen1, Specificity = spec1) %>% 
  arrange(desc(AUC))
```

    ## # A tibble: 1 x 5
    ##   AUC               Specificity       Sensitivity       covariate  knots
    ##   <chr>             <chr>             <chr>             <chr>      <dbl>
    ## 1 0.909608425882688 0.827084783386952 0.889801892036128 colonydist     3

So `colonydist` is the best predictor. Run models with `colonydist` plus
the other covariates next.

``` r
# IRL
# var <- c("depth", "slope", "SST", "Height", "NorthVelocity", "EastVelocity", "chlorA")
# knots <- c(3,4,5)

# Quick knit
var <- c("depth")
k <- c(3)

for (l in var) {
  for (i in knots) {
    for (j in birds){
      TRAIN <- data[data$Ptt!=j,]  # use all groups except 1 in training data
      TEST<- data[data$Ptt==j,]  # keep the other group for testing data
      # run gam with colonydist, knots = 3, and vary the other covariates and number of knots
      # have to define the formula first as a string
      form <- as.formula(paste("pres ~ s(colonydist, k = 3) + s(", l, ", k = i)"))
      GAM <- gam(form, data=TRAIN, bs="cs", family=binomial, select=TRUE, method='GCV.Cp')  
      TEST$GAM_pred <- as.numeric(predict(GAM, type="response", newdata=TEST)) # predict into the test data frame
      roc2 <- roc(TEST$pres, TEST$GAM_pred) # create a roc curve from the test data
      AUCEVAL2 <- as.numeric(roc2[9]) # get the auc value
      co2 <- pROC::coords(roc2, x = "best", best.method = "closest.topleft", ret=c("specificity","sensitivity")) #also get specificity and sensitivity
      spec2 <- as.numeric(co2[1])
      sen2 <- as.numeric(co2[2])
      eva2 <- as.data.frame(rbind(AUCEVAL2,spec2,sen2))
      names(eva2) <- j
      eva2
      if(exists("allevals2")){
        allevals2 <- cbind(allevals2, eva2)
      }else {
        allevals2 <- eva2
      }
    }
    # calculate mean AUC, sensitiviy, and specificity
    allevals2$mean <- rowMeans(allevals2)
    # save as a csv
    write.csv(allevals2, file = paste("GAMs/colonydist_k3+", l, "_k", i, ".csv", sep = "" ))
    # save just the mean, with the variable names to an object
    assign(paste("colonydist_k3_", l, "_k", i, sep = ""), 
           as_tibble(cbind(group = names(allevals2), t(allevals2))) %>%
             filter(group == "mean") %>% 
             select(-group) %>% 
             mutate(var = l, k = i) )
    rm(allevals2)
  }
}
```

Combine the outputs and sort by AUC from highest to lowest.

``` r
# IRL
# dplyr::bind_rows(colonydist_k3_chlorA_k3, colonydist_k3_chlorA_k4, colonydist_k3_chlorA_k5,
#                  colonydist_k3_depth_k3, colonydist_k3_depth_k4, colonydist_k3_depth_k5,
#                  colonydist_k3_slope_k3, colonydist_k3_slope_k4, colonydist_k3_slope_k5,
#                  colonydist_k3_SST_k3, colonydist_k3_SST_k4, colonydist_k3_SST_k5,
#                  colonydist_k3_Height_k3, colonydist_k3_Height_k4, colonydist_k3_Height_k5,
#                  colonydist_k3_NorthVelocity_k3, colonydist_k3_NorthVelocity_k4, colonydist_k3_NorthVelocity_k5,
#                  colonydist_k3_EastVelocity_k3, colonydist_k3_EastVelocity_k4, colonydist_k3_EastVelocity_k5) %>%
#   select(var, k, AUCEVAL2, spec2, sen2) %>% 
#   rename(covariate = var, knots = k,
#          AUC = AUCEVAL2, Sensitivity = sen2, Specificity = spec2) %>% 
#   arrange(desc(AUC))

# Quick knit
colonydist_k3_depth_k3 %>%
  select(var, k, AUCEVAL2, spec2, sen2) %>% 
  rename(covariate = var, knots = k,
         AUC = AUCEVAL2, Sensitivity = sen2, Specificity = spec2) %>% 
  arrange(desc(AUC))
```

    ## # A tibble: 1 x 5
    ##   covariate knots AUC               Specificity       Sensitivity      
    ##   <chr>     <dbl> <chr>             <chr>             <chr>            
    ## 1 depth         3 0.920269042638705 0.840151383292074 0.892637752564834

So colony distance with 3 knots and SST with 5 knots gives the highest
AUC, but chlorophyl A is very close.
