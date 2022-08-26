# Chinstrap penguin tracking on the South Sandwich Islands

Steps taken in the analysis of this dataset (come back here to follow the steps):

1. Crawl the tracks of each penguin using the ```crawl``` package in the script [1a_Crawl_each_penguin](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/1a_Crawl_each_penguin.md). 
A more fully annotated version of the code can be found in [1_inital_processing_with_Crawl](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/1_Initial_processing_with_Crawl.md).

2. Split into separate foraging trips using [2_split_into_trips](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/2_split_into_trips.md). This code also removes duff trips and incorrectly inferred long periods of low speed movement (when the penguin was actually at the colony or when there is a long gap between fixes at sea). I also made animations of the tracks using [Animate_tracks](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/Animate_tracks.md) which show some of these low speed movements.

3. Restrict the analyses to the chick-rearing period by plotting the maximum distance from the colony for each trip, and removing trips that are clearly outliers i.e. pre-moult trips when the bird suddenly goes much further from the colony. This is in [3_Restrict_to_chick-rearing_period](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/3_Restrict_to-chick-rearing_period.md).

4. Gather environmental variables (depth, distance to colony, distance to shelf break, slope, SST, sea surface height anomoly etc etc) for the study region and sample these for all points in the dataset. I also sample at a randon set of locations across the study area, to create the background data. See [4_Environmental_Data](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/4_Environmental_Data.md) for details.

5. Run the GAMs and evaluate the models. See [5_GAMs](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/5_GAMs.md).

6. Make predictions from the final models and do some plots. See [6_Predictions](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/6_Predictions.md).

tiny change!
