# CHPE_Tracking_South_Sandwich_Islands

Steps taken in the analysis of this dataset:

1. Crawl the tracks of each penguin using the ```crawl``` package in the script [1a_Crawl_each_penguin](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/1a_Crawl_each_penguin.md). 
A more fully annotated version of the code can be found in [1_inital_processing_with_Crawl](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/1_Initial_processing_with_Crawl.md).

2. Split into separate foraging trips using [2_split_into_trips](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/2_split_into_trips.md). This code also removes duff trips and incorrectly inferred long periods of low speed movement (when the penguin was actually at the colony or when there is a long gap between fixes at sea). I also made animations of the tracks using [Animate_tracks](https://github.com/GemmaClucas/CHPE_Tracking_South_Sandwich_Islands/blob/master/Animate_tracks.md) which show some of these low speed movements.