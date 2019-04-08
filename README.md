# GarminCSVr
Process Garmin Connect CSV files using R

The R script `csv2r.R` can be used to process a CSV file downloaded from Garmin Connect. The script is written with Running data in mind. Some fake data is supplied in the `data` folder and some example outputs are in the `output` folder.

The script allows a deeper dive into summary data from Garmin Connect. For example, looking at running speed over time.

![img](output/allPace.png?raw=true "image")

...and to look at those trends for runs of different lengths.

![img](output/paceByDist.png?raw=true "image") 


There's also a simple way to compare the cumulative distance from one year to the next.

![img](output/cumulativeDistOverlay.png?raw=true "image")

Plus a few other ideas for plots.

