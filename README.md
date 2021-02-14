# GarminCSVr
Process Garmin Connect CSV files using R

The R scripts `csv2r.R` and `loadAndCalc.R` can be used to process CSV files downloaded from Garmin Connect. The scripts are written with Running data in mind. Some fake data is supplied in the `Data` folder and some example outputs are in the `Output/Plots/` folder.

--

### `csv2r.R`

The script allows a deeper dive into summary data from Garmin Connect. For example, looking at running speed over time.

![img](Output/Plots/allPace.png?raw=true "image")

...and to look at those trends for runs of different lengths.

![img](Output/Plots/paceByDist.png?raw=true "image") 

There's also a simple way to compare the cumulative distance from one year to the next.

![img](Output/Plots/cumulativeDistOverlay.png?raw=true "image")

Plus a few other ideas for plots.

--

### `readAndCalc.R`

The script will compare your progress against a distance-based target. For example, Garmin Connect has a Running 2021 - Stage 1 challenge which is to run 505 km between 1/1/21 and 31/3/21.

For that challenge, progress would be checked with this command:

`process_data("running","2021-01-01","2021-03-31",505)`

