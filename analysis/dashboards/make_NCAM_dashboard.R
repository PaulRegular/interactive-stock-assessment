
## load NCAM model inputs
## (object is named inputs)
load("data/inputs.RData")

## load NCAM model output
## (object is named mshift)
load("data/mshift.RData")

## source plotting functions
## (these functions are used in the Rmarkdown file that makes the dashboard)
source("R/plot.R")

## source some data management functions
## (these functions are used to "tidy" the model output)
source("R/tidy.R")

## source function for making the dashboard
## (this function uses rmarkdown::run or rmarkdown::render)
source("R/vis_fit.R")

## Run the vis_fit function to make the dashboard
vis_fit(mshift, output_file = "analysis/dashboards/NCAM_dashboard.html")
