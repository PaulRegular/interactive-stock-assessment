
## load NCAM model inputs
## (object is named inputs)
load("data/inputs.RData")

## load NCAM model output from the last two years
## (object names match file names)
load("data/ncam_2018.RData")
load("data/ncam_2019.RData")

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
## (comp - named list of model output to compare)
## (last - model output from the last assessment)
vis_fit(ncam_2019, 
        last = ncam_2018,                     
        comp = list("NCAM 2018" = ncam_2018, 
                    "NCAM 2019" = ncam_2019),
        output_file = "analysis/dashboards/NCAM_dashboard.html")


