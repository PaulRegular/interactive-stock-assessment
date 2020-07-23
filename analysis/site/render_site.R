

## Copy several files from the dashboard dir. The getting_started.Rmd should capture all 
## css etc. dependancies needed for the NCAM_dashboard.html to work
files <- c("getting_started.Rmd", "knit_button.png", 
           "length-weight_data.csv", "skeleton.Rmd", 
           "NCAM_dashboard.html")
file.copy(file.path("analysis/dashboards", files),
          file.path("analysis/site", files))

rmarkdown::render_site("analysis/site")


