
library(webshot)

## helper function for exporting dashboard tabs using webshot
dashboard_webshot <- function(in_file = NULL, out_file = NULL, 
                              vwidth = 1200, vheight = 800, zoom = 10, delay = 10,
                              ...) {
    
    webshot(url = file.path("file://", getwd(), in_file),
            file = out_file,
            delay = delay, vwidth = vwidth, vheight = vheight, zoom = zoom)
}

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#tagging",
                  out_file = "analysis/paper/figures/NCAM_dashboard_tagging_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#rv-survey",
                  out_file = "analysis/paper/figures/NCAM_dashboard_rv-survey_tab.png")
