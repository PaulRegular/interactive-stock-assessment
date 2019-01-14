
library(webshot)
library(magick)

## helper function for exporting dashboard tabs using webshot
dashboard_webshot <- function(in_file = NULL, out_file = NULL, 
                              vwidth = 1200, vheight = 800, zoom = 3, delay = 10,
                              ...) {
    
    webshot(url = file.path("file://", getwd(), in_file),
            file = out_file,
            delay = delay, vwidth = vwidth, vheight = vheight, zoom = zoom)
}

## take screenshots of NCAM dashboard
dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#background",
                  out_file = "analysis/paper/figures/NCAM_dashboard_background_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#catch",
                  out_file = "analysis/paper/figures/NCAM_dashboard_catch_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#rv-survey",
                  out_file = "analysis/paper/figures/NCAM_dashboard_rv-survey_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#tagging",
                  out_file = "analysis/paper/figures/NCAM_dashboard_tagging_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#trends",
                  out_file = "analysis/paper/figures/NCAM_dashboard_trends_tab.png")

dashboard_webshot(in_file = "analysis/dashboards/NCAM_dashboard.html#output",
                  out_file = "analysis/paper/figures/NCAM_dashboard_tables_tab.png")

## combine screenshots into one image
img_a <- image_read("analysis/paper/figures/NCAM_dashboard_background_tab.png") %>% 
    image_border("white", "300x50") %>% 
    image_annotate("a)", color = "black", size = 300)
img_b <- image_read("analysis/paper/figures/NCAM_dashboard_rv-survey_tab.png") %>% 
    image_border("white", "300x50") %>% 
    image_annotate("b)", color = "black", size = 300)
img_c <- image_read("analysis/paper/figures/NCAM_dashboard_tagging_tab.png") %>% 
    image_border("white", "300x50") %>% 
    image_annotate("c)", color = "black", size = 300)
img_d <- image_read("analysis/paper/figures/NCAM_dashboard_tables_tab.png") %>% 
    image_border("white", "300x50") %>% 
    image_annotate("d)", color = "black", size = 300)

side_a <- image_append(image_scale(c(img_a, img_b), "x3000"))
side_b <- image_append(image_scale(c(img_c, img_d), "x3000"))
img <- image_append(image_scale(c(side_a, side_b), "x6000"), stack = TRUE) %>% 
    image_border("white", "100x100")

image_write(img, path = "analysis/paper/figures/NCAM_dashboard_combined_screenshots.png", 
            format = "png")
## ...text may be too small in print with this option...may need independent figures

