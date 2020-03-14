#' Make a flexdashboard for visualizing the model fit
#'
#' Assumes the working directory is the project directory
#'
#' @param fit            Object produced by \code{\link{fit_model}}
#' @param comp           Named list of model objects to compare
#' @param last           Model object from the last assessment
#' @param output_file    Name of file to export using \code{\link{rmarkdown::render}}.
#'                       If NULL, flexdashboard will be rendered using \code{\link{rmarkdown::run}}
#' @param output_format  R markdown output format (see \code{\link{rmarkdown::run}})
#'
#' @export
#'

vis_fit <- function(fit = NULL, comp = NULL, last = NULL, output_file = NULL, output_format = NULL) {

  pkg <- c("rmarkdown", "shiny", "flexdashboard", "plotly", "viridis")
  for (p in pkg) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(paste(p, "is needed for vis_fit to work. Please install it."), call. = FALSE)
    }
  }
  if (packageVersion("plotly") > "4.8") {
    message("Warning: plots will not render correctly in the dashboard using a version of plotly > 4.8.")
    ans <- readline(prompt = "Press [enter] to revert to version 4.8 or [esc] to abort. ")
    ans <- readline(prompt = "are you sure you want to revert to plotly version 4.8? ")
    if (substr(ans, 1, 1) == "y") {
      remove.packages("plotly")
      devtools::install_version("plotly", version = "4.8.0", repos = "http://cran.us.r-project.org")
    } else {
      stop("plotly version 4.8 is required.")
    }
  }

  ## simplify file and dir specification
  if (!is.null(output_file)) {
    output_dir <- normalizePath(dirname(output_file))
    output_file <- basename(output_file)
  }

  ## make a tmp object in the global environment for use by rmarkdown
  ## (rmarkdown::run likes to operate on objects in the global environment)
  tmp <- as.list(environment())
  assign("tmp", tmp, globalenv())

  ## keep most objects created in the rmd file local (i.e. not in the global)
  local({
    if (is.null(output_file)) {
      rmarkdown::run(file = "inst/rmd/vis_fit.Rmd")
    } else {
      rmarkdown::render(input = "inst/rmd/vis_fit.Rmd",
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = output_format)
    }
  })

}
