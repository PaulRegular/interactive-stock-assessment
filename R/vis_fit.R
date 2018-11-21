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
