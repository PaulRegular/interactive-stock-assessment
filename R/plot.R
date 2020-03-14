
#' Helper function for plotly labels
#'
#' @param  label   Label for plot
#' @param  offset  Number of pixels to offset from axis
#'
#' @return A list to supply to \code{annotations} argument in \code{plotly::layout}
#'
#' @export
#'

top_label <- function(label, offset = 0) {
  list(
    text = label,
    font = list(size = 14),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    yshift = -offset,
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
}

#' @export
#' @rdname top_label
bottom_label <- function(label, offset = 20) {
  list(
    text = label,
    font = list(size = 14),
    xref = "paper",
    yref = "paper",
    yanchor = "top",
    yshift = -offset,
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 0,
    showarrow = FALSE
  )
}

#' @export
#' @rdname top_label
left_label <- function(label, offset = 20) {
  list(
    text = label,
    font = list(size = 14),
    xref = "paper",
    yref = "paper",
    yanchor = "center",
    xanchor = "right",
    align = "center",
    xshift = -offset,
    x = 0,
    y = 0.5,
    textangle = -90,
    showarrow = FALSE
  )
}

#' @export
#' @rdname top_label
right_label <- function(label, offset = 0) {
  list(
    text = label,
    font = list(size = 14),
    xref = "paper",
    yref = "paper",
    yanchor = "center",
    xanchor = "left",
    align = "center",
    xshift = -offset,
    x = 1,
    y = 0.5,
    textangle = -90,
    showarrow = FALSE
  )
}


#' Plot trends
#'
#' @param data         Output produced by \code{\link{tidy_model}}
#' @param value        Name of value to plot
#' @param group        Name of column to group by
#' @param ylab         Label for the y axis
#' @param title        Plot title
#' @param ci           How should confidence interevals be represented? ("ribbon", "line", "none")
#' @param slider       Add range slider?
#' @param annotate     Add annotations
#' @param showlegend   Show legend
#' @param lwd          Line width
#' @param textposition Position of annotations (can be character vector of equal levels to the group)
#' @param col          Vector of colors
#'
#' @export
#'
#' @import plotly
#'

plot_trend <- function(data, group = NULL, ylab = "", title = "", ci = "ribbon",
                       slider = FALSE, annotate = FALSE, showlegend = FALSE, lwd = 2,
                       textposition = "right",
                       col = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
                               '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
                               '#bcbd22', '#17becf')) {

  d <- data
  if (!is.null(group)) {
    names(d)[names(d) == group] <- "grp"
    if (class(d$grp) != "factor") d$grp <- factor(d$grp)
    trace_names <- c(NULL, NULL)
  } else {
    d$grp <- factor(ylab)
    trace_names <- c("95% CI", "Estimate")
  }
  d <- d %>%
    group_by(grp) %>%
    mutate(text = ifelse(year == max(year), as.character(grp), ""))
  if (length(textposition) == 1) textposition <- rep(textposition, nlevels(d$grp))
  d$textposition <- textposition[d$grp]

  p <- plot_ly(x = ~year, color = ~grp, colors = col[seq(nlevels(d$grp))],
               data = d, showlegend = showlegend)

  if (ci == "ribbon") {
    p <- p %>% add_ribbons(ymin = ~lwr, ymax = ~upr,
                           line = list(width = 0),
                           opacity = 0.4,
                           name = trace_names[1],
                           showlegend = FALSE)
  }

  if (ci == "line") {
    p <- p %>% add_ribbons(ymin = ~lwr, ymax = ~upr,
                           alpha = 0.4,
                           line = list(width = lwd),
                           fillcolor = "rgba(0, 0, 0, 0)",
                           name = trace_names[1],
                           showlegend = FALSE)
  }
  if (slider) {
    p <- p %>% rangeslider()
  }
  if (annotate) {
    p <- p %>% add_text(x = ~year + 0.2, y = ~est,
                        text = ~text, textposition = ~textposition,
                        hoverinfo = "none")
  }
  min_year <- min(d$year)
  max_year <- ifelse(annotate, max(d$year) + 5, max(d$year) + 0.5)

  p <- p %>%
    add_lines(y = ~est, name = trace_names[2], line = list(width = lwd)) %>%
    layout(title = title,
           xaxis = list(title = "Year", range = c(min_year, max_year),
                        tickformat = "d"),
           yaxis = list(title = ylab))
  p

}



#' Plot retrospective fits
#'
#' @param data       Data produced by \code{\link{tidy_retro}}
#' @param col        One color or vector of colors of equal length to the number of folds
#' @inheritDotParams plot_trend -col -group
#'
#' @export
#'
#' @import plotly
#'

plot_retro <- function(data, col = '#1f77b4', ...) {

  folds <- nlevels(data$terminal_year)
  max_year <- tail(levels(data$terminal_year), 1)
  if (length(col) == 1) { col <- rep(col, folds) }
  data[data$terminal_year != max_year, c("lwr", "upr")] <- NA
  p <- plot_trend(data, group = "terminal_year", col = col, ...)
  data$grp <- data$terminal_year
  marker_data <- data %>% group_by(terminal_year) %>% filter(dplyr::row_number() == dplyr::n())
  p %>% add_markers(data = marker_data, x = ~year, y = ~est)

}


#' Plot relative differences from retrospective fits
#'
#' @description Plot of the relative differences of the estimates from the
#' model with all years included against estimates from a series of retrospective
#' "peals" or "folds" of the data.
#'
#' @param data       Data produced by \code{\link{tidy_retro}}
#' @param ylab       Label for the y axis
#' @param col        Plot color
#'
#' @export
#'

plot_mohns <- function(data = NULL, ylab = "Relative difference", col = '#1f77b4') {

  d <- data
  d$terminal_year <- as.numeric(as.character(d$terminal_year))
  if (max(d$year) > max(d$terminal_year)) {
    d$terminal_year <- d$terminal_year + 1 # plus one if the estimates are steped forward by one year
  }
  td <- d[d$terminal_year == max(d$terminal_year), c("year", "est")]   # terminal data
  rd <- d[d$terminal_year != max(d$terminal_year) &
            d$year == d$terminal_year,
          c("year", "est", "terminal_year")]                           # retrospective data
  cd <- merge(rd, td, by = "year", suffixes = c("_r", "_t"))           # combined data (r = retro, t = terminal)
  cd$pdiff <- (cd$est_r - cd$est_t) / cd$est_t

  mohns <- mean(cd$pdiff)

  plot_ly(data = cd, x = ~year, y = ~pdiff,
          hoverinfo = "x+y", showlegend = FALSE,
          color = I(col)) %>%
    add_markers() %>%
    add_segments(x = ~year, xend = ~year, y = 0, yend = ~pdiff) %>%
    layout(xaxis = list(title = "Year", tickformat = "d"),
           yaxis = list(title = ylab),
           annotations = top_label(paste("Mohn's rho =", round(mohns, 3))))

}

#' Plot differences from retrospective fits
#'
#' @description Plot of the differences of the estimates from the
#' model with all years included against estimates from a series of retrospective
#' "peals" or "folds" of the data. Confidence intervals around the differences
#' are included. Normal distribution is assumed (mean difference = terminal mean - retro mean,
#' sd difference = terminal sd + retro sd).
#'
#' @param data       Data produced by \code{\link{tidy_retro}}
#' @param ylab       Label for the y axis
#' @param in_col     Color of intervals that include 0
#' @param out_col    Color of intervals that bound 0
#'
#' @export
#'

plot_retro_deltas <- function(data = NULL, ylab = "Difference",
                              in_col = '#1f77b4', out_col = '#d62728') {

  d <- data
  d$terminal_year <- as.numeric(as.character(d$terminal_year))
  if (max(d$year) > max(d$terminal_year)) {
    d$terminal_year <- d$terminal_year + 1 # plus one if the estimates are steped forward by one year
  }
  td <- d[d$terminal_year == max(d$terminal_year), c("year", "est", "sd")]   # terminal data
  rd <- d[d$terminal_year != max(d$terminal_year) &
            d$year == d$terminal_year,
          c("year", "est", "sd", "terminal_year")]                     # retrospective data
  cd <- merge(rd, td, by = "year", suffixes = c("_r", "_t"))           # combined data (r = retro, t = terminal)

  cd$mean <- cd$est_r - cd$est_t
  cd$sd <- cd$sd_r + cd$sd_t
  cd$lwr <- cd$mean - (qnorm(0.975) * cd$sd)
  cd$upr <- cd$mean + (qnorm(0.975) * cd$sd)
  cd$out <- factor(cd$lwr < 0 & cd$upr < 0 | cd$lwr > 0 & cd$upr > 0, levels = c("FALSE", "TRUE"))

  plot_ly(data = cd, x = ~year, y = ~mean,
          showlegend = FALSE,
          color = ~out, colors = c(in_col, out_col)) %>%
    add_markers(name = "Difference") %>%
    add_segments(x = ~year, xend = ~year, y = ~lwr, yend = ~upr,
                 name = "95% CI") %>%
    layout(xaxis = list(title = "Year", tickformat = "d"),
           yaxis = list(title = ylab))

}


#' Plot projection results
#'
#' Taylored to catch multiplier projections. Not yet generalized for other projection scenarios.
#'
#' @param data         Data produced by \code{\link{tidy_model}}
#' @param group        Name of column to group by
#' @param xlim         x axis limits
#' @param ylim         y axis limits
#' @param ylab         y label
#' @param ticksuffix   suffix for y axis labels
#' @param vline        vertical line placement
#' @param hline        horizontal line placement
#' @param col          colors to use in the plot
#' @param showlegend   show legend (only controls ribbon)
#' @param markers      add markers to the plot?
#' @param darnen_proj  darken ribbion in through the projection years? Expects
#'                     a 'type' column in the data with projection years identified
#'                     by 'Projected'
#'
#' @export
#'

plot_proj <- function(data = NULL, group = NULL, xlim = NULL, ylim = NULL, ylab = NULL, ticksuffix = "",
                      vline = NULL, hline = NULL, col = NULL, showlegend = FALSE, markers = TRUE,
                      darken_proj = FALSE) {

  d <- data
  col_rgb <- paste0("rgb(", paste(col2rgb(col), collapse = ","), ")")
  lines <- list()
  if (!is.null(vline)) {
    lines[[1]] <- list(type = "line", x0 = vline, x1 = vline,
                       y0 = min(data$lwr), y1 = max(data$upr),
                       yref = "y", xref = "x",
                       line = list(width = 1, dash = "dot"))
  }
  if (!is.null(hline)) {
    lines[[2]] <- list(type = "line", x0 = min(data$year), x1 = max(data$year),
                       y0 = hline, y1 = hline, yref = "y", xref = "x",
                       line = list(width = 1, dash = "dot"))
  }

  if (!is.null(group)) {
    names(d)[names(d) == group] <- "grp"
    if (class(d$grp) != "factor") d$grp <- factor(d$grp)
  } else {
    d$grp <- factor(ylab)
  }

  p <- plot_ly(data = d, x = ~year, y = ~est, ymin = ~lwr, ymax = ~upr,
               frame = ~scenario, color = ~grp, legendgroup = ~grp,
               colors = col[seq(nlevels(d$grp))]) %>%
    add_ribbons(line = list(width = 0), showlegend = showlegend,
                opacity = 0.4) %>%
    add_lines(showlegend = FALSE)

  if (darken_proj) {
    p <- p %>%
      add_ribbons(data = d[d$type == "Projected", ],
                  line = list(width = 0), showlegend = FALSE,
                  opacity = 0.4)
  }

  if (markers) {
    p <- p %>% add_markers(data = d, showlegend = FALSE,
                           marker = list(size = 10, color = "white",
                                         line = list(color = col_rgb,
                                                     width = 2)))
  }

  p %>%
    animation_opts(transition = 0) %>%
    animation_slider(currentvalue = list(prefix = "Catch multiplier: ",
                                         font = list(color = "black", size = 13))) %>%
    layout(shapes = lines,
           xaxis = list(title = "Year", range = xlim,
                        tickvals = min(data$year):max(data$year),
                        tickformat = "d"),
           yaxis = list(title = ylab, range = ylim,
                        ticksuffix = ticksuffix))
}


#' Plot model fits to data
#'
#' @param data        data.frame with 'obs' and 'pred' columns
#' @param ylab        yaxis label
#' @param obs_col     color for observations (ignored if a group is supplied)
#' @param pred_col    color for predictions (ignored if a group is supplied)
#' @param showlegend  show legend?
#' @param title       plot title
#' @param type        type of yaxis (e.g. "log")
#' @param rangemode   rangemode for yaxis
#' @param group       column to group observed and fitted values by
#' @param group_cols  colors for the groups (must equal number of groups)
#' @param stack_group stack values across groups (add y values)?
#' @param frame       column for creating animation frames
#'
#' @export
#'

plot_obs_pred <- function(data, ylab = "", obs_col = '#1f77b4', pred_col = '#ff7f0e',
                          showlegend = FALSE, title = "", type = "-", rangemode = "normal",
                          group = NULL, group_cols = NULL, stack_group = NULL,
                          frame = NULL) {

  if (is.null(group)) {
    data <- data
  } else {
    data$group <- data[, group]
    data <- crosstalk::SharedData$new(data, ~group)
  }

  if (!is.null(frame)) {
    f <- as.formula(paste0("~", frame))
  } else {
    f <- NULL
  }

  p <- plot_ly(data = data, x = ~year, showlegend = showlegend, fill = "none", frame = f)

  if (is.null(group)) {
    p <- p %>%
      add_markers(y = ~obs, name = "Observed", legendgroup = "Observed",
                  color = I(obs_col)) %>%
      add_lines(y = ~pred, name = "Predicted", legendgroup = "Predicted",
                color = I(pred_col))
  } else {
    p <- p %>%
      add_markers(y = ~obs, color = ~group, legendgroup = ~group,
                  colors = group_cols,
                  stackgroup = ifelse(stack_group, "obs", NULL)) %>%
      add_lines(y = ~pred, color = ~group, legendgroup = ~group,
                colors = group_cols, showlegend = FALSE,
                stackgroup = ifelse(stack_group, "pred", NULL))
  }

  if (!is.null(frame) && type == "log") {
    ylim <- range(log10(data$pred))
  } else {
    ylim <- NULL
  }

  p <- p %>%
    layout(
      xaxis = list(title = "Year", tickformat = "d"),
      yaxis = list(title = ylab, type = type, rangemode = rangemode,
                   range = ylim),
      annotations = top_label(title)
    )

  if (!is.null(group)) {
    p <- p %>% highlight(on = "plotly_click", off = "plotly_relayout")
  }

  if (!is.null(frame)) {
    p <- p %>%
      animation_opts(frame = 500, transition = 0)
  }

  p

}


#' Facet plot of model fits to data
#'
#' @inheritParams     plot_obs_pred
#' @param             by             group plots by age or year
#' @param             which          vector of ages or years to display
#' @inheritDotParams  plot_obs_pred
#'
#' @export
#'

facet_obs_pred <- function(data, by = "age", which = NULL,
                           ylab = "", showlegend = TRUE, nrows = 1,
                           ...) {

  names(data)[names(data) == by] <- "by"
  plot_list <- lapply(which, function(nm) {
    data %>%
      filter(by == nm) %>%
      plot_obs_pred(title = nm,
                    showlegend = showlegend & nm == which[1], # only show legend from first plot
                    ...)
  })

  subplot(plot_list, nrows = nrows, shareX = TRUE,
          titleX = FALSE, margin = 0.04) %>%
    layout(
      annotations = list(
        bottom_label("Year"),
        left_label(ylab, offset = 30)
      )
    )


}


#' Make bubble plot of model residuals by age and year
#'
#' @param data        data.frame with 'year', 'age', 'res' and 'zero' (optional) columns.
#'                    'zero' columns itentifies censored observations
#' @param pos_col     color for positive values
#' @param neg_col     color for negative values
#' @param na_col      color for censored values
#'
#' @export
#'

bubble_plot <- function(data, pos_col = "#E41A1C", neg_col = "#377EB8", na_col = "#A9A9A9") {
  d <- data
  d$sign <- ifelse(d$res > 0, "+", "-")
  if ("zero" %in% names(d)) {
    d$res[d$zero == 1] <- min(abs(d$res))
    d$sign[d$zero == 1] <- "0"
    cols <- c(pos_col, neg_col, na_col)
  } else {
    d$zero <- 0
    cols <- c(pos_col, neg_col)
  }
  ## add a small amount of noise, otherwise plotly does not plot values with the same number
  d$abs_res <- jitter(abs(d$res), factor = 0.000000000001)
  plot_ly(data = d, x = ~year, y = ~age, size = ~abs_res, text = ~round(res, 2),
          color = ~sign, colors = cols, sizes = c(5, 500), symbol = ~zero,
          symbols = c("16", "1")) %>%
    add_markers() %>%
    layout(xaxis = list(title = "Year", tickformat = "d"),
           yaxis = list(title = "Age"))
}


#' Make residual plot by year, cohort, age and expected value
#'
#' @param data      data.frame with 'year', 'age', 'res', 'fit', 'cohort' and 'rec' columns
#' @param col       color of the points
#' @param line_col  color of the loess smoother line added to the plots
#'
#' @export
#'

plot_resid <- function(data, col = '#1f77b4', line_col = '#d62728') {

  d <- crosstalk::SharedData$new(data, ~rec)
  year_plot <- plot_ly(data = d, x = ~year, y = ~res) %>%
    add_markers(color = I(col), alpha = 0.8, name = "Year") %>%
    add_lines(y = ~fitted(loess(res ~ year)),
              line = list(color = line_col),
              name = "Loess Smoother", showlegend = FALSE) %>%
    layout(annotations = bottom_label("Year", offset = 15),
           xaxis = list(title = ""))
  cohort_plot <- plot_ly(data = d, x = ~cohort, y = ~res) %>%
    add_markers(color = I(col), alpha = 0.8, name = "Cohort") %>%
    add_lines(y = ~fitted(loess(res ~ cohort)),
              line = list(color = line_col),
              name = "Loess Smoother", showlegend = FALSE) %>%
    layout(annotations = bottom_label("Cohort", offset = 15),
           xaxis = list(title = ""))
  age_plot <- plot_ly(data = d, x = ~age, y = ~res) %>%
    add_markers(color = I(col), alpha = 0.8, name = "Age") %>%
    add_lines(y = ~fitted(loess(res ~ age)),
              line = list(color = line_col),
              name = "Loess Smoother", showlegend = FALSE) %>%
    layout(annotations = bottom_label("Age", offset = 15),
           xaxis = list(title = ""))
  fit_plot <- plot_ly(data = d, x = ~fit, y = ~res) %>%
    add_markers(color = I(col), alpha = 0.8, name = "Expected") %>%
    add_lines(y = ~fitted(loess(res ~ fit)),
              line = list(color = line_col),
              name = "Loess Smoother", showlegend = FALSE) %>%
    layout(annotations = bottom_label("Expected", offset = 15),
           xaxis = list(title = ""))
  subplot(year_plot, cohort_plot, age_plot, fit_plot, nrows = 2,
          margin = c(0.01, 0.01, 0.1, 0.01),
          titleX = TRUE, titleY = FALSE, shareY = TRUE) %>%
    layout(showlegend = FALSE,
           annotations = left_label("Standardized log residuals",
                                    offset = 35)) %>%
    highlight(off = "plotly_relayout")

}




#' Quick visual of age-year matrix
#'
#' @param mat     age-year matrix
#' @param ages    vector of ages
#' @param years   vector of years
#' @param name    name of metric
#' @param tails   matrix to these terminal years for buttons
#' @param col     colors to interpolate into a ramp
#'
#' @export
#'

plot_surface <- function(mat = NULL, ages = NULL, years = NULL, name = NULL,
                         tails = c(20, 10, 5), col = viridis::viridis(200)) {

  ind <- seq(length(years))
  ind_list <- list(b1 = tail(ind, tails[1]),
                   b2 = tail(ind, tails[2]),
                   b3 = tail(ind, tails[3]))

  plot_ly(y = ages, colors = col) %>%
    add_surface(x = years, z = mat, name = name) %>%
    add_surface(x = years[ind_list$b1], z = mat[, ind_list$b1],
                visible = FALSE, name = name) %>%
    add_surface(x = years[ind_list$b2], z = mat[, ind_list$b2],
                visible = FALSE, name = name) %>%
    add_surface(x = years[ind_list$b3], z = mat[, ind_list$b3],
                visible = FALSE, name = name) %>%
    colorbar(title = name) %>%
    layout(
      scene = list(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Age"),
        zaxis = list(title = name)
      ),
      updatemenus = list(
        list(
          type = "buttons",
          y = 0.8,
          buttons = list(

            list(method = "update",
                 args = list(list(visible = c(TRUE, FALSE, FALSE, FALSE))),
                 label = "All years"),

            list(method = "update",
                 args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE))),
                 label = paste0("Last ", tails[1], "y")),

            list(method = "update",
                 args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE))),
                 label = paste0("Last ", tails[2], "y")),

            list(method = "update",
                 args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE))),
                 label = paste0("Last ", tails[3], "y"))

          ))
      ))

}


#' Quick visual of Z, M and F estimates
#'
#' @param z,m,f   age-year matrix
#' @param ages    vector of ages
#' @param years   vector of years
#' @param name    name of metric
#'
#' @export
#'

plot_zmf_surface <- function(z = NULL, m = NULL, f = NULL,
                             ages = NULL, years = NULL) {

  zcols <- RColorBrewer::brewer.pal(3, "Purples")[-1]
  zcols <- paste0("rgb(", apply(col2rgb(zcols), 2, paste, collapse = ","), ")")
  mcols <- RColorBrewer::brewer.pal(3, "Blues")[-1]
  mcols <- paste0("rgb(", apply(col2rgb(mcols), 2, paste, collapse = ","), ")")
  fcols <- RColorBrewer::brewer.pal(3, "Reds")[-1]
  fcols <- paste0("rgb(", apply(col2rgb(fcols), 2, paste, collapse = ","), ")")

  plot_ly(x = years, y = ages) %>%
    add_surface(z = z, cmin = min(z), cmax = max(z),
                name = "Z", colorscale = list(c(0,1), zcols)) %>%
    add_surface(z = m, cmin = min(m), cmax = max(m),
                name = "M", colorscale = list(c(0,1), mcols)) %>%
    add_surface(z = f, cmin = min(f), cmax = max(f),
                name = "F", colorscale = list(c(0,1), fcols)) %>%
    layout(
      scene = list(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Age"),
        zaxis = list(title = "Rate")
      ),
      updatemenus = list(
        list(
          type = "buttons",
          y = 0.8,
          buttons = list(

            list(method = "update",
                 args = list(list(visible = c(FALSE, TRUE, TRUE))),
                 label = "Remove Z"),

            list(method = "update",
                 args = list(list(visible = c(TRUE, FALSE, TRUE))),
                 label = "Remove M"),

            list(method = "update",
                 args = list(list(visible = c(TRUE, TRUE, FALSE))),
                 label = "Remove F"),

            list(method = "update",
                 args = list(list(visible = c(TRUE, TRUE, TRUE))),
                 label = "All")

          ))
      ))

}



## helper function for adding grouped line/ribbon legends
.add_lines_ribbons <- function(p, x = NULL, y = NULL, ymin = NULL, ymax = NULL,
                               ..., data = NULL, inherit = TRUE) {
  add_lines(p, x = x, y = y, ..., data = data, inherit = TRUE) %>%
    add_ribbons(p, x = x, ymin = ymin, ymax = ymax, ..., data = data, inherit = TRUE,
                line = list(width = 0), opacity = 0.4, showlegend = FALSE)
}


#' Plot model comparisons
#'
#' @param data  Output produced by \code{\link{tidy_model}}
#' @param ylab  Y label
#' @param col   Group colors
#' @param showlegend   Show legend
#'
#' @export
#'

plot_comps <- function(data = NULL, ylab = NULL, col = NULL, showlegend = TRUE) {

  d <- data
  base <- plot_ly(x = ~year, y = ~est, ymin = ~lwr, ymax = ~upr, showlegend = showlegend)
  d$model <- factor(d$model)
  models <- levels(d$model)
  model_cols <- col[seq(nlevels(d$model))]
  for (i in seq_along(models)) {
    base <- base %>% .add_lines_ribbons(data = d[d$model == models[i], ],
                                        name = models[i], legendgroup = models[i],
                                        color = I(model_cols[i]))
  }
  base %>%
    layout(xaxis = list(title = "Year", tickformat = "d"),
           yaxis = list(title = ylab))

}


#' Plot catch bounds and landings predictions
#'
#' @param data  Output produced by \code{\link{fit_model}}
#'
#' @export
#'

plot_landings <- function(data = NULL, cols = c("#31688EFF", "#35B779FF")) {

  tabs <- tidy_model(data)
  fit <- data
  min_year <- min(fit$tmb.data$years)
  max_year <- max(fit$tmb.data$years)

  landings <- data.frame(metric = "Predicted", tabs$log_pred_catch_wt)
  p1 <- trans_est(landings) %>%
    plot_trend(ylab = "Landings (t)", group = "metric", annotate = TRUE, col = cols) %>%
    add_ribbons(ymin = fit$tmb.data$landings_L, ymax = fit$tmb.data$landings_U,
                color = I(cols[2]), opacity = 0.5, line = list(width = 0),
                name = "Catch bounds") %>%
    add_lines(y = fit$tmb.data$landings, color = I(cols[2]), linetype = I(3),
              name = "Observed") %>%
    add_text(y = tail(fit$tmb.data$landings, 1), x = max_year + 0.2, text = "Observed",
             textposition = "bottom right", color = I(cols[2])) %>%
    layout(yaxis = list(type = "log"),
           xaxis = list(range = c(min_year, max_year + 8)))

  d <- data.frame(year = landings$year,
                  pred = exp(landings$est),
                  obs = fit$tmb.data$landings)
  d$ratio <- d$pred / d$obs
  p2 <- plot_ly(data = d, x = ~year, y = ~ratio, showlegend = FALSE) %>%
    add_ribbons(x = fit$tmb.data$years, ymin = fit$tmb.data$LRL, ymax = fit$tmb.data$LRU,
                color = I(cols[2]), opacity = 0.5, line = list(width = 0), name = "Catch bounds") %>%
    add_lines(color = I(cols[1]), name = "Pred/Obs") %>%
    add_lines(y = rep(1, nrow(d)), linetype = I(3), color = I(cols[2])) %>%
    add_text(y = tail(d$ratio, 1), x = max_year + 0.2, text = "Pred/Obs",
             textposition = "right", color = I(cols[1])) %>%
    add_text(y = tail(fit$tmb.data$LRU, 1), x = max_year + 0.2, text = "Bounds",
             textposition = "bottom right", color = I(cols[2])) %>%
    layout(yaxis = list(title = "Predicted/Observed"),
           xaxis = list(range = c(min_year, max_year + 8)))

  subplot(p2, p1, shareX = TRUE, nrows = 2, heights = c(0.4, 0.6), titleY = TRUE)


}




#' Plot with both vertical and horizontal errorbars
#'
#' @details Expects ~ to be used for x, y, x_lwr, etc. args
#'
#' @param data     Plot data
#' @param x        x data
#' @param y        y data
#' @param x_lwr    lower CI for x data
#' @param x_upr    upper CI for x data
#' @param y_lwr    lower CI for y data
#' @param y_upr    upper CI for y data
#' @param xlab     x axis label
#' @param ylab     y axis label
#' @param text     hover text
#' @param col      variable for color coding points (e.g. year)
#' @param clab     label for the colorbar
#'
#' @export
#'

plot_xy_errorbar <- function(data, x = NULL, y = NULL,
                             x_lwr = NULL, x_upr = NULL,
                             y_lwr = NULL, y_upr = NULL,
                             xlab = NULL, ylab = NULL,
                             text = NULL, col = NULL, clab = NULL) {


  col_vec <- data[, labels(terms(col))]
  line_cols <- toRGB(viridis::viridis(length(col_vec))[cut(col_vec, length(col_vec))], alpha = 0.5)

  p <- plot_ly()
  for (i in seq(nrow(data))) {
    p <- p %>%
      add_segments(data = data[i, ], text = text,
                   x = x_lwr, y = y, xend = x_upr, yend = y,
                   name = "95% CI", legendgroup = "95% CI",
                   showlegend = i == 1,
                   line = list(color = line_cols[i])) %>%
      add_segments(data = data[i, ], text = text,
                   x = x, y = y_lwr, xend = x, yend = y_upr,
                   name = "95% CI", legendgroup = "95% CI",
                   showlegend = FALSE,
                   line = list(color = line_cols[i]))
  }

  p %>%
    add_markers(name = "Estimate", data = data, x = x, y = y, color = col,
                text = text) %>%
    colorbar(title = clab) %>%
    layout(xaxis = list(title = xlab),
           yaxis = list(title = ylab))

}







