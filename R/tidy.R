
#' Makes one data.frame from a list of many
#'
#' @param l         A list containing data.frames.
#' @param add_id    If the list is named and add_id is TRUE, an id column will be added.
#' @export
#'

rbind_list <- function(l, add_id = TRUE) {
  all <- do.call("rbind", l)
  if (add_id) { all$id <- rep(names(l), sapply(l, nrow)) }
  rownames(all) <- NULL
  all
}


#' Tidy value output from sdreport
#'
#' @param model  model object produced by \code{\link{fit_model}}
#'
#' @return Returns a list of data.frames with estimates (est), standard deviations (sd),
#' lower (lwr) and upper (upr) 95% confidence intervals of reported objects
#'
#' @export
#'

tidy_sdreport <- function(model) {

  ## Extract key objects
  sd.rep <- model$sd.rep
  tmb.data <- model$tmb.data
  values <- sd.rep$value
  sds <- sd.rep$sd
  lwr <- values - qnorm(0.975) * sds
  upr <- values + qnorm(0.975) * sds
  value_names <- sort(unique(names(values)))

  ## Split sdreport by name
  split_values <- split(unname(values), names(values))
  split_sds <- split(unname(sds), names(values))
  split_lwr <- split(unname(lwr), names(values))
  split_upr <- split(unname(upr), names(values))

  ## Convert to data.frame and add key identifiers
  l <- vector("list", length(split_values))
  names(l) <- names(split_values)
  for (nm in value_names) {
    vals <- data.frame(est = split_values[[nm]], sd = split_sds[[nm]], lwr = split_lwr[[nm]], upr = split_upr[[nm]])
    if (grepl("^Cproj|^Fproj|^Hproj", nm)) {
      if (grepl("^Cproj", nm)) scenario <- tmb.data$Cp
      if (grepl("^Fproj", nm)) scenario <- tmb.data$Fp
      if (grepl("^Hproj", nm)) scenario <- tmb.data$HCR
      vals <- cbind(expand.grid(scenario = scenario, year = tmb.data$proj_years), vals)
    } else {
      if (length(split_values[[nm]]) == tmb.data$Y) {
        vals <- cbind(data.frame(year = tmb.data$years), vals)
      }
      if (length(split_values[[nm]]) == tmb.data$Y + 1) {
        vals <- cbind(data.frame(year = tmb.data$years_plus1), vals)
      }
    }
    l[[nm]] <- vals
  }
  l

}



#' Tidy model output
#'
#' This function wrangles output from one or more models produced by \code{\link{fit_model}}
#'
#' @param ...           Pass one or more model objects by name.
#' @param model_list    Alternate option to ...; pass one or more model objects in a named list.
#'
#' @export
#'

tidy_model <- function(..., model_list = NULL) {

  if (is.null(model_list)) {
    model_list <- list(...)
  }
  sdreps <- lapply(model_list, tidy_sdreport)
  values <- names(which(table(unlist(lapply(sdreps, names))) == length(sdreps))) # convoluted way to get matching names only
  l <- vector("list", length(values))
  names(l) <- values
  for (nm in values) {
    l[[nm]] <- rbind_list(lapply(sdreps, `[[`, nm))
    names(l[[nm]])[names(l[[nm]]) == "id"] <- "model"
  }
  l

}



#' Tidy retrospective model fits
#'
#' This function wrangles output appended to model object by \code{\link{run_retro}}
#'
#' @inheritParams run_retro
#'
#' @export
#'

tidy_retro <- function(model) {

  max_year <- max(model$tmb.data$years)
  last <- tidy_model(model)
  retro <- tidy_model(model_list = model$retro)
  nms <- names(retro)
  retro <- lapply(nms, function(nm) {
    ind <- names(retro[[nm]]) == "model"
    names(retro[[nm]])[ind] <- "terminal_year"
    retro[[nm]]$terminal_year <- as.numeric(retro[[nm]]$terminal_year)
    last[[nm]]$terminal_year <- max_year
    d <- rbind(retro[[nm]], last[[nm]])
    d$terminal_year <- factor(d$terminal_year)
    d
  })
  names(retro) <- nms
  retro

}

#' Transform output from 'tidy' functions
#'
#' @param transform    Function to use for transforming the data
#' @param scale        Divide values by this number to convert units
#'
#' @export
#'

trans_est <- function(data, transform = exp, scale = 1) {
  if (!is.null(transform)) {
    data[, c("est", "lwr", "upr")] <-  transform(data[, c("est", "lwr", "upr")])
  }
  data[, c("est", "lwr", "upr")] <- data[, c("est", "lwr", "upr")] / scale
  data
}


#' Convert age / year matrix to a data.frame
#'
#' @param mat    matrix
#' @param ages   vector of ages
#' @param years  vector of years
#' @param value  name of value
#'
#' @export
#'

tidy_mat <- function(mat, ages = NULL, years = NULL, value = NULL) {
  dimnames(mat) <- list(age = ages, year = years)
  dat <- as.data.frame.table(mat, responseName = value, stringsAsFactors = FALSE)
  dat$age <- as.numeric(dat$age)
  dat$year <- as.numeric(dat$year)
  dat
}


#' Gather and combine two matricies
#'
#' @param mats    named list of two matricies to combine
#' @param ages    vector of ages
#' @param years   vector of years
#'
#' @export
#'

tidy_mats <- function(mats, ages = NULL, years = NULL) {

  d_list <- lapply(names(mats), function(m) {
    tidy_mat(mats[[m]], ages = ages, years = years, value = m)
  })

  d <- merge(d_list[[1]], d_list[[2]], by = c("age", "year"))
  if (length(d_list) > 2) {
    d <- lapply(d_list[3:length(d_list)], function(x, y) merge(x, y, by = c("age", "year")), d)
    d <- d[[1]]
  }

  d

}




#' Produce a nice table of parameter estimates
#'
#' @param model model object produced by \code{\link{fit_model}}
#'
#' @return Returns a data.frame formated for use with Rmarkdown and knitr::kable
#'
#' @export
#'

tidy_pars <- function(model) {

  max_year <- max(model$tmb.data$years)
  max_year_plus1 <- max(model$tmb.data$years_plus1)
  tabs <- tidy_model(model)
  est <- as.list(model$sd.rep, "Est")
  sd <- as.list(model$sd.rep, "Std")

  ## List of values to extract along with the scale - conversion function - name - symbol
  par_det <- c("log_std_log_RV_I" = "1 - exp - RV survey observation error - $\\sigma_{RV}$",
               "log_std_log_SN_GN_I" = "1 - exp - SN survey observation error - $\\sigma_{SN}$",
               "log_std_SN_GN_Qy" = "1 - exp - SN q random walk error - $\\sigma_{SN_{RW}}$",
               "log_std_lcp" = "1 - exp - Age composition error - $\\sigma_{P}$",
               "log_std_pe" = "1 - exp - Process error variance - $\\sigma_{\\delta}$",
               "logit_ar_pe_age" = "1 - plogis - Age correlation in process errors - $\\varphi_{\\delta, age}$",
               "logit_ar_pe_year" = "1 - plogis - Year correlation in process errors - $\\varphi_{\\delta, year}$",
               "log_std_logF" = "1 - exp - F variance parameter - $\\sigma_{F}$",
               "logit_ar_logF_age" = "1 - plogis - Age correlation in F - $\\varphi_{F,age}$",
               "logit_ar_logF_year" = "1 - plogis - Year correlation in F - $\\varphi_{F,year}$",
               "log_std_D" = "1 - exp - D variance parameter - $\\sigma_{D}$",
               "logit_ar_D_age" = "1 - plogis - Age correlation in D - $\\varphi_{D,age}$",
               "logit_ar_D_year" = "1 - plogis - Year correlation in D - $\\varphi_{D,year}$",
               "log_R1" = "1000000 - exp - Mean log-recruitment (pre 1992; million) - $r_1$",
               "log_R2" = "1000000 - exp - Mean log-recruitment (post 1992; million) - $r_2$",
               "log_sd_log_R" = "1 - exp - Variance of log-recruitment - $\\sigma_{r}$",
               "log_std_old_Udev" = "1 - exp - Variance of tagging F deviation from stock F (pre 1997) - $\\sigma_{f_{x,1}}$",
               "log_std_Udev" = "1 - exp - Variance of tagging F deviation from stock F (post 1997) - $\\sigma_{f_{x,2}}$",
               "log_old_tag_k" = "1 - exp - NB overdispersion parameter for pre 1997 tag experiments - $\\kappa_1$",
               "log_tag_k" = "1 - exp - NB overdispersion parameter for post 1997 tag experiments - $\\kappa_2$")

  par_tab <- lapply(names(par_det), function(n) {
    det <- unname(unlist(strsplit(par_det[n], " - ")))
    data.frame("Quantity" = det[3],
               "Symbol" = det[4],
               "Estimate" = do.call(det[2], list(est[[n]])) / as.numeric(det[1]),
               "CV" = sd[[n]])
  })
  par_tab <- do.call(rbind, par_tab)
  rownames(par_tab) <- names(par_det)

  ## Values to extract from sdreport
  ## YEAR is replaced with max_year object
  sdrep_det <- c("log_blim" = "1000 - exp - Limit reference point (kt) - $B_{lim}$",
                 "log_ssb" = "1000 - exp - YPLUS spawning stock biomass (kt) - $SSB_{YPLUS}$",
                 "log_ssb_lrp" = "1 - exp - YPLUS relative spawning stock biomass - $SSB_{YPLUS} / B_{lim}$",
                 "log_biomass" = "1000 - exp - YPLUS total biomass (kt) - $B_{YPLUS}$",
                 "log_aveZ" = "1 - exp - YEAR average total mortality - $\\bar{Z}_{5+,YEAR}$",
                 "log_aveM" = "1 - exp - YEAR average natural mortality - $\\bar{M}_{5+,YEAR}$",
                 "log_aveF" = "1 - exp - YEAR average fishing mortality - $\\bar{F}_{5+,YEAR}$")
  sdrep_det <- gsub("YEAR", max_year, sdrep_det)
  sdrep_det <- gsub("YPLUS", max_year_plus1, sdrep_det)

  sdrep_tab <- lapply(names(sdrep_det), function(n) {
    det <- unname(unlist(strsplit(sdrep_det[n], " - ")))
    last_est <- tail(tabs[[n]], 1)
    data.frame("Quantity" = det[3],
               "Symbol" = det[4],
               "Estimate" = do.call(det[2], list(last_est$est)) / as.numeric(det[1]),
               "CV" = last_est$sd)
  })
  sdrep_tab <- do.call(rbind, sdrep_tab)
  rownames(sdrep_tab) <- names(sdrep_det)

  k <- length(model$opt$par)
  nll <- model$opt$objective
  n <- Inf
  aic <- 2 * k + 2 * nll + ((2 * (k + 1) * (k + 2))/(n - k - 1))
  lik_tab <- data.frame("Quantity" = c("Number of parameters", "Negative log-likelihood", "Akaike information criterion"),
                        "Symbol" = c("$k$", "$ln(L)$", "$AIC$"),
                        "Estimate" = c(k, nll, aic),
                        "CV" = c(NA, NA, NA),
                        row.names = c("k", "nll", "aic"))

  rbind(lik_tab, par_tab, sdrep_tab)

}




