#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: spatial structure (P4.12)
#
#----------------------------------------------------------#
#
# Step 18 tests the residuals of the revised models for spatial
# autocorrelation (P3.6). The literature review (P4.12) added two checks of
# the spatial structure itself:
#
#   SPDE     the grid random effect (1 | SITMAP) replaced by a continuous
#            spatial random field over the sites, a Gaussian Markov random
#            field on a triangulation (sdmTMB), so that space is not cut at
#            the edges of the grid squares. Compared by AIC and by the
#            estimates.
#   spatial+ a spatial random effect can pull a fixed effect towards or away
#            from zero when the covariate itself varies smoothly in space
#            (Hodges & Reich 2010). spatial+ (Dupont et al. 2022) removes the
#            smooth spatial part of each covariate first: each column of the
#            management terms is replaced by its residual from a smooth of
#            the coordinates, and the model is refitted. An estimate that
#            moves by more than config.yml `revised_models: autocorrelation:
#            spatial_plus_flag_se` standard errors is flagged as confounded
#            with space.
#
#----------------------------------------------------------#

#' Fixed effects of an sdmTMB fit, in the layout of tmb_coef_table().
sdmtmb_coef_table <- function(fit, id, sample) {
  co <- sdmTMB::tidy(fit, effects = "fixed")
  z <- stats::qnorm(0.975)
  data.frame(
    model = id, sample = sample, term = co$term,
    estimate = co$estimate, std_error = co$std.error,
    z_value = co$estimate / co$std.error, p_value = 2 * stats::pnorm(-abs(co$estimate / co$std.error)),
    conf_low = co$estimate - z * co$std.error, conf_high = co$estimate + z * co$std.error,
    ratio = exp(co$estimate), ratio_low = exp(co$estimate - z * co$std.error),
    ratio_high = exp(co$estimate + z * co$std.error),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' The same model with a spatial random field in place of the grid square.
#'
#' The other random intercepts are kept. The mesh is built on the sites'
#' representative points in km, with no two vertices closer than `cutoff`.
#'
#' @param m       a fitted-model record (binomial)
#' @param cutoff  config.yml `revised_models: autocorrelation:
#'                spde_mesh_cutoff_km`
#' @return list(fit, coefficients, AIC, sanity (TRUE when sdmTMB::sanity()
#'         finds nothing wrong), range_km, sigma), or NULL when it failed
fit_spde_model <- function(m, cutoff = CONFIG$revised_models$autocorrelation$spde_mesh_cutoff_km) {
  data <- m$data
  data$X_KM <- data$SITE_X / 1000
  data$Y_KM <- data$SITE_Y / 1000
  groups <- vapply(reformulas::findbars(m$formula_object), function(b) deparse(b[[3]]), character(1))
  groups <- setdiff(groups, "SITMAP")
  for (g in groups) data[[g]] <- factor(data[[g]])
  formula <- stats::update(
    reformulas::nobars(m$formula_object),
    stats::as.formula(paste("~ . +", paste0("(1 | ", groups, ")", collapse = " + ")))
  )
  fit <- tryCatch(
    suppressMessages(suppressWarnings(sdmTMB::sdmTMB(
      formula,
      data = data, mesh = sdmTMB::make_mesh(data, c("X_KM", "Y_KM"), cutoff = cutoff),
      family = stats::binomial(), spatial = "on"
    ))),
    error = function(e) {
      message("  ! SPDE model for ", m$id, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(fit)) {
    return(NULL)
  }
  sanity <- suppressMessages(sdmTMB::sanity(fit, silent = TRUE))
  ran <- sdmTMB::tidy(fit, effects = "ran_pars")
  list(
    fit = fit,
    coefficients = sdmtmb_coef_table(fit, paste0(m$id, "_spde"), m$sample),
    AIC = stats::AIC(fit),
    sanity = isTRUE(sanity$all_ok),
    range_km = ran$estimate[ran$term == "range"][1],
    sigma = ran$estimate[ran$term == "sigma_O"][1]
  )
}

#' spatial+: the model refitted with the spatial part of its covariates
#' removed.
#'
#' The columns of the model matrix of `terms` (the management terms, not the
#' era, season or intercept) are each regressed on a thin-plate smooth of the
#' coordinates, s(SITE_X, SITE_Y, k) (mgcv, Gaussian), and replaced by the
#' residuals.
#'
#' @param m      a fitted-model record (binomial)
#' @param terms  the fixed terms whose columns are residualised
#' @param k      config.yml `revised_models: autocorrelation: spatial_plus_k`
#' @return a fitted-model record of fit_tmb(); its terms are the original
#'         column names with "_sp" appended
fit_spatial_plus <- function(m, terms, k = CONFIG$revised_models$autocorrelation$spatial_plus_k) {
  data <- m$data
  x <- stats::model.matrix(stats::reformulate(terms), data)[, -1, drop = FALSE]
  clean <- paste0(gsub("[^A-Za-z0-9_]", "_", colnames(x)), "_sp")
  for (i in seq_len(ncol(x))) {
    smooth <- mgcv::gam(
      x[, i] ~ s(SITE_X, SITE_Y, k = k),
      data = data.frame(SITE_X = data$SITE_X, SITE_Y = data$SITE_Y), method = "REML"
    )
    data[[clean[i]]] <- stats::residuals(smooth)
  }
  fixed_other <- setdiff(attr(stats::terms(reformulas::nobars(m$formula_object)), "term.labels"), c(terms, colnames(x)))
  # Interactions of the terms are columns of x and so are replaced as well.
  fixed_other <- fixed_other[!vapply(fixed_other, function(t) {
    all(strsplit(t, ":", fixed = TRUE)[[1]] %in% all.vars(stats::reformulate(terms)))
  }, logical(1))]
  bars <- vapply(reformulas::findbars(m$formula_object), function(b) paste0("(", deparse(b), ")"), character(1))
  formula <- stats::as.formula(paste(
    all.vars(m$formula_object)[1], "~", paste(c(clean, fixed_other, bars), collapse = " + ")
  ))
  out <- fit_tmb(paste0(m$id, "_spatialplus"), paste(m$label, "(spatial+)"), formula, data,
    group = "spatial+", sample = m$sample
  )
  out$column_map <- stats::setNames(colnames(x), clean)
  out
}

#' Estimates of a model and of its spatial+ refit, side by side.
#'
#' @param m        the original fitted-model record
#' @param plus     fit_spatial_plus() of it
#' @param flag_se  config.yml `revised_models: autocorrelation:
#'                 spatial_plus_flag_se`
#' @return one row per residualised term: both estimates, the shift in
#'         standard errors of the original, and the flag
spatial_plus_comparison <- function(m, plus, flag_se = CONFIG$revised_models$autocorrelation$spatial_plus_flag_se) {
  original <- tmb_coef_table(list(m))
  refit <- tmb_coef_table(list(plus))
  if (!nrow(original) || !nrow(refit)) {
    return(data.frame())
  }
  refit <- refit[refit$term %in% names(plus$column_map), ]
  refit$term <- unname(plus$column_map[refit$term])
  both <- dplyr::inner_join(
    original[, c("model", "sample", "term", "estimate", "std_error", "ratio", "ratio_low", "ratio_high")],
    refit[, c("term", "estimate", "std_error", "ratio", "ratio_low", "ratio_high")],
    by = "term", suffix = c("", "_spatialplus")
  )
  both$shift_in_se <- (both$estimate_spatialplus - both$estimate) / both$std_error
  both$flag <- abs(both$shift_in_se) > flag_se
  both
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#
