#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: fitting and diagnosing the revised models
#
#----------------------------------------------------------#
#
# The revised models (steps 17-20) are fitted with glmmTMB. Each fit is a
# record like the ones of fit_binomial() (R/functions/model_helpers.R), with
# the data the model was fitted on, so that the diagnostics, the predictions
# and the residual tests use exactly those records. The helpers add, beyond
# what the legacy helpers do:
#
#   * odds ratios (or rate ratios) with Wald 95 % confidence intervals;
#   * generalised variance inflation factors of the fixed effects;
#   * a formal separation check (detectseparation::detect_separation) and,
#     where it or the size of the estimates suggests separation, a
#     bias-reduced refit without random effects (brglm2::brglmFit);
#   * marginal predicted probabilities with 95 % CIs (marginaleffects);
#   * DHARMa's test of spatial autocorrelation on residuals aggregated per
#     persistent site, and a fit with a smooth of the coordinates in place of
#     the grid random effect (mgcv), to compare against.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Fitting -----
#--------------------------------------------------#

#' Fit one glmmTMB model and capture whatever happens.
#'
#' The records incomplete on the model's variables are dropped first, so that
#' `data` of the returned record holds exactly the records the model used.
#'
#' @param id       short stable identifier
#' @param label    human-readable description
#' @param formula  model formula, with lme4-style random intercepts
#' @param data     data frame
#' @param family   a family object, binomial() by default
#' @param group    grouping label for the report
#' @param sample   which records: "with imputed" or "observed only"
#' @param ziformula  the zero part of a hurdle model (with a truncated
#'                 family), ~0 for none
#' @return a list: id, label, group, sample, engine, family, formula (text),
#'         formula_object, data, n_input, fit (NULL if fitting failed),
#'         error, warnings
fit_tmb <- function(id, label, formula, data, family = stats::binomial(),
                    group = NA_character_, sample = NA_character_, ziformula = ~0) {
  vars <- intersect(c(all.vars(formula), all.vars(ziformula)), names(data))
  data <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]

  warnings_seen <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      glmmTMB::glmmTMB(formula = formula, ziformula = ziformula, data = data, family = family),
      error = function(e) structure(conditionMessage(e), class = "model_error")
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  failed <- inherits(fit, "model_error")
  if (failed) message("  ! model ", id, " failed: ", as.character(fit))

  list(
    id = id, label = label, group = group, sample = sample,
    engine = "glmmTMB", family = family$family,
    formula = paste0(
      paste(deparse(formula, width.cutoff = 500L), collapse = " "),
      if (!identical(ziformula, ~0)) {
        paste0("; zero part ", paste(deparse(ziformula, width.cutoff = 500L), collapse = " "))
      }
    ),
    formula_object = formula,
    data = data,
    n_input = nrow(data),
    fit = if (failed) NULL else fit,
    error = if (failed) as.character(fit) else NA_character_,
    warnings = if (length(warnings_seen)) paste(unique(warnings_seen), collapse = " | ") else NA_character_
  )
}

#' Fit a list of model specifications with fit_tmb().
#'
#' Entries of a spec that fit_tmb() does not take (e.g. the alpha and b of a
#' connectivity profile model) are left for the caller.
fit_tmb_set <- function(specs) {
  out <- lapply(specs, function(s) do.call(fit_tmb, s[intersect(names(s), names(formals(fit_tmb)))]))
  names(out) <- vapply(out, function(m) m$id, character(1))
  out
}

#' Did the optimiser reach a proper optimum?
tmb_converged <- function(m) {
  !is.null(m$fit) && isTRUE(m$fit$sdr$pdHess) && isTRUE(m$fit$fit$convergence == 0)
}

#--------------------------------------------------#
## Result tables -----
#--------------------------------------------------#

#' Fit statistics, one row per model.
tmb_fit_table <- function(models) {
  do.call(rbind, lapply(models, function(m) {
    response <- all.vars(m$formula_object)[1]
    out <- data.frame(
      model = m$id, label = m$label, group = m$group, sample = m$sample,
      n_obs = m$n_input,
      n_positive = if (m$family == "binomial") sum(m$data[[response]] == 1) else NA_integer_,
      AIC = NA_real_, BIC = NA_real_, logLik = NA_real_, df = NA_integer_,
      status = "failed", formula = m$formula,
      stringsAsFactors = FALSE
    )
    if (is.null(m$fit)) {
      return(out)
    }
    ll <- stats::logLik(m$fit)
    out$AIC <- stats::AIC(m$fit)
    out$BIC <- stats::BIC(m$fit)
    out$logLik <- as.numeric(ll)
    out$df <- as.integer(attr(ll, "df"))
    out$status <- if (!tmb_converged(m)) {
      "not converged"
    } else if (is.na(m$warnings)) {
      "ok"
    } else {
      "fitted with warnings"
    }
    out
  }))
}

#' Fixed effects, with Wald 95 % CIs and on the ratio scale.
#'
#' For a binomial model the ratio is the odds ratio, for a count model the
#' rate ratio. The zero part of a hurdle model follows, its terms prefixed
#' "zi:", as odds ratios of a zero.
tmb_coef_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit)) {
      return(NULL)
    }
    parts <- summary(m$fit)$coefficients
    co <- parts$cond
    if (!is.null(parts$zi) && nrow(parts$zi)) {
      zi <- parts$zi
      rownames(zi) <- paste0("zi:", rownames(zi))
      co <- rbind(co, zi)
    }
    if (is.null(co) || !nrow(co)) {
      return(NULL)
    }
    z <- stats::qnorm(0.975)
    data.frame(
      model = m$id, sample = m$sample, term = rownames(co),
      estimate = co[, 1], std_error = co[, 2], z_value = co[, 3], p_value = co[, 4],
      conf_low = co[, 1] - z * co[, 2], conf_high = co[, 1] + z * co[, 2],
      ratio = exp(co[, 1]), ratio_low = exp(co[, 1] - z * co[, 2]), ratio_high = exp(co[, 1] + z * co[, 2]),
      stringsAsFactors = FALSE, row.names = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

#' Random-intercept variances, with the number of levels.
tmb_ranef_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit)) {
      return(NULL)
    }
    vc <- glmmTMB::VarCorr(m$fit)$cond
    if (!length(vc)) {
      return(NULL)
    }
    levels <- vapply(names(vc), function(g) dplyr::n_distinct(m$data[[g]]), integer(1))
    data.frame(
      model = m$id, sample = m$sample, group = names(vc),
      levels = levels,
      variance = vapply(vc, function(v) v[1, 1], numeric(1)),
      sd = vapply(vc, function(v) attr(v, "stddev")[1], numeric(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

#--------------------------------------------------#
## Collinearity -----
#--------------------------------------------------#

#' Generalised variance inflation factors of the fixed effects.
#'
#' Computed from the design matrix of the main effects (interactions and
#' polynomial terms of a variable already in the model left out, since they
#' are collinear with it by construction), as in car::vif(): for a term with
#' df columns, GVIF = det(R_term) det(R_rest) / det(R), and GVIF^(1/(2 df))
#' is comparable to the square root of an ordinary VIF.
#'
#' @param m  a fitted-model record
#' @return one row per term
gvif_table <- function(m) {
  fixed <- reformulas::nobars(m$formula_object)
  labels <- attr(stats::terms(fixed), "term.labels")
  labels <- labels[!grepl(":", labels, fixed = TRUE) & !grepl("^I\\(", labels)]
  if (length(labels) < 2) {
    return(data.frame())
  }
  x <- stats::model.matrix(stats::reformulate(labels), data = m$data)
  assign <- attr(x, "assign")
  x <- x[, -1, drop = FALSE]
  assign <- assign[-1]
  keep <- apply(x, 2, stats::sd) > 0
  x <- x[, keep, drop = FALSE]
  assign <- assign[keep]
  r <- stats::cor(x)
  det_r <- det(r)
  rows <- lapply(unique(assign), function(a) {
    cols <- assign == a
    gvif <- det(r[cols, cols, drop = FALSE]) * det(r[!cols, !cols, drop = FALSE]) / det_r
    data.frame(
      model = m$id, sample = m$sample, term = labels[a], df = sum(cols),
      GVIF = gvif, GVIF_adjusted = gvif^(1 / (2 * sum(cols))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#--------------------------------------------------#
## Separation -----
#--------------------------------------------------#

#' Check a binomial model's fixed effects for separation.
#'
#' Runs detectseparation::detect_separation (a linear program) on the model
#' without its random effects, which shows whether any fixed-effect estimate
#' is infinite.
#'
#' @return a list: separated (TRUE / FALSE / NA) and the infinite terms
separation_check <- function(m) {
  out <- list(separated = NA, terms = NA_character_)
  if (m$family != "binomial" || !nrow(m$data)) {
    return(out)
  }
  check <- tryCatch(
    suppressWarnings(stats::glm(
      reformulas::nobars(m$formula_object),
      data = m$data, family = stats::binomial(), method = detectseparation::detect_separation
    )),
    error = function(e) {
      message("  ! separation check of ", m$id, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(check)) {
    return(out)
  }
  infinite <- names(check$coefficients)[is.infinite(check$coefficients)]
  list(
    separated = isTRUE(check$outcome),
    terms = if (length(infinite)) paste(infinite, collapse = ", ") else NA_character_
  )
}

#' Bias-reduced refit of a binomial model, without random effects.
#'
#' The fallback for a separated model (config.yml `revised_models:
#' bias_reduced_fallback`): mean bias-reduced maximum likelihood keeps every
#' estimate finite. The random intercepts are dropped, since brglm2 fits
#' GLMs only; the year enters as a fixed factor instead.
#'
#' @return a coefficient table like tmb_coef_table(), or NULL
bias_reduced_refit <- function(m) {
  fixed <- reformulas::nobars(m$formula_object)
  if ("YEAR" %in% names(m$data) && dplyr::n_distinct(m$data$YEAR) > 1) {
    fixed <- stats::update(fixed, ~ . + factor(YEAR))
  }
  fit <- tryCatch(
    suppressWarnings(stats::glm(
      fixed,
      data = m$data, family = stats::binomial(), method = brglm2::brglmFit, type = "AS_mean"
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  co <- stats::coef(summary(fit))
  z <- stats::qnorm(0.975)
  data.frame(
    model = paste0(m$id, "_brglm"), sample = m$sample, term = rownames(co),
    estimate = co[, 1], std_error = co[, 2], z_value = co[, 3], p_value = co[, 4],
    conf_low = co[, 1] - z * co[, 2], conf_high = co[, 1] + z * co[, 2],
    ratio = exp(co[, 1]), ratio_low = exp(co[, 1] - z * co[, 2]), ratio_high = exp(co[, 1] + z * co[, 2]),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#--------------------------------------------------#
## Diagnostics -----
#--------------------------------------------------#

#' Diagnostics of one revised model.
#'
#' Flags a fit that did not converge (non-positive-definite Hessian), a
#' random-effect variance at zero, (quasi-)separation, inflated random-effect
#' variances and collinear terms, and adds the DHARMa uniformity and
#' dispersion tests.
#'
#' @return a one-row data frame
tmb_diagnostics <- function(m, settings = CONFIG$diagnostics, revised = CONFIG$revised_models,
                            seed = CONFIG$seed) {
  out <- data.frame(
    model = m$id, sample = m$sample, converged = NA, min_random_sd = NA_real_,
    max_random_variance = NA_real_, max_abs_estimate = NA_real_, max_std_error = NA_real_,
    separation = NA, separated_terms = NA_character_, max_gvif_adjusted = NA_real_,
    dharma_uniformity_p = NA_real_, dharma_dispersion_p = NA_real_,
    flags = if (is.null(m$fit)) "failed" else "",
    stringsAsFactors = FALSE
  )
  if (is.null(m$fit)) {
    return(out)
  }
  out$converged <- tmb_converged(m)
  co <- summary(m$fit)$coefficients$cond
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  if (nrow(co)) {
    out$max_abs_estimate <- max(abs(co[, 1]))
    out$max_std_error <- max(co[, 2], na.rm = TRUE)
  }
  vc <- glmmTMB::VarCorr(m$fit)$cond
  if (length(vc)) {
    sds <- vapply(vc, function(v) attr(v, "stddev")[1], numeric(1))
    out$min_random_sd <- min(sds)
    out$max_random_variance <- max(sds^2)
  }
  sep <- separation_check(m)
  out$separation <- sep$separated
  out$separated_terms <- sep$terms
  gvif <- gvif_table(m)
  if (nrow(gvif)) out$max_gvif_adjusted <- max(gvif$GVIF_adjusted)

  set.seed(seed)
  residuals <- tryCatch(
    DHARMa::simulateResiduals(m$fit, n = settings$dharma_simulations, plot = FALSE),
    error = function(e) NULL
  )
  if (!is.null(residuals)) {
    out$dharma_uniformity_p <- DHARMa::testUniformity(residuals, plot = FALSE)$p.value
    out$dharma_dispersion_p <- DHARMa::testDispersion(residuals, plot = FALSE)$p.value
  }

  separated <- isTRUE(out$separation) ||
    isTRUE(out$max_abs_estimate > settings$separation_abs_estimate) ||
    isTRUE(out$max_std_error > settings$separation_std_error)
  flags <- c(
    if (!isTRUE(out$converged)) "not converged",
    if (isTRUE(out$min_random_sd < 1e-4)) "random-effect variance at zero",
    if (separated) "possible separation",
    if (isTRUE(out$max_random_variance > settings$max_random_variance)) "random-effect variance inflated",
    if (isTRUE(out$max_gvif_adjusted > revised$max_gvif)) "collinear terms",
    if (!is.na(m$warnings)) "fitting warnings"
  )
  out$flags <- paste(flags, collapse = "; ")
  out
}

#' Diagnostics, one row per model.
tmb_diagnostics_table <- function(models) {
  do.call(rbind, lapply(models, tmb_diagnostics))
}

#--------------------------------------------------#
## Marginal predictions -----
#--------------------------------------------------#

#' Marginal predictions with 95 % CIs.
#'
#' Each `by` variable is set counterfactually to each of its values for every
#' record of the model's data, and the predictions (random effects at zero,
#' re.form = NA) are averaged within each combination
#' (marginaleffects::avg_predictions). The linear predictors are averaged
#' and back-transformed, so that the CI, computed on the link scale, stays in
#' range: for a binomial model the estimate is the probability at the
#' records' average linear predictor; for a zero-truncated negative binomial,
#' the expected count given presence, mu / (1 - (theta / (theta + mu))^theta),
#' which increases with mu.
#'
#' @param m   a fitted-model record
#' @param by  the variables to predict for, e.g. c("TIMING", "METHOD")
marginal_predictions <- function(m, by) {
  if (is.null(m$fit) || !all(by %in% names(m$data))) {
    return(data.frame())
  }
  grid <- lapply(by, function(v) sort(unique(m$data[[v]])))
  names(grid) <- by
  back_transform <- switch(m$family,
    binomial = stats::plogis,
    truncated_nbinom2 = {
      theta <- glmmTMB::sigma(m$fit)
      function(eta) exp(eta) / (1 - (theta / (theta + exp(eta)))^theta)
    },
    stop("No back-transformation for the family ", m$family, call. = FALSE)
  )
  p <- tryCatch(
    marginaleffects::avg_predictions(
      m$fit,
      newdata = m$data[, intersect(all.vars(m$formula_object), names(m$data)), drop = FALSE],
      variables = grid, by = by, re.form = NA,
      type = "link", transform = back_transform
    ),
    error = function(e) {
      message("  ! predictions of ", m$id, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(p)) {
    return(data.frame())
  }
  p <- as.data.frame(p)
  data.frame(
    model = m$id, sample = m$sample, p[, by, drop = FALSE],
    estimate = p$estimate, conf_low = p$conf.low, conf_high = p$conf.high,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Predicted occupancy across the flight season.
#'
#' Sets the visit's day to each value of a grid in turn for every record of
#' the model's data, and back-transforms the average linear predictor (random
#' effects at zero), as in marginal_predictions(). With the quadratic season
#' term the day is DOY_STD; with the flight curve it is DOY_DEV, and each
#' record takes the log relative activity of its species' curve at that day
#' and its own elevation.
#'
#' @param m      a fitted-model record with DOY_STD or LOG_ACTIVITY in its
#'               formula
#' @param curve  flight_curve_table() of step 16, for LOG_ACTIVITY
#' @param days   values of DOY_DEV
season_curve <- function(m, curve = NULL, days = seq(min(m$data$DOY_DEV), max(m$data$DOY_DEV), length.out = 25)) {
  if (is.null(m$fit)) {
    return(data.frame())
  }
  vars <- all.vars(m$formula_object)
  newdata <- m$data[, intersect(c(vars, "DRUH", "DOY_DEV", "ELEVATION"), names(m$data)), drop = FALSE]
  predict_at <- function(nd) {
    tryCatch(
      as.data.frame(marginaleffects::avg_predictions(
        m$fit,
        newdata = nd, re.form = NA, type = "link", transform = stats::plogis
      )),
      error = function(e) NULL
    )
  }
  rows <- if ("LOG_ACTIVITY" %in% vars) {
    if (is.null(curve)) stop("season_curve() needs the flight curve for ", m$id, call. = FALSE)
    days <- unique(round(days))
    lapply(days, function(day) {
      nd <- newdata
      nd$LOG_ACTIVITY <- log(relative_activity(nd$DRUH, rep(day, nrow(nd)), nd$ELEVATION, curve))
      p <- predict_at(nd)
      if (is.null(p)) NULL else data.frame(DOY_DEV = day, p[, c("estimate", "conf.low", "conf.high")])
    })
  } else {
    lapply(days, function(day) {
      nd <- newdata
      nd$DOY_STD <- day / CONFIG$revised_models$doy_unit_days
      p <- predict_at(nd)
      if (is.null(p)) NULL else data.frame(DOY_DEV = day, p[, c("estimate", "conf.low", "conf.high")])
    })
  }
  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(rows)) {
    return(data.frame())
  }
  data.frame(
    model = m$id, sample = m$sample, DOY_DEV = rows$DOY_DEV,
    estimate = rows$estimate, conf_low = rows$conf.low, conf_high = rows$conf.high
  )
}

#' The estimates of chosen terms, one row per model and term, formatted.
#'
#' @param coefs  tmb_coef_table() (or a table of the same layout)
#' @param drop   regular expression of the terms to leave out
key_estimates <- function(coefs, drop = "^(zi:)?\\(Intercept\\)$|^(zi:)?ERA|DOY_STD|LOG_ACTIVITY") {
  coefs %>%
    dplyr::filter(!grepl(drop, term)) %>%
    dplyr::transmute(
      model, sample, term,
      ratio_ci = sprintf("%.2f (%.2f-%.2f)", ratio, ratio_low, ratio_high),
      p_value
    )
}

#--------------------------------------------------#
## Spatial autocorrelation -----
#--------------------------------------------------#

#' Moran's I of a model's residuals, aggregated per persistent site.
#'
#' DHARMa's scaled residuals are summed within each site, and the test is run
#' on the sites' mean coordinates (DHARMa::testSpatialAutocorrelation).
#' Several records of one site would otherwise share coordinates.
#'
#' @param m      a fitted-model record (glmmTMB or mgcv)
#' @param fit    the fitted model, m$fit by default
#' @param site   the site column of m$data
#' @return a one-row data frame: sites, observed and expected Moran's I, p
spatial_autocorrelation_test <- function(m, fit = m$fit, site = "SITE_ID", seed = CONFIG$seed,
                                         n_sim = CONFIG$diagnostics$dharma_simulations) {
  out <- data.frame(
    model = m$id, sample = m$sample, sites = NA_integer_,
    moran_observed = NA_real_, moran_expected = NA_real_, p_value = NA_real_,
    stringsAsFactors = FALSE
  )
  if (is.null(fit)) {
    return(out)
  }
  set.seed(seed)
  res <- tryCatch(
    if (inherits(fit, "gam")) {
      # DHARMa cannot simulate from an mgcv fit; the responses are simulated
      # from its fitted probabilities (conditional on the smooths) instead.
      p <- stats::fitted(fit)
      DHARMa::createDHARMa(
        simulatedResponse = matrix(stats::rbinom(length(p) * n_sim, 1, p), nrow = length(p)),
        observedResponse = fit$y, fittedPredictedResponse = p, integerResponse = TRUE
      )
    } else {
      DHARMa::simulateResiduals(fit, n = n_sim, plot = FALSE)
    },
    error = function(e) {
      message("  ! residuals of ", m$id, " could not be simulated: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) {
    return(out)
  }
  group <- m$data[[site]]
  aggregated <- DHARMa::recalculateResiduals(res, group = group)
  # recalculateResiduals() aggregates in the order of the sorted group levels.
  xy <- stats::aggregate(m$data[, c("SITE_X", "SITE_Y")], by = list(site = factor(group)), FUN = mean)
  test <- DHARMa::testSpatialAutocorrelation(aggregated, x = xy$SITE_X, y = xy$SITE_Y, plot = FALSE)
  out$sites <- nrow(xy)
  out$moran_observed <- unname(test$statistic["observed"])
  out$moran_expected <- unname(test$statistic["expected"])
  out$p_value <- test$p.value
  out
}

#' The same model with a smooth of the coordinates instead of the grid.
#'
#' The grid random effect (1 | SITMAP) is replaced by a thin-plate smooth
#' s(SITE_X, SITE_Y, k); the other random intercepts become random-effect
#' smooths (bs = "re"). Fitted with mgcv::bam by fast REML.
#'
#' @param m  a fitted-model record (binomial)
#' @param k  basis dimension of the spatial smooth
#' @return the fitted gam, or NULL when it fails
fit_spatial_smooth <- function(m, k = CONFIG$revised_models$autocorrelation$smooth_k) {
  fixed <- reformulas::nobars(m$formula_object)
  groups <- vapply(reformulas::findbars(m$formula_object), function(b) deparse(b[[3]]), character(1))
  groups <- setdiff(groups, "SITMAP")
  data <- m$data
  for (g in groups) data[[g]] <- factor(data[[g]])
  smooth <- c(
    sprintf("s(SITE_X, SITE_Y, k = %d)", k),
    sprintf("s(%s, bs = \"re\")", groups)
  )
  formula <- stats::update(fixed, stats::as.formula(paste("~ . +", paste(smooth, collapse = " + "))))
  tryCatch(
    suppressWarnings(mgcv::bam(formula, data = data, family = stats::binomial(), method = "fREML")),
    error = function(e) {
      message("  ! smooth model for ", m$id, " failed: ", conditionMessage(e))
      NULL
    }
  )
}

#' Fixed effects of a gam, in the layout of tmb_coef_table().
gam_coef_table <- function(fit, id, sample) {
  co <- summary(fit)$p.table
  z <- stats::qnorm(0.975)
  data.frame(
    model = id, sample = sample, term = rownames(co),
    estimate = co[, 1], std_error = co[, 2], z_value = co[, 3], p_value = co[, 4],
    conf_low = co[, 1] - z * co[, 2], conf_high = co[, 1] + z * co[, 2],
    ratio = exp(co[, 1]), ratio_low = exp(co[, 1] - z * co[, 2]), ratio_high = exp(co[, 1] + z * co[, 2]),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#--------------------------------------------------#
## Reporting a set of revised models -----
#--------------------------------------------------#

#' Write the standard tables for a set of revised models.
#'
#' Fit statistics, fixed effects (with odds or rate ratios and CIs), random
#' effects, diagnostics, collinearity, the bias-reduced refits of separated
#' models and the formulas.
#'
#' @param models  list of fitted-model records from fit_tmb()
#' @param prefix  file-name prefix, e.g. "17_nausithous"
#' @param ratio   what the ratio columns are: "odds ratio" or "rate ratio"
#' @return the diagnostics table, invisibly
report_tmb_set <- function(models, prefix, ratio = "odds ratio") {
  fits <- tmb_fit_table(models)
  report_table(
    fits[, setdiff(names(fits), "formula")], "Model fit statistics",
    paste0(prefix, "_fit_statistics"),
    max_rows = nrow(fits)
  )

  coefs <- tmb_coef_table(models)
  if (nrow(coefs)) {
    report_csv(
      coefs,
      sprintf("Fixed effects on the link scale, and as %ss with 95%% Wald CIs (ratio, ratio_low, ratio_high)", ratio),
      paste0(prefix, "_coefficients")
    )
  }

  ranefs <- tmb_ranef_table(models)
  if (nrow(ranefs)) {
    report_csv(ranefs, "Random-intercept variances", paste0(prefix, "_random_effects"))
  }

  diagnostics <- tmb_diagnostics_table(models)
  report_table(
    diagnostics[, c(
      "model", "sample", "converged", "separation", "max_gvif_adjusted",
      "dharma_uniformity_p", "dharma_dispersion_p", "flags"
    )],
    paste(
      "Diagnostics: convergence, separation (detectseparation and the",
      "size of the estimates), the largest adjusted GVIF, and the DHARMa",
      "uniformity and dispersion tests; the CSV has the random-effect",
      "variances and the largest estimates too"
    ),
    paste0(prefix, "_diagnostics"),
    max_rows = nrow(diagnostics)
  )

  gvif <- do.call(rbind, lapply(Filter(function(m) !is.null(m$fit), models), gvif_table))
  if (!is.null(gvif) && nrow(gvif)) {
    report_csv(
      gvif,
      "Generalised variance inflation factors of the main effects (GVIF_adjusted = GVIF^(1/(2 df)))",
      paste0(prefix, "_gvif")
    )
  }

  separated <- models[diagnostics$model[grepl("separation", diagnostics$flags)]]
  # Models that differ only in their random effects have the same refit.
  fixed_key <- vapply(separated, function(m) {
    paste(paste(deparse(reformulas::nobars(m$formula_object)), collapse = ""), nrow(m$data))
  }, character(1))
  separated <- separated[!duplicated(fixed_key)]
  binomial <- all(vapply(separated, function(m) m$family == "binomial", logical(1)))
  if (length(separated) && isTRUE(CONFIG$revised_models$bias_reduced_fallback) && binomial) {
    refits <- do.call(rbind, lapply(separated, bias_reduced_refit))
    if (!is.null(refits) && nrow(refits)) {
      report_table(
        refits[, c("model", "sample", "term", "estimate", "std_error", "p_value", "ratio", "ratio_low", "ratio_high")],
        paste(
          "Bias-reduced refits (brglm2, mean bias reduction) of the models",
          "flagged for possible separation, without random effects and with",
          "the year as a fixed factor"
        ),
        paste0(prefix, "_bias_reduced"),
        max_rows = nrow(refits)
      )
    }
  }

  issues <- fits[fits$status != "ok", c("model", "sample", "status")]
  if (nrow(issues)) {
    issues$message <- vapply(models[issues$model], function(m) {
      if (is.null(m$fit)) m$error else if (!is.na(m$warnings)) m$warnings else "non-positive-definite Hessian"
    }, character(1))
    report_table(issues, "Models that failed, warned or did not converge", paste0(prefix, "_fitting_issues"))
  }

  report_csv(
    fits[, c("model", "sample", "formula")], "Model specifications as fitted",
    paste0(prefix, "_specifications")
  )
  invisible(diagnostics)
}

#' AIC comparison of models fitted on the same records.
#'
#' Stops when the models' n differ: their AIC values would not be comparable.
#'
#' @param models  fitted-model records
#' @param ids     the ids to compare
aic_table_tmb <- function(models, ids) {
  tab <- tmb_fit_table(models[ids])
  tab <- tab[!is.na(tab$AIC), c("model", "label", "sample", "n_obs", "df", "AIC", "logLik", "status")]
  if (!nrow(tab)) {
    return(tab)
  }
  stopifnot(length(unique(tab$n_obs)) == 1)
  tab <- tab[order(tab$AIC), , drop = FALSE]
  tab$delta_AIC <- tab$AIC - min(tab$AIC)
  rownames(tab) <- NULL
  tab
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#
