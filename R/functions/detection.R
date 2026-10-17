#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: detection sensitivity (P4.4)
#
#----------------------------------------------------------#
#
# The revised models are naive occupancy models: with essentially one visit
# per site and year, a negative record mixes absence with non-detection, and
# what they estimate is the probability of recording the species, the product
# of occupancy and detection (Guillera-Arroita et al. 2014). The literature
# review (P4.4) advised against a single-visit occupancy model as the
# headline, because its detection is identified by the parametric form alone
# (Lele et al. 2012; Knape & Korner-Nievergelt 2015), and proposed two
# sensitivity analyses instead:
#
#   known detection  the management model refitted with the detection fixed
#                    at p: P(record) = p * psi, psi on the logit scale. How
#                    far the management estimates move as p falls from 1
#                    (the naive model) shows how much they could owe to
#                    detection being imperfect.
#   mixed design     a Bayesian occupancy model of the persistent site x year
#                    (spOccupancy::PGOcc), with every visit to it as a
#                    replicate. The few site-years visited more than once
#                    identify the detection, helped by a weakly informative
#                    prior on it (no per-visit detection probability of adult
#                    Phengaris is published); the single visits add to the
#                    occupancy part (von Hirschheydt et al. 2023, Doser et al.
#                    2024). Refitted with a vague prior, to show how much the
#                    detection estimate owes to the prior.
#
# Used by step 23.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Known detection -----
#--------------------------------------------------#

#' A binomial family for a known detection probability.
#'
#' The probability of recording the species is p0 times the inverse logit of
#' the linear predictor: the chance that the site is occupied, times the
#' known chance p0 of finding the species there when it is.
#'
#' @param p0  the detection probability, in (0, 1]
#' @return a binomial family object with the link logit(mu / p0)
known_detection_family <- function(p0) {
  checkmate::assert_number(p0, lower = 0, upper = 1)
  link <- structure(
    list(
      linkfun = function(mu) stats::qlogis(mu / p0),
      linkinv = function(eta) p0 * stats::plogis(eta),
      mu.eta = function(eta) p0 * stats::dlogis(eta),
      valideta = function(eta) TRUE,
      name = sprintf("logit(mu/%g)", p0)
    ),
    class = "link-glm"
  )
  stats::binomial(link = link)
}

#' Refit a revised model with a known detection probability.
#'
#' Neither glmmTMB nor lme4 fits a family with its own link reliably (lme4
#' accepts one, but its numerical Hessian breaks down), so the refit is a
#' GLM: the model's fixed effects, with the year as a fixed factor in place
#' of the random intercepts, as in bias_reduced_refit(). p0 = 1 gives the
#' naive GLM it is compared with. Iteratively reweighted least squares can
#' step outside (0, p0), where the link is undefined, so the likelihood is
#' maximised directly (BFGS, analytic gradient, starting from the naive
#' GLM); the standard errors come from the Hessian. Where some records'
#' share of positives exceeds p0, occupancy runs into 1 and the estimates
#' grow without bound; the status says so.
#'
#' @param m   a fitted-model record (fit_tmb()) of a binomial model
#' @param p0  the detection probability
#' @return a coefficient table like tmb_coef_table(), with p and the fit
#'         status, or NULL when the fit failed
fit_known_detection <- function(m, p0, settings = CONFIG$diagnostics) {
  data <- m$data
  fixed <- reformulas::nobars(m$formula_object)
  if ("YEAR" %in% names(data) && dplyr::n_distinct(data$YEAR) > 1) {
    fixed <- stats::update(fixed, ~ . + factor(YEAR))
  }
  x <- stats::model.matrix(fixed, data)
  y <- data[[all.vars(fixed)[1]]]
  start <- suppressWarnings(stats::glm.fit(x, y, family = stats::binomial()))$coefficients
  # Columns aliased with others (the field-form era with the years) are
  # dropped, as glm() does.
  x <- x[, !is.na(start), drop = FALSE]
  start <- start[!is.na(start)]
  eps <- 1e-12
  negloglik <- function(beta) {
    mu <- pmin(pmax(p0 * stats::plogis(drop(x %*% beta)), eps), 1 - eps)
    -sum(y * log(mu) + (1 - y) * log(1 - mu))
  }
  gradient <- function(beta) {
    eta <- drop(x %*% beta)
    mu <- pmin(pmax(p0 * stats::plogis(eta), eps), 1 - eps)
    -drop(crossprod(x, (y / mu - (1 - y) / (1 - mu)) * p0 * stats::dlogis(eta)))
  }
  fit <- tryCatch(
    stats::optim(start, negloglik, gradient, method = "BFGS", hessian = TRUE, control = list(maxit = 1000)),
    error = function(e) structure(conditionMessage(e), class = "model_error")
  )
  if (inherits(fit, "model_error")) {
    message("  ! known-detection refit of ", m$id, " at p = ", p0, " failed: ", as.character(fit))
    return(NULL)
  }
  covariance <- tryCatch(solve(fit$hessian), error = function(e) NULL)
  se <- if (is.null(covariance)) rep(NA_real_, length(fit$par)) else sqrt(pmax(diag(covariance), 0))
  status <- c(
    if (fit$convergence != 0) "not converged",
    if (is.null(covariance)) "singular Hessian",
    if (any(abs(fit$par[-1]) > settings$separation_abs_estimate)) "estimates unbounded (occupancy at 1)"
  )
  z <- stats::qnorm(0.975)
  data.frame(
    model = m$id, sample = m$sample, p = p0, term = colnames(x),
    estimate = fit$par, std_error = se, p_value = 2 * stats::pnorm(-abs(fit$par / se)),
    ratio = exp(fit$par), ratio_low = exp(fit$par - z * se), ratio_high = exp(fit$par + z * se),
    status = if (length(status)) paste(status, collapse = "; ") else "ok",
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#--------------------------------------------------#
## Mixed-design occupancy model -----
#--------------------------------------------------#

#' Clean column names of a model matrix, usable in a formula.
.matrix_names <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

#' The arrays of an occupancy model with repeated visits.
#'
#' The occupancy unit is a persistent site in a year; every record of the
#' species at it is a visit, in date order. The occupancy covariates are
#' those of the unit's first visit; a unit whose visits disagree on them is
#' counted in `conflicting`.
#'
#' @param data      revised_model_data() of one species and sample, complete
#'                  on the variables of both parts
#' @param occ_terms fixed terms of the occupancy part, e.g. MANAGEMENT_TERMS
#' @param det_terms fixed terms of the detection part, e.g. "ERA", "DOY_STD",
#'                  "I(DOY_STD^2)"
#' @param occ_re    random-intercept groups of the occupancy part (unit-level)
#' @param det_re    random-intercept groups of the detection part
#'                  (visit-level)
#' @param unit      the columns identifying an occupancy unit
#' @return list(y, occ.covs, det.covs, occ_formula, det_formula, occ_columns,
#'         det_columns, design = a one-row summary of the units)
occupancy_arrays <- function(data, occ_terms, det_terms, occ_re = c("SITMAP", "YEAR"),
                             det_re = "OBSERVER", unit = c("SITE_ID", "YEAR")) {
  data <- data %>%
    dplyr::mutate(.unit = do.call(paste, dplyr::across(dplyr::all_of(unit)))) %>%
    dplyr::arrange(.unit, DATUM_OD, ID_LOKAL) %>%
    dplyr::group_by(.unit) %>%
    dplyr::mutate(.visit = dplyr::row_number()) %>%
    dplyr::ungroup()
  units <- unique(data$.unit)
  j <- match(data$.unit, units)
  n_units <- length(units)
  n_visits <- max(data$.visit)
  as_matrix <- function(v) {
    out <- matrix(NA_real_, n_units, n_visits)
    out[cbind(j, data$.visit)] <- v
    out
  }

  occ_x <- stats::model.matrix(stats::reformulate(occ_terms), data)[, -1, drop = FALSE]
  colnames(occ_x) <- .matrix_names(colnames(occ_x))
  det_x <- stats::model.matrix(stats::reformulate(det_terms), data)[, -1, drop = FALSE]
  colnames(det_x) <- .matrix_names(colnames(det_x))

  first <- data$.visit == 1
  occ_covs <- as.data.frame(occ_x[first, , drop = FALSE])
  for (g in occ_re) occ_covs[[g]] <- as.numeric(factor(data[[g]][first]))
  conflicting <- sum(vapply(split(as.data.frame(occ_x), j), function(x) nrow(unique(x)) > 1, logical(1)))

  det_covs <- lapply(as.data.frame(det_x), as_matrix)
  for (g in det_re) det_covs[[g]] <- as_matrix(as.numeric(factor(data[[g]])))

  y <- as_matrix(data$POSITIVE)
  visits <- rowSums(!is.na(y))
  detections <- rowSums(y, na.rm = TRUE)
  re_part <- function(groups) if (length(groups)) paste0(" + ", paste0("(1 | ", groups, ")", collapse = " + ")) else ""

  list(
    y = y,
    occ.covs = occ_covs,
    det.covs = det_covs,
    occ_formula = stats::as.formula(paste("~", paste(colnames(occ_x), collapse = " + "), re_part(occ_re))),
    det_formula = stats::as.formula(paste("~", paste(colnames(det_x), collapse = " + "), re_part(det_re))),
    occ_columns = colnames(occ_x),
    det_columns = colnames(det_x),
    design = data.frame(
      records = nrow(data),
      units = n_units,
      units_visited_twice_or_more = sum(visits > 1),
      visits_in_those_units = sum(visits[visits > 1]),
      units_with_detection = sum(detections > 0),
      units_with_mixed_outcomes = sum(visits > 1 & detections > 0 & detections < visits),
      max_visits = n_visits,
      units_with_conflicting_covariates = conflicting
    )
  )
}

#' Fit the mixed-design occupancy model.
#'
#' spOccupancy::PGOcc, with normal priors on the logit scale: variance
#' `vague_var` on every coefficient but the detection intercept, which gets
#' mean logit(`prior_p`) and variance `prior_var`.
#'
#' @param arrays     occupancy_arrays()
#' @param prior_p    prior mean of the detection probability at the
#'                   reference levels of the detection covariates
#' @param prior_var  prior variance of the detection intercept
#' @param mcmc       list(samples, burn, thin, chains)
#' @return the PGOcc fit, or NULL when it failed
fit_mixed_occupancy <- function(arrays, prior_p, prior_var, mcmc,
                                vague_var = CONFIG$detection$prior_var_vague, seed = CONFIG$seed) {
  n_alpha <- length(arrays$det_columns) + 1
  set.seed(seed)
  tryCatch(
    spOccupancy::PGOcc(
      occ.formula = arrays$occ_formula,
      det.formula = arrays$det_formula,
      data = list(y = arrays$y, occ.covs = arrays$occ.covs, det.covs = arrays$det.covs),
      priors = list(
        beta.normal = list(mean = 0, var = vague_var),
        alpha.normal = list(
          mean = c(stats::qlogis(prior_p), rep(0, n_alpha - 1)),
          var = c(prior_var, rep(vague_var, n_alpha - 1))
        )
      ),
      n.samples = mcmc$samples, n.burn = mcmc$burn, n.thin = mcmc$thin, n.chains = mcmc$chains,
      verbose = FALSE
    ),
    error = function(e) {
      message("  ! occupancy model failed: ", conditionMessage(e))
      NULL
    }
  )
}

#' The largest Rhat of the fixed coefficients of a PGOcc fit.
mcmc_max_rhat <- function(fit) {
  if (is.null(fit)) NA_real_ else max(c(fit$rhat$beta, fit$rhat$alpha), na.rm = TRUE)
}

#' Fit the mixed-design occupancy model, and once more with longer chains
#' when it has not converged.
#'
#' @param ...       passed to fit_mixed_occupancy()
#' @param mcmc      list(samples, burn, thin, chains)
#' @param max_rhat  config.yml `detection: max_rhat`
#' @param factor    config.yml `detection: extend_factor`: the samples, the
#'                  burn-in and the thinning of the second run are this many
#'                  times longer, so that it keeps the same number of draws
#' @return list(fit, extended = whether the second run was needed, mcmc =
#'         the settings of the fit returned)
fit_mixed_occupancy_converged <- function(arrays, prior_p, prior_var, mcmc,
                                          max_rhat = CONFIG$detection$max_rhat,
                                          factor = CONFIG$detection$extend_factor) {
  fit <- fit_mixed_occupancy(arrays, prior_p, prior_var, mcmc)
  if (is.null(fit) || isTRUE(mcmc_max_rhat(fit) <= max_rhat)) {
    return(list(fit = fit, extended = FALSE, mcmc = mcmc))
  }
  longer <- utils::modifyList(mcmc, list(
    samples = mcmc$samples * factor, burn = mcmc$burn * factor, thin = mcmc$thin * factor
  ))
  message("  not converged (Rhat ", round(mcmc_max_rhat(fit), 2), "): refitting with chains ", factor, " times longer")
  list(fit = fit_mixed_occupancy(arrays, prior_p, prior_var, longer), extended = TRUE, mcmc = longer)
}

#' Posterior summaries of a PGOcc fit.
#'
#' @param fit    fit_mixed_occupancy()
#' @param model  label of the fit
#' @return one row per fixed coefficient: part ("occupancy" or "detection"),
#'         term, posterior mean, sd, 2.5 / 50 / 97.5 % quantiles, the same
#'         as odds ratios, Rhat and effective sample size
mixed_occupancy_table <- function(fit, model) {
  if (is.null(fit)) {
    return(data.frame())
  }
  part <- function(samples, rhat, ess, label) {
    s <- as.matrix(samples)
    q <- t(apply(s, 2, stats::quantile, c(0.025, 0.5, 0.975)))
    data.frame(
      model = model, part = label, term = colnames(s),
      mean = colMeans(s), sd = apply(s, 2, stats::sd),
      q2.5 = q[, 1], q50 = q[, 2], q97.5 = q[, 3],
      odds_ratio = exp(colMeans(s)), or_low = exp(q[, 1]), or_high = exp(q[, 3]),
      rhat = as.numeric(rhat), ess = as.numeric(ess),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  rbind(
    part(fit$beta.samples, fit$rhat$beta, fit$ESS$beta, "occupancy"),
    part(fit$alpha.samples, fit$rhat$alpha, fit$ESS$alpha, "detection")
  )
}

#' The detection probability at the reference levels of the detection
#' covariates (the median day, the older field form), and with the newer
#' form, from the posterior.
#'
#' @param fit        fit_mixed_occupancy()
#' @param era_column the detection column of the newer form, or NULL
detection_probability <- function(fit, model, era_column = NULL) {
  if (is.null(fit)) {
    return(data.frame())
  }
  a <- as.matrix(fit$alpha.samples)
  summarise <- function(eta, label) {
    p <- stats::plogis(eta)
    data.frame(
      model = model, quantity = label, mean = mean(p),
      q2.5 = stats::quantile(p, 0.025), q97.5 = stats::quantile(p, 0.975),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  rbind(
    summarise(a[, "(Intercept)"], "p at the median day, older form"),
    if (!is.null(era_column) && era_column %in% colnames(a)) {
      summarise(a[, "(Intercept)"] + a[, era_column], "p at the median day, newer form")
    }
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#
