#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: Natura 2000 designation (P4.8)
#
#----------------------------------------------------------#
#
# The Natura 2000 sites (EVL) designated for a species were chosen where its
# populations and their habitat were known, so a plain comparison of records
# inside and outside them mixes the effect of protection with the selection
# of good sites. Across six butterfly monitoring schemes the positive
# relationships with Natura 2000 cover mostly vanished once land cover was
# accounted for (Pellissier et al. 2020). The literature review (P4.8)
# proposed two designs, both reported as associations:
#
#   matching     records inside an EVL designated for their species matched
#                to records outside it on covariates designation cannot have
#                changed (full matching on the propensity score, MatchIt;
#                exactly within the field-form era), then the occupancy model
#                fitted with the matching weights;
#   persistence  at the sites where the species was found in 2012-2018, the
#                chance of finding it again in 2019-2024, inside and outside
#                the EVLs (Kajzer-Bonk & Nowicki 2022 found the declines of
#                both species mainly outside Natura 2000). The EVLs predate
#                both periods, so this tests whether designation goes with
#                populations being maintained.
#
#----------------------------------------------------------#

#' Match the records inside an EVL designated for their species to records
#' outside it.
#'
#' @param data        the records, complete on the covariates
#' @param covariates  the matching covariates
#' @param treatment   the designation flag
#' @param exact       covariates matched exactly
#' @param method      MatchIt method; "full" (full matching, optmatch)
#' @return the matchit object
match_designation <- function(data, covariates, treatment = "EVL_species", exact = "ERA",
                              method = CONFIG$natura2000$matching_method) {
  # Full matching is done by optmatch, which MatchIt only suggests.
  if (method == "full" && !requireNamespace("optmatch", quietly = TRUE)) {
    stop("Full matching needs the package optmatch; run renv::restore().", call. = FALSE)
  }
  MatchIt::matchit(
    stats::reformulate(covariates, response = treatment),
    data = as.data.frame(data),
    method = method, distance = "glm", estimand = "ATT",
    exact = if (length(exact)) stats::reformulate(exact) else NULL
  )
}

#' Covariate balance before and after matching.
#'
#' @param m  match_designation()
#' @return one row per covariate: standardised mean difference and variance
#'         ratio, before and after; the matched sample sizes in attributes
balance_table <- function(m) {
  s <- summary(m, standardize = TRUE)
  before <- as.data.frame(s$sum.all)
  after <- as.data.frame(s$sum.matched)
  data.frame(
    covariate = rownames(before),
    smd_before = before[["Std. Mean Diff."]],
    smd_after = after[rownames(before), "Std. Mean Diff."],
    variance_ratio_before = before[["Var. Ratio"]],
    variance_ratio_after = after[rownames(before), "Var. Ratio"],
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' The matched sample sizes: all and effective, treated and control.
matched_sizes <- function(m) {
  nn <- summary(m)$nn
  data.frame(
    sample = rownames(nn), control = nn[, "Control"], treated = nn[, "Treated"],
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' The designation estimate after matching.
#'
#' Two outcome models on the matched records, both with the matching
#' covariates again (doubly robust) and the season and era terms:
#'
#'   GLMM  glmmTMB with the random intercepts of the revised models and the
#'         matching weights as case weights (model-based CI);
#'   GLM   quasi-binomial GLM with the year as a fixed factor and a CI robust
#'         to clustering within the matching subclasses (sandwich::vcovCL), as
#'         the MatchIt documentation recommends.
#'
#' @param m           match_designation()
#' @param covariates  the matching covariates
#' @param terms       further fixed terms, e.g. the season and era terms
#' @param treatment   the designation flag
#' @return one row per outcome model: the odds ratio of the designation with
#'         its 95 % CI and p-value
designation_estimates <- function(m, covariates, terms, treatment = "EVL_species") {
  md <- MatchIt::match.data(m)
  fixed <- c(treatment, covariates, terms)
  z <- stats::qnorm(0.975)
  row <- function(model, estimate, se, n) {
    data.frame(
      outcome_model = model, records = n, estimate = estimate, std_error = se,
      odds_ratio = exp(estimate), or_low = exp(estimate - z * se), or_high = exp(estimate + z * se),
      p_value = 2 * stats::pnorm(-abs(estimate / se)),
      stringsAsFactors = FALSE
    )
  }
  glmm <- tryCatch(
    suppressWarnings(glmmTMB::glmmTMB(
      stats::as.formula(paste("POSITIVE ~", paste(fixed, collapse = " + "), "+", revised_random_effects())),
      data = md, family = stats::binomial(), weights = weights
    )),
    error = function(e) NULL
  )
  glm <- tryCatch(
    stats::glm(
      stats::as.formula(paste("POSITIVE ~", paste(c(fixed, "factor(YEAR)"), collapse = " + "))),
      data = md, family = stats::quasibinomial(), weights = weights
    ),
    error = function(e) NULL
  )
  rbind(
    if (!is.null(glmm)) {
      co <- summary(glmm)$coefficients$cond
      row("GLMM, matching weights", co[treatment, 1], co[treatment, 2], nrow(md))
    },
    if (!is.null(glm)) {
      v <- sandwich::vcovCL(glm, cluster = md$subclass)
      row("GLM, cluster-robust by subclass", stats::coef(glm)[[treatment]], sqrt(v[treatment, treatment]), nrow(md))
    }
  )
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#
