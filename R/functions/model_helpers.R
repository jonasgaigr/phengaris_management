#----------------------------------------------------------#
#
#       Phengaris spp. management in Czechia
#
#       Helper functions: model fitting and result tables
#
#----------------------------------------------------------#
#
# These helpers do not change any model. Each one is still fitted with the same
# function (stats::glm or lme4::glmer), the same formula, the same binomial
# family and the same data subset as in the original scripts. What the helpers
# add is:
#
#   * a stable identifier and a readable label for every model,
#   * capture of fitting errors and warnings, so that one model that fails to
#     converge does not abort the whole cascade,
#   * extraction of the numbers that summary() prints into tidy data frames
#     that can go straight into the manuscript.
#
# Note on AIC. The original scripts compared models with summary(model)$AIC.
# That element only exists for merMod objects, so comparisons involving a glm
# silently returned NULL. The helpers below use stats::AIC(), which returns the
# same AIC value for both model classes, and refit every model of a comparison
# set on one common data frame, because AIC values of models fitted to
# different observations cannot be compared.
#
#----------------------------------------------------------#

#--------------------------------------------------#
## Fitting -----
#--------------------------------------------------#

#' Fit one binomial model and capture whatever happens.
#'
#' @param id       short stable identifier, e.g. "nau_timmet"
#' @param label    human-readable description for tables and the report
#' @param formula  model formula, passed through unchanged
#' @param data     data frame the model is fitted on
#' @param engine   "glmer" for lme4::glmer, "glm" for stats::glm
#' @param group    grouping label used to organise the report, e.g. "Management"
#' @return a list describing the fit; the fit element is NULL if fitting failed
fit_binomial <- function(id, label, formula, data,
                         engine = c("glmer", "glm"), group = NA_character_) {
  engine <- match.arg(engine)

  warnings_seen <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      if (engine == "glmer") {
        lme4::glmer(formula = formula, data = data, family = "binomial")
      } else {
        stats::glm(formula = formula, data = data, family = "binomial")
      },
      error = function(e) {
        structure(conditionMessage(e), class = "model_error")
      }
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  failed <- inherits(fit, "model_error")
  if (failed) {
    message("  ! model ", id, " failed: ", as.character(fit))
  }

  list(
    id       = id,
    label    = label,
    group    = group,
    engine   = engine,
    formula  = paste(deparse(formula), collapse = " "),
    n_input  = nrow(data),
    fit      = if (failed) NULL else fit,
    error    = if (failed) as.character(fit) else NA_character_,
    warnings = if (length(warnings_seen)) {
      paste(unique(warnings_seen), collapse = " | ")
    } else {
      NA_character_
    }
  )
}

#' Fit a list of model specifications.
#'
#' Each element of specs is a list with the arguments of fit_binomial().
#' Returns a list of fitted-model records, named by model id.
fit_binomial_set <- function(specs) {
  out <- lapply(specs, function(s) do.call(fit_binomial, s))
  names(out) <- vapply(out, function(m) m$id, character(1))
  out
}

#--------------------------------------------------#
## Result tables -----
#--------------------------------------------------#

#' Fit statistics, one row per model.
model_fit_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit)) {
      return(data.frame(
        model = m$id, label = m$label, group = m$group, engine = m$engine,
        n_obs = NA_integer_, AIC = NA_real_, BIC = NA_real_,
        logLik = NA_real_, deviance = NA_real_, df = NA_integer_,
        status = "failed", formula = m$formula,
        stringsAsFactors = FALSE
      ))
    }
    ll <- stats::logLik(m$fit)
    data.frame(
      model    = m$id,
      label    = m$label,
      group    = m$group,
      engine   = m$engine,
      n_obs    = tryCatch(stats::nobs(m$fit), error = function(e) NA_integer_),
      AIC      = as.numeric(stats::AIC(m$fit)),
      BIC      = as.numeric(stats::BIC(m$fit)),
      logLik   = as.numeric(ll),
      deviance = tryCatch(as.numeric(stats::deviance(m$fit)), error = function(e) NA_real_),
      df       = as.integer(attr(ll, "df")),
      status   = if (is.na(m$warnings)) "ok" else "fitted with warnings",
      formula  = m$formula,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Diagnostics of one fitted model.
#'
#' Flags what a convergence warning does not catch: a singular fit,
#' (quasi-)complete separation, shown by huge estimates or standard errors,
#' and a random-intercept variance so large that the grouping factor
#' separates the response. Adds the DHARMa tests of residual uniformity and
#' dispersion on simulated quantile residuals. Thresholds and the number of
#' simulations are in config.yml `diagnostics`; the simulations start from
#' config.yml `seed`.
#'
#' @param m  one fitted-model record, from fit_binomial()
#' @return a one-row data frame
model_diagnostics <- function(m, settings = CONFIG$diagnostics, seed = CONFIG$seed) {
  out <- data.frame(
    model = m$id, singular = NA, max_abs_estimate = NA_real_,
    max_std_error = NA_real_, max_random_variance = NA_real_,
    dharma_uniformity_p = NA_real_, dharma_dispersion_p = NA_real_,
    flags = if (is.null(m$fit)) "failed" else "",
    stringsAsFactors = FALSE
  )
  if (is.null(m$fit)) return(out)

  co <- stats::coef(summary(m$fit))
  out$max_abs_estimate <- max(abs(co[, 1]))
  out$max_std_error <- max(co[, 2])
  if (m$engine == "glmer") {
    out$singular <- lme4::isSingular(m$fit)
    out$max_random_variance <- max(as.data.frame(lme4::VarCorr(m$fit))$vcov)
  }

  set.seed(seed)
  residuals <- tryCatch(
    DHARMa::simulateResiduals(m$fit, n = settings$dharma_simulations, plot = FALSE),
    error = function(e) NULL
  )
  if (!is.null(residuals)) {
    out$dharma_uniformity_p <- DHARMa::testUniformity(residuals, plot = FALSE)$p.value
    out$dharma_dispersion_p <- DHARMa::testDispersion(residuals, plot = FALSE)$p.value
  }

  flags <- c(
    if (isTRUE(out$singular)) "singular fit",
    if (out$max_abs_estimate > settings$separation_abs_estimate ||
        out$max_std_error > settings$separation_std_error) "possible separation",
    if (isTRUE(out$max_random_variance > settings$max_random_variance)) "random-effect variance inflated",
    if (!is.na(m$warnings)) "fitting warnings"
  )
  out$flags <- paste(flags, collapse = "; ")
  out
}

#' Diagnostics, one row per model.
model_diagnostics_table <- function(models) {
  do.call(rbind, lapply(models, model_diagnostics))
}

#' Fixed-effect coefficients, one row per model term.
model_coef_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit)) return(NULL)
    co <- stats::coef(summary(m$fit))
    if (is.null(co) || nrow(co) == 0) return(NULL)
    data.frame(
      model     = m$id,
      label     = m$label,
      term      = rownames(co),
      estimate  = co[, 1],
      std_error = co[, 2],
      statistic = co[, 3],
      p_value   = co[, 4],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

#' Random-effect variances, one row per grouping factor (mixed models only).
model_ranef_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit) || m$engine != "glmer") return(NULL)
    vc <- as.data.frame(lme4::VarCorr(m$fit))
    data.frame(
      model    = m$id,
      label    = m$label,
      group    = vc$grp,
      term     = vc$var1,
      variance = vc$vcov,
      sd       = vc$sdcor,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

#' Models that failed to fit or warned, with the reason.
model_issue_table <- function(models) {
  rows <- lapply(models, function(m) {
    if (is.null(m$fit) || !is.na(m$warnings)) {
      return(data.frame(
        model = m$id, label = m$label,
        status = if (is.null(m$fit)) "failed" else "warning",
        message = if (is.null(m$fit)) m$error else m$warnings,
        stringsAsFactors = FALSE
      ))
    }
    NULL
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

#--------------------------------------------------#
## Model comparison -----
#--------------------------------------------------#

#' The one data frame a set of models can be compared on.
#'
#' AIC only compares models fitted to the same observations. The models of a
#' comparison set were often fitted on different subsets (e.g. all records
#' against records with a known site area), and lme4 and glm silently drop
#' rows with NA in a model's own variables, so their n differed. The common
#' data are the records present in every model's data, restricted to those
#' complete on every variable any of the models uses, and with a finite value
#' of every transformed term (log10(0) is -Inf).
#'
#' @param specs  the model specifications of the set (see fit_binomial())
#' @return a data frame
comparison_data <- function(specs) {
  ids <- Reduce(intersect, lapply(specs, function(s) s$data$ID_RECORD))
  data <- specs[[1]]$data
  data <- data[data$ID_RECORD %in% ids, , drop = FALSE]
  vars <- unique(unlist(lapply(specs, function(s) all.vars(s$formula))))
  data <- data[stats::complete.cases(data[, intersect(vars, names(data)), drop = FALSE]), , drop = FALSE]
  finite <- rep(TRUE, nrow(data))
  for (s in specs) {
    mf <- tryCatch(
      stats::model.frame(reformulas::subbars(s$formula), data = data, na.action = stats::na.pass),
      error = function(e) NULL
    )
    if (is.null(mf)) next
    for (col in mf) {
      # poly() terms are matrices, and also numeric, so the matrix test comes
      # first.
      if (is.matrix(col)) {
        finite <- finite & apply(is.finite(col), 1, all)
      } else if (is.numeric(col)) {
        finite <- finite & is.finite(col)
      }
    }
  }
  data[finite, , drop = FALSE]
}

#' AIC comparison of a set of models, refitted on common data.
#'
#' Each model of the set is refitted with its own formula and engine on
#' comparison_data(), so that every row of the table has the same n. The
#' legacy fits themselves, on their own subsets, are reported unchanged in
#' the fit-statistics table.
#'
#' @param specs  all model specifications of the step
#' @param ids    the ids of the models to compare
#' @return the comparison, best first, with delta AIC, the n of the common
#'         data and the n of each model's own fit
aic_comparison <- function(specs, ids) {
  spec_ids <- vapply(specs, function(s) s$id, character(1))
  set <- specs[spec_ids %in% ids]
  if (!length(set)) return(data.frame())

  common <- comparison_data(set)
  n_own <- vapply(set, function(s) {
    vars <- intersect(all.vars(s$formula), names(s$data))
    sum(stats::complete.cases(s$data[, vars, drop = FALSE]))
  }, numeric(1))
  refits <- fit_binomial_set(lapply(set, function(s) {
    s$data <- common
    s
  }))

  tab <- model_fit_table(refits)
  tab$n_own_data <- n_own
  tab <- tab[!is.na(tab$AIC), c("model", "label", "engine", "n_obs", "n_own_data", "df", "AIC", "BIC", "logLik", "status")]
  if (!nrow(tab)) return(tab)
  tab <- tab[order(tab$AIC), , drop = FALSE]
  tab$delta_AIC <- tab$AIC - min(tab$AIC)
  rownames(tab) <- NULL
  stopifnot(length(unique(tab$n_obs)) == 1)
  tab
}

#' Report a list of AIC comparisons, one table each.
#'
#' @param specs        all model specifications of the step
#' @param comparisons  list of list(ids, caption, stem), as returned by
#'                     model_set_for()
report_comparisons <- function(specs, comparisons) {
  for (cmp in comparisons) {
    tab <- aic_comparison(specs, cmp$ids)
    report_table(
      tab,
      paste0(cmp$caption, if (nrow(tab)) sprintf(" (all refitted on the same %d records)", tab$n_obs[1])),
      cmp$stem,
      max_rows = 20
    )
  }
  invisible(NULL)
}

#--------------------------------------------------#
## Reporting a model set -----
#--------------------------------------------------#

#' Write the standard set of result tables for a group of models.
#'
#' Adds fit statistics, fixed effects, random effects and any fitting issues to
#' the currently open report, and writes each of them to CSV.
#'
#' @param models  list of fitted-model records
#' @param prefix  file-name prefix, e.g. "09_nausithous"
report_model_set <- function(models, prefix) {
  fits <- model_fit_table(models)
  report_table(
    fits[, setdiff(names(fits), "formula")],
    "Model fit statistics",
    paste0(prefix, "_fit_statistics"),
    max_rows = nrow(fits)
  )

  coefs <- model_coef_table(models)
  if (nrow(coefs)) {
    report_table(
      coefs, "Fixed-effect coefficients",
      paste0(prefix, "_coefficients"),
      max_rows = nrow(coefs)
    )
  }

  ranefs <- model_ranef_table(models)
  if (nrow(ranefs)) {
    report_table(
      ranefs, "Random-effect variances",
      paste0(prefix, "_random_effects"),
      max_rows = nrow(ranefs)
    )
  }

  diagnostics <- model_diagnostics_table(models)
  report_table(
    diagnostics,
    paste(
      "Model diagnostics: singular fits, signs of separation, inflated",
      "random-effect variances and DHARMa residual tests"
    ),
    paste0(prefix, "_diagnostics"),
    max_rows = nrow(diagnostics)
  )

  issues <- model_issue_table(models)
  if (nrow(issues)) {
    report_table(
      issues, "Models that failed or warned during fitting",
      paste0(prefix, "_fitting_issues"),
      max_rows = nrow(issues)
    )
  }

  # The formulas are kept in their own table so that the methods section of the
  # manuscript can be checked against exactly what was fitted.
  report_table(
    fits[, c("model", "label", "engine", "formula")],
    "Model specifications as fitted",
    paste0(prefix, "_specifications"),
    max_rows = nrow(fits)
  )

  invisible(fits)
}

#----------------------------------------------------------#
# End helpers -----
#----------------------------------------------------------#
