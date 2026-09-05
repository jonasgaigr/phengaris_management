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
# same AIC value for both model classes.
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

#' AIC comparison of a named subset of models.
#'
#' Reproduces the AIC comparisons the original scripts printed to the console,
#' ordered from best to worst and with delta AIC added.
#'
#' @param models  the full list of fitted-model records
#' @param ids     character vector of model ids to compare
aic_comparison <- function(models, ids) {
  ids <- ids[ids %in% names(models)]
  if (!length(ids)) return(data.frame())
  tab <- model_fit_table(models[ids])
  tab <- tab[!is.na(tab$AIC), c("model", "label", "n_obs", "df", "AIC", "BIC", "logLik")]
  if (!nrow(tab)) return(tab)
  tab <- tab[order(tab$AIC), , drop = FALSE]
  tab$delta_AIC <- tab$AIC - min(tab$AIC)
  rownames(tab) <- NULL
  tab
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
