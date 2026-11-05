# InteroMap model fitting
# -----------------------------------------------------------------------------
# Reads the cleaned public CSVs produced by interomap_data_prep.R and (re)fits:
#   - Study 1: 10 Bayesian mixed models (weak priors), one per outcome.
#   - Study 2: 4 Bayesian mixed models with Study-1-informed priors.
# Saves:
#   analysis/study1_fits.rds  (list of brms fits, keyed by z-scored DV name)
#   analysis/study2_fits.rds  (list of brms fits, keyed by z-scored DV name)
#
# Outcomes use only the per-pixel AVERAGE valence/intensity metrics
# (avg_valence_combined / avg_intensity_combined). The stroke-level valence /
# intensity indices have been dropped.

library(brms)
library(cmdstanr)
library(posterior)
library(dplyr)
library(tidyr)
library(readr)

# Repository root (the directory containing code/, data/, documentation/).
# Searches upward from the script's location and the working directory, so it
# works however the script is run (RStudio "Source", Rscript, or source()).
find_root <- function(d) {
  repeat {
    if (all(dir.exists(file.path(d, c("code", "data", "documentation"))))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) return(NULL)
    d <- parent
  }
}
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE),
                                       value = TRUE)[1])
editor_file <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  p <- rstudioapi::getSourceEditorContext()$path
  if (is.null(p)) "" else p
} else ""
starts <- c(dirname(script_file), dirname(editor_file), getwd())
starts <- starts[!is.na(starts) & nzchar(starts)]
base_dir <- NULL
for (start in starts) {
  base_dir <- find_root(normalizePath(start, mustWork = FALSE))
  if (!is.null(base_dir)) break
}
if (is.null(base_dir)) {
  stop("Repository root (code/, data/, documentation/) not found; ",
       "set the working directory to the repository root and re-run.")
}
SEED <- 8473

# ---- Load cleaned public data ----
df1 <- read_csv(file.path(base_dir, "data", "processed", "interomap_study1_data.csv"), show_col_types = FALSE)
df2 <- read_csv(file.path(base_dir, "data", "processed", "interomap_study2_data.csv"), show_col_types = FALSE)

# ---- Prepare model-ready variables (z-scored within study) ----
prepare <- function(df) {
  df %>%
    mutate(
      avg_valence_z             = as.numeric(scale(avg_valence_combined)),
      avg_intensity_z           = as.numeric(scale(avg_intensity_combined)),
      extent_log_z              = as.numeric(scale(log(extent_combined + 1))),
      valence_by_extent_asinh_z   = as.numeric(scale(asinh(valence_by_extent_combined))),
      intensity_by_extent_asinh_z = as.numeric(scale(asinh(intensity_by_extent_combined))),
      centroid_x_front_z        = as.numeric(scale(centroid_x_front)),
      centroid_y_front_z        = as.numeric(scale(centroid_y_front)),
      sd_x_front_z              = as.numeric(scale(sd_x_front)),
      sd_y_front_z              = as.numeric(scale(sd_y_front)),
      entropy_15_front_z        = as.numeric(scale(entropy_15_front)),
      timepoint = factor(timepoint, levels = c("pre", "post")),
      group     = factor(group, levels = c("non_gambler", "gambler")),
      PID       = factor(PID)
    )
}

df1 <- prepare(df1)
df2 <- prepare(df2)

# ---- Model specification ----
fit_model <- function(dv, data, prior) {
  brm(
    formula = as.formula(paste(dv, "~ timepoint * group + (1 | PID)")),
    data    = data,
    prior   = prior,
    family  = gaussian(),
    iter    = 4000, warmup = 2000, chains = 4, cores = 4,
    seed    = SEED,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.9),
    save_pars    = save_pars(all = TRUE),
    sample_prior = TRUE
  )
}

weak_prior <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 1), class = "sd")
)

# Study 1 outcomes (pre-registered + exploratory; stroke metrics removed)
study1_dvs <- c(
  "avg_valence_z",
  "avg_intensity_z",
  "extent_log_z",
  "valence_by_extent_asinh_z",
  "intensity_by_extent_asinh_z",
  "centroid_x_front_z",
  "centroid_y_front_z",
  "sd_x_front_z",
  "sd_y_front_z",
  "entropy_15_front_z"
)

# Study 2 pre-registered outcomes
study2_dvs <- c(
  "avg_valence_z",
  "centroid_y_front_z",
  "sd_x_front_z",
  "entropy_15_front_z"
)

# ---- Fit Study 1 (weak priors), four models at a time ----
cat("Fitting Study 1 models (", length(study1_dvs), " models)...\n", sep = "")
t0 <- Sys.time()

study1_fits <- lapply(study1_dvs, function(dv) {
  cat("  Study 1:", dv, "\n")
  fit_model(dv, df1, weak_prior)
})
names(study1_fits) <- study1_dvs

cat("Study 1 fitting complete (", format(difftime(Sys.time(), t0)), ").\n", sep = "")
saveRDS(study1_fits, file.path(base_dir, "code", "study1_fits.rds"))

# ---- Build Study-1-informed priors and fit Study 2 ----
# Priors = normal(Study 1 posterior mean, Study 1 posterior SE),
# following Verhagen & Wagenmakers (2014).
build_informed_prior <- function(fit) {
  fx <- posterior_summary(
    fit,
    variable = c("b_Intercept", "b_timepointpost", "b_groupgambler",
                 "b_timepointpost:groupgambler")
  )
  sd_pid <- posterior_summary(fit, variable = "sd_PID__Intercept")

  c(
    set_prior(sprintf("normal(%s, %s)",
                      fx["b_Intercept", "Estimate"],
                      fx["b_Intercept", "Est.Error"]),
              class = "Intercept"),
    set_prior(sprintf("normal(%s, %s)",
                      fx["b_timepointpost", "Estimate"],
                      fx["b_timepointpost", "Est.Error"]),
              class = "b", coef = "timepointpost"),
    set_prior(sprintf("normal(%s, %s)",
                      fx["b_groupgambler", "Estimate"],
                      fx["b_groupgambler", "Est.Error"]),
              class = "b", coef = "groupgambler"),
    set_prior(sprintf("normal(%s, %s)",
                      fx["b_timepointpost:groupgambler", "Estimate"],
                      fx["b_timepointpost:groupgambler", "Est.Error"]),
              class = "b", coef = "timepointpost:groupgambler"),
    set_prior(sprintf("normal(%s, %s)",
                      sd_pid["sd_PID__Intercept", "Estimate"],
                      sd_pid["sd_PID__Intercept", "Est.Error"]),
              class = "sd")
  )
}

cat("\nFitting Study 2 models (", length(study2_dvs), " models, informed priors)...\n", sep = "")
t1 <- Sys.time()

study2_fits <- lapply(study2_dvs, function(dv) {
  cat("  Study 2:", dv, "\n")
  fit_model(dv, df2, build_informed_prior(study1_fits[[dv]]))
})
names(study2_fits) <- study2_dvs

cat("Study 2 fitting complete (", format(difftime(Sys.time(), t1)), ").\n", sep = "")
saveRDS(study2_fits, file.path(base_dir, "code", "study2_fits.rds"))

cat("\nDone. Saved study1_fits.rds and study2_fits.rds.\n")
