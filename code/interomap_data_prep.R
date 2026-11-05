# InteroMap data preparation
# -----------------------------------------------------------------------------
# Reads the de-identified raw inputs in data/raw/ and produces the processed
# analysis files in data/processed/:
#
#   1. Public, de-identified analysis CSVs (one per study) containing the
#      variables needed to reproduce the analyses plus the MAIA-2 items and
#      subscales.
#   2. Demographics CSVs (one per study; PID + age, gender, cultural background,
#      racial lineage) used by the reports for the demographics table.
#
# Raw inputs (data/raw/) have already had Qualtrics/Prolific identifiers and
# metadata columns removed and a sequential participant ID (PID) assigned; the
# pixel-map CSV filenames and stroke CSVs are keyed by the same PID.
#
# Cleaning, applied BEFORE any descriptives/analyses:
#   a. Completion: keep only participants who completed the study. In these
#      exports Qualtrics marks screen-outs as `Finished = 1` / `Progress = 100`,
#      so completion is defined by `eligible == 1` (screen-outs have
#      `eligible == 0` and never saw the demographics, MAIA, task, or body maps).
#   b. Self-report data-quality item (`DQC`): "Do you believe your responses in
#      this survey are thoughtful and honest enough to be used for research
#      purposes?" Participants who answered "No" (`DQC != "1"`) are excluded.
#   c. MAIA-2 attention check (`MAIA-2_34`, instructed to answer "Always"):
#      participants who did not select "Always" (`== 5`) are excluded.
#
# Outcome metrics: only the per-pixel average valence/intensity metrics are
# used. The area-weighted stroke-level "Stroke Valence" / "Stroke Intensity"
# indices are dropped entirely.

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
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

GRID_SIZES <- c(10, 15, 20, 50)

total_pixels_male   <- (403 * 946) * 2
total_pixels_female <- (447 * 946) * 2

# -----------------------------------------------------------------------------
# Pixel-map helpers (rasterised body maps -> per-view + whole-body metrics)
# -----------------------------------------------------------------------------

get_cell_id <- function(x, y, grid_size, img_w, img_h) {
  cell_x <- pmin(pmax(ceiling(x / (img_w / grid_size)), 1), grid_size)
  cell_y <- pmin(pmax(ceiling(y / (img_h / grid_size)), 1), grid_size)
  paste(cell_x, cell_y, sep = "_")
}

read_pixel_maps_raw <- function(zip_path, timepoint_label) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  unzip(zip_path, exdir = tmp_dir)
  files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)

  map_dfr(files, function(f) {
    fname   <- basename(f)
    pid     <- as.integer(sub("-.*", "", fname))
    persona <- ifelse(grepl("Female", fname), "female", "male")
    view    <- ifelse(grepl("Front", fname), "front", "back")
    df <- read_csv(f, show_col_types = FALSE)
    if (nrow(df) == 0) return(NULL)
    df |>
      mutate(
        PID       = pid,
        persona   = persona,
        view      = view,
        timepoint = timepoint_label
      )
  })
}

# A cell is "reachable" for a given persona/view/grid_size if ANY pixel, from
# ANY response, ever landed there. Since the drawing surface is the body
# silhouette (not the full bounding rectangle), many grid cells can never be
# populated; this proxies which cells are in-bounds so entropy can be
# normalised against the true number of possible cells, not grid_size^2.
build_reachable_cells <- function(all_raw) {
  all_raw |>
    distinct(persona, view, x, y) |>
    mutate(img_w = ifelse(persona == "female", 447, 403), img_h = 946) |>
    tidyr::crossing(grid_size = GRID_SIZES) |>
    mutate(cell_id = get_cell_id(x, y, grid_size, img_w, img_h)) |>
    distinct(persona, view, grid_size, cell_id) |>
    count(persona, view, grid_size, name = "n_reachable")
}

calc_entropy <- function(x, y, grid_size, img_w, img_h, n_reachable = NA) {
  cell_id <- get_cell_id(x, y, grid_size, img_w, img_h)

  all_cells <- levels(interaction(1:grid_size, 1:grid_size, sep = "_"))
  cell_id   <- factor(cell_id, levels = all_cells)

  counts <- table(cell_id)
  probs  <- as.numeric(counts) / sum(counts)
  probs  <- probs[probs > 0]

  raw <- -sum(probs * log(probs))
  norm <- if (is.na(n_reachable) || n_reachable <= 1) NA_real_ else raw / log(n_reachable)
  list(raw = raw, norm = norm)
}

summarize_pixel_maps <- function(all_raw, reachable_lookup) {
  all_raw |>
    group_by(PID, persona, view, timepoint) |>
    group_modify(function(df, keys) {
      img_w <- ifelse(keys$persona == "female", 447, 403)
      img_h <- 946

      nr <- function(gs) {
        val <- reachable_lookup |>
          filter(persona == keys$persona, view == keys$view, grid_size == gs) |>
          pull(n_reachable)
        if (length(val) == 0) NA_real_ else val
      }

      e10  <- calc_entropy(df$x, df$y, 10, img_w, img_h, nr(10))
      e15  <- calc_entropy(df$x, df$y, 15, img_w, img_h, nr(15))
      e20  <- calc_entropy(df$x, df$y, 20, img_w, img_h, nr(20))
      e50  <- calc_entropy(df$x, df$y, 50, img_w, img_h, nr(50))

      tibble(
        n_pixels        = nrow(df),
        # Valence/intensity were scored on a -5 to +5 scale (0 = midpoint); the
        # pixel CSVs store them as 1..11, so subtract 6 to centre at 0.
        sum_valence     = sum(df$valence - 6, na.rm = TRUE),
        sum_intensity   = sum(df$intensity - 6, na.rm = TRUE),
        centroid_x      = mean(df$x, na.rm = TRUE),
        centroid_y      = mean(df$y, na.rm = TRUE),
        sd_x            = sd(df$x, na.rm = TRUE),
        sd_y            = sd(df$y, na.rm = TRUE),
        entropy_10      = e10$raw,
        entropy_15      = e15$raw,
        entropy_20      = e20$raw,
        entropy_50      = e50$raw,
        entropy_10_norm = e10$norm,
        entropy_15_norm = e15$norm,
        entropy_20_norm = e20$norm,
        entropy_50_norm = e50$norm
      )
    }) |>
    ungroup()
}

# -----------------------------------------------------------------------------
# MAIA-2 scoring
# -----------------------------------------------------------------------------

maia_item <- function(i) {
  ifelse(i %in% c(5, 6, 7, 8, 9, 10, 11, 12, 15),
         paste0("MAIA-2_", i, "_r"),
         paste0("MAIA-2_", i))
}

maia_subscales <- list(
  MAIA_Noticing    = maia_item(1:4),
  MAIA_NotDistract = maia_item(5:10),
  MAIA_NotWorry    = maia_item(11:15),
  MAIA_AttnReg     = maia_item(16:22),
  MAIA_EmoAware    = maia_item(23:27),
  MAIA_SelfReg     = maia_item(28:31),
  MAIA_BodyListen  = maia_item(32:34),
  MAIA_Trusting    = maia_item(35:37)
)

# Reverse-scored items, expressed in the renumbered 1-37 scheme.
maia_reverse <- paste0("MAIA-2_", c(5, 6, 7, 8, 9, 10, 11, 12, 15))

# -----------------------------------------------------------------------------
# Demographic coding schemes (Qualtrics numeric codes -> labels).
# -----------------------------------------------------------------------------

gender_levels <- c(
  "0" = "Non-binary", "1" = "Woman", "2" = "Man",
  "3" = "Self-described/Other", "4" = "Don't know", "5" = "No answer"
)

cultural_levels <- c(
  "0"  = "African", "1" = "European", "2" = "East Asian", "3" = "South Asian",
  "4"  = "South East Asian", "5" = "Hispanic or Latinx", "6" = "Indigenous",
  "7"  = "Middle Eastern", "8" = "Other", "9" = "Don't know",
  "10" = "Prefer not to answer"
)

racial_levels <- c(
  "0" = "Asian", "1" = "Black", "2" = "European/White", "3" = "Hispanic",
  "4" = "Indigenous", "5" = "Pacific Islander", "6" = "Other",
  "7" = "Don't know", "8" = "Prefer not to answer"
)

# Recode a comma-separated multi-select code column into comma-separated labels.
recode_multi <- function(x, levels) {
  vapply(strsplit(replace_na(as.character(x), ""), ",", fixed = TRUE), function(codes) {
    codes <- trimws(codes)
    codes <- codes[nzchar(codes) & codes %in% names(levels)]
    if (length(codes) == 0) NA_character_ else paste(levels[codes], collapse = ", ")
  }, character(1))
}

# Outcome columns kept in the public CSVs (raw, unstandardised; reports z-score).
outcome_vars <- c(
  "avg_valence_combined",
  "avg_intensity_combined",
  "extent_combined",
  "valence_by_extent_combined",
  "intensity_by_extent_combined",
  "centroid_x_front",
  "centroid_y_front",
  "sd_x_front",
  "sd_y_front",
  "entropy_15_front"
)

# -----------------------------------------------------------------------------
# Full pipeline, parameterised by study
# -----------------------------------------------------------------------------

process_study <- function(study_label,
                          qualtrics_gamble_path,
                          qualtrics_non_gamble_path,
                          pixelmaps_gamble_dir,
                          pixelmaps_non_gamble_dir,
                          public_output_path,
                          demographics_output_path) {

  cat("\n==============================\nProcessing", study_label, "\n==============================\n")

  # 1. Read the de-identified Qualtrics exports (PID already assigned). Columns
  #    are read as character and coerced explicitly below; this matches how the
  #    original Qualtrics metadata rows forced character types during parsing,
  #    and keeps code/multi-select fields (e.g. "Racial lineage") consistent
  #    between the gamble and non-gamble files.
  qualtrics_gamble <- read_csv(
    qualtrics_gamble_path, show_col_types = FALSE,
    col_types = cols(PID = col_integer(), .default = col_character())
  ) |>
    mutate(group = "gambler")

  qualtrics_non_gamble <- read_csv(
    qualtrics_non_gamble_path, show_col_types = FALSE,
    col_types = cols(PID = col_integer(), .default = col_character())
  ) |>
    mutate(group = "non_gambler")

  # 2. Fix non_gamble column inconsistencies (typos / naming differences).
  qualtrics_non_gamble <- qualtrics_non_gamble |>
    rename(
      `persona choice` = `Persona Choice`,
      post_detach_notice = psot_detach_notice,
      DQC = Q1
    )

  # 3. Read pixel maps and calculate the per-pixel average metrics.
  all_raw_pixels <- bind_rows(
    read_pixel_maps_raw(file.path(pixelmaps_gamble_dir, "pixel-maps-premap.zip"), "pre"),
    read_pixel_maps_raw(file.path(pixelmaps_gamble_dir, "pixel-maps-postmap.zip"), "post"),
    read_pixel_maps_raw(file.path(pixelmaps_non_gamble_dir, "pixel-maps-premap.zip"), "pre"),
    read_pixel_maps_raw(file.path(pixelmaps_non_gamble_dir, "pixel-maps-postmap.zip"), "post")
  )

  reachable_cells   <- build_reachable_cells(all_raw_pixels)
  pixel_metrics_raw <- summarize_pixel_maps(all_raw_pixels, reachable_cells)

  # separate front and back, then pivot wide
  pixel_metrics_wide <- pixel_metrics_raw |>
    mutate(
      total_pixels        = ifelse(persona == "male", total_pixels_male, total_pixels_female),
      avg_valence         = sum_valence / n_pixels,
      avg_intensity       = sum_intensity / n_pixels,
      valence_by_extent   = sum_valence * (n_pixels / total_pixels),
      intensity_by_extent = sum_intensity * (n_pixels / total_pixels)
    ) |>
    select(PID, timepoint, view, total_pixels,
           n_pixels, avg_valence, avg_intensity,
           valence_by_extent, intensity_by_extent,
           centroid_x, centroid_y, sd_x, sd_y,
           entropy_10, entropy_15, entropy_20, entropy_50,
           entropy_10_norm, entropy_15_norm, entropy_20_norm, entropy_50_norm) |>
    pivot_wider(
      names_from  = view,
      values_from = c(n_pixels, avg_valence, avg_intensity,
                      valence_by_extent, intensity_by_extent,
                      centroid_x, centroid_y, sd_x, sd_y,
                      entropy_10, entropy_15, entropy_20, entropy_50,
                      entropy_10_norm, entropy_15_norm, entropy_20_norm, entropy_50_norm)
    ) |>
    mutate(
      n_pixels_front               = replace_na(n_pixels_front, 0),
      n_pixels_back                = replace_na(n_pixels_back, 0),
      valence_by_extent_front      = replace_na(valence_by_extent_front, 0),
      valence_by_extent_back       = replace_na(valence_by_extent_back, 0),
      intensity_by_extent_front    = replace_na(intensity_by_extent_front, 0),
      intensity_by_extent_back     = replace_na(intensity_by_extent_back, 0),
      extent_combined              = (n_pixels_front + n_pixels_back) / total_pixels,
      valence_by_extent_combined   = valence_by_extent_front + valence_by_extent_back,
      intensity_by_extent_combined = intensity_by_extent_front + intensity_by_extent_back,
      # whole-body per-pixel mean; NA when nothing was drawn
      avg_valence_combined   = ifelse(n_pixels_front + n_pixels_back == 0, NA_real_,
                                      (replace_na(avg_valence_front, 0)   * n_pixels_front +
                                       replace_na(avg_valence_back, 0)    * n_pixels_back) /
                                      (n_pixels_front + n_pixels_back)),
      avg_intensity_combined = ifelse(n_pixels_front + n_pixels_back == 0, NA_real_,
                                      (replace_na(avg_intensity_front, 0) * n_pixels_front +
                                       replace_na(avg_intensity_back, 0)  * n_pixels_back) /
                                      (n_pixels_front + n_pixels_back))
    )

  pixel_metrics_wide <- pixel_metrics_wide |>
    rename_with(
      ~ str_replace(., "^(entropy_\\d+)_norm_(front|back)$", "\\1_\\2_norm"),
      matches("_norm_(front|back)$")
    )

  # 4. Join pixel metrics to Qualtrics (force pre/post rows per participant so
  #    participants with empty maps are retained).
  timepoints <- tibble(timepoint = c("pre", "post"))

  df_gamble_long <- qualtrics_gamble |>
    crossing(timepoints) |>
    left_join(pixel_metrics_wide, by = c("PID", "timepoint"))

  df_non_gamble_long <- qualtrics_non_gamble |>
    crossing(timepoints) |>
    left_join(pixel_metrics_wide, by = c("PID", "timepoint"))

  df_combined <- bind_rows(df_gamble_long, df_non_gamble_long) |>
    relocate(PID)

  # 5. CLEANING (applied before any descriptives/analyses)
  n_participants <- function(d) n_distinct(d$PID)

  n0 <- n_participants(df_combined)

  # a. Completion: keep only participants who completed the study.
  df_combined <- df_combined |>
    filter(as.numeric(eligible) == 1)
  n1 <- n_participants(df_combined)
  cat("\nCleaning:\n")
  cat("  Excluded (did not complete study / screen-out):", n0 - n1, "\n")

  # b. Self-report data-quality item: exclude "No" (do not use my data).
  df_combined <- df_combined |>
    filter(as.character(DQC) == "1")
  n2 <- n_participants(df_combined)
  cat("  Excluded (DQC answered 'No'):", n1 - n2, "\n")

  # c. MAIA-2 attention check (item 34, instructed "Always" = 5).
  df_combined <- df_combined |>
    filter(as.numeric(`MAIA-2_34`) == 5)
  n3 <- n_participants(df_combined)
  cat("  Excluded (failed MAIA attention check):", n2 - n3, "\n")
  cat("  Participants remaining after cleaning:", n3, "\n")

  # 6. MAIA-2: drop the attention-check item, renumber to 1-37, score subscales.
  df_clean <- df_combined |>
    select(-`MAIA-2_34`)

  maia_old <- c(paste0("MAIA-2_", 1:33), paste0("MAIA-2_", 35:38))
  maia_new <- paste0("MAIA-2_", 1:37)
  df_clean <- df_clean |>
    rename_with(~ maia_new, all_of(maia_old)) |>
    mutate(across(all_of(maia_new), as.numeric))

  # reverse-score the relevant items (kept only transiently for subscale means)
  df_clean <- df_clean |>
    mutate(across(all_of(maia_reverse), ~ 5 - ., .names = "{.col}_r"))

  for (sub in names(maia_subscales)) {
    df_clean[[sub]] <- rowMeans(df_clean[maia_subscales[[sub]]], na.rm = TRUE)
  }

  df_clean <- df_clean |>
    select(-ends_with("_r"))

  # 7. Public, de-identified analysis CSV: analysis variables + MAIA only.
  public <- df_clean |>
    select(all_of(c("PID", "group", "timepoint", outcome_vars, maia_new,
                    names(maia_subscales)))) |>
    arrange(PID, timepoint)

  write_csv(public, public_output_path)
  cat("Wrote public CSV:", public_output_path,
      "(", nrow(public), "rows,", ncol(public), "columns )\n")

  # 8. Demographics CSV (one row per participant).
  demographics <- df_clean |>
    filter(timepoint == "pre") |>
    mutate(
      Age = as.numeric(Age),
      Gender = factor(Gender, levels = names(gender_levels),
                      labels = unname(gender_levels)),
      `Cultural background` = recode_multi(`Cultural background`, cultural_levels),
      `Racial lineage`      = recode_multi(`Racial lineage`, racial_levels)
    ) |>
    select(PID, Age, Gender, `Cultural background`, `Racial lineage`) |>
    arrange(PID)

  write_csv(demographics, demographics_output_path)
  cat("Wrote demographics CSV:", demographics_output_path, "\n")

  invisible(list(public = public, demographics = demographics))
}

# -----------------------------------------------------------------------------
# Run both studies
# -----------------------------------------------------------------------------

study1 <- process_study(
  study_label               = "Study 1",
  qualtrics_gamble_path     = file.path(base_dir, "data", "raw", "study1", "qualtrics_gamble.csv"),
  qualtrics_non_gamble_path = file.path(base_dir, "data", "raw", "study1", "qualtrics_non_gamble.csv"),
  pixelmaps_gamble_dir      = file.path(base_dir, "data", "raw", "study1", "gamble"),
  pixelmaps_non_gamble_dir  = file.path(base_dir, "data", "raw", "study1", "non_gamble"),
  public_output_path        = file.path(base_dir, "data", "processed", "interomap_study1_data.csv"),
  demographics_output_path  = file.path(base_dir, "data", "processed", "interomap_study1_demographics.csv")
)

study2 <- process_study(
  study_label               = "Study 2",
  qualtrics_gamble_path     = file.path(base_dir, "data", "raw", "study2", "qualtrics_gamble.csv"),
  qualtrics_non_gamble_path = file.path(base_dir, "data", "raw", "study2", "qualtrics_non_gamble.csv"),
  pixelmaps_gamble_dir      = file.path(base_dir, "data", "raw", "study2", "gamble"),
  pixelmaps_non_gamble_dir  = file.path(base_dir, "data", "raw", "study2", "non_gamble"),
  public_output_path        = file.path(base_dir, "data", "processed", "interomap_study2_data.csv"),
  demographics_output_path  = file.path(base_dir, "data", "processed", "interomap_study2_demographics.csv")
)

cat("\nDone.\n")
