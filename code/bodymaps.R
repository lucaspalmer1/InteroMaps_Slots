# Bodymap figure panels for Study 1 and Study 2.
# Outputs labelled PNG panels into code/bodymaps/study1/ and study2/.

library(magick)
library(dplyr)

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
editor_file <- if (requireNamespace("rstudioapi", quietly = TRUE)) {
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
base <- file.path(base_dir, "data", "raw")
out_root <- file.path(base_dir, "code", "bodymaps")

# Resolve a map file, tolerating the " (1)" suffix present on some Study 1 files
map_file <- function(study, grp, phase, map_type, face, view) {
  dir <- file.path(base, study, grp, phase)   # grp = gamble/non_gamble; phase = premaps/postmaps
  pat <- sprintf("^%s-%s%s( \\([0-9]+\\))?\\.png$", map_type, face, view)
  files <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(files) != 1)
    stop(sprintf("Expected 1 match for '%s' in '%s', found %d", pat, dir, length(files)))
  files
}

build_2x2_panel <- function(ng_pre, ng_post, g_pre, g_post, title, output_path) {
  imgs <- lapply(list(image_read(ng_pre), image_read(ng_post),
                      image_read(g_pre),  image_read(g_post)),
                 function(x) image_resize(x, "400x900"))

  add_label <- function(img, txt) {
    w <- image_info(img)$width
    label_bar <- image_blank(w, 50, color = "black") |>
      image_annotate(txt, size = 30, color = "white", gravity = "Center", location = "+0+0")
    image_append(c(label_bar, img), stack = TRUE)
  }

  img1 <- add_label(imgs[[1]], "NG Pre")
  img2 <- add_label(imgs[[2]], "NG Post")
  img3 <- add_label(imgs[[3]], "G Pre")
  img4 <- add_label(imgs[[4]], "G Post")

  row <- image_append(c(img1, img2, img3, img4))
  w <- image_info(row)$width

  title_bar <- image_blank(w, 60, color = "black") |>
    image_annotate(title, size = 36, color = "white", gravity = "Center")
  legend_bar <- image_blank(w, 40, color = "black") |>
    image_annotate("NG = People without gambling experience; G = People with gambling experience",
                   size = 22, color = "gray80", gravity = "Center")

  final <- image_append(c(title_bar, row, legend_bar), stack = TRUE)
  image_write(final, output_path)
  cat("Saved:", output_path, "\n")
}

build_front_back_panel <- function(
    ng_pre_front,  ng_pre_back,
    ng_post_front, ng_post_back,
    g_pre_front,   g_pre_back,
    g_post_front,  g_post_back,
    title, output_path) {

  paths <- list(
    ng_pre_front, ng_pre_back, ng_post_front, ng_post_back,
    g_pre_front,  g_pre_back,  g_post_front,  g_post_back
  )
  labels <- c(
    "NG Pre\nFront", "NG Pre\nBack", "NG Post\nFront", "NG Post\nBack",
    "G Pre\nFront",  "G Pre\nBack",  "G Post\nFront",  "G Post\nBack"
  )

  imgs <- lapply(paths, function(p) image_resize(image_read(p), "300x900"))

  add_label <- function(img, txt) {
    w <- image_info(img)$width
    label_bar <- image_blank(w, 70, color = "black") |>
      image_annotate(txt, size = 24, color = "white", gravity = "Center", location = "+0+0")
    image_append(c(label_bar, img), stack = TRUE)
  }

  labelled <- mapply(add_label, imgs, labels, SIMPLIFY = FALSE)
  divider <- image_blank(6, image_info(labelled[[1]])$height, color = "gray50")

  row <- image_append(c(
    labelled[[1]], labelled[[2]], labelled[[3]], labelled[[4]],
    divider,
    labelled[[5]], labelled[[6]], labelled[[7]], labelled[[8]]
  ))

  w <- image_info(row)$width
  title_bar <- image_blank(w, 60, color = "black") |>
    image_annotate(title, size = 36, color = "white", gravity = "Center")
  legend_bar <- image_blank(w, 40, color = "black") |>
    image_annotate("NG = People without gambling experience; G = People with gambling experience",
                   size = 22, color = "gray80", gravity = "Center")

  final <- image_append(c(title_bar, row, legend_bar), stack = TRUE)
  image_write(final, output_path)
  cat("Saved:", output_path, "\n")
}

overlay_grid <- function(img, grid_size = 15) {
  info <- image_info(img)
  w <- info$width; h <- info$height
  cw <- w / grid_size; ch <- h / grid_size

  canvas <- image_blank(w, h, color = "transparent")
  canvas <- image_draw(canvas)
  for (i in 1:(grid_size - 1)) {
    x <- round(i * cw)
    lines(c(x, x), c(0, h), col = "#F4A26180", lwd = 1)
  }
  for (i in 1:(grid_size - 1)) {
    y <- round(i * ch)
    lines(c(0, w), c(y, y), col = "#F4A26180", lwd = 1)
  }
  dev.off()

  image_composite(img, canvas, operator = "over")
}

build_density_grid_panel <- function(ng_pre, ng_post, g_pre, g_post, title, output_path, grid_size = 15) {
  imgs <- lapply(list(image_read(ng_pre), image_read(ng_post),
                      image_read(g_pre),  image_read(g_post)),
                 function(x) overlay_grid(image_resize(x, "400x900"), grid_size = grid_size))

  add_label <- function(img, txt) {
    w <- image_info(img)$width
    label_bar <- image_blank(w, 50, color = "black") |>
      image_annotate(txt, size = 30, color = "white", gravity = "Center", location = "+0+0")
    image_append(c(label_bar, img), stack = TRUE)
  }

  img1 <- add_label(imgs[[1]], "NG Pre")
  img2 <- add_label(imgs[[2]], "NG Post")
  img3 <- add_label(imgs[[3]], "G Pre")
  img4 <- add_label(imgs[[4]], "G Post")

  row <- image_append(c(img1, img2, img3, img4))
  w <- image_info(row)$width

  title_bar <- image_blank(w, 60, color = "black") |>
    image_annotate(title, size = 36, color = "white", gravity = "Center")
  legend_bar <- image_blank(w, 40, color = "black") |>
    image_annotate("NG = People without gambling experience; G = People with gambling experience",
                   size = 22, color = "gray80", gravity = "Center")

  final <- image_append(c(title_bar, row, legend_bar), stack = TRUE)
  image_write(final, output_path)
  cat("Saved:", output_path, "\n")
}

# ── Run all panels for both studies ──────────────────────────────────────────
specs <- list(
  list(mtype = "DensityMap", face = "Female", slug = "density_female"),
  list(mtype = "DensityMap", face = "Male",   slug = "density_male"),
  list(mtype = "ValenceMap", face = "Female", slug = "valence_female"),
  list(mtype = "ValenceMap", face = "Male",   slug = "valence_male")
)

build_study_figures <- function(study, study_slug) {
  out_dir <- file.path(out_root, study_slug)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  for (s in specs) {
    mf <- function(grp, phase, view) map_file(study, grp, phase, s$mtype, s$face, view)

    build_2x2_panel(
      ng_pre  = mf("non_gamble", "premaps",  "Front"),
      ng_post = mf("non_gamble", "postmaps", "Front"),
      g_pre   = mf("gamble",     "premaps",  "Front"),
      g_post  = mf("gamble",     "postmaps", "Front"),
      title   = sprintf("%s - %s Front", s$mtype, s$face),
      output_path = file.path(out_dir, paste0(s$slug, "_front.png"))
    )

    build_front_back_panel(
      ng_pre_front  = mf("non_gamble", "premaps",  "Front"),
      ng_pre_back   = mf("non_gamble", "premaps",  "Back"),
      ng_post_front = mf("non_gamble", "postmaps", "Front"),
      ng_post_back  = mf("non_gamble", "postmaps", "Back"),
      g_pre_front   = mf("gamble",     "premaps",  "Front"),
      g_pre_back    = mf("gamble",     "premaps",  "Back"),
      g_post_front  = mf("gamble",     "postmaps", "Front"),
      g_post_back   = mf("gamble",     "postmaps", "Back"),
      title   = sprintf("%s - %s Front + Back", s$mtype, s$face),
      output_path = file.path(out_dir, paste0(s$slug, "_frontback.png"))
    )

    if (s$mtype == "DensityMap") {
      build_density_grid_panel(
        ng_pre  = mf("non_gamble", "premaps",  "Front"),
        ng_post = mf("non_gamble", "postmaps", "Front"),
        g_pre   = mf("gamble",     "premaps",  "Front"),
        g_post  = mf("gamble",     "postmaps", "Front"),
        title   = sprintf("%s - %s Front (15x15 entropy grid)", s$mtype, s$face),
        output_path = file.path(out_dir, paste0(s$slug, "_front_grid.png"))
      )
    }
  }

  cat("Done:", study, "\n")
}

build_study_figures("study1", "study1")
build_study_figures("study2", "study2")

# Participant counts (from the processed analysis data).
df_study1 <- read.csv(file.path(base_dir, "data", "processed", "interomap_study1_data.csv"))
df_study2 <- read.csv(file.path(base_dir, "data", "processed", "interomap_study2_data.csv"))

cat("\nStudy 1 participant counts by group:\n")
print(df_study1 |> distinct(PID, group) |> count(group, name = "n_participants"))
cat("\nStudy 2 participant counts by group:\n")
print(df_study2 |> distinct(PID, group) |> count(group, name = "n_participants"))
