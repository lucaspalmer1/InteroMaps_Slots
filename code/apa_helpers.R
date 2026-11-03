# apa_helpers.R — shared APA formatting + export utilities for the InteroMap reports.
# Tables are exported as APA-bordered .xlsx and plain .csv; figures as 300-dpi PNG.
# Source this from each report with: source(file.path(base_dir, "code", "apa_helpers.R"))

library(knitr)
library(openxlsx)
library(ggplot2)

# ---------------------------------------------------------------------------
# Number / statistic formatting (APA 7th ed.)
# ---------------------------------------------------------------------------

# Round a numeric vector to `digits` and return as character (NA-safe).
fmt_num <- function(x, digits = 2) {
  if (is.character(x)) return(x)
  ifelse(is.na(x), "",
         formatC(round(x, digits), format = "f", digits = digits))
}

# APA p-value string: leading zero dropped; "< .001" for very small values.
fmt_p <- function(p) {
  vapply(p, function(x) {
    if (is.na(x)) return("")
    if (x < 0.001) return("< .001")
    sub("^0", "", sprintf("%.3f", x))
  }, character(1))
}

# ---------------------------------------------------------------------------
# Table export (APA-styled .xlsx + .csv) and inline kable
# ---------------------------------------------------------------------------

apa_table <- function(df, name, dir, digits = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(df, file.path(dir, paste0(name, ".csv")), row.names = FALSE, na = "")

  wb <- createWorkbook()
  addWorksheet(wb, "Table")
  writeData(wb, "Table", df, headerStyle = NULL)
  nc <- ncol(df)
  if (nrow(df) > 0) {
    addStyle(wb, "Table",
             createStyle(textDecoration = "bold",
                         border = c("top", "bottom"), borderStyle = "thin"),
             rows = 1, cols = seq_len(nc))
    addStyle(wb, "Table",
             createStyle(border = "bottom", borderStyle = "thin"),
             rows = nrow(df) + 1, cols = seq_len(nc))
  }
  saveWorkbook(wb, file.path(dir, paste0(name, ".xlsx")), overwrite = TRUE)

  if (is.null(digits)) {
    kable(df, format = "html", row.names = FALSE)
  } else {
    kable(df, digits = digits, format = "html", row.names = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Figure export (300-dpi PNG) — returns the plot so it can also be printed
# ---------------------------------------------------------------------------

apa_fig <- function(p, name, dir, width = 6.5, height = 4) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(name, ".png")), plot = p,
         width = width, height = height, units = "in", dpi = 300)
  # Return invisibly so this only saves the PNG; the caller then prints the
  # plot itself (prevents the plot rendering twice in `apa_fig(p, ...); p`).
  invisible(p)
}

# Save a ggplot to file and return the fragment that embeds it inline at that
# exact position. Use inside `results='asis'` loops: `print(ggplot)` there can
# push the image after later text, so we embed the saved PNG instead.
apa_fig_embed <- function(p, name, dir, width = 6.5, height = 4) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(name, ".png"))
  ggsave(path, plot = p, width = width, height = height, units = "in", dpi = 300)
  # include_graphics() returns the path relativised to the render dir; wrap it
  # in markdown so it can be cat()'d inline in a results='asis' chunk.
  rel <- as.character(knitr::include_graphics(path))
  paste0("![](", rel, ")")
}

# ---------------------------------------------------------------------------
# Sequential APA numbering for tables and figures
# ---------------------------------------------------------------------------

apa_tab_no <- 0L
apa_fig_no <- 0L

apa_tab_caption <- function(title) {
  apa_tab_no <<- apa_tab_no + 1L
  paste0("**Table ", apa_tab_no, "**\n\n*", title, "*")
}

apa_fig_caption <- function(title) {
  apa_fig_no <<- apa_fig_no + 1L
  paste0("**Figure ", apa_fig_no, "**\n\n*", title, "*")
}
