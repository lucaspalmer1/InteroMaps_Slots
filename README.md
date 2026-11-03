# InteroMaps Slots

Data, code, and materials for the InteroMaps slot-machine studies (Study 1 and Study 2).

## Repository structure

```
.
├── code/            # analysis scripts and reports
│   ├── interomap_data_prep.R       # raw -> processed data
│   ├── interomap_fit_models.R      # Bayesian mixed models (saves fitted objects)
│   ├── report_study1.Rmd/.html     # Study 1 report (LME models)
│   ├── report_study2.Rmd/.html     # Study 2 report (LME + Bayesian models)
│   ├── report_maia_exploratory.Rmd/.html   # MAIA subscale control models
│   ├── apa_helpers.R / apa_style.css       # shared formatting helpers
│   ├── apa_output/                  # exported APA tables and figures
│   └── bodymaps/                    # generated body-map figure panels
├── data/
│   ├── raw/        # de-identified raw data
│   └── processed/  # cleaned/tidied data used in the models
└── documentation/  # study documentation (to be added)
```

## Raw data

`data/raw/` contains the de-identified inputs: Qualtrics survey exports, the
per-participant pixel-map zips, stroke-level body-map summaries, and the
aggregate density/valence map images used to build the body-map figure panels.

Identifiers were removed before sharing:

- Qualtrics `ResponseId` and `PROLIFIC_PID` were replaced by a sequential
  participant `PID`.
- Timestamps, completion/progress metadata, device metadata (browser/OS/
  resolution), survey-flow IDs, and all free-text "other, please specify"
  fields were dropped.
- The pixel-map CSV filenames and stroke CSVs are keyed by the same `PID`.

## Processed data

`data/processed/` contains the cleaned, tidy files used in the models:

- `interomap_study1_data.csv`, `interomap_study2_data.csv` — analysis data
  (PID, group, timepoint, outcome metrics, MAIA-2 items and subscales).
- `interomap_study1_demographics.csv`, `interomap_study2_demographics.csv` —
  age, gender, cultural background, and racial lineage per participant.
- `exclusions_table.csv` — participant flow / exclusion counts per study.

## Reproducing the analyses

1. Run `code/interomap_data_prep.R` to regenerate `data/processed/` from
   `data/raw/`.
2. Run `code/interomap_fit_models.R` to fit the Bayesian models.
3. Knit the report `.Rmd` files.

## Model fits

The fitted model objects (`code/study1_fits.rds`, `code/study2_fits.rds`) are
large and are not tracked in this repository (see `.gitignore`). Regenerate them
with `code/interomap_fit_models.R`, or download them from the project's OSF
component.
