# Estimated Monthly National Accounts for the United States

This is the replication code for the paper:

Barrett, Philip. ["Estimated Monthly National Accounts for the United
States,"](https://www.imf.org/en/publications/wp/issues/2025/07/03/estimated-monthly-national-accounts-for-the-united-states-568032)
IMF Working Paper.

The paper builds a monthly-frequency estimate of U.S. real GDP (plus 8 NIPA
components) from public monthly indicators, using a state-space model estimated
by GMM with a Kalman filter/smoother. All data used here is pulled from public
sources (FRED, BEA) — no paid subscription or data license is required to
reproduce the results.

## Quick start

1. Install R (>= 4.x) and the packages listed below.
2. Get two free API keys (both are instant, no approval wait):
   - FRED: <https://fred.stlouisfed.org/docs/api/api_key.html>
   - BEA: <https://apps.bea.gov/api/signup/>
3. Copy `.Renviron.example` to `.Renviron` and fill in your keys:
   ```
   FRED_API_KEY=your_key_here
   BEA_API_KEY=your_key_here
   ```
4. From the repo root, run:
   ```r
   Rscript code/R/sessions/mly_GDP.R
   ```

This downloads the underlying data, estimates the model, and writes the results
to `outputs/baseline/`:
- `final_data/output.xlsx` — the monthly GDP and component series, with trend
  and standard-error estimates
- `final_data/everything.rdata` — the full model object, for further analysis
- `graphs/` and `tables/` — the paper's figures and tables

## Regenerating a specific robustness/validation variant

```r
Rscript code/R/sessions/run_variant.R <variant_name>
```

See `code/R/sessions/variant_configs.R` for the full list of variants and how
each one differs from the baseline.

## Data caching

Each pulled series is cached at `data/raw_pulls/<label>.csv`, refreshed
automatically once it's more than 7 days old. This avoids repeated hits to the
FRED/BEA APIs and doubles as an offline fallback. Delete a file in that folder
(or the whole folder) to force a refresh.

## Package dependencies

Core: `dplyr`, `tidyr`, `readr`, `purrr`, `tibble`, `stringr`, `forcats`,
`lubridate`, `ggplot2`, `data.table`, `magrittr`, `here`.

Modeling: `forecast`, `mvtnorm`, `Matrix`, `mFilter`, `ts.extend` (CRAN-archived —
install from the archive: `https://cran.r-project.org/src/contrib/Archive/ts.extend/`).

Data pull: `fredr`, `httr2`.

Output: `openxlsx`, `readxl`, `xtable`, `stargazer`, `sandwich`, `scales`,
`reshape2`.

## Licensing

MIT (see `LICENSE`), with one exception: `code/R/functions/vendor/arma_to_ma.{cpp,R}`
is vendored from the GPL-3-licensed `deseats` package and remains GPL-3 — see
`code/R/functions/vendor/README.md`.

## Provenance

This model was originally developed using Haver Analytics data, which cannot be
redistributed. This repo replaces every Haver-sourced series with a public
equivalent — see `docs/data_sources.md` for the full series-by-series mapping.
