# Estimated Monthly National Accounts for the United States

Replication code for Philip Barrett's monthly GDP nowcasting model: a state-space
model (GMM-estimated state/observation equations, Kalman filter/smoother) that
estimates a monthly-frequency U.S. real GDP series (plus 8 NIPA components) from
public monthly indicators.

This is the **public replication repo** — minimal runnable code only. It pulls all
data from public sources (FRED, BEA); no data license or paid subscription is
required to reproduce the baseline results.

## Setup

1. R (tested on R >= 4.x) with the packages listed below.
2. Free API keys:
   - FRED: <https://fred.stlouisfed.org/docs/api/api_key.html> (instant signup)
   - BEA: <https://apps.bea.gov/api/signup/> (instant signup)
3. Copy `.Renviron.example` to `.Renviron` and fill in your keys:
   ```
   FRED_API_KEY=your_key_here
   BEA_API_KEY=your_key_here
   ```

## Running the baseline model

```r
# from the repo root
Rscript code/R/sessions/mly_GDP.R
```

This pulls fresh data from FRED/BEA (cached locally in `data/raw_pulls/` — see
below), estimates the model, and writes results to `outputs/baseline/`:
`final_data/output.xlsx` (the estimated series + trend + standard errors),
`final_data/everything.rdata` (full model object, for further analysis),
`graphs/` and `tables/` (the paper's figures/tables).

### Regenerating a specific robustness/validation variant

```r
Rscript code/R/sessions/run_variant.R <variant_name>
```

See `code/R/sessions/variant_configs.R` for the full list of variants and exactly
how each one differs from the baseline (every entry there is documented with the
reasoning behind it, not just the override values).

### Data caching

Each pulled series' raw API response is cached at `data/raw_pulls/<label>.csv`,
refreshed automatically once it's more than 7 days old (or immediately via
`options$force.refresh = TRUE`). This avoids hammering the FRED/BEA APIs on
repeated runs and doubles as an offline fallback. Delete a file in that folder
(or the whole folder) to force a full refresh.

### Validating your pull against the published results

```r
Rscript code/R/sessions/validate_data_sources.R          # checks each series against archived reference data
Rscript code/R/sessions/validate_against_prior_release.R # checks the full model output against a prior release
```

## Package dependencies

Core: `dplyr`, `tidyr`, `readr`, `purrr`, `tibble`, `stringr`, `forcats`,
`lubridate`, `ggplot2`, `data.table`, `magrittr`, `here`.

Modeling: `forecast`, `mvtnorm`, `Matrix`, `mFilter`, `ts.extend` (CRAN-archived —
install from the archive: `https://cran.r-project.org/src/contrib/Archive/ts.extend/`).

Data pull: `fredr`, `httr2`.

Output: `openxlsx`, `readxl`, `xtable`, `stargazer`, `sandwich`, `scales`,
`reshape2`.

`renv.lock` is not yet included in this repo — a known gap, on the list to add
before submission (needed for the target journal's replication-archive policy).

## Licensing

MIT (see `LICENSE`), with one exception: `code/R/functions/vendor/arma_to_ma.{cpp,R}`
is vendored from the GPL-3-licensed `deseats` package and remains GPL-3 — see
`code/R/functions/vendor/README.md` for why it's vendored rather than taken as a
package dependency, and the licensing tradeoffs still open for a final decision.

## Provenance

This model was originally developed using Haver Analytics data (not
redistributable). This repo is the public-data port: every Haver series has a
documented public-source replacement — see `docs/data_sources.md` for the full
series-by-series mapping, including two real data-sourcing bugs found and fixed
during the port (mislabeled trade series, mislabeled housing-starts series) and
one contradiction this surfaced in the working paper's own text.
