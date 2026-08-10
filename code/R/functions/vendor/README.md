# code/R/functions/vendor/

Third-party code vendored directly rather than taken as a package dependency,
because the source package's install-time requirements pull in far more than this
codebase needs for the one function actually used.

## `arma_to_ma.{cpp,R}`

Vendored from **`deseats`** (CRAN, v1.1.2), function `MAinftySHORT` in
`src/LinearFilters.cpp`. Copyright Yuanhua Feng and Dominik Schulz (Paderborn
University); **licensed GPL-3** (see `LICENSE-GPL3-vendor.txt`) — unlike the rest of
this repo, these two files are GPL-3, not the repo's overall license.

**Why vendored instead of `install.packages("deseats")`:** `deseats` hard-requires
`shiny` and `animation` (per its `DESCRIPTION` `Imports:`), which in turn need
`bslib`/`sass`/`fs`, which need the `libuv` system library — none of which this
codebase otherwise needs, just to compute one MA(∞)-representation utility used
~9 times in `mly_gdp_gmm.R`/`mly_gdp_asymptotics.R`'s GMM moment-condition
construction.

**Licensing note for the public repo:** GPL-3 vendored code inside an otherwise
MIT-licensed repo is a real consideration, not a minor detail — flagged explicitly
for Philip's decision before the public repo (Stage 2) is finalized. Options: keep
these two files GPL-3 with clear per-file licensing (common practice, e.g. via
`SPDX-License-Identifier` headers), depend on `deseats` properly instead (accepting
its heavier install), or reimplement independently under MIT terms.
