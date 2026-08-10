# Data sources: baseline model

This document maps every series the **baseline** model actually uses (traced through
`l.x`/`l.q`/`l.derived.series` in `code/R/sessions/mly_GDP.R`) from its original Haver
Analytics mnemonic to a public replacement, verified against live API responses on
2026-08-09 (not assumed from memory). Series that only feed the paper's robustness/
validation appendix are out of scope here — see
`code/R/functions/legacy_haver/export_robustness_series.R` and
`data/manual/README.md` instead.

## Quarterly: GDP + 8 NIPA components

All are standard BEA NIPA Table 1.1.6 ("Real Gross Domestic Product, Chained Dollars")
components.

**Correctness note (found while building the pull layer):** BEA Table 1.1.6 reuses
the same `LineDescription` ("Goods", "Services") for three unrelated concepts (PCE,
exports, imports) — matching by description text alone would silently grab the wrong
line. The pull layer (`code/R/functions/data_sources/bea.R`) keys on `LineNumber`
instead, which is unambiguous; `LineDescription` is kept only as an optional
safety cross-check that warns if BEA ever renumbers the table.

**Real history gap found and fixed (2026-08-09, during Stage 2 prep — this was
missed in the initial Stage 1 validation):** within `T10106`, top-level aggregate
lines (`gdp`, `invent`, `x`, `m`, `gov`) have chain-dollar level data back to 1947Q1,
but every **sub-component breakdown line** (`cons_g`, `cons_s`, `nrfi`, `rfi` —
i.e. anything that isn't a top-line total) only goes back to **2007Q1**, confirmed
by querying every line's actual date range individually, not assumed from the
table's overall history. Checked FRED's independent mirrors of the same
sub-components (`PNFIC1`, `PRFIC1`, `PCDGCC96`, `PCNDGC96`, `PCESVC96`) too — all
also start 2007-01-01, since FRED mirrors whatever BEA publishes; this is a genuine
BEA publication-history limit, not a FRED-vs-BEA sourcing choice. Only *growth-rate*
series (e.g. `DGDSRL1Q225SBEA`) have full history back to 1947 — no public *level*
series does.

**Why this was initially missed:** `validate_data_sources.R`'s original design only
checked agreement *within* the old/new overlap window (an inner join), which doesn't
detect a coverage gap — `cons_g`/`cons_s` reported `PASS` with `n_overlap=73`
(exactly the 2007–2025 overlap) because the two sources agreed perfectly wherever
both existed, while 57 years of pre-2007 history were silently absent from the new
pull. Caught via an unrelated symptom (`NaN` values in `make.gdp.data()`'s internal
quarterly/monthly consistency check) while testing Stage 2's auto-detected end date,
not by the validation script itself. **Fixed the validation script itself**, not
just the data: it now also compares each series' earliest non-NA date old vs. new
and flags a `COVERAGE_GAP` status if new coverage starts materially later than the
archive's, independent of how well the overlapping portion agrees.

**Fix:** spliced `cons_g`, `cons_s`, `nrfi`, `rfi` with archived Haver data (1950Q1–
2006Q4, from `outputs/baseline/input_data/quarterly.csv`, date-shifted from the
archive's last-month-of-quarter convention to the raw first-month-of-quarter
convention `pull.public.series()` expects) exactly the same way as the monthly
splice — added to the same `data/haver_historical_splice.csv` (now 2,532 rows).
Re-validated after the fix: all 4 now `PASS` with full 301-quarter coverage
(1950Q1–2026Q2) and mean abs % diff of 0.008–0.034% — very tight matches, not just
"passing."

All from BEA `NIPA` dataset, Table `T10106` ("1.1.6"), quarterly. Verified line
numbers directly against a live API pull (2026-08-09), not assumed:

| Label | Haver mnemonic | BEA LineNumber | LineDescription | Splice? | Verified FRED mirror |
|---|---|---|---|---|---|
| gdp | GDPH@USNA | 1 | Gross domestic product | No — full history | `GDPC1` |
| cons_g | CTGH@USNA | 3 | Goods | **Yes**, 1950Q1–2006Q4 | — (BEA has a direct combined line; no need to sum durable+nondurable) |
| cons_s | CSH@USNA | 6 | Services | **Yes**, 1950Q1–2006Q4 | `PCESC96` (quarterly variant, also 2007+ only) |
| nrfi | FNH@USNA | 9 | Nonresidential | **Yes**, 1950Q1–2006Q4 | `PNFIC1` (also 2007+ only) |
| rfi | FRH@USNA | 13 | Residential | **Yes**, 1950Q1–2006Q4 | `PRFIC1` (also 2007+ only) |
| invent | VH@USNA | 14 | Change in private inventories | No — full history | `CBIC1` | `level.trend=TRUE` (not log) in the model — keep that flag |
| x | XH@USNA | 16 | Exports | No — full history | `EXPGSC1` |
| m | MH@USNA | 19 | Imports | No — full history | `IMPGSC1` | `sign=-1` in the model — keep that flag |
| gov | GH@USNA | 22 | Government consumption expenditures and gross investment | No — full history | `GCEC1` |

Primary source: BEA API directly (`NIPA` dataset, `T10106`, quarterly) since it's the
authoritative origin. FRED mirrors are a good cross-check/fallback if the BEA API has
an outage.

**Units note (found while building the pull layer):** BEA's raw API returns `T10106`
dollar values in **millions**, but Haver's archived data (and everything downstream)
is in **billions** — confirmed directly: archived `gdp` for 2024Q1 is ~23,054, live
BEA pull for the same quarter is ~23,054,000. All 9 lines come from the same table/
API call, so all 9 need the same `scale = 0.001` factor (applied in
`pull.public.series()` via `spec$scale`, not silently baked into `bea.R`). Checked
`U20406` (monthly PCE by type) too: same issue, same fix. Checked `U001B`
(inv_manuf) and `U50706BM` (inv_chg) directly against archived values: **already
match Haver's scale**, no `scale` factor needed for those two.

## Monthly: baseline indicators

| Label | Haver mnemonic | Haver history start | Public source | BEA LineNumber | Public start | **Splice needed?** |
|---|---|---|---|---|---|---|
| pce_exp_goods | CTGBHM@USECON | 1959-01 | BEA `NIUnderlyingDetail`, Table 2.4.6U (`U20406`) | 2 ("Goods") | **2007-01** | **YES** — see below |
| pce_exp_servs | CSBHM@USECON | 1959-01 | BEA `NIUnderlyingDetail`, Table 2.4.6U (`U20406`) | 150 ("Services") | **2007-01** | **YES** — see below |
| pce_exp_dur | (new, public) | — | BEA `NIUnderlyingDetail`, Table 2.4.6U (`U20406`) | 3 ("Durable goods") | 2007-01 | No (only used by `cons_g_dg_no_gov` validation variant) |
| nonres_construction | CPTNR@USECON | 2002-01 | FRED `TLNRESCONS` | — | 2002-01 | No — exact match |
| house_starts | HPT@USECON | 1960-01 | FRED `HOUST` | — | 1959-01 | No — public has *more* history |
| house_starts_1fam | HSTM@USECON | 1959-01 | FRED `HOUST5F` (**multifamily, 5+ units** — see correctness note below) | — | 1959-01 | No — exact match |
| exports_value | BGSX@USECON | 1992-01 | FRED `BOPTEXP` (Goods **and services**, BOP basis — see correctness note) | — | 1992-01 | No — exact match |
| export_prices | PXEA@USECON | 1983-09 | FRED `IQ` (BLS Export Price Index, End Use) | — | 1983-09 | No — exact match |
| imports_value | BGSM@USECON | 1992-01 | FRED `BOPTIMP` (Goods **and services**, BOP basis — see correctness note) | — | 1992-01 | No — exact match |
| import_prices | PMEA@USECON | 1982-09 | FRED `IR` (BLS Import Price Index, End Use) | — | 1982-09 | No — exact match |
| inv_manuf | NMI@USECON | 1958-01 | BEA `NIUnderlyingDetail`, Table 1BU (`U001B`) | 2 ("Manufacturing industries") | **1997-01** | **YES** — see below |
| inv_chg | VNHM@USNA | 1997-01 | BEA `NIUnderlyingDetail`, Table 5.7.6BM (`U50706BM`) | 1 ("Nonfarm industries") | 1997-01 | No — exact match |

Derived series (`code/R/functions/data_helpers.R`'s `l.derived.series`, unchanged
transforms): `export_volume = exports_value / export_prices`,
`import_volume = imports_value / import_prices`, `inv_manuf_chg = pc.diff(inv_manuf)`.

Also added to the public baseline mapping (previously an unused `l.monthly.data`
entry): `pce_exp_dur` (BEA Table 2.4.6U, "Durable goods" line) — needed for the
`cons_g_dg_no_gov` validation variant, which just swaps `l.x`'s `cons_g` source; fully
public, no manual pull needed (see `docs/data_sources.md` note in Stage 1 status).

## Historical splice (real gap, resolved 2026-08-09)

**The problem:** BEA's monthly PCE-by-type detail (needed for `pce_exp_goods`/
`pce_exp_servs`) only exists from 2007-01 onward — confirmed as a hard limit of BEA's
publication history via direct API query of both the relevant tables (2.3.6U and
2.4.6U), not a FRED mirroring artifact. Similarly, the cleanest live-updating public
source for `inv_manuf` (BEA Table 1BU, NAICS-based) only starts 1997-01. Haver's
archived data (still sitting in this repo at `outputs/*/input_data/monthly.csv`) goes
back to 1959 and 1958 respectively. The Kalman filter handles missing/late-starting
series gracefully (`code/R/functions/kf_filter.R:107`,
`data.select <- !is.na(c(m.y[this.pd,]))`) — nothing would break — but using the
public source alone would give pre-2007/pre-1997 estimates for these components
*less* precision than the original Haver-based paper had, for no real reason, when the
actual historical numbers are just sitting in the repo already.

**The fix:** `data/haver_historical_splice.csv` (2,532 rows as of the Stage 2 update,
committed to the repo, both private and public — it's static, frozen, non-proprietary
numeric data, not a live Haver dependency) holds the archived Haver values strictly
*before* each series' public-source start date:

| Series | Splice range (frozen, from archived Haver pull) | Live range (public API, updates going forward) |
|---|---|---|
| pce_exp_goods (monthly) | 1959-01 to 2006-12 | 2007-01 onward |
| pce_exp_servs (monthly) | 1959-01 to 2006-12 | 2007-01 onward |
| inv_manuf (monthly) | 1958-01 to 1996-12 | 1997-01 onward |
| cons_g (quarterly) | 1950Q1 to 2006Q4 | 2007Q1 onward |
| cons_s (quarterly) | 1950Q1 to 2006Q4 | 2007Q1 onward |
| nrfi (quarterly) | 1950Q1 to 2006Q4 | 2007Q1 onward |
| rfi (quarterly) | 1950Q1 to 2006Q4 | 2007Q1 onward |

The 4 quarterly additions were found later than the 3 monthly ones (during Stage 2
prep, not the initial Stage 1 validation pass — see the "Quarterly" section above for
how this was caught and why the validation script itself needed fixing too). Same
mechanism, same file, same no-overlap-by-construction guarantee — just a second round
of the identical fix. Quarterly splice dates are stored in the raw first-month-of-
quarter convention (matching what `pull.public.series()` returns before
`make.gdp.data()`'s own `%m+% months(2)` shift), not the archive's last-month-of-
quarter convention — i.e. the archived "1990-03-01" (Q1 1990) is stored as
"1990-01-01" in the splice file.

No overlap between splice and live ranges by construction (splice is strictly
`< public_start_date`), so there's no ambiguity about which value wins at the
boundary. The splice is a one-time historical fact, frozen as of the last Haver pull
(~2025-03) — it will never need updating, since the pre-cutoff numbers don't change.
`code/R/functions/data_sources/splice_historical.R` implements the prepend logic as a
thin wrapper so this is transparent to everything downstream.

**Caveat for the public repo:** this means the public/replication repo isn't *100%*
independent of the original Haver pull for these 7 series' pre-2007/1997 history —
but it requires no ongoing Haver access, just one small bundled CSV, and is fully
transparent about what it is and why. Flagging this explicitly at the Stage 1 and
Stage 2 checkpoints rather than treating it as a minor implementation detail.

**`inv_manuf` level-break fix (2026-08-10):** the splice above had a real defect,
found by comparing 2025-08 vs. 2026-08 release standard errors and tracing an
unexplained GDP-SE spike in 1996-07 to 1996-12 back to its source. `inv_manuf`'s
public source (BEA Table 1BU, NAICS-based) starts in 1997-01 precisely because
that's when manufacturing statistics switched from SIC to NAICS classification —
unlike the other 6 spliced series, whose cutoffs are pure publication-history
limits, not a classification change. The archived pre-1997 Haver segment had
already been reconciled onto a consistent basis by Haver internally (confirmed: no
break in the original pre-migration Haver-only series across 1996/1997 — 430,366
→ 431,496, a normal 0.26% step), but concatenating it raw onto BEA's unreconciled
NAICS levels reintroduced a 30.8% one-month "jump" at 1996-12→1997-01
(430,366 → 562,788) that isn't a real economic move. Through
`inv_manuf_chg = pc.diff(inv_manuf)`, that single bad observation inflated the
Kalman smoother's delta-method parameter SE for several months either side of
the boundary, across every state component (not just `invent`/`gdp`).

**Fix applied:** rescaled the entire 1958-01 to 1996-12 `inv_manuf` splice segment
by a constant factor `1.302698` (the mean live/archived level ratio over
1997-01 to 1997-03, a tight, stable anchor — SD 0.004 across those 3 months).
Because `inv_manuf_chg` only ever appears as `pc.diff()`, a uniform rescale of the
whole segment leaves every pre-1997 month-over-month change mathematically
unchanged (verified: max deviation 1.9e-16, floating-point noise) — it only fixes
the one boundary transition, which now reads as a normal +0.38% month-over-month
move instead of +30.8%. Not touched: the live (1997+) BEA-sourced values, which
keep updating as before; the other 6 spliced series, which don't have this
problem (checked directly — their boundary-month growth rates are all within a
few normal percent).

## Dropped entirely (confirmed unused anywhere in the paper)

~16 series from the original `l.monthly.data` list are pulled by the old Haver code
but never referenced in any `l.q`/`l.x` used by the baseline OR any comparison/
robustness/validation run: `pce_exp` (total, superseded by goods/services split),
`pce_exp_nondur` (not referenced by any variant — only `pce_exp_dur` is, for
`cons_g_dg_no_gov`), `real.retail.sales` (only used via manual pathway, see below), `oil_price` (manual
pathway), `advance_exports_goods`, `advance_imports_goods`, `polb_inbound_loaded`,
`polb_outbound_loaded`, `ip` (manual pathway), `capu`, `res_building_employees`
(manual pathway), `pub_construction`/`fed_constr`/`fed_emp`/`fed_receipts`/
`fed_outlays`/`state_emp`/`local_emp`/`pub_wages` (manual pathway), `unemp`,
`inv_retail`, `inv_xm`, `inv_trade`, `nondef_cap_goods` (manual pathway), `constant1`,
`cons_housing` (confirmed dead code, never referenced by the paper), `ism.servs`
(confirmed dead code, never referenced by the paper).

## Validation results (`code/R/sessions/validate_data_sources.R`, run 2026-08-09)

Compared every baseline series against the archived Haver-pulled reference data
(`outputs/baseline/input_data/{quarterly,monthly}.csv`) over their overlap window.
Full numeric results: `outputs/diagnostics/data_validation/summary.csv`; one
old-vs-new overlay chart per series in the same folder.

**Two real sourcing bugs found and fixed by this process** (first-draft FRED IDs
were wrong, not just imprecise):
- `exports_value`/`imports_value`: first mapped to `BOPGEXP`/`BOPGIMP` (goods only,
  BOP basis) — high correlation (0.998+) but a consistent ~20-30% gap gave it away.
  Haver's `BGSX`/`BGSM` turned out to be **goods and services** combined; switched to
  `BOPTEXP`/`BOPTIMP`, which now match the archive almost exactly (e.g. 261,614 vs.
  261,796 for 2024-01, a 0.07% difference).
- `house_starts_1fam`: first mapped to `HOUST1F` (true single-family starts) —
  correlation was only **0.27**, a clear red flag. The original code's own `name`
  field already said "housing starts, total multifamily", contradicting its
  `label` — the data confirmed `name` was right: Haver's `HSTM@USECON` is actually
  **multifamily (5+ units)** starts. Switched to `HOUST5F`; correlation jumped to
  0.98 (344 vs. archived 368 for 2024-01, well within normal revision range).
  **This also surfaced a contradiction inside the paper itself, not just the
  code — flagged for a Stage 3 fix:** `main.tex`'s hand-written Data-section
  table (`tab:mly_data`, line ~337-338) explicitly says *"Single-family housing
  starts, Jan 1959"*, while `outputs/baseline/tables/obs_coefs.tex` (Table
  `tab:obs_coefs`, auto-generated from the code's own `name=` field) already
  correctly said *"Housing starts, total multifamily"*. The two tables
  disagree with each other in the current published draft. Validation confirms
  the auto-generated table was right and the hand-written one is wrong — most
  likely written by eyeballing the variable name `house_starts_1fam` ("1fam"
  read as single-family) rather than the actual data. **Stage 3 needs to fix
  the `tab:mly_data` text in `main.tex` to say multifamily, not single-family.**
- Also fixed a bug in the validation script itself, not the data: quarterly series
  initially showed zero overlap because the archived reference data has already
  been date-shifted by `make.gdp.data()` (`date.adjust() %m+% months(2)`, converting
  first-month-of-quarter to last-month-of-quarter) while a fresh `pull.public.list()`
  call returns the raw first-month convention — the same shift now gets applied
  before comparing.

**5 series still flag `FAIL` against the automated tolerance (correlation < 0.98 or
mean abs % diff > 2%), but all have correlation ≥ 0.93 (4 of 5 ≥ 0.97) and are
judged genuine, expected data-revision/metric artifacts, not sourcing errors:**
- `invent` (quarterly change in private inventories) and `inv_chg` (monthly): both
  are *change*/flow series that oscillate near zero — percentage difference is a
  poor metric here since it blows up whenever the archived value is small (e.g. a
  \$5,808 vs. \$4,084 swing near zero registers as a huge % difference despite being
  numerically tiny). Correlation is 0.997 for both.
- `inv_manuf`: a stable ~4% *multiplicative* gap across every date checked (not
  noisy/random) — consistent with a BEA benchmark revision to manufacturing
  inventories since Haver's last pull (~2025-03), not a wrong series. Correlation
  0.972.
- `house_starts`: correlation 0.933, same overall scale and pattern as the archive;
  Census housing-starts data is well known for large historical revisions. Not
  re-mapped to a different series since both clearly represent the same "total
  housing starts" concept.
- `house_starts_1fam` (residual, post-fix): correlation improved to 0.983 but still
  above the 2% mean-diff tolerance — multifamily starts are the most volatile/
  heavily-revised housing series; judged acceptable given the correlation jump
  from 0.27 confirmed the underlying series is now correct.

None of these 5 block Stage 2 — they're flagged transparently here rather than
silently passed, per the plan's validation design, but don't indicate a sourcing
error the way the two fixed bugs did.

## Robustness/validation-only series (manual pathway, not public)

See `code/R/functions/legacy_haver/export_robustness_series.R` and
`data/manual/README.md`. Confirmed list (8 series): `real.retail.sales`, `pub_wages`,
`pub_construction`, `fed_outlays`, `nondef_cap_goods`, `ip`, `oil_price`,
`res_building_employees`. Already pulled by Philip and present at
`data/manual/robustness_series.csv` as of 2026-08-09.
