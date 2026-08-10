##########################################################################################
#
# validate_data_sources.R
#
# Stage 1 checkpoint deliverable: compares freshly-pulled public-source data against
# the archived Haver-pulled reference data (outputs/baseline/input_data/*.csv) over
# their overlapping historical window, for every baseline series. Writes a per-series
# summary table (correlation, max abs diff, mean abs % diff, pass/fail vs. tolerance)
# and one old-vs-new overlay chart per series, so Philip can visually and numerically
# confirm the public-source pipeline reproduces the original Haver data before Stage 2
# re-runs the full estimation on it.
#
##########################################################################################

source(here::here("code/R/sessions/_setup.R"))
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(ggplot2); library(fredr); library(httr2); library(lubridate)

fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))
source('code/R/functions/data_sources/fred.R')
source('code/R/functions/data_sources/bea.R')
source('code/R/functions/data_sources/splice_historical.R')
source('code/R/functions/data_sources/pull_public.R')

l.options <- list(make.gdp.err = FALSE)  # only field _data_spec.R needs
source('code/R/sessions/_data_spec.R')

out.dir <- 'outputs/diagnostics/data_validation/'
if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)

#### 1. Pull fresh public data ####
message('Pulling quarterly series from public sources...')
df.q.new <- pull.public.list(c(list(target = l.quarterly.data$target), l.quarterly.data$components))
# The archived reference data is make.gdp.data()'s OUTPUT, which already shifts
# quarterly dates from first-month-of-quarter (the raw pull convention, matching
# what pull.public.list() returns) to last-month-of-quarter (date.adjust() %m+%
# months(2), see data_helpers.R). Apply the same shift here so the comparison
# lines up on the same date convention as the archive, not first vs last month.
df.q.new <- df.q.new %>% mutate(date = as.character(as.Date(date) %m+% months(2)))
message('Pulling monthly series from public sources...')
df.m.new <- pull.public.list(l.monthly.data)

#### 2. Load archived reference data ####
df.q.old <- read_csv('outputs/baseline/input_data/quarterly.csv', show_col_types = FALSE) %>%
  mutate(date = as.character(date))
df.m.old <- read_csv('outputs/baseline/input_data/monthly.csv', show_col_types = FALSE) %>%
  mutate(date = as.character(date))

#### 3. Compare series-by-series over the overlap window ####
# IMPORTANT: an inner join only checks agreement WHERE both old and new have data --
# it says nothing about whether new's coverage actually matches old's. A series
# whose public source starts decades later than the archive (e.g. a BEA sub-
# component only published from 2007, vs. Haver's 1950) would join fine on the
# 2007+ overlap and report PASS, while silently missing 50+ years of history. Found
# exactly this bug (cons_g/cons_s/nrfi/rfi) after this script had already reported
# PASS for them -- the coverage check below exists specifically to catch it next
# time without relying on noticing an unrelated downstream symptom.
compare.series <- function(label, df.old, df.new) {
  if (!label %in% names(df.old) || !label %in% names(df.new)) {
    return(tibble(label = label, status = 'MISSING', n_overlap = 0L,
                   correlation = NA_real_, max_abs_pct_diff = NA_real_,
                   mean_abs_pct_diff = NA_real_, old_min_date = NA_character_,
                   new_min_date = NA_character_, coverage_gap_days = NA_integer_))
  }

  old.min <- df.old %>% filter(!is.na(.data[[label]])) %>% pull(date) %>% min()
  new.min <- df.new %>% filter(!is.na(.data[[label]])) %>% pull(date) %>% min()
  coverage_gap <- as.integer(as.Date(new.min) - as.Date(old.min))

  df.join <- inner_join(
    df.old %>% select(date, old = all_of(label)),
    df.new %>% select(date, new = all_of(label)),
    by = 'date'
  ) %>% filter(!is.na(old), !is.na(new))

  if (nrow(df.join) < 3) {
    return(tibble(label = label, status = 'NO_OVERLAP', n_overlap = nrow(df.join),
                   correlation = NA_real_, max_abs_pct_diff = NA_real_,
                   mean_abs_pct_diff = NA_real_, old_min_date = old.min,
                   new_min_date = new.min, coverage_gap_days = coverage_gap))
  }

  df.join <- df.join %>% mutate(pct_diff = 100 * (new - old) / abs(old))
  corr <- suppressWarnings(cor(df.join$old, df.join$new))
  max_pct <- max(abs(df.join$pct_diff), na.rm = TRUE)
  mean_pct <- mean(abs(df.join$pct_diff), na.rm = TRUE)

  # Tolerance: flag if correlation < 0.98, mean abs % diff > 2% (revisions between
  # vintages are expected and normal; this catches genuine sourcing errors, not
  # noise), OR new's coverage starts more than ~35 days after old's (one missed
  # period) without a splice explaining the gap.
  status <- if (is.na(corr) || corr < 0.98 || mean_pct > 2) 'FAIL'
            else if (coverage_gap > 35) 'COVERAGE_GAP'
            else 'PASS'

  tibble(label = label, status = status, n_overlap = nrow(df.join),
         correlation = corr, max_abs_pct_diff = max_pct, mean_abs_pct_diff = mean_pct,
         old_min_date = old.min, new_min_date = new.min, coverage_gap_days = coverage_gap)
}

all.quarterly.labels <- c(l.quarterly.data$target$label, sapply(l.quarterly.data$components, function(x) x$label))
all.monthly.labels <- sapply(l.monthly.data, function(x) x$label)

df.summary.q <- map_dfr(all.quarterly.labels, compare.series, df.old = df.q.old, df.new = df.q.new) %>%
  mutate(freq = 'quarterly')
df.summary.m <- map_dfr(all.monthly.labels, compare.series, df.old = df.m.old, df.new = df.m.new) %>%
  mutate(freq = 'monthly')
df.summary <- bind_rows(df.summary.q, df.summary.m) %>% select(freq, everything())

write_csv(df.summary, paste0(out.dir, 'summary.csv'))
message('\n=== Data validation summary ===')
print(df.summary, n = Inf)

n.fail <- sum(df.summary$status %in% c('FAIL', 'MISSING', 'NO_OVERLAP'))
if (n.fail > 0) {
  message('\n*** ', n.fail, ' series need attention -- see summary.csv ***')
} else {
  message('\nAll series passed validation.')
}

#### 4. Overlay charts, old vs new ####
make.overlay.chart <- function(label, df.old, df.new, freq) {
  if (!label %in% names(df.old) || !label %in% names(df.new)) return(invisible(NULL))
  df.plot <- bind_rows(
    df.old %>% select(date, value = all_of(label)) %>% mutate(source = 'Archived (Haver)'),
    df.new %>% select(date, value = all_of(label)) %>% mutate(source = 'New (public)')
  ) %>% mutate(date = as.Date(date)) %>% filter(!is.na(value))

  gg <- ggplot(df.plot, aes(x = date, y = value, color = source, linetype = source)) +
    geom_line(linewidth = 0.7) +
    theme_minimal() +
    labs(title = label, x = NULL, y = NULL, color = NULL, linetype = NULL) +
    theme(legend.position = 'bottom')
  ggsave(paste0(out.dir, label, '.pdf'), gg, width = 8, height = 4)
}

walk(all.quarterly.labels, make.overlay.chart, df.old = df.q.old, df.new = df.q.new, freq = 'quarterly')
walk(all.monthly.labels, make.overlay.chart, df.old = df.m.old, df.new = df.m.new, freq = 'monthly')

message('\nWrote summary + overlay charts to ', out.dir)
