##########################################################################################
#
# pull_public.R
#
# Dispatcher: drop-in replacement for pull.haver()/pull.haver.list() in
# code/R/functions/data_helpers.R. Given a spec (list with `label`, `name`, and either
# `fred_id` or `bea_table`+`bea_line`), pulls from the right public source, applies
# the historical splice where flagged, and returns the same wide-tibble shape
# (date + one column per label) that the rest of the pipeline already expects --
# make.gdp.data() downstream needs zero changes.
#
# Caching (added 2026-08-09, per Philip's request -- "no need to keep pulling the
# data every single time... store it locally and only update if there hasn't been
# new data pulled in, say, the last week or so, or if I tell you explicitly"):
# the RAW per-series pull (before scale/splice, so changes to that logic don't
# require re-pulling) is cached at data/raw_pulls/<label>.csv, keyed on file mtime.
# A cache older than `cache.max.age.days` (default 7) is treated as stale and
# re-pulled; `force.refresh=TRUE` (in options, or per-series in spec) always
# re-pulls regardless of age. Small, git-tracked (per the original plan) -- also
# doubles as an offline fallback if an API is briefly down.
#
# `spec$manual=TRUE` (added for Stage 2 run-variant regeneration) routes to
# pull.manual.csv() (code/R/functions/legacy_haver/pull_manual_csv.R) instead --
# the Haver-sourced CSV for the ~8 series that only feed robustness/validation
# variants (add_extra_gov/nrfi/rfi, everything, cons_g_rrs_no_gov). Not cached
# (already a local file, and not part of the public/automated pipeline at all).
#
##########################################################################################

pull.public.series <- function(spec, options = list()) {
  if (isTRUE(spec$manual)) {
    message("  [manual] ", spec$label)
    df <- pull.manual.csv(labels = spec$label) %>%
      dplyr::mutate(date = as.character(date))
    if (!is.null(spec$scale)) df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(spec$label), ~ .x * spec$scale))
    return(df)
  }

  cache.dir <- options$cache.dir %||% "data/raw_pulls/"
  cache.max.age.days <- options$cache.max.age.days %||% 7
  force.refresh <- isTRUE(options$force.refresh) || isTRUE(spec$force.refresh)
  cache.file <- paste0(cache.dir, spec$label, ".csv")

  cache.fresh <- !force.refresh && file.exists(cache.file) &&
    difftime(Sys.time(), file.info(cache.file)$mtime, units = "days") < cache.max.age.days

  if (cache.fresh) {
    message("  [cache] ", spec$label, " (", round(difftime(Sys.time(), file.info(cache.file)$mtime, units = "days"), 1), "d old)")
    df <- readr::read_csv(cache.file, show_col_types = FALSE, col_types = readr::cols(date = "c")) %>%
      tibble::as_tibble()
  } else {
    message("  [pull]  ", spec$label, if (force.refresh) " (forced)" else if (!file.exists(cache.file)) " (no cache)" else " (stale)")
    df <- if (!is.null(spec$fred_id)) {
      pull.fred(spec$fred_id, spec$label)
    } else if (!is.null(spec$bea_table)) {
      pull.bea(
        table = spec$bea_table, line = spec$bea_line, label = spec$label,
        freq = spec$bea_freq %||% "M", dataset = spec$bea_dataset %||% "NIUnderlyingDetail",
        expect_description = spec$bea_description
      )
    } else {
      stop("Spec for '", spec$label, "' has neither fred_id nor bea_table nor manual=TRUE -- can't pull.")
    }
    dir.create(cache.dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(df, cache.file)
  }

  # Some BEA tables report headline dollar series in millions where Haver's archived
  # convention (and everything downstream) expects billions -- verified empirically
  # per-table against archived Haver data, not assumed. See docs/data_sources.md.
  if (!is.null(spec$scale)) {
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(spec$label), ~ .x * spec$scale))
  }

  if (isTRUE(spec$splice)) df <- splice.historical(df, spec$label)
  df
}

pull.public.list <- function(spec_list, options = list()) {
  dfs <- lapply(spec_list, pull.public.series, options = options)
  Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), dfs)
}
