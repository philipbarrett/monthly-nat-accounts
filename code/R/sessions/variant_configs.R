##########################################################################################
#
# variant_configs.R
#
# Config table for Stage 2's "regenerate every run-folder variant" task (PLAN.md).
# Each entry describes one deviation from the baseline l.options/l.quarterly.data/
# l.monthly.data/l.x/l.q (as defined by run_variant.R's controls + _data_spec.R).
# Sourced by run_variant.R, which looks up a variant by name (command-line arg) and
# applies it.
#
# Every override here is empirically grounded, not guessed: read directly out of
# archived outputs/<variant>/final_data/everything.rdata files where they existed
# (add_extra_gov/nrfi/rfi, baseline_no_gov, cons_g_dg_no_gov, and the actual
# published outputs/baseline_aug2025/ run itself -- which resolved the one real
# ambiguity, efficient.gmm for "baseline", empirically: FALSE, confirmed from that
# archived .rdata, not from comparison_charts.R's contradictory "Efficient GMM"
# label), or from _data_spec.R's own commented-out variant lines (cons_g_rrs_no_gov,
# which has no archived .rdata of its own -- constructed by direct analogy to the
# confirmed cons_g_dg_no_gov mechanism plus the exact label commented at
# _data_spec.R:117). See PLAN.md's Stage 2 "Regenerate baseline + all run variants"
# section for the full derivation and a table of which variant feeds which
# main.tex artifact.
#
# Deliberately excluded (see PLAN.md for why): the entire cons_s/do.goods=FALSE
# branch (dead code, no data, nothing in main.tex depends on it), add_nondef_cap_goods
# (not a standalone run folder -- it's just one component of add_extra_nrfi/
# everything), baseline_ineff (produces no persisted output -- comparison_charts.R's
# GMM-efficiency section has no ggsave() call), and cons_g_dg_rrs_no_gov/cons_g_dg/
# cons_g_dg_rrs (commented out in comparison_charts.R, low-confidence construction).
#
##########################################################################################

l.variant.configs <- list(

  baseline = list(
    save.dir = 'outputs/baseline/',
    options = list(),
    manual.labels = NULL,
    apply = function() invisible(NULL)
  ),

  baseline_err = list(
    # NOT one of the 13 robustness/validation variants comparison_tables.R/
    # comparison_charts.R/robustness.R read -- needed for the website release
    # xlsx's "with_err" file (make_release_xlsx.R), which is baseline (gov
    # present) + make.gdp.err=TRUE. Confirmed against archived outputs/
    # baseline_err/final_data/everything.rdata (components: ...,gov,err).
    save.dir = 'outputs/baseline_err/',
    options = list(make.gdp.err = TRUE),
    manual.labels = NULL,
    apply = function() invisible(NULL)
  ),

  baseline_no_gov = list(
    save.dir = 'outputs/baseline_no_gov/',
    options = list(),
    manual.labels = NULL,
    apply = function() { l.quarterly.data$components$gov <<- NULL }
  ),

  baseline_no_gov_err = list(
    save.dir = 'outputs/baseline_no_gov_err/',
    options = list(make.gdp.err = TRUE),
    manual.labels = NULL,
    apply = function() { l.quarterly.data$components$gov <<- NULL }
  ),

  baseline_no_gov_eff = list(
    save.dir = 'outputs/baseline_no_gov_eff/',
    options = list(efficient.gmm = TRUE),
    manual.labels = NULL,
    apply = function() { l.quarterly.data$components$gov <<- NULL }
  ),

  baseline_no_gov_trend_3 = list(
    save.dir = 'outputs/baseline_no_gov_trend_3/',
    options = list(trend.order = 3),
    manual.labels = NULL,
    apply = function() { l.quarterly.data$components$gov <<- NULL }
  ),

  baseline_no_gov_eq_12 = list(
    save.dir = 'outputs/baseline_no_gov_eq_12/',
    # max.ar.moments (the ceiling ar.gmm.mmsc.opt() is allowed to select up to)
    # must rise with n.order (the target it's asked to fit toward), or GMM
    # estimation for at least one series produces a non-finite state covariance
    # -- confirmed by checking the archived outputs/baseline_no_gov_eq_12/
    # final_data/everything.rdata, which used max.ar.moments=12, not the
    # shared default of 8.
    options = list(n.order = 12, max.ar.moments = 12),
    manual.labels = NULL,
    apply = function() { l.quarterly.data$components$gov <<- NULL }
  ),

  add_extra_gov = list(
    save.dir = 'outputs/add_extra_gov/',
    options = list(),
    manual.labels = c('pub_wages', 'pub_construction', 'fed_outlays'),
    apply = function() { l.q$gov <<- c('pub_wages', 'pub_construction', 'fed_outlays') }
  ),

  add_extra_nrfi = list(
    save.dir = 'outputs/add_extra_nrfi/',
    options = list(),
    manual.labels = c('nondef_cap_goods', 'ip', 'oil_price'),
    apply = function() { l.q$nrfi <<- c('nonres_construction', 'nondef_cap_goods', 'ip', 'oil_price') }
  ),

  add_extra_rfi = list(
    save.dir = 'outputs/add_extra_rfi/',
    options = list(),
    manual.labels = c('res_building_employees'),
    apply = function() { l.q$rfi <<- c('house_starts_1fam', 'house_starts', 'res_building_employees') }
  ),

  everything = list(
    save.dir = 'outputs/everything/',
    options = list(),
    manual.labels = c('pub_wages', 'pub_construction', 'fed_outlays',
                       'nondef_cap_goods', 'ip', 'oil_price', 'res_building_employees'),
    apply = function() {
      l.q$gov <<- c('pub_wages', 'pub_construction', 'fed_outlays')
      l.q$nrfi <<- c('nonres_construction', 'nondef_cap_goods', 'ip', 'oil_price')
      l.q$rfi <<- c('house_starts_1fam', 'house_starts', 'res_building_employees')
    }
  ),

  cons_g_dg_no_gov = list(
    save.dir = 'outputs/cons_g_dg_no_gov/',
    options = list(),
    manual.labels = NULL,
    apply = function() {
      l.quarterly.data$components$gov <<- NULL
      l.x$cons_g <<- NULL
      l.q$cons_g <<- 'pce_exp_dur'
    }
  ),

  cons_g_rrs_no_gov = list(
    save.dir = 'outputs/cons_g_rrs_no_gov/',
    options = list(),
    manual.labels = c('real.retail.sales'),
    apply = function() {
      l.quarterly.data$components$gov <<- NULL
      l.x$cons_g <<- NULL
      l.q$cons_g <<- 'real.retail.sales'
    }
  ),

  cons_g0_no_gov = list(
    save.dir = 'outputs/cons_g0_no_gov/',
    options = list(),
    manual.labels = NULL,
    apply = function() {
      l.quarterly.data$components$gov <<- NULL
      l.x$cons_g <<- NULL
    }
  )
)
