##########################################################################################
#
# splice_historical.R
#
# Prepends frozen historical Haver-sourced data (data/haver_historical_splice.csv)
# ahead of a live public series, for the 3 baseline series where no public source has
# full history (pce_exp_goods, pce_exp_servs, inv_manuf -- see docs/data_sources.md).
# The splice data is strictly before each series' live public source start date, so
# there is no overlap/ambiguity about which value wins at the boundary.
#
##########################################################################################

splice.historical <- function(df, label, splice.path = here::here("data/haver_historical_splice.csv")) {
  if (!file.exists(splice.path)) return(df)

  df.splice <- readr::read_csv(splice.path, show_col_types = FALSE) %>%
    dplyr::filter(label == !!label)
  if (nrow(df.splice) == 0) return(df)

  df.splice.wide <- df.splice %>%
    dplyr::transmute(date = as.character(date), !!label := value)

  dplyr::bind_rows(df.splice.wide, df) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::arrange(date)
}
