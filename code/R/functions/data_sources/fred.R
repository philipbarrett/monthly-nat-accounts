##########################################################################################
#
# fred.R
#
# Pull functions for FRED (Federal Reserve Economic Data). Requires FRED_API_KEY in
# .Renviron (free, instant signup: https://fred.stlouisfed.org/docs/api/api_key.html).
#
# Output contract matches the old pull.haver()/pull.haver.list(): a tibble with a
# `date` column (character, first-of-period convention -- e.g. first month of a
# quarter, matching what FRED and the old raw Haver pull both return) plus one column
# per series label. make.gdp.data()'s existing date.adjust()/%m+% months(2) logic
# handles conversion to the pipeline's internal convention downstream -- unchanged.
#
##########################################################################################

pull.fred <- function(series_id, label, ...) {
  df <- fredr::fredr(series_id = series_id, ...)
  df %>%
    dplyr::transmute(date = as.character(date), !!label := value)
}

pull.fred.list <- function(spec_list) {
  # spec_list: list of list(label=..., fred_id=...)
  dfs <- lapply(spec_list, function(s) pull.fred(s$fred_id, s$label))
  Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), dfs)
}
