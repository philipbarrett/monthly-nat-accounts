##########################################################################################
#
# bea.R
#
# Pull functions for the BEA REST API (https://apps.bea.gov/api/). Requires
# BEA_API_KEY in .Renviron (free, instant signup: https://apps.bea.gov/API/signup/).
#
# Uses a thin httr2/jsonlite wrapper rather than the bea.R CRAN package -- bea.R's
# hard dependencies (Imports, not Suggests) pull in a full Shiny dashboard stack
# (shiny, shinydashboard, DT, googleVis, htmlwidgets) just to wrap a REST API, and
# failed to install cleanly on this machine. See PLAN.md Stage 1.3.
#
# Output contract matches pull.fred(): a tibble with `date` (character, first-of-
# period convention) plus one column per series label.
#
##########################################################################################

bea.api.request <- function(params) {
  key <- Sys.getenv("BEA_API_KEY")
  if (key == "") stop("BEA_API_KEY not set. Copy .Renviron.example to .Renviron and fill it in.")
  req <- httr2::request("https://apps.bea.gov/api/data") |>
    httr2::req_url_query(UserID = key, ResultFormat = "JSON", !!!params) |>
    httr2::req_timeout(60)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  if (!is.null(body$BEAAPI$Results$Error)) {
    stop("BEA API error: ", body$BEAAPI$Results$Error$APIErrorDescription)
  }
  body$BEAAPI$Results
}

bea.parse.timeperiod <- function(tp, freq) {
  yr <- substr(tp, 1, 4)
  if (freq == "M") {
    mo <- substr(tp, 6, 7)
    as.Date(paste0(yr, "-", mo, "-01"))
  } else if (freq == "Q") {
    q <- as.integer(substr(tp, 6, 6))
    first_month <- 3 * (q - 1) + 1  # first-of-period convention, matches FRED/raw Haver
    as.Date(paste0(yr, "-", sprintf("%02d", first_month), "-01"))
  } else {
    stop("Unsupported BEA frequency: ", freq)
  }
}

#' Pull one line item from one BEA table.
#'
#' Keys on LineNumber, not LineDescription: several BEA tables (e.g. NIPA Table
#' 1.1.6) reuse the same LineDescription ("Goods", "Services") for unrelated
#' concepts (PCE goods vs. export goods vs. import goods) -- matching on text alone
#' would silently pull the wrong line. `expect_description` is an optional safety
#' cross-check: if given, warns (doesn't error) when the line's actual description
#' has drifted from what was verified when the mapping was built, since BEA
#' occasionally renumbers tables across revisions.
#'
#' @param table BEA TableName code (e.g. "U20406" for Table 2.4.6U, "T10106" for NIPA
#'   Table 1.1.6).
#' @param line BEA LineNumber (integer or character).
#' @param label Output column name.
#' @param freq "M" or "Q".
#' @param dataset BEA dataset name ("NIUnderlyingDetail" for monthly detail tables,
#'   "NIPA" for standard NIPA tables).
#' @param expect_description Optional: the LineDescription verified for this line
#'   number when the mapping was built (see docs/data_sources.md). Warns if it no
#'   longer matches.
pull.bea <- function(table, line, label, freq = c("M", "Q"),
                      dataset = "NIUnderlyingDetail", year = "ALL",
                      expect_description = NULL) {
  freq <- match.arg(freq)
  results <- bea.api.request(list(
    method = "GETDATA", datasetname = dataset,
    TableName = table, Frequency = freq, Year = year
  ))
  d <- results$Data
  if (is.null(d)) stop("BEA: no data returned for table ", table)

  df <- purrr::map_dfr(d, function(x) {
    tibble::tibble(LineNumber = x$LineNumber, LineDescription = x$LineDescription,
                    TimePeriod = x$TimePeriod, DataValue = x$DataValue)
  })

  df.line <- df %>% dplyr::filter(LineNumber == as.character(line))
  if (nrow(df.line) == 0) {
    stop(
      "BEA: line number '", line, "' not found in table ", table, ". Available lines: ",
      paste(unique(paste0(df$LineNumber, "=", df$LineDescription)), collapse = "; ")
    )
  }
  if (!is.null(expect_description)) {
    actual <- unique(df.line$LineDescription)
    if (!identical(actual, expect_description)) {
      warning(
        "BEA table ", table, " line ", line, " description is now '", actual,
        "', expected '", expect_description, "' -- table may have been revised, ",
        "re-verify the mapping in docs/data_sources.md."
      )
    }
  }

  df.line %>%
    dplyr::transmute(
      date = bea.parse.timeperiod(TimePeriod, freq),
      !!label := as.numeric(gsub(",", "", DataValue))
    ) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date = as.character(date))
}

pull.bea.list <- function(spec_list) {
  # spec_list: list of list(label=..., bea_table=..., bea_line=..., bea_freq=...,
  #            bea_dataset=..., bea_description=...)
  dfs <- lapply(spec_list, function(s) {
    pull.bea(
      table = s$bea_table, line = s$bea_line, label = s$label,
      freq = s$bea_freq %||% "M", dataset = s$bea_dataset %||% "NIUnderlyingDetail",
      expect_description = s$bea_description
    )
  })
  Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), dfs)
}
