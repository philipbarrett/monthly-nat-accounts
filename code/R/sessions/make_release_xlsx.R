##########################################################################################
#
# make_release_xlsx.R
#
# Stage 2 task (PLAN.md): "Website deliverables". Reformats the freshly regenerated
# outputs/baseline/final_data/output.xlsx (make.gdp.err=FALSE) and
# outputs/baseline_err/final_data/output.xlsx (make.gdp.err=TRUE, see
# variant_configs.R) into the exact two-file format Philip's website has always
# published (monthly_nipa_no_err.xlsx / monthly_nipa_with_err.xlsx), matched
# sheet-for-sheet and README-word-for-word against
# monthly releases/2025_08/monthly_nipa_{no_err,with_err}.xlsx -- this is a
# mechanical reformatting task, not a rewrite, so the README text (including its
# one pre-existing typo, "inclue") is preserved exactly rather than corrected.
#
# Output goes to a new dated monthly releases/<YYYY_MM>/ folder (this month's
# date), never overwriting the archived 2025_08 release used as the format
# reference and as the prior-release validation baseline.
#
##########################################################################################

source(here::here("code/R/sessions/_setup.R"))
library(openxlsx)

sheets <- c("level", "trend", "hat", "full_se", "smoother_se", "ks_param_se")

sheet.info <- c(
  level       = "The level estimates.  Units are 1/3 chained 2017 dollars SAAR.  Accordingly, series will add up to their NIPA counterparts within the quarter.",
  trend       = 'The estimated trend.  Units as for "level"',
  hat         = "Percent departures of level from trend",
  full_se     = "Standard error of the estimate. Includes both smoother and parameter uncertianty. Is the square root of the the sum of squares of the data in smoother_se and ks_param_se. Units as for \"level\".",
  smoother_se = 'Square root of Kalman smoother forecast error variance.  Units as for "level".',
  ks_param_se = 'Square root of estimation error variance.  Units as for "level".'
)
make.release.file <- function(source.xlsx, out.path, subtitle, level.note.suffix) {
  wb <- createWorkbook()

  addWorksheet(wb, "README")
  writeData(wb, "README", data.frame(x = 'This file contains the output from "Estimated National Accounts for the United States"'),
            colNames = FALSE, startRow = 1)
  writeData(wb, "README", data.frame(x = subtitle), colNames = FALSE, startRow = 2)
  writeData(wb, "README", data.frame(x = "Sheet"), colNames = FALSE, startCol = 2, startRow = 3)
  writeData(wb, "README", data.frame(x = "Information"), colNames = FALSE, startCol = 3, startRow = 3)
  for (i in seq_along(sheets)) {
    sn <- sheets[i]
    info <- if (sn == "level") paste0(sheet.info[["level"]], level.note.suffix) else sheet.info[[sn]]
    writeData(wb, "README", data.frame(x = sn), colNames = FALSE, startCol = 2, startRow = 3 + i)
    writeData(wb, "README", data.frame(x = info), colNames = FALSE, startCol = 3, startRow = 3 + i)
  }

  for (sn in sheets) {
    d <- read.xlsx(source.xlsx, sheet = sn)
    addWorksheet(wb, sn)
    writeData(wb, sn, d)
  }

  saveWorkbook(wb, out.path, overwrite = TRUE)
  cat("Wrote", out.path, "\n")
}

release.month <- format(Sys.Date(), "%Y_%m")
out.dir <- paste0("monthly releases/", release.month, "/")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

make.release.file(
  "outputs/baseline/final_data/output.xlsx",
  paste0(out.dir, "monthly_nipa_no_err.xlsx"),
  subtitle = "These are the baseline estimates, so abstract from aggregation error.",
  level.note.suffix = "  The exception is gdp, which deviates due to the lack of the aggregation error"
)
make.release.file(
  "outputs/baseline_err/final_data/output.xlsx",
  paste0(out.dir, "monthly_nipa_with_err.xlsx"),
  subtitle = "These are the alternate estimates, which inclue aggregation error.",
  level.note.suffix = ""
)

cat("\nRelease files written to", out.dir, "\n")
