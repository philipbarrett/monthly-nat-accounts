##########################################################################################
#
# _setup.R
#
# Portable environment setup, sourced first by every session script. Replaces the old
# hardcoded setwd("C:/Users/.../monthly_GDP/") calls: works on any machine/OS as long
# as R is started somewhere inside the repo tree.
#
##########################################################################################

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install with install.packages('here').")
}
setwd(here::here())
if (file.exists(".Renviron")) readRenviron(".Renviron")
