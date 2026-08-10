##########################################################################################
#
# validate_against_prior_release.R
#
# Stage 2 task (PLAN.md): compares the newly regenerated "baseline" run (public
# data, commit 15e327c) against the prior Haver-sourced release
# (`monthly releases/2025_08/monthly_nipa_no_err.xlsx` -- the "no_err"/abstract-
# from-aggregation-error file, matching baseline's make.gdp.err=FALSE default,
# NOT the "with_err" file) over their date overlap. Flags revisions beyond 2
# standard errors (using the OLD release's own full_se as the yardstick, since
# that's the uncertainty a reader of the published paper would have been quoting)
# and produces one summary table + one chart per component.
#
##########################################################################################

source(here::here("code/R/sessions/_setup.R"))
library(dplyr); library(tidyr); library(readr); library(purrr); library(tibble)
library(stringr); library(forcats); library(lubridate); library(ggplot2)
library(openxlsx)

dir.create("outputs/diagnostics/validate_against_prior_release", recursive = TRUE, showWarnings = FALSE)

read.release.sheet <- function(path, sheet) {
  read.xlsx(path, sheet = sheet) %>%
    mutate(date = as.Date(date, origin = "1899-12-30")) %>%
    pivot_longer(-date, names_to = "variable", values_to = "value")
}

old.level <- read.release.sheet("monthly releases/2025_08/monthly_nipa_no_err.xlsx", "level") %>% rename(old = value)
old.se    <- read.release.sheet("monthly releases/2025_08/monthly_nipa_no_err.xlsx", "full_se") %>% rename(old.se = value)
new.level <- read.release.sheet("outputs/baseline/final_data/output.xlsx", "level") %>% rename(new = value)
new.se    <- read.release.sheet("outputs/baseline/final_data/output.xlsx", "full_se") %>% rename(new.se = value)

df.compare <- old.level %>%
  inner_join(new.level, by = c("date", "variable")) %>%
  left_join(old.se, by = c("date", "variable")) %>%
  left_join(new.se, by = c("date", "variable")) %>%
  mutate(
    diff = new - old,
    diff.pc = 100 * (new / old - 1),
    diff.in.old.se = diff / old.se,
    flag.2se = abs(diff.in.old.se) > 2
  )

cat("Overlap window:", as.character(min(df.compare$date)), "to", as.character(max(df.compare$date)), "\n")
cat("Total obs compared:", nrow(df.compare), "\n")
cat("Flagged (>2 old-SE revision):", sum(df.compare$flag.2se, na.rm = TRUE),
    sprintf(" (%.2f%%)\n", 100 * mean(df.compare$flag.2se, na.rm = TRUE)))

df.summary <- df.compare %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean.diff.pc = mean(diff.pc, na.rm = TRUE),
    mean.abs.diff.pc = mean(abs(diff.pc), na.rm = TRUE),
    max.abs.diff.pc = max(abs(diff.pc), na.rm = TRUE),
    max.abs.diff.pc.date = date[which.max(abs(diff.pc))],
    n.flagged.2se = sum(flag.2se, na.rm = TRUE),
    pc.flagged.2se = 100 * mean(flag.2se, na.rm = TRUE)
  ) %>%
  arrange(desc(mean.abs.diff.pc))

cat("\n=== Per-component summary ===\n")
print(df.summary, n = Inf)
write_csv(df.summary, "outputs/diagnostics/validate_against_prior_release/summary.csv")

# Recent-window summary too -- old data ends 2025-06; the most policy-relevant
# question is whether recent quarters (closest to the nowcast frontier, where
# public-data plumbing bugs are most likely to bite) look reasonable.
df.summary.recent <- df.compare %>%
  filter(date >= max(date) %m-% months(24)) %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean.abs.diff.pc = mean(abs(diff.pc), na.rm = TRUE),
    max.abs.diff.pc = max(abs(diff.pc), na.rm = TRUE),
    n.flagged.2se = sum(flag.2se, na.rm = TRUE)
  ) %>%
  arrange(desc(mean.abs.diff.pc))
cat("\n=== Per-component summary, last 24 months of overlap ===\n")
print(df.summary.recent, n = Inf)
write_csv(df.summary.recent, "outputs/diagnostics/validate_against_prior_release/summary_recent24m.csv")

# Chart: one facet per component, old vs new level overlay
p1 <- ggplot(df.compare %>% filter(variable != "gov"), aes(x = date)) +
  geom_line(aes(y = old, color = "Prior release (Haver, 2025-08)"), linewidth = 0.4) +
  geom_line(aes(y = new, color = "New (public data, 2026-08)"), linewidth = 0.4, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_manual(values = c("Prior release (Haver, 2025-08)" = "grey40",
                                 "New (public data, 2026-08)" = "#2a78d6")) +
  theme_minimal() +
  labs(title = "Baseline revision: prior Haver release vs. new public-data rebuild",
       x = NULL, y = "Level", color = NULL) +
  theme(legend.position = "bottom")
ggsave("outputs/diagnostics/validate_against_prior_release/level_overlay.pdf", p1, width = 11, height = 8)

p2 <- ggplot(df.compare %>% filter(variable != "gov"), aes(x = date, y = diff.pc)) +
  geom_line(linewidth = 0.3, color = "#2a78d6") +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Percent revision, new vs. prior release", x = NULL, y = "% difference")
ggsave("outputs/diagnostics/validate_against_prior_release/pc_diff.pdf", p2, width = 11, height = 8)

cat("\nSaved: outputs/diagnostics/validate_against_prior_release/{summary.csv, summary_recent24m.csv, level_overlay.pdf, pc_diff.pdf}\n")

cat("\n=== Caveats (read before treating >2SE flags as problems) ===\n")
cat("- 'invent' (inventories) shows large %-diffs (up to ~3000%) by construction:\n")
cat("  it's a near-zero-value flow series, so tiny absolute differences produce\n")
cat("  huge percent differences. Already flagged as a known metric artifact in\n")
cat("  Stage 1's validate_data_sources.R (correlation >=0.93 there); not new.\n")
cat("- cons_g/cons_s are 'exact' monthly data (l.x, not l.q) with near-zero\n")
cat("  observation-error SE (~0.02bn) -- ANY normal BEA data revision between\n")
cat("  the 2025-08 and 2026-08 pull vintages will trivially exceed '2 old-SE',\n")
cat("  so a high pc.flagged.2se for these two specifically reflects the metric's\n")
cat("  sensitivity, not a pipeline bug. GDP's own SE (~10-20bn) is a much more\n")
cat("  meaningful yardstick -- check that column instead.\n")

