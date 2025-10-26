# ==============================================================================
# RQ2 — Generation × Task on energy (repetition-level, mixed model)
#
# DESIGN
#   • Unit of analysis: repetition (NOT aggregated by run).
#   • DV: log1p(energy_j)  (we model log(1+J) to stabilize residuals).
#   • Fixed effects: generation * task  (tests main effects + interaction).
#   • Random effects: (1 | run)  (random intercept for each run to avoid pseudoreplication).
#
# WHAT THIS SCRIPT DOES (no change to the tests themselves)
#   1) Fit the linear mixed-effects model: lmer(log1p(energy_j) ~ generation * task + (1 | run))
#   2) Type III ANOVA for fixed effects (from lmerTest).
#   3) Post-hoc, per task: consecutive generation contrasts (2→3, 3→3.1, 3.1→3.2)
#      with BH correction. Also saves raw p and BH p to CSV for the checklist.
#   4) If interaction is not significant, also report main-effect generation
#      contrasts collapsed across tasks.
#   5) Back-transform EMMs to Joules for human-readable tables (per task and main effect).
#   6) Diagnostics and variance components (sigma_run, sigma_resid, ICC).
#   7) Plots saved:
#        • plots/RQ2_energy_by_generation.png (main-effect EMMs, Joules)
#        • plots/RQ2_energy_by_task_faceted.png (EMMs per task, Joules)
#        • plots/RQ2_diag_qq.png and plots/RQ2_diag_resid_fitted.png (residual checks)
#
# OUTPUT FILES (all under tables/ unless noted)
#   • RQ2_mixed_TypeIII.csv                 — Type III ANOVA table (F, df, p)
#   • RQ2_emm_by_task_joules.csv            — EMMs by generation | task (response scale)
#   • RQ2_emm_generation_joules.csv         — Main-effect EMMs by generation (response scale)
#   • RQ2_consec_within_task_LINK.csv       — Consecutive contrasts within task (model/link scale)
#   • RQ2_consec_within_task_JOULES.csv     — Consecutive contrasts within task (response/J) with BH
#   • RQ2_consec_generation_main_JOULES.csv — Main-effect consecutive contrasts (response/J) with BH,
#                                             only written if interaction p > 0.05
#   • RQ2_summary_table.csv                 — Generation EMMs as “mean ± SE” with 95% CI (J)
#   • RQ2_variance_components.csv           — sigma_run, sigma_resid, ICC (clustering diagnostic)
#   • plots/*.png                           — see “Plots saved” above
#
# DECISIONS (unchanged)
#   • Two-sided tests, α = 0.05.
#   • Post-hoc p-values BH-adjusted. CSVs include both raw and BH p for auditability.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(ggplot2)
  library(scales)
})

# --- folders ------------------------------------------------------------------
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

# --- data slice (repetition-level) --------------------------------------------
stopifnot(exists("df"))
stopifnot(all(c("run","generation","task","energy_j") %in% names(df)))

rep_df <- df %>%
  select(run, generation, task, energy_j, duration_s, tokens) %>%
  drop_na() %>%
  mutate(
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    task       = factor(task),
    run        = factor(run)
  )

# quick check of replication
cat("\n--- RQ2: counts per generation × task (repetition-level) ---\n")
rep_df %>% count(generation, task) %>% arrange(task, generation) %>% print(n = Inf)

# --- model fit (unchanged) ----------------------------------------------------
fit_RQ2 <- lmer(log1p(energy_j) ~ generation * task + (1 | run), data = rep_df)

# --- diagnostics & variance components ----------------------------------------
v  <- as.data.frame(VarCorr(fit_RQ2))
sigma_run <- v$vcov[v$grp == "run"]
sigma_res <- v$vcov[v$grp == "Residual"]
ICC <- sigma_run / (sigma_run + sigma_res)

diag_tbl <- tibble(
  sigma_run = sigma_run,
  sigma_residual = sigma_res,
  ICC = ICC,
  model_singular = isSingular(fit_RQ2),
  convergence = is.null(summary(fit_RQ2)$optinfo$conv$lme4$messages)
)
readr::write_csv(diag_tbl, "tables/RQ2_variance_components.csv")
cat(sprintf("\nRandom-intercept variance (run)=%.4f; residual=%.4f; ICC=%.3f; singular=%s; converged=%s\n",
            sigma_run, sigma_res, ICC,
            diag_tbl$model_singular, diag_tbl$convergence))

# Save residual Q-Q and resid-vs-fitted plots
res_df <- tibble(fitted = fitted(fit_RQ2), resid = resid(fit_RQ2))
p_qq <- ggplot(res_df, aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  labs(title = "RQ2 residuals Q–Q (lmer on log1p energy)",
       x = "Theoretical quantiles", y = "Residuals") +
  theme_minimal()
ggsave("plots/RQ2_diag_qq.png", p_qq, width = 5.5, height = 4.5, dpi = 150, bg = "white")

p_rvf <- ggplot(res_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.6, size = 1) +
  labs(title = "RQ2 residuals vs fitted",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()
ggsave("plots/RQ2_diag_resid_fitted.png", p_rvf, width = 5.5, height = 4.5, dpi = 150, bg = "white")

# --- Type III ANOVA (unchanged) ----------------------------------------------
cat("\n=== MAIN ANALYSIS: Type III ANOVA ===\n")
anova_results <- anova(fit_RQ2, type = 3)
print(anova_results)

# write ANOVA to CSV
anova_table <- data.frame(
  term    = rownames(anova_results),
  Sum_Sq  = anova_results$`Sum Sq`,
  Mean_Sq = anova_results$`Mean Sq`,
  NumDF   = anova_results$NumDF,
  DenDF   = anova_results$DenDF,
  F_value = anova_results$`F value`,
  p_value = anova_results$`Pr(>F)`
)
readr::write_csv(anova_table, "tables/RQ2_mixed_TypeIII.csv")

# p-values for branching
gen_p <- anova_results["generation", "Pr(>F)"]
task_p <- anova_results["task", "Pr(>F)"]
int_p <- anova_results["generation:task", "Pr(>F)"]

cat("\nSummary:\n")
cat("Generation main effect:", ifelse(gen_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", gen_p))
cat("Task main effect:", ifelse(task_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", task_p))
cat("Generation×Task interaction:", ifelse(int_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", int_p))

# --- Post-hoc within task (unchanged test; now with CSVs) ---------------------
emm_within_task <- emmeans(fit_RQ2, ~ generation | task)

# Link scale: get raw and BH p into one CSV for audit
con_link_raw <- contrast(emm_within_task, method = "consec", adjust = "none") %>% as.data.frame()
con_link <- con_link_raw %>%
  group_by(task) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()
readr::write_csv(con_link %>%
                   select(task, contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                 "tables/RQ2_consec_within_task_LINK.csv")

# Response scale (Joules): differences and BH p
emm_within_task_resp <- regrid(emm_within_task)  # back-transform to J
con_resp_raw <- contrast(emm_within_task_resp, "consec", adjust = "none") %>% as.data.frame()
con_resp <- con_resp_raw %>%
  group_by(task) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()
readr::write_csv(con_resp %>%
                   select(task, contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                 "tables/RQ2_consec_within_task_JOULES.csv")

cat("\n=== POST-HOC: Consecutive generation comparisons within tasks (BH) ===\n")
# Print the BH-adjusted link-scale results (matches your original print)
print(contrast(emm_within_task, method = "consec", adjust = "BH"))

# --- If interaction is NOT significant, also show main-generation contrasts ----
if (!is.na(int_p) && int_p > 0.05) {
  cat("\n=== Since interaction NS, also showing main generation comparisons ===\n")
  emm_gen_main <- emmeans(fit_RQ2, ~ generation)
  # Print BH-adjusted contrasts (unchanged from your logic)
  print(contrast(emm_gen_main, method = "consec", adjust = "BH"))
  # Also save on response scale with raw and BH p
  emm_gen_resp <- regrid(emm_gen_main)
  con_main_raw <- contrast(emm_gen_resp, "consec", adjust = "none") %>% as.data.frame()
  con_main <- con_main_raw %>%
    mutate(p_BH = p.adjust(p.value, method = "BH"))
  readr::write_csv(con_main %>%
                     select(contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                   "tables/RQ2_consec_generation_main_JOULES.csv")
}

# --- Back-transformed EMMs for reporting (unchanged; now saved) ---------------
bt_task <- summary(emmeans(fit_RQ2, ~ generation | task), type = "response")
readr::write_csv(as.data.frame(bt_task), "tables/RQ2_emm_by_task_joules.csv")

emm_gen <- emmeans(fit_RQ2, ~ generation)
emm_gen_joules <- summary(emm_gen, type = "response")
readr::write_csv(as.data.frame(emm_gen_joules), "tables/RQ2_emm_generation_joules.csv")

# “mean ± SE” and 95% CI table (J)
summary_table <- emm_gen_joules %>%
  as.data.frame() %>%
  mutate(
    Energy_J = sprintf("%.0f ± %.0f", response, SE),
    CI_95 = sprintf("[%.0f, %.0f]", lower.CL, upper.CL)
  ) %>%
  select(Generation = generation, Energy_J, CI_95)
readr::write_csv(summary_table, "tables/RQ2_summary_table.csv")
print(summary_table)

# --- Plots (applicable) -------------------------------------------------------
# Main: generation EMMs (Joules)
p_main <- ggplot(emm_gen_joules, aes(x = generation, y = response, group = 1)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1, linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(x = "Llama Generation",
       y = "Energy Consumption (J)",
       title = "RQ2: Energy Consumption Across Generations",
       subtitle = "Estimated marginal means averaged across tasks") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank())
ggsave("plots/RQ2_energy_by_generation.png", p_main, width = 7, height = 5, dpi = 300, bg = "white")

# Faceted by task (Joules)
bt_task_df <- as.data.frame(bt_task)
p_facet <- ggplot(bt_task_df, aes(x = generation, y = response, group = task)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, alpha = 0.5) +
  facet_wrap(~ task, nrow = 2) +
  scale_y_continuous(labels = comma) +
  labs(x = "Generation", y = "Energy (J)",
       title = "Energy Consumption by Task and Generation",
       subtitle = if (!is.na(int_p) && int_p > 0.05)
         "Interaction not significant: pattern consistent across tasks"
       else "Per-task EMMs") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/RQ2_energy_by_task_faceted.png", p_facet, width = 10, height = 6, dpi = 300, bg = "white")

# --- Paste-ready key lines ----------------------------------------------------
cat("\n=== KEY RESULTS FOR MANUSCRIPT (Original Scale) ===\n")
cat("\nEnergy consumption by generation (Joules):\n")
print(summary_table)

cat("\nPercent reduction from baseline (Gen 2):\n")
baseline <- emm_gen_joules$response[1]
for (i in 2:nrow(emm_gen_joules)) {
  gen <- emm_gen_joules$generation[i]
  val <- emm_gen_joules$response[i]
  pct <- (1 - val / baseline) * 100
  cat(sprintf("  Gen %s: %.1f%% reduction\n", gen, pct))
}

# If we created the main-effect response-scale contrasts, show them
if (file.exists("tables/RQ2_consec_generation_main_JOULES.csv")) {
  cat("\nConsecutive generation comparisons (Joules difference, BH):\n")
  con_main_print <- readr::read_csv("tables/RQ2_consec_generation_main_JOULES.csv", show_col_types = FALSE)
  print(con_main_print)
}
