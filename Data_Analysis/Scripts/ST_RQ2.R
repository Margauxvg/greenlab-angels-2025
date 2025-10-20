# ===================================================================
# RQ2: GENERATION × TASK INTERACTION ANALYSIS USING MIXED MODEL
# ===================================================================

library(lme4)      # For mixed effects models (lmer)
library(lmerTest)  # Adds p-values to lmer output
library(emmeans)   # For post-hoc comparisons

# --------------------------------------------------------------------
# STEP 1: PREPARE REPETITION-LEVEL DATA
# --------------------------------------------------------------------

# Extract repetition-level data (NOT aggregated by run)
# Each row = one repetition measurement, giving us multiple observations per cell
rep_df <- df %>%
  select(run, generation, task, energy_j, duration_s, tokens) %>%  
  drop_na()  # Remove any rows with missing values

# Verify we have replication within each generation×task cell
# Should show ~20-30 observations per cell (vs. 1 at run-level)
rep_df %>% 
  count(generation, task) %>% 
  print(n = Inf)

# --------------------------------------------------------------------
# STEP 2: FIT LINEAR MIXED EFFECTS MODEL
# --------------------------------------------------------------------

# Model formula breakdown:
# - log1p(energy_j): Log-transform DV for better residual distribution
#                    log1p = log(1+x) is safer than log when values near 0
# - generation * task: Tests main effects + interaction
#                      Expands to: generation + task + generation:task
# - (1 | run): Random intercept for each run
#              Accounts for correlation between repetitions from same run
#              Prevents pseudoreplication (treating dependent obs as independent)
fit_RQ2 <- lmer(log1p(energy_j) ~ generation * task + (1 | run), 
                data = rep_df)

# --------------------------------------------------------------------
# STEP 3: MODEL DIAGNOSTICS
# --------------------------------------------------------------------

# Check 1: Is random effect variance > 0? 
# FALSE = good (runs do differ, justifying mixed model)
# TRUE = random effect unnecessary, could use regular ANOVA
cat("Model singular?", isSingular(fit_RQ2), "\n")

# Check 2: Did optimizer converge properly?
# NULL = good (no convergence issues)
# Any message = potential optimization problems
cat("Convergence warnings:", 
    summary(fit_RQ2)$optinfo$conv$lme4$messages, "\n")

# Check 3: Visual residual diagnostics
par(mfrow=c(1,2))  # Create 2-panel plot

# Q-Q plot: Tests normality of residuals
# Points should follow diagonal line if normal
qqnorm(resid(fit_RQ2), main="Q-Q Plot of Residuals")
qqline(resid(fit_RQ2))

# Residuals vs Fitted: Tests homoscedasticity (constant variance)
# Should show random scatter around horizontal line
plot(fitted(fit_RQ2), resid(fit_RQ2), 
     main="Residuals vs Fitted",
     xlab="Fitted values", ylab="Residuals")
abline(h=0, lty=2, col="red")  # Add reference line at zero

par(mfrow=c(1,1))  # Reset to single plot

# --------------------------------------------------------------------
# STEP 4: STATISTICAL TESTS
# --------------------------------------------------------------------

# Type III ANOVA: Tests each effect controlling for all others
# Important when design is unbalanced (different n per cell)
# Output shows:
# - Generation effect: p-value tests if energy differs across generations
# - Task effect: p-value tests if energy differs across tasks  
# - Interaction: p-value tests if task effect varies by generation
cat("\n=== MAIN ANALYSIS: Type III ANOVA ===\n")
anova_results <- anova(fit_RQ2, type = 3)
print(anova_results)

# Extract p-values for reporting
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

# --------------------------------------------------------------------
# STEP 5: POST-HOC COMPARISONS
# --------------------------------------------------------------------

# Calculate estimated marginal means for each generation within each task
# The '|' means "separately for each task level"
emm_RQ2 <- emmeans(fit_RQ2, ~ generation | task)

# Compare consecutive generations within each task
# "consec" = consecutive comparisons (2 vs 3, 3 vs 3.1, 3.1 vs 3.2)
# "BH" = Benjamini-Hochberg correction for multiple comparisons
cat("\n=== POST-HOC: Consecutive generation comparisons within tasks ===\n")
contrasts_within_task <- contrast(emm_RQ2, method = "consec", adjust = "BH")
print(contrasts_within_task)

# If interaction is NOT significant (p > 0.05), can also look at 
# generation effects collapsed across tasks for simpler interpretation
if (int_p > 0.05) {
  cat("\n=== Since interaction NS, also showing main generation comparisons ===\n")
  emm_gen_main <- emmeans(fit_RQ2, ~ generation)
  contrasts_main <- contrast(emm_gen_main, method = "consec", adjust = "BH")
  print(contrasts_main)
}

# --------------------------------------------------------------------
# STEP 6: BACK-TRANSFORM FOR INTERPRETATION (OPTIONAL)
# --------------------------------------------------------------------

# Since we used log1p, means are on log scale
# To get back to joules for reporting:
cat("\n=== Energy estimates in original scale (Joules) ===\n")
summary(emm_RQ2, type = "response")  # Back-transforms from log scale


# ===================================================================
# RQ2: REPORTING - TABLES AND VISUALIZATIONS (ORIGINAL SCALE)
# ===================================================================

library(ggplot2)

# Create results directory if it doesn't exist
if(!dir.exists("results")) dir.create("results")
if(!dir.exists("plots")) dir.create("plots")

# --------------------------------------------------------------------
# EXPORT TABLES FOR MANUSCRIPT
# --------------------------------------------------------------------

# 1) Type III ANOVA results - manual extraction
anova_results <- anova(fit_RQ2, type = 3)
anova_table <- data.frame(
  term = rownames(anova_results),
  Sum_Sq = anova_results$`Sum Sq`,
  Mean_Sq = anova_results$`Mean Sq`,
  NumDF = anova_results$NumDF,
  DenDF = anova_results$DenDF,
  F_value = anova_results$`F value`,
  p_value = anova_results$`Pr(>F)`
)
write.csv(anova_table, "RQ2_mixed_TypeIII.csv", row.names = FALSE)

# 2) EMMs by generation×task in original Joules scale
bt_task <- summary(emmeans(fit_RQ2, ~ generation | task), type = "response")
write.csv(as.data.frame(bt_task), "RQ2_emm_by_task_joules.csv", row.names = FALSE)

# 3) Main effect EMMs (collapsed across tasks) in Joules
emm_gen <- emmeans(fit_RQ2, ~ generation)
emm_gen_joules <- summary(emm_gen, type = "response")
write.csv(as.data.frame(emm_gen_joules), "RQ2_emm_generation_joules.csv", row.names = FALSE)

# 4) Consecutive contrasts on ORIGINAL scale (difference in Joules)
# Use regrid to get contrasts of back-transformed values
emm_gen_regrid <- regrid(emm_gen)  # Convert to response scale
consec_joules <- contrast(emm_gen_regrid, "consec", adjust = "BH")
write.csv(as.data.frame(consec_joules), "RQ2_consec_generation_joules.csv", row.names = FALSE)

# 5) Create summary table for paper (mean ± SE in Joules)
summary_table <- emm_gen_joules %>%
  as.data.frame() %>%
  mutate(
    Energy_J = sprintf("%.0f ± %.0f", response, SE),
    CI_95 = sprintf("[%.0f, %.0f]", lower.CL, upper.CL)
  ) %>%
  select(Generation = generation, Energy_J, CI_95)
write.csv(summary_table, "RQ2_summary_table.csv", row.names = FALSE)
print(summary_table)

# --------------------------------------------------------------------
# VISUALIZATION
# --------------------------------------------------------------------

# Main plot: Energy by generation with 95% CI (Joules scale)
p_main <- ggplot(emm_gen_joules,
                 aes(x = generation, y = response, group = 1)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.1, linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Llama Generation", 
       y = "Energy Consumption (J)",
       title = "RQ2: Energy Consumption Across Llama Generations",
       subtitle = "Estimated marginal means averaged across all tasks") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("plots/RQ2_energy_by_generation.png", p_main, 
       width = 7, height = 5, dpi = 300, bg = "white")

# Optional: Faceted plot showing all tasks (if you want to show consistency)
bt_task_df <- as.data.frame(bt_task)
p_facet <- ggplot(bt_task_df,
                  aes(x = generation, y = response, group = task)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.1, alpha = 0.5) +
  facet_wrap(~ task, nrow = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Generation", 
       y = "Energy (J)",
       title = "Energy Consumption by Task and Generation",
       subtitle = "Non-significant interaction (p = 0.999) shows consistent pattern") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/RQ2_energy_by_task_faceted.png", p_facet, 
       width = 10, height = 6, dpi = 300, bg = "white")

# --------------------------------------------------------------------
# PRINT KEY RESULTS FOR REPORTING (FIXED)
# --------------------------------------------------------------------

cat("\n=== KEY RESULTS FOR MANUSCRIPT (Original Scale) ===\n")
cat("\nEnergy consumption by generation (Joules):\n")
print(summary_table)

cat("\nPercent reduction from baseline (Gen 2):\n")
baseline <- emm_gen_joules$response[1]
for(i in 2:nrow(emm_gen_joules)) {
  gen <- emm_gen_joules$generation[i]
  val <- emm_gen_joules$response[i]
  pct <- (1 - val/baseline) * 100
  cat(sprintf("  Gen %s: %.1f%% reduction\n", gen, pct))
}

cat("\nConsecutive comparisons (Joules difference):\n")
consec_df <- as.data.frame(consec_joules)
sig_rows <- which(consec_df$p.value < 0.05)

if(length(sig_rows) > 0) {
  cat("Significant differences (p < 0.05):\n")
  print(consec_df[sig_rows, c("contrast", "estimate", "SE", "p.value")])
} else {
  cat("  No significant pairwise differences after BH adjustment\n")
  cat("\nAll consecutive comparisons:\n")
  print(consec_df[, c("contrast", "estimate", "SE", "p.value")])
}