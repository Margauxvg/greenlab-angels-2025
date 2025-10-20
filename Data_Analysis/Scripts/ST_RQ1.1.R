# ==============================================================
# RQ 1.1 — Energy per run across generations (run-level)
# Design: one-way ANOVA (factor = generation)
# DV: energy_j_mean  (raw scale; assumptions already OK)
# Post-hoc: successive generations with BH correction
# Effect sizes: η²p, ω² (omnibus) and Hedges g (pairwise successive)
# Plots: box+jitter, mean±95% CI
# ==============================================================

# 0) Libraries ---------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)   # data wrangling + ggplot
  library(emmeans)     # marginal means + contrasts
  library(effectsize)  # eta^2, omega^2
})

# 1) Sanity checks + prep ---------------------------------------
stopifnot(exists("run_level"))
stopifnot(all(c("generation","energy_j_mean") %in% names(run_level)))

# Keep generations ordered so comparisons make sense
rq11 <- run_level %>%
  select(generation, energy_j_mean) %>%
  drop_na() %>%
  mutate(generation = factor(generation,
                             levels = c("2","3","3.1","3.2"),
                             ordered = TRUE))

# Optional: quick descriptives (reuse if you already printed them)
cat("\n--- RQ1.1 Descriptives (run-level) ---\n")
rq11 %>%
  group_by(generation) %>%
  summarise(
    n_runs        = n(),
    mean_energy   = mean(energy_j_mean),
    median_energy = median(energy_j_mean),
    sd_energy     = sd(energy_j_mean),
    .groups = "drop"
  ) %>%
  arrange(generation) %>%
  print(n = Inf)

# 2) Omnibus: one-way ANOVA on RAW scale ------------------------
# Why raw? Your assumption checker said residuals were normal and variances homogeneous on raw.
fit11 <- aov(energy_j_mean ~ generation, data = rq11)

cat("\n--- RQ1.1 Omnibus ANOVA (energy ~ generation, raw) ---\n")
print(summary(fit11))

# 3) Omnibus effect sizes ---------------------------------------
# η²p = proportion of variance explained by generation (partial)
# ω²  = less biased estimate (report both like a responsible scientist)
eta_sq_11   <- effectsize::eta_squared(fit11, partial = TRUE)
omega_sq_11 <- effectsize::omega_squared(fit11)

cat("\n--- Effect sizes (omnibus) ---\n")
print(eta_sq_11)
print(omega_sq_11)

# 4) Post-hoc: successive generations (BH) -----------------------
# Exactly your planned comparisons: (2 vs 3), (3 vs 3.1), (3.1 vs 3.2)
emm11 <- emmeans(fit11, ~ generation)

cat("\n--- Successive generation contrasts (BH) ---\n")
succ_contr <- contrast(emm11, "consec", adjust = "BH")  # BH over the 3 steps
print(succ_contr)

# 5) Hedges g for those successive steps (easiest correct way) --
# Take the ANOVA residual SD (pooled) and apply the standard small-sample correction.
succ_df <- as.data.frame(succ_contr)     # contains estimate/SE/t/p/BH p
rmse <- sigma(fit11)                     # pooled SD from ANOVA residuals
edf  <- df.residual(fit11)               # residual df
J    <- 1 - 3/(4*edf - 1)                # small-sample correction for Hedges g

succ_df$cohen_d  <- succ_df$estimate / rmse
succ_df$hedges_g <- succ_df$cohen_d * J

cat("\n--- Successive contrasts with standardized effects ---\n")
print(succ_df[, c("contrast","estimate","SE","t.ratio","p.value","cohen_d","hedges_g")],
      row.names = FALSE)

# 6) Ordered trend across generations (optional but useful) -----
# Tests linear / quadratic / cubic components over ordered factor levels.
cat("\n--- Polynomial trend over ordered generations ---\n")
poly_tests <- contrast(emm11, "poly")
print(poly_tests)

# 7) Plots -------------------------------------------------------
# 7a) Box + jitter: shows distribution and each run
p_box <- ggplot(rq11, aes(generation, energy_j_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Energy per run (J)", fill = "Generation") +
  theme_minimal()
print(p_box)
# If you want a file: ggsave("RQ11_box_jitter_energy_by_generation.png", p_box, width=8, height=5, dpi=150, bg="white")

# 7b) Mean ± 95% CI (from EMMs on raw scale)
emm11_df <- as.data.frame(summary(emm11))  # emmean, lower.CL, upper.CL on raw
p_mean_ci <- ggplot(emm11_df, aes(x = generation, y = emmean, group = 1)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(x = "Generation", y = "Estimated mean energy per run (J)") +
  theme_minimal()
print(p_mean_ci)
# If you want a file: ggsave("RQ11_mean_CI_energy_by_generation.png", p_mean_ci, width=8, height=5, dpi=150, bg="white")

# 8) One-paragraph console summary so Results basically writes itself ----------
cat("\n=== RQ1.1 Summary ===\n")
p_anova <- summary(fit11)[[1]]["generation","Pr(>F)"]
cat(sprintf("Omnibus ANOVA: p = %.4g for generation effect.\n", p_anova))

# Linear trend component (headline for ordered generations)
poly_df <- as.data.frame(poly_tests)
lin <- poly_df[poly_df$contrast == "linear", , drop=FALSE]
if (nrow(lin)) {
  cat(sprintf("Linear trend across generations: t = %.2f, p = %.4g.\n",
              lin$t.ratio, lin$p.value))
}

# Most pronounced successive step (usually 3.1 -> 3.2)
best <- succ_df[order(succ_df$p.value), ][1, ]
cat(sprintf("Strongest successive change: %s, Δ = %.2f J, p(BH) = %.4g, Hedges g = %.2f.\n",
            best$contrast, best$estimate, best$p.value, best$hedges_g))

cat("Omnibus effect sizes:\n")
print(eta_sq_11)
print(omega_sq_11)


