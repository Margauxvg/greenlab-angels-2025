# ==============================================================================
# RQ 1.1 — Energy per run across generations (run-level)
# ------------------------------------------------------------------------------
# PURPOSE
#   Test whether average energy per run differs across Llama generations,
#   ignoring parameter size. This is a one-way between-groups design.
#
# PRIMARY DV
#   - energy_j_mean  (mean Joules per run; already aggregated at run level)
#
# FACTOR
#   - generation ∈ {2, 3, 3.1, 3.2}  (treated as an ordered factor for trend tests)
#
# HYPOTHESIS
#   H0: μ_gen2 = μ_gen3 = μ_gen3.1 = μ_gen3.2
#   H1: At least one generation mean differs (expect lower energy for newer gens).
#
# ASSUMPTIONS (from prior checker)
#   - Residuals ≈ normal and variances ≈ homogeneous on RAW scale → use standard
#     one-way ANOVA. If your checker later says otherwise, switch to log1p or
#     Welch in the two obvious places below.
#
# INPUTS (expected in the environment)
#   - data.frame run_level with columns:
#       generation (factor or coercible), energy_j_mean (numeric)
#   - Nothing else is required. All other columns are ignored.
#
# ANALYSIS STEPS
#   1) Omnibus one-way ANOVA on raw energy_j_mean.
#   2) Omnibus effect sizes:
#        • η² (eta squared) and ω² (omega squared).
#   3) Planned post-hoc (successive) contrasts with BH correction:
#        • 2→3, 3→3.1, 3.1→3.2 on the RAW scale (differences in Joules).
#        • Hedges g for each step using pooled residual SD and small-sample
#          correction (J).
#   4) Orthogonal polynomial trend over ordered generations:
#        • Linear, quadratic, cubic components (BH-adjusted p).
#   5) Plots that match the hypothesis:
#        • ECDF overlay by generation (full distribution comparison).
#        • Estimation plot for successive differences with BH-adjusted 95% CIs.
#
# OUTPUT FILES (created if "tables"/"plots" dirs exist or are creatable)
#   - tables/RQ11_omnibus.csv
#       term, df1, df2, F_value, p_value, eta2, omega2, note
#   - tables/RQ11_pairwise_consec.csv
#       contrast, estimate_J, SE, t_value, p_raw, p_BH, hedges_g
#         • estimate_J is the raw Joules difference (newer − older).
#         • hedges_g magnitude rough guide: 0.2 small, 0.5 medium, 0.8 large.
#   - tables/RQ11_trend_poly.csv
#       component (linear/quadratic/cubic), estimate, SE, t_value, p_raw, p_BH
#   - plots/RQ11_ecdf_by_generation.png
#       ECDF overlay of energy per run by generation.
#   - plots/RQ11_successive_diffs_CI.png
#       Point estimates and BH-adjusted CIs for the three successive steps.
#
# CONSOLE SUMMARY (paste-ready lines)
#   - ANOVA F, p, η², ω².
#   - Strongest successive step with ΔJ, p(BH), Hedges g.
#
# INTERPRETATION NOTES
#   - Prefer p_BH for the successive contrasts (3 planned tests).
#   - If a CI for ΔJ crosses 0, that step isn’t significant on the chosen error
#     rate. Negative Δ means the newer gen uses less energy (good).
#   - A large η²/ω² (e.g., >0.14) signals a big generation effect in context.
#
# HOW TO REUSE FOR ANOTHER DV
#   - Replace 'energy_j_mean' everywhere with your DV.
#   - Keep generation as an ordered factor to retain trend tests.
#
# FAILURE MODES / SANITY
#   - If generation has <2 levels or any group has n<2, ANOVA will be fragile.
#   - If run_level lacks expected columns, stop and fix your data wrangling.
#   - If assumptions flip (your checker says log1p), re-fit on log1p and report
#     back-transformed EMMs if needed.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(emmeans)
  library(effectsize)
  library(ggplot2)
  library(scales)
})

# ---------- 0) Prep ----------
stopifnot(exists("run_level"))
stopifnot(all(c("generation","energy_j_mean") %in% names(run_level)))

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

rq11 <- run_level %>%
  select(generation, energy_j_mean) %>%
  drop_na() %>%
  mutate(generation = factor(generation,
                             levels = c("2","3","3.1","3.2"),
                             ordered = TRUE))

# ---------- 1) Omnibus ANOVA (RAW) ----------
fit11 <- aov(energy_j_mean ~ generation, data = rq11)
anova_tab <- summary(fit11)[[1]]
eta_tab   <- effectsize::eta_squared(fit11, partial = TRUE)  # == eta^2 in one-way
omega_tab <- effectsize::omega_squared(fit11)

omnibus_csv <- tibble(
  term    = "generation",
  df1     = unname(anova_tab["generation","Df"]),
  df2     = unname(anova_tab["Residuals","Df"]),
  F_value = unname(anova_tab["generation","F value"]),
  p_value = unname(anova_tab["generation","Pr(>F)"]),
  eta2    = eta_tab$Eta2[1],
  omega2  = omega_tab$Omega2[1],
  note    = "one-way ANOVA on raw scale"
)
readr::write_csv(omnibus_csv, "tables/RQ11_omnibus.csv")

cat("\n--- Omnibus ANOVA ---\n"); print(omnibus_csv)

# ---------- 2) Successive pairwise (2→3, 3→3.1, 3.1→3.2) ----------
emm11 <- emmeans(fit11, ~ generation)

# raw p first
succ_none <- contrast(emm11, "consec", adjust = "none") %>% as.data.frame()
# BH across the 3 planned steps
succ_none$p_BH <- p.adjust(succ_none$p.value, method = "BH")

# Hedges g via pooled residual SD
rmse <- sigma(fit11)
edf  <- df.residual(fit11)
J    <- 1 - 3/(4*edf - 1)

pairwise_csv <- succ_none %>%
  transmute(
    contrast,                # "generation3 - generation2", etc.
    estimate_J = estimate,   # difference in J
    SE,
    t_value = t.ratio,
    p_raw   = p.value,
    p_BH,
    hedges_g = (estimate_J / rmse) * J
  )
readr::write_csv(pairwise_csv, "tables/RQ11_pairwise_consec.csv")

cat("\n--- Successive pairwise (raw & BH) ---\n"); print(pairwise_csv)

# ---------- 3) Orthogonal trend (linear/quadratic/cubic) ----------
poly_df <- contrast(emm11, "poly") %>% as.data.frame()
poly_df$p_BH <- p.adjust(poly_df$p.value, method = "BH")
trend_csv <- poly_df %>%
  transmute(component = contrast, estimate, SE, t_value = t.ratio, p_raw = p.value, p_BH)
readr::write_csv(trend_csv, "tables/RQ11_trend_poly.csv")

cat("\n--- Trend components ---\n"); print(trend_csv)

# ---------- 4) Plots that actually help the story ----------
# 4a) ECDF overlay by generation (full distribution comparison)
p_ecdf <- ggplot(rq11, aes(energy_j_mean, color = generation)) +
  stat_ecdf(linewidth = 0.9) +   # ← use linewidth for lines
  labs(x = "Energy per run (J)", y = "Empirical CDF", color = "Generation") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
ggsave("plots/RQ11_ecdf_by_generation.png", p_ecdf, width = 8, height = 5, dpi = 150, bg = "white")

# 4b) Estimation plot: successive differences with 95% CI on the differences
succ_ci <- confint(contrast(emm11, "consec"), adjust = "BH") %>% as.data.frame()
# Clean labels: "3-2", "3.1-3", "3.2-3.1"
succ_ci$step <- gsub("generation", "", succ_ci$contrast)
succ_ci$step <- gsub(" - generation", "-", succ_ci$step)

p_diffs <- ggplot(succ_ci, aes(x = step, y = estimate)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(x = "Successive step", y = "Δ Energy (J) with 95% CI (BH-adjusted)") +
  theme_minimal()
ggsave("plots/RQ11_successive_diffs_CI.png", p_diffs, width = 7.5, height = 5, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/RQ11_ecdf_by_generation.png\n- plots/RQ11_successive_diffs_CI.png\n")

# ---------- 5) Tiny paste-ready summary ----------
cat("\n=== RQ1.1 Summary ===\n")
cat(sprintf("ANOVA: F(%d,%d) = %.2f, p = %.4g; η² = %.2f, ω² = %.2f.\n",
            omnibus_csv$df1, omnibus_csv$df2, omnibus_csv$F_value,
            omnibus_csv$p_value, omnibus_csv$eta2, omnibus_csv$omega2))

best <- pairwise_csv %>% arrange(p_BH) %>% slice(1)
cat(sprintf("Largest step: %s, Δ = %.0f J, p(BH) = %.4g, Hedges g = %.2f.\n",
            best$contrast, best$estimate_J, best$p_BH, best$hedges_g))

