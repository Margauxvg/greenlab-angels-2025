# ==============================================================
# RQ 1.2 — Scaling with parameter size (within generation)
# Unit of analysis: RUN (already averaged over repetitions)
# DV (primary): J_per_B = energy_j_mean / model_size_b
# Factors: generation (ordered), size_f (factor from model_size_b)
# Omnibus: two-way ANOVA on RAW J_per_B (assumptions previously OK)
# Post-hoc: successive generations within each size (BH)
# Extras: Hedges g for those steps, size comparisons within gen,
#         ordered trend across gens within size, two plots,
#         slope comparison if identifiable.
# No CSVs. Just prints and plots.
# ==============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(emmeans)
  library(effectsize)
})

# 0) Build the analysis slice --------------------------------------------------
# - Make J_per_B if missing
# - Keep generations ordered
# - Factorize sizes (1B, 3B, 7B...); order small -> large for nicer plots
stopifnot(all(c("generation","model_size_b","energy_j_mean") %in% names(run_level)))
rq12 <- run_level %>%
  mutate(
    J_per_B    = if ("J_per_B" %in% names(.)) J_per_B else energy_j_mean / pmax(model_size_b, 1e-9),
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    size_f     = factor(model_size_b, levels = sort(unique(model_size_b)), ordered = TRUE)
  ) %>%
  select(generation, size_f, model_size_b, energy_j_mean, J_per_B) %>%
  drop_na()

# 1) Descriptives (print only) -------------------------------------------------
cat("\n--- RQ1.2 Descriptives (by generation × size) ---\n")
rq12 %>%
  group_by(generation, size_f) %>%
  summarise(
    n_runs          = n(),
    mean_energy_J   = mean(energy_j_mean),
    median_energy_J = median(energy_j_mean),
    sd_energy_J     = sd(energy_j_mean),
    mean_J_per_B    = mean(J_per_B),
    median_J_per_B  = median(J_per_B),
    sd_J_per_B      = sd(J_per_B),
    .groups = "drop"
  ) %>%
  arrange(generation, size_f) %>%
  print(n = Inf)

# 2) Two-way ANOVA on RAW J_per_B ---------------------------------------------
# Why raw? Your assumption checker said factorial ANOVA on raw J/B was fine here.
fit12 <- aov(J_per_B ~ generation * size_f, data = rq12)

cat("\n--- RQ1.2 Omnibus ANOVA (J_per_B ~ generation * size) ---\n")
print(summary(fit12))

# 3) Omnibus effect sizes ------------------------------------------------------
# Partial η² and ω² for each term: generation, size, interaction.
eta_sq_12   <- effectsize::eta_squared(fit12, partial = TRUE)
omega_sq_12 <- effectsize::omega_squared(fit12)

cat("\n--- Effect sizes (η²p and ω²) ---\n")
print(eta_sq_12)
print(omega_sq_12)

# 4) Post-hoc: successive generations within each size (BH) --------------------
# Exactly your planned story: 2→3, 3→3.1, 3.1→3.2 inside EACH size level that exists.
emm12_g_by_size <- emmeans(fit12, ~ generation | size_f)

cat("\n--- Successive generation contrasts within size (BH) ---\n")
succ_gen_within_size <- contrast(emm12_g_by_size, "consec", adjust = "BH")
print(succ_gen_within_size)

# 5) Hedges g for those successive steps (simple and correct) ------------------
# Use ANOVA residual SD (pooled) and the small-sample correction.
succ_df <- as.data.frame(succ_gen_within_size)
rmse  <- sigma(fit12)                  # pooled residual SD
edf   <- df.residual(fit12)
Jcorr <- 1 - 3/(4*edf - 1)             # correction factor for Hedges g

succ_df$cohen_d  <- succ_df$estimate / rmse
succ_df$hedges_g <- succ_df$cohen_d * Jcorr

cat("\n--- Successive gen contrasts with Hedges g (by size) ---\n")
for (sz in unique(succ_df$size_f)) {
  cat(sprintf("\nSize %s:\n", as.character(sz)))
  print(succ_df[succ_df$size_f == sz,
                c("contrast","estimate","SE","t.ratio","p.value","hedges_g")],
        row.names = FALSE)
}

# 6) Size comparisons within each generation (BH) ------------------------------
# Only meaningful for generations that actually have ≥2 sizes; others are skipped.
emm12_size_within_gen <- emmeans(fit12, ~ size_f | generation)

cat("\n--- Size contrasts within generation (BH; only gens with ≥2 sizes) ---\n")
# Use try() so it doesn’t error when a generation has only one size
print(try(pairs(emm12_size_within_gen, adjust = "BH"), silent = TRUE))

# 7) Ordered trend across gens within size ------------------------------------
# Tests linear/quadratic/cubic trend over the ordered generations, separately per size.
cat("\n--- Polynomial trend across generations within each size ---\n")
poly_within_size <- contrast(emm12_g_by_size, "poly")
print(poly_within_size)

# 8) Plots (fixed jitter issue; choose ONE jitter approach) -------------------

# 8a) Box + jitter: J_per_B by generation, colored by size
# We DODGE the boxes by size, and we JITTER the points using position_jitterdodge.
# Do NOT also set width/height in geom_jitter when you pass a position object.
p_box <- ggplot(rq12, aes(generation, J_per_B, fill = size_f)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7,
               position = position_dodge(width = 0.75)) +
  geom_jitter(aes(color = size_f),
              alpha = 0.55, size = 1,
              position = position_jitterdodge(jitter.width = 0.08,
                                              dodge.width  = 0.75)) +
  labs(x = "Generation", y = "Energy per billion params (J/B)",
       fill = "Size (B)", color = "Size (B)") +
  theme_minimal()
print(p_box)

# 8b) Mean ± 95% CI: EMMs of J_per_B by gen × size
emm12 <- emmeans(fit12, ~ generation * size_f)
emm12_df <- as.data.frame(emm12)

p_mean_ci <- ggplot(emm12_df,
                    aes(x = generation, y = emmean, group = size_f,
                        color = size_f, shape = size_f)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(x = "Generation", y = "Estimated J/B (mean ± 95% CI)",
       color = "Size (B)", shape = "Size (B)") +
  theme_minimal()
print(p_mean_ci)

# 9) Slope analysis (energy vs size) if identifiable --------------------------
# Aim: compare how energy scales with size across generations.
# You need ≥2 distinct sizes inside ≥2 different generations to compare slopes.
size_count <- rq12 %>%
  group_by(generation) %>%
  summarise(k_sizes = n_distinct(model_size_b), .groups = "drop")
cat("\nSizes per generation:\n"); print(size_count)

gens_with_slope <- size_count %>% filter(k_sizes >= 2) %>% pull(generation)

if (length(gens_with_slope) >= 2) {
  # Compare slopes on a stabilised scale: log1p(E) ~ size * generation
  fit_scale <- lm(log1p(energy_j_mean) ~ model_size_b * generation, data = rq12)
  
  cat("\n--- Slope comparison via emtrends (log1p(E) vs size, by gen) ---\n")
  tr <- emtrends(fit_scale, ~ generation, var = "model_size_b")
  print(tr)
  
  cat("\n--- Are slopes different across gens? (BH) ---\n")
  print(pairs(tr, adjust = "BH"))
} else {
  cat("\n--- Slope comparison skipped ---\n",
      "Reason: need ≥2 sizes in ≥2 generations to compare slopes.\n",
      "We’ll only estimate within-gen slopes where possible (descriptive):\n", sep = "")
  if (length(gens_with_slope) >= 1) {
    for (g in gens_with_slope) {
      cat(sprintf("\nGeneration %s: slope of log1p(E) ~ size\n", g))
      m <- lm(log1p(energy_j_mean) ~ model_size_b, data = rq12 %>% filter(generation == g))
      print(summary(m))
    }
  }
}

# 10) Minimal textual summary so the Results paragraph writes itself -----------
cat("\n=== RQ1.2 Summary ===\n")
anova_tab <- summary(fit12)[[1]]
pg   <- anova_tab["generation","Pr(>F)"]
ps   <- anova_tab["size_f","Pr(>F)"]
pint <- anova_tab["generation:size_f","Pr(>F)"]
cat(sprintf("Two-way ANOVA (J/B): Gen p = %.4g, Size p = %.4g, Gen×Size p = %.4g.\n",
            pg, ps, pint))

# Pick the strongest within-size successive gen change
best_row <- succ_df[order(succ_df$p.value), ][1, ]
cat(sprintf("Strongest successive gen change (within size %s): %s, Δ = %.2f J/B, p(BH) = %.3g, Hedges g = %.2f.\n",
            as.character(best_row$size_f), best_row$contrast,
            best_row$estimate, best_row$p.value, best_row$hedges_g))

cat("\nOmnibus effect sizes:\n")
print(eta_sq_12)
print(omega_sq_12)

