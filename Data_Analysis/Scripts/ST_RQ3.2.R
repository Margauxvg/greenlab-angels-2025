# ==============================================================
# RQ 3.2 — Inference duration and energy consumption
# Focus here: average power per run = energy / duration (W)
# Design: run-level, grouped by generation (ignore size)
# Assumptions failed earlier → NONPARAMETRIC ONLY.
# Omnibus: Kruskal–Wallis on power across generations
# Post-hoc: successive Wilcoxon (2→3, 3→3.1, 3.1→3.2), BH-adjusted
# Effect sizes: epsilon^2 (omnibus), Cliff's delta (pairwise)
# Plots: box+jitter of power; scatter of duration vs energy (by generation)
# ==============================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, purrr
  library(ggplot2)
})

# 0) Data slice and derived power ---------------------------------------------
# Need in run_level: generation, energy_j_mean, duration_s_mean
stopifnot(all(c("generation","energy_j_mean","duration_s_mean") %in% names(run_level)))

rq32 <- run_level %>%
  mutate(
    # average power (Watts) per run; guard against zero/NA duration
    power_w_run = if (!"power_w_run" %in% names(.))
      energy_j_mean / pmax(duration_s_mean, 1e-9)
    else power_w_run,
    generation  = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  ) %>%
  select(generation, energy_j_mean, duration_s_mean, power_w_run) %>%
  drop_na(power_w_run)

# 1) Descriptives (print only) -------------------------------------------------
cat("\n--- RQ3.2 Descriptives (by generation) ---\n")
rq32 %>%
  group_by(generation) %>%
  summarise(
    n_runs            = n(),
    mean_duration_s   = mean(duration_s_mean),
    median_duration_s = median(duration_s_mean),
    sd_duration_s     = sd(duration_s_mean),
    mean_energy_J     = mean(energy_j_mean),
    median_energy_J   = median(energy_j_mean),
    sd_energy_J       = sd(energy_j_mean),
    mean_power_W      = mean(power_w_run),
    median_power_W    = median(power_w_run),
    sd_power_W        = sd(power_w_run),
    .groups = "drop"
  ) %>%
  arrange(generation) %>%
  print(n = Inf)

# 2) Omnibus: Kruskal–Wallis on power (nonparametric) -------------------------
# Null: the distributions of average power are identical across generations.
cat("\n--- Kruskal–Wallis: power_w_run ~ generation ---\n")
kw <- kruskal.test(power_w_run ~ generation, data = rq32)
print(kw)

# Omnibus effect size: epsilon^2 = (H - k + 1) / (n - k)
H <- as.numeric(kw$statistic)
k <- nlevels(rq32$generation)
n <- nrow(rq32)
eps2 <- (H - k + 1) / (n - k)
cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# 3) Planned post-hoc: successive Wilcoxon, BH-corrected ----------------------
# We only test adjacent generations: (2 vs 3), (3 vs 3.1), (3.1 vs 3.2).
gens  <- levels(rq32$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

# Helper: Wilcoxon + Cliff's delta for one adjacent pair
# Direction: delta > 0 means NEWER gen (g2) tends to have HIGHER power (worse energetically).
one_step <- function(g1, g2) {
  sub <- rq32 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  
  wt <- wilcox.test(power_w_run ~ generation, data = sub, exact = FALSE)
  
  # Cliff's delta oriented as "newer minus older"
  x <- sub$power_w_run[sub$generation == g1]  # older
  y <- sub$power_w_run[sub$generation == g2]  # newer
  nx <- length(x); ny <- length(y)
  m <- outer(y, x, "-")
  delta_newer <- (sum(m > 0) - sum(m < 0)) / (nx * ny)
  
  tibble(
    contrast      = paste(g2, "-", g1),  # newer - older
    W             = unname(wt$statistic),
    p_raw         = wt$p.value,
    cliffs_delta  = delta_newer          # >0 means newer uses higher W
  )
}

wtab <- purrr::map2_dfr(pairs$g1, pairs$g2, one_step) %>%
  mutate(p_BH = p.adjust(p_raw, method = "BH"))

cat("\n--- Successive Wilcoxon tests on power (BH across 3 steps) ---\n")
print(wtab)

# 4) Optional correlation check: duration vs energy (Spearman) -----------------
# This answers the "inference duration and energy consumption" part directly.
# We compute Spearman within each generation, then BH-adjust across 4 tests.
spearman_by_gen <- rq32 %>%
  group_by(generation) %>%
  group_modify(~{
    ct <- suppressWarnings(cor.test(.x$duration_s_mean, .x$energy_j_mean, method = "spearman"))
    tibble(rho = unname(ct$estimate), p = ct$p.value)
  }) %>%
  ungroup() %>%
  mutate(p_BH = p.adjust(p, method = "BH"))

cat("\n--- Spearman correlation: duration vs energy, within generation (BH) ---\n")
print(spearman_by_gen, n = Inf)

# 5) Plots ---------------------------------------------------------------------

# Box + jitter of average power by generation
p_box <- ggplot(rq32, aes(generation, power_w_run, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Average power per run (W)", fill = "Generation") +
  theme_minimal()
print(p_box)

# Scatter: duration vs energy, colored by generation
# Same scales across gens, no smoothing unless you want it.
p_scatter <- ggplot(rq32, aes(duration_s_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(x = "Inference duration (s)", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
print(p_scatter)

# 6) One-paragraph console summary --------------------------------------------
cat("\n=== RQ3.2 Summary ===\n")
cat(sprintf("Kruskal–Wallis on power: H = %.3f, df = %d, p = %.4g; epsilon^2 = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change in power: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer-older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))
cat("Spearman duration–energy by gen (BH):\n"); print(spearman_by_gen, n = Inf)
