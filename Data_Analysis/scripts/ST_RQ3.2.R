# ==============================================================================
# RQ 3.2 — Inference duration and energy consumption (REVISED)
#
# DESIGN CHANGE (Addressing Professor Feedback):
#   • Unit of analysis: REPETITION (N=900, raw data).
#   • Reason: Averaging runs caused a loss of statistical power.
#   • DV: Instantaneous power per repetition (W) = energy_j / duration_s.
#   • Factor: generation (ordered: 2, 3, 3.1, 3.2).
#
# TESTS (Non-parametric due to non-normality):
#   1) Omnibus: Kruskal–Wallis on power_w ~ generation.
#   2) Planned post-hoc (successive, BH-adjusted): Wilcoxon rank-sum.
#      - Effect size: Cliff’s delta (positive = newer gen uses MORE power).
#   3) Correlation check: Spearman ρ between duration and energy (repetition-level).
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, purrr, readr
  library(effsize)    # For automated Cliff's delta calculation
})

# ---------- 0) Data Check & Preparation ----------
# Ensure we are using the full repetition dataset, not the averaged runs
if (!exists("repetition_level")) {
  stop("Error: 'repetition_level' dataframe not found. Please load the raw repetition data first.")
}

stopifnot(all(c("generation", "energy_j", "duration_s") %in% names(repetition_level)))

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

# Calculate Power (W) for every single repetition
rq32 <- repetition_level %>%
  mutate(
    # Avoid division by zero if duration is essentially zero
    power_w = energy_j / pmax(duration_s, 1e-9),
    generation = factor(generation, levels = c("2", "3", "3.1", "3.2"), ordered = TRUE)
  ) %>%
  select(generation, energy_j, duration_s, power_w) %>%
  drop_na(power_w, energy_j, duration_s)

cat(sprintf("Data loaded: %d repetitions across %d generations.\n", nrow(rq32), nlevels(rq32$generation)))

# ---------- 1) Descriptives (Repetition Level) ----------
cat("\n--- RQ3.2 Descriptives (Repetition Level) ---\n")
desc <- rq32 %>%
  group_by(generation) %>%
  summarise(
    n_obs             = n(),
    mean_duration_s   = mean(duration_s),
    median_duration_s = median(duration_s),
    sd_duration_s     = sd(duration_s),
    mean_energy_J     = mean(energy_j),
    median_energy_J   = median(energy_j),
    sd_energy_J       = sd(energy_j),
    mean_power_W      = mean(power_w),
    median_power_W    = median(power_w),
    sd_power_W        = sd(power_w),
    .groups = "drop"
  ) %>% arrange(generation)

print(desc, n = Inf)
readr::write_csv(desc, "tables/RQ32_descriptives_repetition_level.csv")

# ---------- 2) Omnibus: Kruskal–Wallis on Power ----------
cat("\n--- Kruskal–Wallis: power_w ~ generation ---\n")
kw <- kruskal.test(power_w ~ generation, data = rq32)
print(kw)

H <- as.numeric(kw$statistic)
k <- nlevels(rq32$generation)
n <- nrow(rq32)
eps2 <- (H - k + 1) / (n - k) # Epsilon-squared effect size

omnibus_csv <- tibble(
  term     = "generation",
  H_stat   = H,
  df       = k - 1,
  p_value  = kw$p.value,
  epsilon2 = eps2,
  note     = "Kruskal–Wallis on power (repetition-level)"
)
readr::write_csv(omnibus_csv, "tables/RQ32_omnibus_kw.csv")
cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# ---------- 3) Post-hoc: Successive Wilcoxon (BH-adjusted) ----------
gens  <- levels(rq32$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

one_step <- function(g1, g2) {
  sub <- rq32 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  
  # Wilcoxon Rank Sum Test
  wt <- wilcox.test(power_w ~ generation, data = sub, exact = FALSE)
  
  # Cliff's Delta calculation
  # We use the 'effsize' package or manual calc if preferred. 
  # Here manual to ensure direction: (Newer - Older)
  x <- sub$power_w[sub$generation == g1] # Older
  y <- sub$power_w[sub$generation == g2] # Newer
  
  # Cliff's delta manual: ( #(y>x) - #(y<x) ) / (nx * ny)
  # Positive delta means Newer (y) values are generally larger than Older (x)
  d_est <- cliff.delta(y, x)$estimate
  
  tibble(
    contrast     = paste(g2, "-", g1),
    W            = unname(wt$statistic),
    p_raw        = wt$p.value,
    cliffs_delta = d_est
  )
}

wtab <- purrr::map2_dfr(pairs$g1, pairs$g2, one_step) %>%
  mutate(
    p_BH = p.adjust(p_raw, method = "BH"),
    direction_note = "Cliff's delta > 0 means NEWER gen has HIGHER power"
  )

cat("\n--- Successive Wilcoxon tests on power (BH-adjusted) ---\n")
print(wtab, n = Inf)
readr::write_csv(wtab, "tables/RQ32_wilcoxon_consec.csv")

# ---------- 4) Spearman Correlation: Duration vs Energy (Repetition Level) ----------
spearman_by_gen <- rq32 %>%
  group_by(generation) %>%
  group_modify(~{
    ct <- suppressWarnings(cor.test(.x$duration_s, .x$energy_j,
                                    method = "spearman"))
    tibble(rho = unname(ct$estimate), p = ct$p.value)
  }) %>%
  ungroup() %>%
  mutate(p_BH = p.adjust(p, method = "BH"))

cat("\n--- Spearman correlation: duration vs energy (repetition-level) ---\n")
print(spearman_by_gen, n = Inf)
readr::write_csv(spearman_by_gen, "tables/RQ32_spearman_duration_energy.csv")

# ---------- 5) Plots (Updated for High Density Data) ----------

# 5a) Box + Violin of Power (Better for large N than just jitter)
p_box <- ggplot(rq32, aes(generation, power_w, fill = generation)) +
  geom_violin(alpha = 0.3, trim = FALSE, color = NA) + # Added violin for density shape
  geom_boxplot(width = 0.2, outlier.alpha = 0.2, outlier.size = 0.5) +
  labs(
    title = "Power Distribution by Generation (Repetition Level)",
    subtitle = "Analysis on N=900 repetitions",
    x = "Generation", 
    y = "Power (W)", 
    fill = "Generation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots/RQ32_power_distribution.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 5b) Scatter: Duration vs Energy (High density adjustment)
p_scatter <- ggplot(rq32, aes(duration_s, energy_j, color = generation)) +
  geom_point(alpha = 0.4, size = 1.5) + # Lower alpha for overlapping points
  geom_smooth(method = "lm", se = FALSE, size = 0.8, linetype = "dashed") + # Add trend lines
  labs(
    title = "Inference Duration vs Energy Consumption",
    subtitle = "Repetition-level correlation",
    x = "Inference Duration (s)", 
    y = "Energy (J)", 
    color = "Generation"
  ) +
  theme_minimal()

ggsave("plots/RQ32_scatter_duration_vs_energy.png", p_scatter,
       width = 8, height = 6, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/RQ32_power_distribution.png\n- plots/RQ32_scatter_duration_vs_energy.png\n")

# ---------- 6) Summary Output ----------
cat("\n=== RQ3.2 Final Summary (Repetition Level) ===\n")
cat(sprintf("Omnibus (Kruskal-Wallis): H = %.3f, p = %.4g, Eps² = %.3f\n", H, kw$p.value, eps2))
cat("Pairwise Contrasts (Cliff's Delta):\n")
print(wtab %>% select(contrast, p_BH, cliffs_delta))