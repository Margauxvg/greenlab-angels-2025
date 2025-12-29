# ==============================================================================
# RQ 3.3 — Energy vs accuracy across generations
#
# DESIGN
#   • Unit of analysis: RUN (already averaged over repetitions).
#   • DV: accuracy (use run-level column `accuracy_mean` if present, else `accuracy`).
#   • Factor: generation (ordered: 2, 3, 3.1, 3.2).
#
# TESTS (nonparametric ONLY; assumptions failed for accuracy)
#   1) Omnibus: Kruskal–Wallis on accuracy ~ generation.
#      - Effect size: epsilon² = (H − k + 1) / (n − k).
#   2) Planned post-hoc (successive, BH-adjusted): Wilcoxon rank-sum for
#      (2→3), (3→3.1), (3.1→3.2).
#      - Effect size: Cliff’s delta, signed so that
#        delta > 0 means the NEWER generation (g2) tends to have HIGHER accuracy (better).
#   3) Correlation check (secondary): Spearman ρ between accuracy and energy within
#      each generation; BH-adjusted across gens.
#
# OUTPUT FILES
#   tables/RQ33_descriptives.csv
#     generation, n_runs, mean/median/sd of accuracy (+ min/max), rho_E_acc (per gen)
#   tables/RQ33_omnibus_kw.csv
#     term, H_stat, df, p_value, epsilon2, note
#   tables/RQ33_wilcoxon_consec.csv
#     contrast, W, p_raw, p_BH, cliffs_delta, direction_note
#   tables/RQ33_spearman_accuracy_energy.csv
#     generation, rho, p, p_BH
#
# PLOTS
#   plots/RQ33_box_accuracy_by_generation.png       (box + jitter of accuracy)
#   plots/RQ33_scatter_accuracy_vs_energy.png       (accuracy vs energy, by gen)
#
# CONSOLE SUMMARY
#   - H, df, p, epsilon².
#   - Strongest successive change with BH p and Cliff’s delta.
#   - Per-gen Spearman with BH.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, purrr, readr
  library(ggplot2)
})

stopifnot(exists("run_level"))
stopifnot("generation" %in% names(run_level), "energy_j_mean" %in% names(run_level))

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

# ---------- 0) Data slice (pick accuracy column and normalize) ----------
acc_col <- dplyr::case_when(
  "accuracy_mean" %in% names(run_level) ~ "accuracy_mean",
  "accuracy" %in% names(run_level)      ~ "accuracy",
  TRUE ~ NA_character_
)
if (is.na(acc_col)) stop("RQ3.3 requires 'accuracy_mean' or 'accuracy' in run_level.")

rq33 <- run_level %>%
  mutate(
    generation   = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    accuracy_val = suppressWarnings(as.numeric(.data[[acc_col]])),
    accuracy_val = ifelse(!is.na(accuracy_val) & accuracy_val > 1, accuracy_val/100, accuracy_val)
  ) %>%
  select(generation, energy_j_mean, accuracy_val) %>%
  drop_na(accuracy_val, energy_j_mean)

# ---------- 1) Descriptives ----------
cat("\n--- RQ3.3 Descriptives (accuracy by generation) ---\n")
desc <- rq33 %>%
  group_by(generation) %>%
  summarise(
    n_runs           = n(),
    mean_accuracy    = mean(accuracy_val),
    median_accuracy  = median(accuracy_val),
    sd_accuracy      = sd(accuracy_val),
    min_accuracy     = min(accuracy_val),
    max_accuracy     = max(accuracy_val),
    rho_E_acc        = suppressWarnings(cor(energy_j_mean, accuracy_val, method = "spearman",
                                            use = "complete.obs")),
    .groups = "drop"
  ) %>% arrange(generation)
print(desc, n = Inf)
readr::write_csv(desc, "tables/RQ33_descriptives.csv")

# ---------- 2) Omnibus: Kruskal–Wallis on accuracy ----------
cat("\n--- Kruskal–Wallis: accuracy ~ generation ---\n")
kw <- kruskal.test(accuracy_val ~ generation, data = rq33)
print(kw)

H <- as.numeric(kw$statistic)
k <- nlevels(rq33$generation)
n <- nrow(rq33)
eps2 <- (H - k + 1) / (n - k)

omnibus_csv <- tibble(
  term     = "generation",
  H_stat   = H,
  df       = k - 1,
  p_value  = kw$p.value,
  epsilon2 = eps2,
  note     = "Kruskal–Wallis on accuracy (run-level)"
)
readr::write_csv(omnibus_csv, "tables/RQ33_omnibus_kw.csv")
cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# ---------- 3) Post-hoc: successive Wilcoxon (BH) ----------
gens  <- levels(rq33$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

one_step <- function(g1, g2) {
  sub <- rq33 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  wt <- wilcox.test(accuracy_val ~ generation, data = sub, exact = FALSE)
  
  x <- sub$accuracy_val[sub$generation == g1]  # older
  y <- sub$accuracy_val[sub$generation == g2]  # newer
  nx <- length(x); ny <- length(y)
  m  <- outer(y, x, "-")
  delta_newer <- (sum(m > 0) - sum(m < 0)) / (nx * ny)  # >0 = newer higher accuracy
  
  tibble(
    contrast      = paste(g2, "-", g1),
    W             = unname(wt$statistic),
    p_raw         = wt$p.value,
    cliffs_delta  = delta_newer
  )
}

wtab <- purrr::map2_dfr(pairs$g1, pairs$g2, one_step) %>%
  mutate(
    p_BH = p.adjust(p_raw, method = "BH"),
    direction_note = "Cliff's delta > 0 means newer gen has higher accuracy (better)"
  )
cat("\n--- Successive Wilcoxon tests on accuracy (BH across 3 steps) ---\n")
print(wtab, n = Inf)
readr::write_csv(wtab, "tables/RQ33_wilcoxon_consec.csv")

# ---------- 4) Spearman: accuracy vs energy within generation (BH) ----------
spearman_by_gen <- rq33 %>%
  group_by(generation) %>%
  group_modify(~{
    ct <- suppressWarnings(cor.test(.x$accuracy_val, .x$energy_j_mean, method = "spearman"))
    tibble(rho = unname(ct$estimate), p = ct$p.value)
  }) %>%
  ungroup() %>%
  mutate(p_BH = p.adjust(p, method = "BH"))
cat("\n--- Spearman correlation: accuracy vs energy, within generation (BH) ---\n")
print(spearman_by_gen, n = Inf)
readr::write_csv(spearman_by_gen, "tables/RQ33_spearman_accuracy_energy.csv")

# ---------- 5) Plots ----------
# 5a) Box + jitter of accuracy by generation
p_box <- ggplot(rq33, aes(generation, accuracy_val, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Accuracy", fill = "Generation") +
  theme_minimal()
ggsave("plots/RQ33_box_accuracy_by_generation.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 5b) Scatter: accuracy vs energy, colored by generation
p_scatter <- ggplot(rq33, aes(accuracy_val, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(x = "Accuracy", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/RQ33_scatter_accuracy_vs_energy.png", p_scatter,
       width = 8, height = 5, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/RQ33_box_accuracy_by_generation.png\n- plots/RQ33_scatter_accuracy_vs_energy.png\n")

# ---------- 6) One-paragraph console summary ----------
cat("\n=== RQ3.3 Summary ===\n")
cat(sprintf("Kruskal–Wallis on accuracy: H = %.3f, df = %d, p = %.4g; epsilon² = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change in accuracy: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer−older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))
cat("Spearman accuracy–energy by gen (BH):\n"); print(spearman_by_gen, n = Inf)
