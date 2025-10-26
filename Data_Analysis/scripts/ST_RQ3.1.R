# ==============================================================================
# RQ 3.1 — Energy per output token (J/token) across generations
#
# DESIGN
#   • Unit of analysis: RUN (already averaged over repetitions).
#   • DV: e_per_tok_mean  (J/token, run-level).
#   • Factors: generation (ordered: 2, 3, 3.1, 3.2).
#
# TESTS (nonparametric ONLY; assumptions were violated previously)
#   1) Omnibus: Kruskal–Wallis on e_per_tok_mean ~ generation.
#      - Effect size: epsilon² = (H − k + 1) / (n − k).
#   2) Planned post-hoc (successive, BH-adjusted): Wilcoxon rank-sum for
#      (2→3), (3→3.1), (3.1→3.2).
#      - Effect size: Cliff’s delta, signed so that
#        delta > 0  means the NEWER generation (g2) tends to have larger J/token.
#
# OUTPUT FILES
#   tables/RQ31_descriptives.csv
#     generation, n_runs, mean_Ept, median_Ept, sd_Ept
#   tables/RQ31_omnibus_kw.csv
#     term, H_stat, df, p_value, epsilon2, note
#   tables/RQ31_wilcoxon_consec.csv
#     contrast, W, p_raw, p_BH, cliffs_delta, direction_note
#
# PLOTS
#   plots/RQ31_box_jtoken_by_generation.png         (box + jitter, J/token)
#   plots/RQ31_scatter_tokens_vs_jtoken.png         (only if tokens_mean present)
#
# CONSOLE SUMMARY
#   - H, df, p, epsilon².
#   - Strongest successive change with BH p and Cliff’s delta.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, purrr, readr
  library(ggplot2)
})

# ---------- 0) Data slice (run-level) ----------
stopifnot(all(c("generation","e_per_tok_mean") %in% names(run_level)))

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

rq31 <- run_level %>%
  select(generation, e_per_tok_mean, tokens_mean) %>%
  drop_na(e_per_tok_mean) %>%
  mutate(generation = factor(generation,
                             levels = c("2","3","3.1","3.2"),
                             ordered = TRUE))

cat("\n--- RQ3.1 Descriptives (J/token by generation) ---\n")
desc <- rq31 %>%
  group_by(generation) %>%
  summarise(
    n_runs     = n(),
    mean_Ept   = mean(e_per_tok_mean),
    median_Ept = median(e_per_tok_mean),
    sd_Ept     = sd(e_per_tok_mean),
    .groups = "drop"
  )
print(desc, n = Inf)
readr::write_csv(desc, "tables/RQ31_descriptives.csv")

# ---------- 1) Omnibus: Kruskal–Wallis ----------
cat("\n--- Kruskal–Wallis: e_per_tok_mean ~ generation ---\n")
kw <- kruskal.test(e_per_tok_mean ~ generation, data = rq31)
print(kw)

H <- as.numeric(kw$statistic)
k <- nlevels(rq31$generation)
n <- nrow(rq31)
eps2 <- (H - k + 1) / (n - k)  # epsilon-squared

omnibus_csv <- tibble(
  term     = "generation",
  H_stat   = H,
  df       = k - 1,
  p_value  = kw$p.value,
  epsilon2 = eps2,
  note     = "Kruskal–Wallis on J/token (run-level)"
)
readr::write_csv(omnibus_csv, "tables/RQ31_omnibus_kw.csv")

cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# ---------- 2) Planned post-hoc: successive Wilcoxon (BH) ----------
gens  <- levels(rq31$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

# Helper: Wilcoxon + Cliff's delta for one adjacent pair
# NOTE: delta > 0 means the NEWER gen (g2) tends to have LARGER J/token.
one_step <- function(g1, g2) {
  sub <- rq31 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  
  wt <- wilcox.test(e_per_tok_mean ~ generation, data = sub, exact = FALSE)
  
  x <- sub$e_per_tok_mean[sub$generation == g1]  # older
  y <- sub$e_per_tok_mean[sub$generation == g2]  # newer
  nx <- length(x); ny <- length(y)
  m  <- outer(y, x, "-")
  delta_newer <- (sum(m > 0) - sum(m < 0)) / (nx * ny)
  
  tibble(
    contrast      = paste(g2, "-", g1),  # newer - older
    W             = unname(wt$statistic),
    p_raw         = wt$p.value,
    cliffs_delta  = delta_newer
  )
}

wtab <- purrr::map2_dfr(pairs$g1, pairs$g2, one_step) %>%
  mutate(
    p_BH = p.adjust(p_raw, method = "BH"),
    direction_note = "Cliff's delta > 0 means newer gen has larger J/token"
  )

cat("\n--- Successive Wilcoxon tests (BH-adjusted across 3 steps) ---\n")
print(wtab, n = Inf)
readr::write_csv(wtab, "tables/RQ31_wilcoxon_consec.csv")

# ---------- 3) Plots ----------
# 3a) Box + jitter
p_box <- ggplot(rq31, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Energy per token (J/token)", fill = "Generation") +
  theme_minimal()
ggsave("plots/RQ31_box_jtoken_by_generation.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 3b) Scatter: tokens vs J/token (if token counts available)
if ("tokens_mean" %in% names(rq31) && any(!is.na(rq31$tokens_mean))) {
  p_scatter <- ggplot(rq31, aes(tokens_mean, e_per_tok_mean, color = generation)) +
    geom_point(alpha = 0.8, size = 2) +
    labs(x = "Output tokens (mean per run)",
         y = "Energy per token (J/token)",
         color = "Generation") +
    theme_minimal()
  ggsave("plots/RQ31_scatter_tokens_vs_jtoken.png", p_scatter,
         width = 8, height = 5, dpi = 150, bg = "white")
}

cat("\nSaved plots:\n- plots/RQ31_box_jtoken_by_generation.png\n")
if (file.exists("plots/RQ31_scatter_tokens_vs_jtoken.png")) {
  cat("- plots/RQ31_scatter_tokens_vs_jtoken.png\n")
}

# ---------- 4) One-paragraph console summary ----------
cat("\n=== RQ3.1 Summary ===\n")
cat(sprintf("Kruskal–Wallis: H = %.3f, df = %d, p = %.4g; epsilon² = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer−older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))

