# ==============================================================
# RQ 3.1 — Energy per output token (J/token) across generations
# Assumptions were violated earlier → NONPARAMETRIC ONLY.
# Omnibus: Kruskal–Wallis
# Post-hoc: successive Wilcoxon (2→3, 3→3.1, 3.1→3.2), BH-adjusted
# Effect sizes: epsilon^2 (omnibus), Cliff's delta (pairwise)
# Plots: box+jitter; scatter tokens vs J/token
# ==============================================================

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, purrr
  library(ggplot2)
})

# 0) Data slice (run-level) ----------------------------------------------------
# Expect in run_level: generation, e_per_tok_mean (J/token), tokens_mean (optional)
stopifnot(all(c("generation","e_per_tok_mean") %in% names(run_level)))

rq31 <- run_level %>%
  select(generation, e_per_tok_mean, tokens_mean) %>%
  drop_na(e_per_tok_mean) %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"),
                             ordered = TRUE))

cat("\n--- RQ3.1 Descriptives (J/token by generation) ---\n")
rq31 %>%
  group_by(generation) %>%
  summarise(
    n_runs     = n(),
    mean_Ept   = mean(e_per_tok_mean),
    median_Ept = median(e_per_tok_mean),
    sd_Ept     = sd(e_per_tok_mean),
    .groups = "drop"
  ) %>% print(n = Inf)

# 1) Omnibus: Kruskal–Wallis ---------------------------------------------------
cat("\n--- Kruskal–Wallis: e_per_tok_mean ~ generation ---\n")
kw <- kruskal.test(e_per_tok_mean ~ generation, data = rq31)
print(kw)

# Omnibus effect size: epsilon^2 = (H - k + 1) / (n - k)
H <- as.numeric(kw$statistic)
k <- nlevels(rq31$generation)
n <- nrow(rq31)
eps2 <- (H - k + 1) / (n - k)
cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# 2) Planned post-hoc: successive Wilcoxon with BH correction ------------------
# Adjacent generations only: (2 vs 3), (3 vs 3.1), (3.1 vs 3.2)
gens  <- levels(rq31$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

# Helper: Wilcoxon + Cliff's delta for one adjacent pair
# NOTE on delta: we compute it so that **positive delta means the NEWER gen (g2) tends to be larger**.
one_step <- function(g1, g2) {
  sub <- rq31 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  
  wt <- wilcox.test(e_per_tok_mean ~ generation, data = sub, exact = FALSE)
  
  # Cliff's delta (direction set to "newer minus older"):
  x <- sub$e_per_tok_mean[sub$generation == g1]  # older
  y <- sub$e_per_tok_mean[sub$generation == g2]  # newer
  # delta_newer = P(y>x) - P(y<x)
  nx <- length(x); ny <- length(y)
  m <- outer(y, x, "-")
  delta_newer <- (sum(m > 0) - sum(m < 0)) / (nx * ny)
  
  tibble(
    contrast      = paste(g2, "-", g1),   # newer - older (by label)
    W             = unname(wt$statistic),
    p_raw         = wt$p.value,
    cliffs_delta  = delta_newer           # >0 means newer has larger J/token
  )
}

wtab <- purrr::map2_dfr(pairs$g1, pairs$g2, one_step) %>%
  mutate(p_BH = p.adjust(p_raw, method = "BH"))

cat("\n--- Successive Wilcoxon tests (BH-adjusted across 3 steps) ---\n")
print(wtab)

# 3) Plots ---------------------------------------------------------------------

# Box + jitter: distribution of J/token by generation
p_box <- ggplot(rq31, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Energy per token (J/token)", fill = "Generation") +
  theme_minimal()
print(p_box)

# Scatter: token length vs J/token, colored by generation (helps spot dilution/overhead effects)
if ("tokens_mean" %in% names(rq31) && any(!is.na(rq31$tokens_mean))) {
  p_scatter <- ggplot(rq31, aes(tokens_mean, e_per_tok_mean, color = generation)) +
    geom_point(alpha = 0.8, size = 2) +
    labs(x = "Output tokens (mean per run)", y = "Energy per token (J/token)",
         color = "Generation") +
    theme_minimal()
  print(p_scatter)
}

# 4) One-paragraph console summary --------------------------------------------
cat("\n=== RQ3.1 Summary ===\n")
cat(sprintf("Kruskal–Wallis: H = %.3f, df = %d, p = %.4g; epsilon^2 = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer-older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))

