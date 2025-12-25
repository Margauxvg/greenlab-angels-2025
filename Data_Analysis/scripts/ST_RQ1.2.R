# ==============================================================================
# RQ 1.2 — Scaling with parameter size (within generation)
# Unit of analysis: RUN (already averaged over repetitions)
#
# DV (primary):  J_per_B = energy_j_mean / model_size_b   (Joules per billion params)
# Factors:       generation (ordered), size_f (factor from model_size_b)
#
# What this script does (same style as RQ1.1):
#   1) Omnibus two-way ANOVA on RAW J_per_B:
#        - Tests generation, size, and their interaction.
#        - Outputs partial η² and partial ω² per term (computed from SS/MS; no NAs).
#   2) Planned post-hoc (successive) contrasts WITHIN EACH SIZE (BH-adjusted).
#        - 2→3, 3→3.1, 3.1→3.2 for each size level that exists.
#        - Hedges g per step using pooled residual SD and small-sample correction.
#   3) Size contrasts within generation (only for generations with ≥ 2 sizes; BH).
#   4) Optional trend within size (BH) if estimable.
#   5) Plots:
#        - ECDF of J/B by generation, faceted by size.
#        - EMMs mean ± 95% CI for generation × size.
#
# OUTPUT FILES
#   - tables/RQ12_omnibus.csv
#       term, df1, df2, F_value, p_value, eta2_partial, omega2_partial, note
#   - tables/RQ12_consec_by_size.csv
#       size_f, contrast, estimate_JperB, SE, t_value, p_raw, p_BH, hedges_g
#   - tables/RQ12_size_within_gen.csv   (only if any generation has ≥ 2 sizes)
#       generation, contrast, estimate_JperB, SE, t_value, p_raw, p_BH
#   - tables/RQ12_trend_within_size.csv (only if estimable)
#       size_f, component, estimate, SE, t_value, p_raw, p_BH
#   - plots/RQ12_ecdf_JperB_by_size.png
#   - plots/RQ12_emmeans_mean_CI.png
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(emmeans)
  library(effectsize)  # still loaded, but we compute ES manually to avoid NA issues
})

if (!is.character(getOption("na.print", NULL))) options(na.print = "NA")

# ---------- 0) Build the analysis slice ----------
stopifnot(all(c("generation","model_size_b","energy_j_mean") %in% names(run_level)))

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("plots",  showWarnings = FALSE, recursive = TRUE)

rq12 <- run_level %>%
  mutate(
    J_per_B    = if ("J_per_B" %in% names(.)) J_per_B else energy_j_mean / pmax(model_size_b, 1e-9),
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    size_f     = factor(model_size_b, levels = sort(unique(model_size_b)), ordered = TRUE)
  ) %>%
  select(generation, size_f, model_size_b, energy_j_mean, J_per_B) %>%
  drop_na()

trim <- function(x) trimws(as.character(x))

# ---------- 1) Omnibus two-way ANOVA (RAW J/B) ----------
fit12   <- aov(J_per_B ~ generation * size_f, data = rq12)
aov_tab <- summary(fit12)[[1]]
aov_df  <- as.data.frame(aov_tab)
aov_df$term <- trim(rownames(aov_tab))

# Residual components
SS_e  <- aov_df %>% filter(term == "Residuals") %>% pull(`Sum Sq`)
MS_e  <- aov_df %>% filter(term == "Residuals") %>% pull(`Mean Sq`)
df_e  <- aov_df %>% filter(term == "Residuals") %>% pull(Df)

# Function to compute partial eta^2 and partial omega^2 from ANOVA table
compute_es <- function(term_label) {
  row <- aov_df %>% filter(term == term_label)
  if (!nrow(row)) return(c(eta2p = NA_real_, omega2p = NA_real_))
  SS_t <- row$`Sum Sq`[1]
  df_t <- row$Df[1]
  # partial eta^2: SS_term / (SS_term + SS_error)
  eta2p <- if (is.finite(SS_t) && is.finite(SS_e)) SS_t / (SS_t + SS_e) else NA_real_
  # partial omega^2: (SS_term - df_term*MS_error) / (SS_term + SS_error + MS_error)
  omega2p <- if (all(is.finite(c(SS_t, df_t, MS_e, SS_e))))
    (SS_t - df_t * MS_e) / (SS_t + SS_e + MS_e) else NA_real_
  # clip tiny negatives to 0
  omega2p <- ifelse(!is.na(omega2p) & omega2p < 0, 0, omega2p)
  c(eta2p = eta2p, omega2p = omega2p)
}

terms_df <- aov_df %>%
  filter(term != "Residuals") %>%
  transmute(
    term,
    df1     = Df,
    df2     = df_e,
    F_value = `F value`,
    p_value = `Pr(>F)`
  )

es_mat <- t(sapply(terms_df$term, compute_es))
omnibus_csv <- terms_df %>%
  mutate(
    eta2_partial   = es_mat[, "eta2p"],
    omega2_partial = es_mat[, "omega2p"],
    note = "two-way ANOVA on raw J/B"
  )

readr::write_csv(omnibus_csv, "tables/RQ12_omnibus.csv")
cat("\n--- RQ1.2 Omnibus ANOVA (J_per_B ~ generation * size) ---\n")
print(as.data.frame(omnibus_csv), row.names = FALSE)

# ---------- 2) Successive generation contrasts WITHIN EACH SIZE ----------
emm12_g_by_size <- emmeans(fit12, ~ generation | size_f)

succ_none <- contrast(emm12_g_by_size, "consec", adjust = "none") %>% as.data.frame()
succ_none <- succ_none %>% filter(is.finite(estimate), is.finite(SE)) %>%
  group_by(size_f) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()

# Hedges g via pooled residual SD
rmse  <- sigma(fit12)
edf   <- df.residual(fit12)
Jcorr <- 1 - 3/(4*edf - 1)

pairwise_csv <- succ_none %>%
  transmute(
    size_f,
    contrast,
    estimate_JperB = estimate,
    SE,
    t_value = t.ratio,
    p_raw   = p.value,
    p_BH,
    hedges_g = (estimate_JperB / rmse) * Jcorr
  )

readr::write_csv(pairwise_csv, "tables/RQ12_consec_by_size.csv")
cat("\n--- Successive generation contrasts within size (raw & BH) ---\n")
print(as.data.frame(pairwise_csv), row.names = FALSE)

# ---------- 3) Size contrasts WITHIN EACH GENERATION (only where ≥2 sizes) ----------
emm12_size_within_gen <- emmeans(fit12, ~ size_f | generation)

size_pairs <- pairs(emm12_size_within_gen, adjust = "none") %>% as.data.frame()
size_pairs <- size_pairs %>% filter(is.finite(estimate), is.finite(SE)) %>%
  group_by(generation) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()

if (nrow(size_pairs) > 0) {
  out_size <- size_pairs %>%
    transmute(
      generation, contrast,
      estimate_JperB = estimate, SE,
      t_value = t.ratio, p_raw = p.value, p_BH
    )
  readr::write_csv(out_size, "tables/RQ12_size_within_gen.csv")
  cat("\n--- Size contrasts within generation (raw & BH; only gens with ≥2 sizes) ---\n")
  print(as.data.frame(out_size), row.names = FALSE)
} else {
  cat("\n--- Size contrasts within generation ---\nNo generation has ≥2 sizes; skipped.\n")
}

# ---------- 4) Polynomial trend across generations WITHIN EACH SIZE ----------
poly_df <- contrast(emm12_g_by_size, "poly") %>% as.data.frame()
poly_df <- poly_df %>% filter(is.finite(estimate), is.finite(SE)) %>%
  group_by(size_f) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()

if (nrow(poly_df) > 0) {
  trend_csv <- poly_df %>%
    transmute(size_f,
              component = contrast,
              estimate, SE, t_value = t.ratio, p_raw = p.value, p_BH)
  readr::write_csv(trend_csv, "tables/RQ12_trend_within_size.csv")
  cat("\n--- Polynomial trend across generations within each size ---\n")
  print(as.data.frame(trend_csv), row.names = FALSE)
} else {
  cat("\n--- Polynomial trend within size ---\nInsufficient levels per size; skipped.\n")
}

# ---------- 5) Plots ----------
# 5a) ECDF: J/B by generation, faceted by size
p_ecdf <- ggplot(rq12, aes(J_per_B, color = generation)) +
  stat_ecdf(linewidth = 0.9) +
  facet_wrap(~ size_f, scales = "free_x") +
  labs(x = "Energy per billion params (J/B)", y = "Empirical CDF", color = "Generation") +
  theme_minimal()

ggsave("plots/RQ12_ecdf_JperB_by_size.png", p_ecdf,
       width = 10, height = 6, dpi = 150, bg = "white")

# 5b) EMMs mean ± 95% CI for gen × size
emm12_all <- emmeans(fit12, ~ generation * size_f)
emm12_df  <- as.data.frame(emm12_all)

p_mean_ci <- ggplot(emm12_df,
                    aes(x = generation, y = emmean, group = size_f,
                        color = size_f, shape = size_f)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(x = "Generation", y = "Estimated J/B (mean ± 95% CI)",
       color = "Size (B)", shape = "Size (B)") +
  theme_minimal()

ggsave("plots/RQ12_emmeans_mean_CI.png", p_mean_ci,
       width = 9, height = 6, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/RQ12_ecdf_JperB_by_size.png\n- plots/RQ12_emmeans_mean_CI.png\n")

# ---------- 6) Tiny paste-ready summary ----------
cat("\n=== RQ1.2 Summary ===\n")
p_gen  <- omnibus_csv %>% filter(term == "generation") %>% pull(p_value)
p_size <- omnibus_csv %>% filter(term == "size_f")    %>% pull(p_value)
p_int  <- omnibus_csv %>% filter(term == "generation:size_f") %>% pull(p_value)

cat(sprintf("Two-way ANOVA (J/B): Gen p = %.4g, Size p = %.4g, Gen×Size p = %.4g.\n",
            p_gen, p_size, p_int))

if (exists("pairwise_csv") && nrow(pairwise_csv) > 0) {
  best_row <- pairwise_csv %>% arrange(p_BH) %>% slice(1)
  cat(sprintf("Strongest successive gen change (within size %s): %s, Δ = %.2f J/B, p(BH) = %.3g, Hedges g = %.2f.\n",
              as.character(best_row$size_f),
              best_row$contrast, best_row$estimate_JperB, best_row$p_BH, best_row$hedges_g))
} else {
  cat("No estimable successive gen contrasts within any size (likely insufficient levels).\n")
}

cat("\nOmnibus effect sizes (partial):\n")
print(as.data.frame(omnibus_csv %>% select(term, eta2_partial, omega2_partial)), row.names = FALSE)
