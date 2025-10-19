# =====================================================================
# UNIVERSAL ASSUMPTION CHECKER (works for all your RQs)
# - Fits the right linear model for the grouping (e.g., generation, generation*task)
# - Checks residual normality (Shapiro) and variance homogeneity (Levene)
# - Tries log1p and sqrt transforms if raw fails
# - Saves Q–Q plots and returns a tidy summary + a blunt recommendation
# =====================================================================

library(tidyverse)
library(car)
library(ggplot2)

# ---------- 0) Prep: make sure derived DVs exist and factors are sane ----------
# You can keep this as-is unless you rename columns.
run_level <- run_level %>%
  mutate(
    # keep gens ordered for sanity in plots/tests
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  )

# Derived metrics used by some RQs:
if (!"J_per_B" %in% names(run_level)) {
  run_level <- run_level %>% mutate(J_per_B = energy_j_mean / pmax(model_size_b, 1e-9))
}
if (!"power_w_run" %in% names(run_level)) {
  run_level <- run_level %>% mutate(power_w_run = energy_j_mean / pmax(duration_s_mean, 1e-9))
}

# ---------- 1) Generic checker function ----------
# data:     a data.frame (use run_level)
# dv:       string, column name of the dependent variable
# groups:   character vector, grouping columns (1-way or factorial)
# label:    short label for output files (e.g., "RQ11")
# outdir:   where to save Q–Q plots
check_assumptions <- function(data, dv, groups, label = "RQ", outdir = "plots/diagnostics") {
  stopifnot(dv %in% names(data))
  stopifnot(all(groups %in% names(data)))
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # subset, drop NAs, coerce groups to factors
  df <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  for (g in groups) df[[g]] <- factor(df[[g]])
  names(df)[names(df) == dv] <- "y"
  
  # create a cell factor for Levene across all combinations (works for factorial too)
  df$cell <- interaction(df[groups], drop = TRUE)
  
  # build the model formula y ~ g1 * g2 * ...
  rhs <- paste(groups, collapse = " * ")
  fm  <- as.formula(paste("y ~", rhs))
  
  run_one <- function(y_vec, scale_label) {
    tmp <- df; tmp$y <- y_vec
    fit <- lm(fm, data = tmp)
    res <- resid(fit)
    
    p_sh <- if (length(res) >= 3 && length(res) <= 5000)
      suppressWarnings(shapiro.test(res)$p.value) else NA_real_
    p_lv <- tryCatch(car::leveneTest(y ~ cell, data = tmp)[["Pr(>F)"]][1],
                     error = function(e) NA_real_)
    
    # Save Q–Q plot
    qq <- ggplot(data.frame(res = res), aes(sample = res)) +
      stat_qq() + stat_qq_line() +
      labs(title = paste0(label, " Q–Q: ", dv, " (", scale_label, ")"),
           x = "Theoretical", y = "Residuals") +
      theme_minimal()
    ggsave(file.path(outdir, paste0(label, "_QQ_", dv, "_", scale_label, ".png")),
           qq, width = 5, height = 4, dpi = 150, bg = "white")
    
    tibble(scale = scale_label, shapiro_p = p_sh, levene_p = p_lv)
  }
  
  # RAW
  out <- run_one(df$y, "raw")
  
  # LOG1P and SQRT for nonnegative DVs
  if (all(df$y >= 0, na.rm = TRUE)) {
    out <- bind_rows(out, run_one(log1p(df$y), "log1p"))
    out <- bind_rows(out, run_one(sqrt(df$y),  "sqrt"))
  }
  
  out <- out %>% mutate(ok = shapiro_p >= 0.05 & levene_p >= 0.05)
  
  # Recommendation logic (one-way vs factorial)
  pick <- function(sc) out %>% filter(scale == sc) %>% slice(1)
  ng <- length(groups)
  
  rec <- if (ng == 1) {
    if (isTRUE(pick("raw")$ok))        "Use one-way ANOVA on raw"
    else if (isTRUE(pick("log1p")$ok)) "Use one-way ANOVA on log1p"
    else if (isTRUE(pick("sqrt")$ok))  "Use one-way ANOVA on sqrt"
    else if (!is.na(pick("raw")$shapiro_p) && pick("raw")$shapiro_p >= 0.05 &&
             !is.na(pick("raw")$levene_p)  && pick("raw")$levene_p  <  0.05)
      "Use Welch one-way ANOVA on raw (unequal variances)"
    else if (!is.na(pick("log1p")$shapiro_p) && pick("log1p")$shapiro_p >= 0.05 &&
             !is.na(pick("log1p")$levene_p)  && pick("log1p")$levene_p  <  0.05)
      "Use Welch one-way ANOVA on log1p (unequal variances)"
    else "Use Kruskal–Wallis (nonparametric)"
  } else {
    if (isTRUE(pick("raw")$ok))        paste0("Use factorial ANOVA on raw: ", deparse(fm))
    else if (isTRUE(pick("log1p")$ok)) paste0("Use factorial ANOVA on log1p: ", deparse(fm))
    else if (isTRUE(pick("sqrt")$ok))  paste0("Use factorial ANOVA on sqrt: ", deparse(fm))
    else "Use ART (Aligned Rank Transform) for factorials (assumptions unmet)"
  }
  
  list(summary = out, recommendation = rec)
}

# =====================================================================
# RQ-SPECIFIC CALLS
# Only edit inside the ### EDIT HERE lines if your column names differ.
# Each block writes a CSV and prints a one-line recommendation.
# =====================================================================

dir.create("plots/diagnostics", showWarnings = FALSE)

# ---------- RQ 1.1 ----------
# Groups: generation
# DV:     energy (per run)
res_RQ11 <- check_assumptions(
  data   = run_level,
  dv     = "energy_j_mean",     ### EDIT HERE if your energy column name differs
  groups = c("generation"),
  label  = "RQ11"
)
write_csv(res_RQ11$summary, "RQ11_assumption_summary.csv")
cat("RQ11:", res_RQ11$recommendation, "\n")

# ---------- RQ 1.2 ----------
# Groups: generation, model_size_b
# DV:     energy per parameter (Joules per billion params)
res_RQ12 <- check_assumptions(
  data   = run_level,
  dv     = "J_per_B",           ### EDIT HERE if you prefer per-param: energy_j_mean/(model_size_b*1e9)
  groups = c("generation","model_size_b"),
  label  = "RQ12"
)
write_csv(res_RQ12$summary, "RQ12_assumption_summary.csv")
cat("RQ12:", res_RQ12$recommendation, "\n")

# ---------- RQ 2 ----------
# Groups: generation, task
# DV:     energy (per run)
res_RQ2 <- check_assumptions(
  data   = run_level,
  dv     = "energy_j_mean",     ### EDIT HERE if your energy column name differs
  groups = c("generation","task"),
  label  = "RQ2"
)
write_csv(res_RQ2$summary, "RQ2_assumption_summary.csv")
cat("RQ2:", res_RQ2$recommendation, "\n")

# ---------- RQ 3.1 ----------
# Groups: generation
# DV:     energy per output token (your current e_per_tok_mean is total-token based)
res_RQ31 <- check_assumptions(
  data   = run_level,
  dv     = "e_per_tok_mean",    ### EDIT HERE if you later add true output-token metric
  groups = c("generation"),
  label  = "RQ31"
)
write_csv(res_RQ31$summary, "RQ31_assumption_summary.csv")
cat("RQ31:", res_RQ31$recommendation, "\n")

# ---------- RQ 3.2 ----------
# Groups: generation
# DV:     energy per duration = average power (W)
res_RQ32 <- check_assumptions(
  data   = run_level,
  dv     = "power_w_run",       ### EDIT HERE if you computed a different power metric
  groups = c("generation"),
  label  = "RQ32"
)
write_csv(res_RQ32$summary, "RQ32_assumption_summary.csv")
cat("RQ32:", res_RQ32$recommendation, "\n")

# ---------- RQ 3.3 ----------
# Groups: generation
# DV:     accuracy (0–1 or %). If missing, this block will politely yell.
if ("accuracy" %in% names(run_level)) {
  res_RQ33 <- check_assumptions(
    data   = run_level,
    dv     = "accuracy",        ### EDIT HERE only if your accuracy column has another name
    groups = c("generation"),
    label  = "RQ33"
  )
  write_csv(res_RQ33$summary, "RQ33_assumption_summary.csv")
  cat("RQ33:", res_RQ33$recommendation, "\n")
} else {
  message("RQ3.3 skipped: 'accuracy' missing in run_level. Add it, then rerun this block.")
}
