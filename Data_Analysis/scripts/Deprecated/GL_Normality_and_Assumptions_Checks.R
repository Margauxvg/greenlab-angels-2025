# =====================================================================
# UNIVERSAL ASSUMPTION CHECKER (same logic as before)
# Now also writes one CSV per RQ with rows for raw/log1p/sqrt.
# =====================================================================

library(tidyverse)
library(car)
library(ggplot2)

# ---------- 0) Prep: keep factors/derived DVs the same ----------
run_level <- run_level %>%
  mutate(
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  )

if (!"J_per_B" %in% names(run_level)) {
  run_level <- run_level %>% mutate(J_per_B = energy_j_mean / pmax(model_size_b, 1e-9))
}
if (!"power_w_run" %in% names(run_level)) {
  run_level <- run_level %>% mutate(power_w_run = energy_j_mean / pmax(duration_s_mean, 1e-9))
}
# Ensure accuracy_mean exists, is numeric, and on [0,1]
if ("accuracy_mean" %in% names(run_level)) {
  run_level <- run_level %>%
    mutate(
      accuracy_mean = suppressWarnings(as.numeric(accuracy_mean)),
      accuracy_mean = ifelse(!is.na(accuracy_mean) & accuracy_mean > 1,
                             accuracy_mean/100, accuracy_mean)
    )
}

# ---------- 1) Generic checker function (unchanged logic) ----------
check_assumptions <- function(data, dv, groups, label = "RQ", outdir = "plots/diagnostics") {
  stopifnot(dv %in% names(data))
  stopifnot(all(groups %in% names(data)))
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # subset, drop NAs, coerce groups to factors
  df <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  for (g in groups) df[[g]] <- factor(df[[g]])
  names(df)[names(df) == dv] <- "y"
  
  # create a cell factor for Levene across all combinations
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

# ---------- 2) Helper to write CSV per RQ ----------
if (!dir.exists("tables")) dir.create("tables", recursive = TRUE)

save_assessment <- function(label, res, dv, groups, data, outdir = "tables") {
  df <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  n_total <- nrow(df)
  cell_counts <- df %>%
    mutate(across(all_of(groups), as.factor)) %>%
    count(across(all_of(groups)), name = "n")
  min_cell_n <- if (nrow(cell_counts)) min(cell_counts$n) else NA_integer_
  
  out <- res$summary %>%
    mutate(
      RQ = label,
      dv = dv,
      groups = paste(groups, collapse = " * "),
      n_total = n_total,
      min_cell_n = min_cell_n,
      recommendation = res$recommendation
    ) %>%
    select(RQ, dv, groups, scale, shapiro_p, levene_p, ok, n_total, min_cell_n, recommendation)
  
  readr::write_csv(out, file.path(outdir, paste0(label, "_assumption_summary.csv")))
  cat(sprintf("%s: wrote tables/%s_assumption_summary.csv\n", label, label))
}

# =====================================================================
# RQ-SPECIFIC CALLS (same as before, now with CSV saves)
# =====================================================================

dir.create("plots/diagnostics", showWarnings = FALSE)

# ---------- RQ 1.1 ----------
res_RQ11 <- check_assumptions(
  data   = run_level,
  dv     = "energy_j_mean",
  groups = c("generation"),
  label  = "RQ11"
)
cat("RQ11:", res_RQ11$recommendation, "\n")
save_assessment("RQ11", res_RQ11, "energy_j_mean", c("generation"), run_level)

# ---------- RQ 1.2 ----------
res_RQ12 <- check_assumptions(
  data   = run_level,
  dv     = "J_per_B",
  groups = c("generation","model_size_b"),
  label  = "RQ12"
)
cat("RQ12:", res_RQ12$recommendation, "\n")
save_assessment("RQ12", res_RQ12, "J_per_B", c("generation","model_size_b"), run_level)

# ---------- RQ 2 ----------
res_RQ2 <- check_assumptions(
  data   = repetition_level,
  dv     = "energy_j_mean",
  groups = c("generation","task"),
  label  = "RQ2"
)
cat("RQ2:", res_RQ2$recommendation, "\n")
save_assessment("RQ2", res_RQ2, "energy_j_mean", c("generation","task"), repetition_level)

# ---------- RQ 3.1 ----------
res_RQ31 <- check_assumptions(
  data   = run_level,
  dv     = "e_per_tok_mean",
  groups = c("generation"),
  label  = "RQ31"
)
cat("RQ31:", res_RQ31$recommendation, "\n")
save_assessment("RQ31", res_RQ31, "e_per_tok_mean", c("generation"), run_level)

# ---------- RQ 3.2 ----------
res_RQ32 <- check_assumptions(
  data   = run_level,
  dv     = "power_w_run",
  groups = c("generation"),
  label  = "RQ32"
)
cat("RQ32:", res_RQ32$recommendation, "\n")
save_assessment("RQ32", res_RQ32, "power_w_run", c("generation"), run_level)

# ---------- RQ 3.3 ----------
res_RQ33 <- check_assumptions(
  data   = run_level,
  dv     = "accuracy_mean",
  groups = c("generation"),
  label  = "RQ33"
)
cat("RQ33:", res_RQ33$recommendation, "\n")
save_assessment("RQ33", res_RQ33, "accuracy_mean", c("generation"), run_level)

