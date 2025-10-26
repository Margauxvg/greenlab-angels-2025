library(tidyverse)
library(car)
library(ggplot2)
library(tidyverse)
suppressPackageStartupMessages({
  library(tidyverse)
  library(emmeans)
  library(effectsize)
  library(lmerTest)
  library(ggplot2)
  library(scales)
  library(lme4)
})



#_________________
#SETUP
#_________________

# Create directory structure for plots and tables

# Define base directories
base_dirs <- c("plots", "tables")

# Define RQ subdirectories
rq_dirs <- c("RQ1.1", "RQ1.2", "RQ2", "RQ3.1", "RQ3.2", "RQ3.3")

# Define main categories
categories <- c("Descriptive_Data_Exploration", "Assumptions_Verification", "Statistical_Tests")

# Build subdirectory list
subdirs <- character()
for (cat in categories) {
  subdirs <- c(subdirs, cat)
  for (rq in rq_dirs) {
    subdirs <- c(subdirs, file.path(cat, rq))
  }
}

# Create all directories
for (base in base_dirs) {
  for (sub in subdirs) {
    dir_path <- file.path(base, sub)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }
}

runs <- read_csv("GL_Final_Run_Table_Edited.csv", show_col_types = FALSE)
names(runs)
acc_hit <- "accuracy"
if (length(acc_hit) >= 1) {
  runs$.accuracy_raw <- suppressWarnings(as.numeric(runs[[acc_hit[1]]]))
} else if (all(c("correct","total") %in% names(runs))) {
  runs$.accuracy_raw <- suppressWarnings(as.numeric(runs$correct) / pmax(as.numeric(runs$total), 1))
} else {
  runs$.accuracy_raw <- NA_real_
}
# Clean + derive
df <- runs %>%
  rename(
    duration_s = `time(s)`,
    energy_j   = `energy` 
  ) %>%
  filter(is.na(`__done`) | `__done` == "DONE") %>%
  mutate(
    run        = factor(run),
    repetition = as.integer(repetition),
    generation = factor(generation, ordered = TRUE),
    task       = factor(task),
    
    # keep label and numeric versions of model size
    model_size_label = factor(trimws(model_size),
                              levels = c("1B","3B","7B","8B"), ordered = TRUE),
    model_size_b = readr::parse_number(model_size),
    
    tokens     = as.integer(tokens),
    duration_s = as.numeric(duration_s),
    energy_j   = as.numeric(energy_j),
    
    # Accuracy normalized to [0,1] when possible; leaves NA if not present
    accuracy   = suppressWarnings(as.numeric(.accuracy_raw)),
    accuracy   = ifelse(!is.na(accuracy) & accuracy > 1, accuracy/100, accuracy),
    
    avg_power_w      = energy_j / pmax(duration_s, 1e-9),
    energy_per_token = energy_j / pmax(tokens, 1L)
  )

# ---- Build run-level table (one row per run) ----
run_level <- df %>%
  group_by(run, generation, task, model_size_label, model_size_b) %>%
  summarise(
    n_reps            = n(),
    energy_j_mean     = mean(energy_j, na.rm = TRUE),
    energy_j_sd       = sd(energy_j,   na.rm = TRUE),
    duration_s_mean   = mean(duration_s, na.rm = TRUE),
    tokens_mean       = mean(tokens,   na.rm = TRUE),
    e_per_tok_mean    = mean(energy_per_token, na.rm = TRUE),
    accuracy_mean     = mean(accuracy, na.rm = TRUE),
    accuracy_sd       = sd(accuracy,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    logE = log1p(energy_j_mean),
    power_w_run = energy_j_mean / pmax(duration_s_mean, 1e-9)
  )

# ---- Build repetition-level table (one row per repetition) ----
# Same column names as run_level.
# Only difference: we include 'repetition' and set n_reps = 1, sd = NA.
repetition_level <- df %>%
  mutate(
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  ) %>%
  transmute(
    # keys
    run, repetition, generation, task, model_size_label, model_size_b,
    # "run-level-like" columns, but per repetition
    n_reps            = 1L,
    energy_j_mean     = energy_j,
    energy_j_sd       = NA_real_,
    duration_s_mean   = duration_s,
    tokens_mean       = tokens,
    e_per_tok_mean    = energy_per_token,
    accuracy_mean     = accuracy,
    accuracy_sd       = NA_real_,
    logE              = log1p(energy_j)
  )

# sanity: each run once
stopifnot(!any(duplicated(run_level$run)))

# Sanity checks: one row per run×repetition;
stopifnot(!any(duplicated(repetition_level[, c("run","repetition")])))
stopifnot(all(!is.na(repetition_level$energy_j_mean)))


#____________________________
#DESCRIPTIVE DATA EXPLORATION
#____________________________

# ======================================================================
# RQ1.1 — generation effect (ignore size)
# ======================================================================

# Descriptives (ordered: mean, median, var, sd, min, max) + CSV
rq11_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs        = n(),
    mean_energy   = mean(energy_j_mean, na.rm = TRUE),
    median_energy = median(energy_j_mean, na.rm = TRUE),
    var_energy    = var(energy_j_mean, na.rm = TRUE),
    sd_energy     = sd(energy_j_mean, na.rm = TRUE),
    min_energy    = min(energy_j_mean, na.rm = TRUE),
    max_energy    = max(energy_j_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(generation)
rq11_desc
write_csv(rq11_desc, "tables/Descriptive_Data_Exploration/RQ1.1/rq11_desc.csv")

# Plots
p_box <- ggplot(run_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35) +   # legend shows by default when fill maps to a var
  labs(x = "Generation", y = "Energy per run (J)", fill = "Generation") +
  theme_minimal()

ggsave("plots/Descriptive_Data_Exploration/RQ1.1/rq11_box_energy_by_generation_runs.png",
       p_box, width = 7, height = 5, dpi = 150, bg = "white")

# Density: overlaid by generation (pooled)
p_den <- ggplot(run_level, aes(energy_j_mean, color = generation, fill = generation)) +
  geom_density(alpha = 0.15) +
  labs(x = "Energy per run (J)", y = "Density", 
       color = "Generation", fill = "Generation") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ1.1/rq11_density_energy_by_generation_runs.png", p_den, width = 8, height = 5, dpi = 150, bg = "white")

# ======================================================================
# RQ1.2 — scaling with parameter size (within generation)
# ======================================================================

# Descriptives
run_level <- run_level %>%
  mutate(J_per_B = energy_j_mean / pmax(model_size_b, 1e-9))  # Joules per billion params

# group by generation and size (so 3.2@1B and 3.2@3B are separate)
rq12_desc <- run_level %>%
  group_by(generation, model_size_b) %>%
  summarise(
    n_runs            = n(),
    mean_energy_J     = mean(energy_j_mean, na.rm = TRUE),
    median_energy_J   = median(energy_j_mean, na.rm = TRUE),
    var_energy_J      = var(energy_j_mean, na.rm = TRUE),
    sd_energy_J       = sd(energy_j_mean, na.rm = TRUE),
    min_energy_J      = min(energy_j_mean, na.rm = TRUE),
    max_energy_J      = max(energy_j_mean, na.rm = TRUE),
    mean_J_per_B      = mean(J_per_B, na.rm = TRUE),
    median_J_per_B    = median(J_per_B, na.rm = TRUE),
    var_J_per_B       = var(J_per_B, na.rm = TRUE),
    sd_J_per_B        = sd(J_per_B, na.rm = TRUE),
    min_J_per_B       = min(J_per_B, na.rm = TRUE),
    max_J_per_B       = max(J_per_B, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(generation, model_size_b)
rq12_desc
write_csv(rq12_desc, "tables/Descriptive_Data_Exploration/RQ1.2/rq12_desc.csv")

# Plots
# Boxplot of J per B by generation; fill shows size so 3.2's 1B vs 3B are distinct
p_rq12_box <- ggplot(run_level, aes(generation, J_per_B, fill = factor(model_size_b))) +
  geom_boxplot(outlier.alpha = 0.35) +
  labs(x="Generation", y="Energy per B params (J/B)", fill="Size (B)") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ1.2/rq12_box_JperB_by_gen_runs.png", p_rq12_box, width=8, height=5, dpi=150, bg="white")

# Density of J per B, faceted by generation; color by size (makes 3.2’s two sizes obvious)
p_rq12_den <- ggplot(run_level, aes(J_per_B, color = factor(model_size_b), fill = factor(model_size_b))) +
  geom_density(alpha = 0.15) +
  facet_wrap(~ generation, scales = "free_y") +
  labs(x="Energy per B params (J/B)", y="Density", color="Size (B)", fill="Size (B)") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ1.2/rq12_density_JperB_by_gen_runs.png", p_rq12_den, width=9, height=6, dpi=150, bg="white")

# ======================================================================
# RQ2 — task type × generation
# ======================================================================

# Descriptives (repetition-level, ordered stats) + CSV
rq2_desc <- repetition_level %>%
  group_by(generation, task) %>%
  summarise(
    n_reps      = n(),
    mean_E      = mean(energy_j_mean, na.rm = TRUE),
    median_E    = median(energy_j_mean, na.rm = TRUE),
    var_E       = var(energy_j_mean, na.rm = TRUE),
    sd_E        = sd(energy_j_mean, na.rm = TRUE),
    min_E       = min(energy_j_mean, na.rm = TRUE),
    max_E       = max(energy_j_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(task, generation)

rq2_desc
write_csv(rq2_desc, "tables/Descriptive_Data_Exploration/RQ2/rq2_desc.csv")

# Plots
repetition_level <- repetition_level %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# shared ranges so facets use the SAME scales
x_range <- range(repetition_level$energy_j_mean, na.rm = TRUE)
y_range <- x_range  # scatter uses energy on y

# 1) BOX  energy by task, colored by task
p_box_task <- ggplot(repetition_level, aes(task, energy_j_mean, fill = task)) +
  geom_boxplot(outlier.alpha = 0.35) +
  labs(x = "Task", y = "Energy per run (J)", fill = "Task") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ2/rq2_box_energy_by_task.png", p_box_task,
       width = 9, height = 5, dpi = 150, bg = "white")

# 2) SCATTER energy vs generation, faceted by task, SAME y scale
p_scatter_task_gen <- ggplot(repetition_level, aes(generation, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2, position = position_jitter(width = 0.12, height = 0)) +
  facet_wrap(~ task, scales = "fixed") +
  coord_cartesian(ylim = y_range) +
  labs(x = "Generation", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ2/rq2_scatter_energy_by_generation_facet_task.png", p_scatter_task_gen,
       width = 10, height = 6, dpi = 150, bg = "white")
# 3) VIOLIN energy by generation, faceted by task (repetition-level density)
p_violin_task_gen <- ggplot(repetition_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.6) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9) +
  facet_wrap(~ task, scales = "fixed") +
  labs(x = "Generation", y = "Energy per run (J)", fill = "Generation") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ2/rq2_violin_energy_by_generation_facet_task.png", p_violin_task_gen,
       width = 10, height = 6, dpi = 150, bg = "white")


# ======================================================================
# RQ3.1 — energy cost per token (by generation)
# ======================================================================

# Descriptives 
rq31_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs      = n(),
    mean_Ept    = mean(e_per_tok_mean, na.rm = TRUE),
    median_Ept  = median(e_per_tok_mean, na.rm = TRUE),
    var_Ept     = var(e_per_tok_mean, na.rm = TRUE),
    sd_Ept      = sd(e_per_tok_mean, na.rm = TRUE),
    min_Ept     = min(e_per_tok_mean, na.rm = TRUE),
    max_Ept     = max(e_per_tok_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(generation)

rq31_desc
write_csv(rq31_desc, "tables/Descriptive_Data_Exploration/RQ3.1/rq31_desc.csv")

# Plots
# Boxplot: energy per token by generation WITH legend
p_rq31_box <- ggplot(run_level, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
  labs(x = "Generation", y = "Energy per token (J/token)", fill = "Generation") +
  theme_minimal()

ggsave("plots/Descriptive_Data_Exploration/RQ3.1/rq31_box_Ept_by_generation_runs.png",
       p_rq31_box, width = 7, height = 5, dpi = 150, bg = "white")

# Scatter: tokens vs energy, colored by generation
p_rq31_scatter <- ggplot(run_level, aes(tokens_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean tokens per run", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/Descriptive_Data_Exploration/RQ3.1/rq31_scatter_tokens_vs_energy_by_generation_runs.png",
       p_rq31_scatter, width = 8, height = 5, dpi = 150, bg = "white")

# ======================================================================
# RQ3.2 — inference duration vs energy (by generation)
# ======================================================================

# Descriptives 
rq32_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs              = n(),
    mean_duration_s     = mean(duration_s_mean, na.rm = TRUE),
    median_duration_s   = median(duration_s_mean, na.rm = TRUE),
    var_duration_s      = var(duration_s_mean, na.rm = TRUE),
    sd_duration_s       = sd(duration_s_mean, na.rm = TRUE),
    min_duration_s      = min(duration_s_mean, na.rm = TRUE),
    max_duration_s      = max(duration_s_mean, na.rm = TRUE),
    mean_energy_J       = mean(energy_j_mean, na.rm = TRUE),
    median_energy_J     = median(energy_j_mean, na.rm = TRUE),
    var_energy_J        = var(energy_j_mean, na.rm = TRUE),
    sd_energy_J         = sd(energy_j_mean, na.rm = TRUE),
    min_energy_J        = min(energy_j_mean, na.rm = TRUE),
    max_energy_J        = max(energy_j_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(generation)

rq32_desc
write_csv(rq32_desc, "tables/Descriptive_Data_Exploration/RQ3.2/rq32_desc.csv")

# Plots
# 1) Boxplot: duration by generation (legend included)
p_rq32_box <- ggplot(run_level, aes(generation, duration_s_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
  labs(x = "Generation", y = "Inference duration per run (s)", fill = "Generation") +
  theme_minimal()

ggsave("plots/Descriptive_Data_Exploration/RQ3.2/rq32_box_duration_by_generation_runs.png",
       p_rq32_box, width = 7, height = 5, dpi = 150, bg = "white")

# 2) Scatter: duration vs energy, colored by generation
p_rq32_scatter <- ggplot(run_level, aes(duration_s_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Inference duration per run (s)", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()

ggsave("plots/Descriptive_Data_Exploration/RQ3.2/rq32_scatter_duration_vs_energy_runs.png",
       p_rq32_scatter, width = 8, height = 5, dpi = 150, bg = "white")

# ======================================================================
# RQ3.3 — Energy vs accuracy (run-level)
# ======================================================================

stopifnot(exists("run_level"))

acc_col <- dplyr::case_when(
  "accuracy" %in% names(run_level) ~ "accuracy",
  "accuracy_mean" %in% names(run_level) ~ "accuracy_mean",
  TRUE ~ NA_character_
)

if (is.na(acc_col)) {
  message("RQ3.3 skipped: no 'accuracy' or 'accuracy_mean' column in run_level.")
} else {
  rl_acc <- run_level %>%
    mutate(
      generation   = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
      accuracy_val = suppressWarnings(as.numeric(.data[[acc_col]])),
      # If someone fed percentages, normalize to [0,1]
      accuracy_val = ifelse(!is.na(accuracy_val) & accuracy_val > 1, accuracy_val/100, accuracy_val)
    )
  
  #Descriptives
  rq33_desc <- rl_acc %>%
    group_by(generation) %>%
    summarise(
      n_runs          = n(),
      mean_accuracy   = mean(accuracy_val, na.rm = TRUE),
      median_accuracy = median(accuracy_val, na.rm = TRUE),
      var_accuracy    = var(accuracy_val, na.rm = TRUE),
      sd_accuracy     = sd(accuracy_val, na.rm = TRUE),
      min_accuracy    = min(accuracy_val, na.rm = TRUE),
      max_accuracy    = max(accuracy_val, na.rm = TRUE),
      # quick association per gen at run level
      rho_E_acc       = suppressWarnings(cor(energy_j_mean, accuracy_val, method = "spearman",
                                             use = "complete.obs")),
      .groups = "drop"
    ) %>%
    arrange(generation)
  
  print(rq33_desc, n = Inf)
  readr::write_csv(rq33_desc, "tables/Descriptive_Data_Exploration/RQ3.3/rq33_desc.csv")
  
  #Plots
  # Boxplot: accuracy by generation
  p_rq33_box <- ggplot(rl_acc, aes(generation, accuracy_val, fill = generation)) +
    geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
    labs(x = "Generation", y = "Accuracy", fill = "Generation") +
    theme_minimal()
  ggsave("plots/Descriptive_Data_Exploration/RQ3.3/rq33_box_accuracy_by_generation_runs.png",
         p_rq33_box, width = 7, height = 5, dpi = 150, bg = "white")
  
  # Scatter: accuracy vs energy, colored by generation
  p_rq33_scatter <- ggplot(rl_acc, aes(accuracy_val, energy_j_mean, color = generation)) +
    geom_point(alpha = 0.8, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Accuracy", y = "Energy per run (J)", color = "Generation") +
    theme_minimal()
  ggsave("plots/Descriptive_Data_Exploration/RQ3.3/rq33_scatter_accuracy_vs_energy_runs.png",
         p_rq33_scatter, width = 8, height = 5, dpi = 150, bg = "white")
}





#_____________________________________________
#NORMALITY AND ASSUMPTIONS VERIFICATION
#___________________________________________



# UNIVERSAL ASSUMPTION CHECKER
# Writes one CSV per RQ with rows for raw/log1p/sqrt.


#Generic checker function
check_assumptions <- function(data, dv, groups, label = "RQ", outdir = "plots/Assumptions_Verification") {
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

#Helper to write CSV per RQ
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
  cat(sprintf("%s: wrote %s/%s_assumption_summary.csv\n", label, outdir, label))
}

# RQ-SPECIFIC CALLS

# ---------- RQ 1.1 ----------
res_RQ11 <- check_assumptions(
  data   = run_level,
  dv     = "energy_j_mean",
  groups = c("generation"),
  label  = "RQ11",
  outdir = "plots/Assumptions_Verification/RQ1.1"
)
cat("RQ11:", res_RQ11$recommendation, "\n")
save_assessment("RQ11", res_RQ11, "energy_j_mean", c("generation"), run_level, 
                outdir = "tables/Assumptions_Verification/RQ1.1")

# ---------- RQ 1.2 ----------
res_RQ12 <- check_assumptions(
  data   = run_level,
  dv     = "J_per_B",
  groups = c("generation","model_size_b"),
  label  = "RQ12",
  outdir = "plots/Assumptions_Verification/RQ1.2"
)
cat("RQ12:", res_RQ12$recommendation, "\n")
save_assessment("RQ12", res_RQ12, "J_per_B", c("generation","model_size_b"), run_level,
                outdir = "tables/Assumptions_Verification/RQ1.2")

# ---------- RQ 2 ----------
res_RQ2 <- check_assumptions(
  data   = repetition_level,
  dv     = "energy_j_mean",
  groups = c("generation","task"),
  label  = "RQ2",
  outdir = "plots/Assumptions_Verification/RQ2"
)
cat("RQ2:", res_RQ2$recommendation, "\n")
save_assessment("RQ2", res_RQ2, "energy_j_mean", c("generation","task"), repetition_level,
                outdir = "tables/Assumptions_Verification/RQ2")

# ---------- RQ 3.1 ----------
res_RQ31 <- check_assumptions(
  data   = run_level,
  dv     = "e_per_tok_mean",
  groups = c("generation"),
  label  = "RQ31",
  outdir = "plots/Assumptions_Verification/RQ3.1"
)
cat("RQ31:", res_RQ31$recommendation, "\n")
save_assessment("RQ31", res_RQ31, "e_per_tok_mean", c("generation"), run_level,
                outdir = "tables/Assumptions_Verification/RQ3.1")

# ---------- RQ 3.2 ----------
res_RQ32 <- check_assumptions(
  data   = run_level,
  dv     = "power_w_run",
  groups = c("generation"),
  label  = "RQ32",
  outdir = "plots/Assumptions_Verification/RQ3.2"
)
cat("RQ32:", res_RQ32$recommendation, "\n")
save_assessment("RQ32", res_RQ32, "power_w_run", c("generation"), run_level,
                outdir = "tables/Assumptions_Verification/RQ3.2")

# ---------- RQ 3.3 ----------
res_RQ33 <- check_assumptions(
  data   = run_level,
  dv     = "accuracy_mean",
  groups = c("generation"),
  label  = "RQ33",
  outdir = "plots/Assumptions_Verification/RQ3.3"
)
cat("RQ33:", res_RQ33$recommendation, "\n")
save_assessment("RQ33", res_RQ33, "accuracy_mean", c("generation"), run_level,
                outdir = "tables/Assumptions_Verification/RQ3.3")




#__________________
#STATISTICAL TESTS
#_________________


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
#     one-way ANOVA. 
# INPUTS 
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
# CONSOLE SUMMARY
#   - ANOVA F, p, η², ω².
#   - Strongest successive step with ΔJ, p(BH), Hedges g.
#
# ==============================================================================

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
readr::write_csv(omnibus_csv, "tables/Statistical_Tests/RQ1.1/RQ11_omnibus.csv")

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
readr::write_csv(pairwise_csv, "tables/Statistical_Tests/RQ1.1/RQ11_pairwise_consec.csv")

cat("\n--- Successive pairwise (raw & BH) ---\n"); print(pairwise_csv)

# ---------- 3) Orthogonal trend (linear/quadratic/cubic) ----------
poly_df <- contrast(emm11, "poly") %>% as.data.frame()
poly_df$p_BH <- p.adjust(poly_df$p.value, method = "BH")
trend_csv <- poly_df %>%
  transmute(component = contrast, estimate, SE, t_value = t.ratio, p_raw = p.value, p_BH)
readr::write_csv(trend_csv, "tables/Statistical_Tests/RQ1.1/RQ11_trend_poly.csv")

cat("\n--- Trend components ---\n"); print(trend_csv)

# ---------- 4) Plots ----------
# 4a) ECDF overlay by generation (full distribution comparison)
p_ecdf <- ggplot(rq11, aes(energy_j_mean, color = generation)) +
  stat_ecdf(linewidth = 0.9) +   # ← use linewidth for lines
  labs(x = "Energy per run (J)", y = "Empirical CDF", color = "Generation") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ1.1/RQ11_ecdf_by_generation.png", p_ecdf, width = 8, height = 5, dpi = 150, bg = "white")

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
ggsave("plots/Statistical_Tests/RQ1.1/RQ11_successive_diffs_CI.png", p_diffs, width = 7.5, height = 5, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/Statistical_Tests/RQ1.1/RQ11_ecdf_by_generation.png\n- plots/Statistical_Tests/RQ1.1/RQ11_successive_diffs_CI.png\n")

# ---------- 5) summary ----------
cat("\n=== RQ1.1 Summary ===\n")
cat(sprintf("ANOVA: F(%d,%d) = %.2f, p = %.4g; η² = %.2f, ω² = %.2f.\n",
            omnibus_csv$df1, omnibus_csv$df2, omnibus_csv$F_value,
            omnibus_csv$p_value, omnibus_csv$eta2, omnibus_csv$omega2))

best <- pairwise_csv %>% arrange(p_BH) %>% slice(1)
cat(sprintf("Largest step: %s, Δ = %.0f J, p(BH) = %.4g, Hedges g = %.2f.\n",
            best$contrast, best$estimate_J, best$p_BH, best$hedges_g))

# ==============================================================================
# RQ 1.2 — Scaling with parameter size (within generation)
# Unit of analysis: RUN (already averaged over repetitions)
#
# DV (primary):  J_per_B = energy_j_mean / model_size_b   (Joules per billion params)
# Factors:       generation (ordered), size_f (factor from model_size_b)
#
# What this script does:
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
# ==============================================================================

if (!is.character(getOption("na.print", NULL))) options(na.print = "NA")

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

readr::write_csv(omnibus_csv, "tables/Statistical_Tests/RQ1.2/RQ12_omnibus.csv")
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

readr::write_csv(pairwise_csv, "tables/Statistical_Tests/RQ1.2/RQ12_consec_by_size.csv")
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
  readr::write_csv(out_size, "tables/Statistical_Tests/RQ1.2/RQ12_size_within_gen.csv")
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
  readr::write_csv(trend_csv, "tables/Statistical_Tests/RQ1.2/RQ12_trend_within_size.csv")
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

ggsave("plots/Statistical_Tests/RQ1.2/RQ12_ecdf_JperB_by_size.png", p_ecdf,
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

ggsave("plots/Statistical_Tests/RQ1.2/RQ12_emmeans_mean_CI.png", p_mean_ci,
       width = 9, height = 6, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/Statistical_Tests/RQ1.2/RQ12_ecdf_JperB_by_size.png\n- plots/Statistical_Tests/RQ1.2/RQ12_emmeans_mean_CI.png\n")

# ---------- 6) summary ----------
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

# ==============================================================================
# RQ2 — Generation × Task on energy (repetition-level, mixed model)
#
# DESIGN
#   • Unit of analysis: repetition (NOT aggregated by run).
#   • DV: log1p(energy_j)  (we model log(1+J) to stabilize residuals).
#   • Fixed effects: generation * task  (tests main effects + interaction).
#   • Random effects: (1 | run)  (random intercept for each run to avoid pseudoreplication).
#
# WHAT THIS SCRIPT DOES
#   1) Fit the linear mixed-effects model: lmer(log1p(energy_j) ~ generation * task + (1 | run))
#   2) Type III ANOVA for fixed effects (from lmerTest).
#   3) Post-hoc, per task: consecutive generation contrasts (2→3, 3→3.1, 3.1→3.2)
#      with BH correction. Also saves raw p and BH p to CSV for the checklist.
#   4) If interaction is not significant, also report main-effect generation
#      contrasts collapsed across tasks.
#   5) Back-transform EMMs to Joules for human-readable tables (per task and main effect).
#   6) Diagnostics and variance components (sigma_run, sigma_resid, ICC).
# DECISIONS 
#   • Two-sided tests, α = 0.05.
#   • Post-hoc p-values BH-adjusted. CSVs include both raw and BH p for auditability.
# ==============================================================================

rep_df <- df %>%
  select(run, generation, task, energy_j, duration_s, tokens) %>%
  drop_na() %>%
  mutate(
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    task       = factor(task),
    run        = factor(run)
  )

# quick check of replication
cat("\n--- RQ2: counts per generation × task (repetition-level) ---\n")
rep_df %>% count(generation, task) %>% arrange(task, generation) %>% print(n = Inf)


# --- model fit ----------------------------------------------------
fit_RQ2 <- lmer(log1p(energy_j) ~ generation * task + (1 | run), data = rep_df)
options(emmeans = list(msg.note = FALSE))

# --- diagnostics & variance components ----------------------------------------
v  <- as.data.frame(VarCorr(fit_RQ2))
sigma_run <- v$vcov[v$grp == "run"]
sigma_res <- v$vcov[v$grp == "Residual"]
ICC <- sigma_run / (sigma_run + sigma_res)

diag_tbl <- tibble(
  sigma_run = sigma_run,
  sigma_residual = sigma_res,
  ICC = ICC,
  model_singular = isSingular(fit_RQ2),
  convergence = is.null(summary(fit_RQ2)$optinfo$conv$lme4$messages)
)
readr::write_csv(diag_tbl, "tables/Statistical_Tests/RQ2/RQ2_variance_components.csv")
cat(sprintf("\nRandom-intercept variance (run)=%.4f; residual=%.4f; ICC=%.3f; singular=%s; converged=%s\n",
            sigma_run, sigma_res, ICC,
            diag_tbl$model_singular, diag_tbl$convergence))

# Save residual Q-Q and resid-vs-fitted plots
res_df <- tibble(fitted = fitted(fit_RQ2), resid = resid(fit_RQ2))
p_qq <- ggplot(res_df, aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  labs(title = "RQ2 residuals Q–Q (lmer on log1p energy)",
       x = "Theoretical quantiles", y = "Residuals") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ2/RQ2_diag_qq.png", p_qq, width = 5.5, height = 4.5, dpi = 150, bg = "white")

p_rvf <- ggplot(res_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.6, size = 1) +
  labs(title = "RQ2 residuals vs fitted",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ2/RQ2_diag_resid_fitted.png", p_rvf, width = 5.5, height = 4.5, dpi = 150, bg = "white")

# --- Type III ANOVA ----------------------------------------------
cat("\n=== MAIN ANALYSIS: Type III ANOVA ===\n")
anova_results <- anova(fit_RQ2, type = 3)
print(anova_results)

# write ANOVA to CSV
anova_table <- data.frame(
  term    = rownames(anova_results),
  Sum_Sq  = anova_results$`Sum Sq`,
  Mean_Sq = anova_results$`Mean Sq`,
  NumDF   = anova_results$NumDF,
  DenDF   = anova_results$DenDF,
  F_value = anova_results$`F value`,
  p_value = anova_results$`Pr(>F)`
)
readr::write_csv(anova_table, "tables/Statistical_Tests/RQ2/RQ2_mixed_TypeIII.csv")

# p-values for branching
gen_p <- anova_results["generation", "Pr(>F)"]
task_p <- anova_results["task", "Pr(>F)"]
int_p <- anova_results["generation:task", "Pr(>F)"]

cat("\nSummary:\n")
cat("Generation main effect:", ifelse(gen_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", gen_p))
cat("Task main effect:", ifelse(task_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", task_p))
cat("Generation×Task interaction:", ifelse(int_p < 0.05, "SIGNIFICANT", "not significant"),
    sprintf("(p = %.4f)\n", int_p))

# --- Post-hoc within task  ---------------------
emm_within_task <- emmeans(fit_RQ2, ~ generation | task)

# Link scale: get raw and BH p into one CSV for audit
con_link_raw <- contrast(emm_within_task, method = "consec", adjust = "none") %>% as.data.frame()
con_link <- con_link_raw %>%
  group_by(task) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()
readr::write_csv(con_link %>%
                   select(task, contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                 "tables/Statistical_Tests/RQ2/RQ2_consec_within_task_LINK.csv")

# Response scale (Joules): differences and BH p
emm_within_task_resp <- regrid(emm_within_task)  # back-transform to J
con_resp_raw <- contrast(emm_within_task_resp, "consec", adjust = "none") %>% as.data.frame()
con_resp <- con_resp_raw %>%
  group_by(task) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup()
readr::write_csv(con_resp %>%
                   select(task, contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                 "tables/Statistical_Tests/RQ2/RQ2_consec_within_task_JOULES.csv")

cat("\n=== POST-HOC: Consecutive generation comparisons within tasks (BH) ===\n")
# Print the BH-adjusted link-scale results 
print(contrast(emm_within_task, method = "consec", adjust = "BH"))

# --- If interaction is NOT significant, also show main-generation contrasts ----
if (!is.na(int_p) && int_p > 0.05) {
  cat("\n=== Since interaction NS, also showing main generation comparisons ===\n")
  emm_gen_main <- emmeans(fit_RQ2, ~ generation)
  # Print BH-adjusted contrasts (unchanged from your logic)
  print(contrast(emm_gen_main, method = "consec", adjust = "BH"))
  # Also save on response scale with raw and BH p
  emm_gen_resp <- regrid(emm_gen_main)
  con_main_raw <- contrast(emm_gen_resp, "consec", adjust = "none") %>% as.data.frame()
  con_main <- con_main_raw %>%
    mutate(p_BH = p.adjust(p.value, method = "BH"))
  readr::write_csv(con_main %>%
                     select(contrast, estimate, SE, df, t.ratio, p_raw = p.value, p_BH),
                   "tables/Statistical_Tests/RQ2/RQ2_consec_generation_main_JOULES.csv")
}

# --- Back-transformed EMMs for reporting  ---------------
bt_task <- summary(emmeans(fit_RQ2, ~ generation | task), type = "response")
readr::write_csv(as.data.frame(bt_task), "tables/Statistical_Tests/RQ2/RQ2_emm_by_task_joules.csv")

emm_gen <- emmeans(fit_RQ2, ~ generation)
emm_gen_joules <- summary(emm_gen, type = "response")
readr::write_csv(as.data.frame(emm_gen_joules), "tables/Statistical_Tests/RQ2/RQ2_emm_generation_joules.csv")

# "mean ± SE" and 95% CI table (J)
summary_table <- emm_gen_joules %>%
  as.data.frame() %>%
  mutate(
    Energy_J = sprintf("%.0f ± %.0f", response, SE),
    CI_95 = sprintf("[%.0f, %.0f]", lower.CL, upper.CL)
  ) %>%
  select(Generation = generation, Energy_J, CI_95)
readr::write_csv(summary_table, "tables/Statistical_Tests/RQ2/RQ2_summary_table.csv")
print(summary_table)

# --- Plots  -------------------------------------------------------
# Main: generation EMMs (Joules)
p_main <- ggplot(emm_gen_joules, aes(x = generation, y = response, group = 1)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1, linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(x = "Llama Generation",
       y = "Energy Consumption (J)",
       title = "RQ2: Energy Consumption Across Generations",
       subtitle = "Estimated marginal means averaged across tasks") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank())
ggsave("plots/Statistical_Tests/RQ2/RQ2_energy_by_generation.png", p_main, width = 7, height = 5, dpi = 300, bg = "white")

# Faceted by task (Joules)
bt_task_df <- as.data.frame(bt_task)
p_facet <- ggplot(bt_task_df, aes(x = generation, y = response, group = task)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, alpha = 0.5) +
  facet_wrap(~ task, nrow = 2) +
  scale_y_continuous(labels = comma) +
  labs(x = "Generation", y = "Energy (J)",
       title = "Energy Consumption by Task and Generation",
       subtitle = if (!is.na(int_p) && int_p > 0.05)
         "Interaction not significant: pattern consistent across tasks"
       else "Per-task EMMs") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/Statistical_Tests/RQ2/RQ2_energy_by_task_faceted.png", p_facet, width = 10, height = 6, dpi = 300, bg = "white")

# --- summary ----------------------------------------------------
cat("\n=== KEY RESULTS FOR MANUSCRIPT (Original Scale) ===\n")
cat("\nEnergy consumption by generation (Joules):\n")
print(summary_table)

cat("\nPercent reduction from baseline (Gen 2):\n")
baseline <- emm_gen_joules$response[1]
for (i in 2:nrow(emm_gen_joules)) {
  gen <- emm_gen_joules$generation[i]
  val <- emm_gen_joules$response[i]
  pct <- (1 - val / baseline) * 100
  cat(sprintf("  Gen %s: %.1f%% reduction\n", gen, pct))
}

# If we created the main-effect response-scale contrasts, show them
if (file.exists("tables/Statistical_Tests/RQ2/RQ2_consec_generation_main_JOULES.csv")) {
  cat("\nConsecutive generation comparisons (Joules difference, BH):\n")
  con_main_print <- readr::read_csv("tables/Statistical_Tests/RQ2/RQ2_consec_generation_main_JOULES.csv", show_col_types = FALSE)
  print(con_main_print)
}


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
#      - Effect size: Cliff's delta, signed so that
#        delta > 0  means the NEWER generation (g2) tends to have larger J/token.
#
# CONSOLE SUMMARY
#   - H, df, p, epsilon².
#   - Strongest successive change with BH p and Cliff's delta.
# ==============================================================================


stopifnot(all(c("generation","e_per_tok_mean") %in% names(run_level)))

rq31 <- run_level %>%
  select(generation, e_per_tok_mean, tokens_mean) %>%
  drop_na(e_per_tok_mean) %>%
  mutate(generation = factor(generation,
                             levels = c("2","3","3.1","3.2"),
                             ordered = TRUE))

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
readr::write_csv(omnibus_csv, "tables/Statistical_Tests/RQ3.1/RQ31_omnibus_kw.csv")

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
readr::write_csv(wtab, "tables/Statistical_Tests/RQ3.1/RQ31_wilcoxon_consec.csv")

# ---------- 3) Plots ----------
# 3a) Box + jitter
p_box <- ggplot(rq31, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Energy per token (J/token)", fill = "Generation") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ3.1/RQ31_box_jtoken_by_generation.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 3b) Scatter: tokens vs J/token (if token counts available)
if ("tokens_mean" %in% names(rq31) && any(!is.na(rq31$tokens_mean))) {
  p_scatter <- ggplot(rq31, aes(tokens_mean, e_per_tok_mean, color = generation)) +
    geom_point(alpha = 0.8, size = 2) +
    labs(x = "Output tokens (mean per run)",
         y = "Energy per token (J/token)",
         color = "Generation") +
    theme_minimal()
  ggsave("plots/Statistical_Tests/RQ3.1/RQ31_scatter_tokens_vs_jtoken.png", p_scatter,
         width = 8, height = 5, dpi = 150, bg = "white")
}

cat("\nSaved plots:\n- plots/Statistical_Tests/RQ3.1/RQ31_box_jtoken_by_generation.png\n")
if (file.exists("plots/Statistical_Tests/RQ3.1/RQ31_scatter_tokens_vs_jtoken.png")) {
  cat("- plots/Statistical_Tests/RQ3.1/RQ31_scatter_tokens_vs_jtoken.png\n")
}

# ---------- 4) summary ----------
cat("\n=== RQ3.1 Summary ===\n")
cat(sprintf("Kruskal–Wallis: H = %.3f, df = %d, p = %.4g; epsilon² = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer−older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))


# ==============================================================================
# RQ 3.2 — Inference duration and energy consumption
#
# DESIGN
#   • Unit of analysis: RUN (already averaged over repetitions).
#   • DV: average power per run (W) = energy_j_mean / duration_s_mean.
#   • Factor: generation (ordered: 2, 3, 3.1, 3.2). Size ignored here.
#
# TESTS (nonparametric ONLY; you said assumptions failed)
#   1) Omnibus: Kruskal–Wallis on power_w_run ~ generation.
#      - Effect size: epsilon² = (H − k + 1) / (n − k).
#   2) Planned post-hoc (successive, BH-adjusted): Wilcoxon rank-sum for
#      (2→3), (3→3.1), (3.1→3.2).
#      - Effect size: Cliff's delta, signed so that
#        delta > 0 means the NEWER generation (g2) tends to have HIGHER power (worse).
#   3) Correlation check (secondary): Spearman ρ between duration and energy within
#      each generation; BH-adjusted across gens.
#
#  SUMMARY
#   - H, df, p, epsilon².
#   - Strongest successive change with BH p and Cliff's delta.
#   - Per-gen Spearman with BH.
# ==============================================================================

stopifnot(all(c("generation","energy_j_mean","duration_s_mean") %in% names(run_level)))

rq32 <- run_level %>%
  mutate(
    power_w_run = if (!"power_w_run" %in% names(.))
      energy_j_mean / pmax(duration_s_mean, 1e-9) else power_w_run,
    generation  = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  ) %>%
  select(generation, energy_j_mean, duration_s_mean, power_w_run) %>%
  drop_na(power_w_run, energy_j_mean, duration_s_mean)

# ---------- 1) Descriptives ----------
cat("\n--- RQ3.2 Descriptives (by generation) ---\n")
desc <- rq32 %>%
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
  ) %>% arrange(generation)
print(desc, n = Inf)
readr::write_csv(desc, "tables/Statistical_Tests/RQ3.2/RQ32_descriptives.csv")

# ---------- 2) Omnibus: Kruskal–Wallis on power ----------
cat("\n--- Kruskal–Wallis: power_w_run ~ generation ---\n")
kw <- kruskal.test(power_w_run ~ generation, data = rq32)
print(kw)

H <- as.numeric(kw$statistic)
k <- nlevels(rq32$generation)
n <- nrow(rq32)
eps2 <- (H - k + 1) / (n - k)

omnibus_csv <- tibble(
  term     = "generation",
  H_stat   = H,
  df       = k - 1,
  p_value  = kw$p.value,
  epsilon2 = eps2,
  note     = "Kruskal–Wallis on average power (run-level)"
)
readr::write_csv(omnibus_csv, "tables/Statistical_Tests/RQ3.2/RQ32_omnibus_kw.csv")
cat(sprintf("Epsilon-squared (omnibus): %.3f\n", eps2))

# ---------- 3) Post-hoc: successive Wilcoxon (BH) ----------
gens  <- levels(rq32$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

one_step <- function(g1, g2) {
  sub <- rq32 %>%
    filter(generation %in% c(g1, g2)) %>%
    mutate(generation = droplevels(generation))
  wt <- wilcox.test(power_w_run ~ generation, data = sub, exact = FALSE)
  
  x <- sub$power_w_run[sub$generation == g1]  # older
  y <- sub$power_w_run[sub$generation == g2]  # newer
  nx <- length(x); ny <- length(y)
  m  <- outer(y, x, "-")
  delta_newer <- (sum(m > 0) - sum(m < 0)) / (nx * ny)  # >0 = newer higher power
  
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
    direction_note = "Cliff's delta > 0 means newer gen has higher power (worse)"
  )
cat("\n--- Successive Wilcoxon tests on power (BH across 3 steps) ---\n")
print(wtab, n = Inf)
readr::write_csv(wtab, "tables/Statistical_Tests/RQ3.2/RQ32_wilcoxon_consec.csv")

# ---------- 4) Spearman: duration vs energy within generation (BH) ----------
spearman_by_gen <- rq32 %>%
  group_by(generation) %>%
  group_modify(~{
    ct <- suppressWarnings(cor.test(.x$duration_s_mean, .x$energy_j_mean,
                                    method = "spearman"))
    tibble(rho = unname(ct$estimate), p = ct$p.value)
  }) %>%
  ungroup() %>%
  mutate(p_BH = p.adjust(p, method = "BH"))
cat("\n--- Spearman correlation: duration vs energy, within generation (BH) ---\n")
print(spearman_by_gen, n = Inf)
readr::write_csv(spearman_by_gen, "tables/Statistical_Tests/RQ3.2/RQ32_spearman_duration_energy.csv")

# ---------- 5) Plots ----------
# 5a) Box + jitter of average power by generation
p_box <- ggplot(rq32, aes(generation, power_w_run, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Average power per run (W)", fill = "Generation") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ3.2/RQ32_box_power_by_generation.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 5b) Scatter: duration vs energy, colored by generation
p_scatter <- ggplot(rq32, aes(duration_s_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(x = "Inference duration (s)", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ3.2/RQ32_scatter_duration_vs_energy.png", p_scatter,
       width = 8, height = 5, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/Statistical_Tests/RQ3.2/RQ32_box_power_by_generation.png\n- plots/Statistical_Tests/RQ3.2/RQ32_scatter_duration_vs_energy.png\n")

# ---------- 6) summary ----------
cat("\n=== RQ3.2 Summary ===\n")
cat(sprintf("Kruskal–Wallis on power: H = %.3f, df = %d, p = %.4g; epsilon² = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change in power: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer−older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))
cat("Spearman duration–energy by gen (BH):\n"); print(spearman_by_gen, n = Inf)


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
#      - Effect size: Cliff's delta, signed so that
#        delta > 0 means the NEWER generation (g2) tends to have HIGHER accuracy (better).
#   3) Correlation check (secondary): Spearman ρ between accuracy and energy within
#      each generation; BH-adjusted across gens.
# SUMMARY
#   - H, df, p, epsilon².
#   - Strongest successive change with BH p and Cliff's delta.
#   - Per-gen Spearman with BH.
# ==============================================================================
stopifnot(exists("run_level"))
stopifnot("generation" %in% names(run_level), "energy_j_mean" %in% names(run_level))

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
readr::write_csv(desc, "tables/Statistical_Tests/RQ3.3/RQ33_descriptives.csv")

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
readr::write_csv(omnibus_csv, "tables/Statistical_Tests/RQ3.3/RQ33_omnibus_kw.csv")
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
readr::write_csv(wtab, "tables/Statistical_Tests/RQ3.3/RQ33_wilcoxon_consec.csv")

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
readr::write_csv(spearman_by_gen, "tables/Statistical_Tests/RQ3.3/RQ33_spearman_accuracy_energy.csv")

# ---------- 5) Plots ----------
# 5a) Box + jitter of accuracy by generation
p_box <- ggplot(rq33, aes(generation, accuracy_val, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.55, size = 1) +
  labs(x = "Generation", y = "Accuracy", fill = "Generation") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ3.3/RQ33_box_accuracy_by_generation.png", p_box,
       width = 8, height = 5, dpi = 150, bg = "white")

# 5b) Scatter: accuracy vs energy, colored by generation
p_scatter <- ggplot(rq33, aes(accuracy_val, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(x = "Accuracy", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/Statistical_Tests/RQ3.3/RQ33_scatter_accuracy_vs_energy.png", p_scatter,
       width = 8, height = 5, dpi = 150, bg = "white")

cat("\nSaved plots:\n- plots/Statistical_Tests/RQ3.3/RQ33_box_accuracy_by_generation.png\n- plots/Statistical_Tests/RQ3.3/RQ33_scatter_accuracy_vs_energy.png\n")

# ---------- 6) One-paragraph console summary ----------
cat("\n=== RQ3.3 Summary ===\n")
cat(sprintf("Kruskal–Wallis on accuracy: H = %.3f, df = %d, p = %.4g; epsilon² = %.3f.\n",
            H, k - 1, kw$p.value, eps2))
best <- wtab[order(wtab$p_BH), ][1, ]
cat(sprintf("Strongest successive change in accuracy: %s, Wilcoxon p(BH) = %.3g, Cliff's delta (newer−older) = %.2f.\n",
            best$contrast, best$p_BH, best$cliffs_delta))
cat("Spearman accuracy–energy by gen (BH):\n"); print(spearman_by_gen, n = Inf)

