suppressPackageStartupMessages({
  library(tidyverse)
  library(car)
  library(FSA)
  library(emmeans)
  library(ARTool)
  library(effsize)
  library(lmerTest)
  library(scales)
  library(lme4)
  library(effectsize)
})

# --- GLOBAL SETTINGS ---

# Create directory structure for plots and tables
base_dirs <- c("plots", "tables")
rq_dirs <- c("RQ1.1", "RQ1.2", "RQ2", "RQ3.1", "RQ3.2", "RQ3.3")
categories <- c("Descriptive_Data_Exploration", "Assumptions_Verification", "Statistical_Tests")
subdirs <- character()
for (cat in categories) {
  subdirs <- c(subdirs, cat)
  for (rq in rq_dirs) {
    subdirs <- c(subdirs, file.path(cat, rq))
  }
}

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
repetition_level <- df %>%
  mutate(
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE)
  ) %>%
  transmute(
    # Keys
    run, repetition, generation, task, model_size_label, model_size_b,
    
    # Define DVs here so they are available for all subsequent tests
    accuracy_val      = ifelse(accuracy > 1, accuracy / 100, accuracy),
    power_w           = energy_j / pmax(duration_s, 1e-9),
    
    # Descriptive columns
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

# Ensures legends are at the bottom and outputs are sharp PDFs
theme_set(theme_minimal() + theme(legend.position = "bottom"))

# Helper function to save files in a standardized way
save_rq_plot <- function(plot, context, rq, description) {
  # Format: [rq number]_[Informative Plot Caption].pdf
  filename <- paste0(rq, "_", gsub(" ", "_", description), ".pdf")
  filepath <- file.path("plots", context, rq, filename)
  
  if (!dir.exists(dirname(filepath))) dir.create(dirname(filepath), recursive = TRUE)
  
  ggsave(filepath, plot, device = "pdf", width = 8, height = 6)
}

save_rq_table <- function(table, context, rq, description) {
 
  # Format: [rq number]_[Informative Plot Caption].pdf
  filename <- paste0(rq, "_", gsub(" ", "_", description), ".csv")
  filepath <- file.path("tables", context, rq, filename)
  
  if (!dir.exists(dirname(filepath))) dir.create(dirname(filepath), recursive = TRUE)
  
  # Process the table: Round all numeric values to 3 decimal places
  table_clean <- table %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  write_csv(table_clean, filepath)
}

# sanity: each run once
stopifnot(!any(duplicated(run_level$run)))

# Sanity checks: one row per run×repetition;
stopifnot(!any(duplicated(repetition_level[, c("run","repetition")])))
stopifnot(all(!is.na(repetition_level$energy_j_mean)))


# --- DESCRIPTIVE DATA EXPLORATION ---

# ======================================================================
# RQ1.1 — GENERATION
# ======================================================================

# 1. Tables
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

save_rq_table(rq11_desc, "Descriptive_Data_Exploration", "RQ1.1", "Descriptive_Summary")

# 2. Plots
p_rq11_box <- ggplot(run_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", size = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Llama Generation", y = "Total Energy Consumption (J)", fill = "Generation")

save_rq_plot(p_rq11_box, "Descriptive_Data_Exploration", "RQ1.1", "Energy_Consumption_Distribution_Across_Generations_with_Trendline")

# Density Plot
p_rq11_den <- ggplot(run_level, aes(energy_j_mean, color = generation, fill = generation)) +
  geom_density(alpha = 0.15) +
  labs(x = "Energy per Run (J)", y = "Density")
save_rq_plot(p_rq11_den, "Descriptive_Data_Exploration", "RQ1.1", "Residual_Density_Distribution_of_Energy_by_Generation")

# ======================================================================
# RQ1.2 — MODEL SIZE
# ======================================================================

# 1. Tables
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

save_rq_table(rq12_desc, "Descriptive_Data_Exploration", "RQ1.2", "Descriptive_Summary")

# 2. Plots
# Boxplot of J per B by generation; fill shows size so 3.2's 1B vs 3B are distinct
p_rq12_box <- ggplot(run_level, aes(generation, J_per_B, fill = model_size_label)) +
  geom_boxplot(outlier.alpha = 0.35) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.75), color = "darkred") +
  labs(x = "Generation", y = "Efficiency (J/B)", fill = "Model Size")
save_rq_plot(p_rq12_box, "Descriptive_Data_Exploration", "RQ1.2", "Efficiency_Scaling_Trend_Across_Generations")

# Density of J per B, faceted by generation; color by size 
# This makes the bimodality or scaling shifts within generations obvious.
p_rq12_den <- ggplot(run_level, aes(J_per_B, color = model_size_label, fill = model_size_label)) +
  geom_density(alpha = 0.15) +
  facet_wrap(~ generation, scales = "free_y") +
  labs(x = "Energy Efficiency (Joules per Billion Parameters)", 
       y = "Density", 
       color = "Model Size", 
       fill = "Model Size") +
  theme(legend.position = "bottom")

save_rq_plot(p_rq12_den, "Descriptive_Data_Exploration", "RQ1.2", "Density_Distribution_of_Scaling_Efficiency_by_Model_Size")

# ======================================================================
# RQ2 — TASK TYPE
# ======================================================================

# 1. Tables (repetition-level, ordered stats) + CSV
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

save_rq_table(rq2_desc, "Descriptive_Data_Exploration", "RQ2", "Descriptive_Summary")

# 2. Plots
repetition_level <- repetition_level %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# VIOLIN energy by generation, faceted by task (repetition-level density)
p_rq2_violin <- ggplot(repetition_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  facet_wrap(~ task, scales = "fixed") +
  labs(x = "Generation", y = "Energy Consumption per Task Execution (J)")

save_rq_plot(p_rq2_violin, "Descriptive_Data_Exploration", "RQ2", "Task_Specific_Energy_Variability_Across_Generations")

# ======================================================================
# RQ3.1 — TOKEN
# ======================================================================

# 1. Tables 
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

save_rq_table(rq31_desc, "Descriptive_Data_Exploration", "RQ3.1", "Descriptive_Summary")

# 2. Plots
# Boxplot: energy per token by generation WITH legend
p_rq31_box <- ggplot(run_level, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Generation", y = "Energy Efficiency (Joules/Token)")

save_rq_plot(p_rq31_box, "Descriptive_Data_Exploration", "RQ3.1", "Comparative_Energy_Cost_per_Token_by_Generation")

# ======================================================================
# RQ3.2 — INFERENCE TIME
# ======================================================================

# 1. Tables
# Ensure power is calculated at repetition level
repetition_level <- repetition_level %>%
  mutate(power_w = energy_j_mean / pmax(duration_s_mean, 1e-9))

rq32_desc <- repetition_level %>%
  group_by(generation) %>%
  summarise(
    n_reps               = n(),
    # Duration Stats
    mean_duration_s      = mean(duration_s_mean, na.rm = TRUE),
    median_duration_s    = median(duration_s_mean, na.rm = TRUE),
    sd_duration_s        = sd(duration_s_mean, na.rm = TRUE),
    # Energy Stats
    mean_energy_J        = mean(energy_j_mean, na.rm = TRUE),
    median_energy_J      = median(energy_j_mean, na.rm = TRUE),
    sd_energy_J          = sd(energy_j_mean, na.rm = TRUE),
    # Power Stats
    mean_power_W         = mean(power_w, na.rm = TRUE),
    median_power_W       = median(power_w, na.rm = TRUE),
    var_power_W          = var(power_w, na.rm = TRUE),
    sd_power_W           = sd(power_w, na.rm = TRUE),
    min_power_W          = min(power_w, na.rm = TRUE),
    max_power_W          = max(power_w, na.rm = TRUE),
    .groups = "drop"
  )

save_rq_table(rq32_desc, "Descriptive_Data_Exploration", "RQ3.2", "Descriptive_Summary")

# 2. Plots
# Boxplot: duration by generation (legend included)
# Plot: Power Trend
p_rq32_power <- ggplot(repetition_level, aes(generation, power_w, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Generation", y = "Average Power Consumption (Watts)")

save_rq_plot(p_rq32_power, "Descriptive_Data_Exploration", "RQ3.2", "Hardware_Power_Demand_Full_Repetition_Distribution")

# ======================================================================
# RQ3.3 — ACCURACY
# ======================================================================

# 1. Prepare Granular Data
rep_acc <- repetition_level %>%
  drop_na(accuracy_val)

# 2. Tables: Descriptive Summary
rq33_desc <- rep_acc %>%
  group_by(generation) %>%
  summarise(
    n_observations   = n(),
    mean_accuracy    = mean(accuracy_val),
    median_accuracy  = median(accuracy_val),
    sd_accuracy      = sd(accuracy_val),
    # Spearman Rho at the repetition level shows the true coupling
    rho_energy_acc   = suppressWarnings(cor(energy_j_mean, accuracy_val, 
                                            method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) %>%
  arrange(generation)

save_rq_table(rq33_desc, "Descriptive_Data_Exploration",  "RQ3.3", "Descriptive_Summary")

# 3. Plots: Distribution and Trade-off
# 3a. Boxplot with Jitter
# Jittering is essential here because repetition accuracy is often exactly 0 or 1
p_rq33_box <- ggplot(rep_acc, aes(x = generation, y = accuracy_val, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.2, width = 0.6) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.1, size = 0.5) + # Shows the "cloud" of data
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Generation", y = "Accuracy Score (0-1)", 
       caption = "Red diamonds indicate mean; jittered points show individual repetitions.") +
  theme_minimal() + theme(legend.position = "none")

save_rq_plot(p_rq33_box, "Descriptive_Data_Exploration", "RQ3.3", "Accuracy_Repetition_Distribution")

# 3b. Scatter: Accuracy vs Energy (Repetition Level)
p_rq33_scatter <- ggplot(rep_acc, aes(x = accuracy_val, y = energy_j_mean, color = generation)) +
  geom_jitter(alpha = 0.3, width = 0.02) + 
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(x = "Accuracy Score", y = "Energy Consumption (J)", color = "Generation") +
  theme_minimal() + theme(legend.position = "bottom")

save_rq_plot(p_rq33_scatter, "Descriptive_Data_Exploration", "RQ3.3", "Accuracy_Energy_Tradeoff_Repetitions")


# --- NORMALITY AND ASSUMPTIONS TESTING ---

# Generic function (Updated to ensure directory logic is clean)
check_assumptions <- function(data, dv, groups, rq_label) {
  stopifnot(dv %in% names(data))
  stopifnot(all(groups %in% names(data)))
  
  # Prepare data
  df <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  for (g in groups) df[[g]] <- factor(df[[g]])
  names(df)[names(df) == dv] <- "y"
  df$cell <- interaction(df[groups], drop = TRUE)
  
  rhs <- paste(groups, collapse = " * ")
  fm  <- as.formula(paste("y ~", rhs))
  
  run_one <- function(y_vec, scale_label) {
    tmp <- df; tmp$y <- y_vec
    fit <- lm(fm, data = tmp)
    res <- resid(fit)
    
    # 1. Statistical Tests
    p_sh <- if (length(res) >= 3 && length(res) <= 5000)
      suppressWarnings(shapiro.test(res)$p.value) else NA_real_
    p_lv <- tryCatch(car::leveneTest(y ~ cell, data = tmp)[["Pr(>F)"]][1],
                     error = function(e) NA_real_)
    
    # 2. Generate Plot Object
    qq_plot <- ggplot(data.frame(res = res), aes(sample = res)) +
      stat_qq() + 
      stat_qq_line() +
      labs(x = "Theoretical Quantiles", y = "Sample Residuals") + 
      theme_minimal()
    
    # 3. USE Standardized Save Function
    save_rq_plot(qq_plot, "Assumptions_Verification", rq_label, "QQ Plot of Residuals on Raw Scale")
    
    tibble(scale = scale_label, shapiro_p = p_sh, levene_p = p_lv)
  }
  
  out <- run_one(df$y, "raw")
  
  # Recommendation logic
  is_norm <- !is.na(out$shapiro_p) && out$shapiro_p >= 0.05
  is_homo <- !is.na(out$levene_p) && out$levene_p >= 0.05
  ng <- length(groups)
  
  rec <- if (ng == 1) {
    if (is_norm && is_homo) "Use one-way ANOVA (Parametric)"
    else if (is_norm && !is_homo) "Use Welch one-way ANOVA"
    else "Use Kruskal-Wallis (Non-parametric)"
  } else {
    if (is_norm && is_homo) "Use factorial ANOVA"
    else "Use ART (Aligned Rank Transform)"
  }
  
  list(summary = out, recommendation = rec)
}

save_assessment <- function(rq, res, dv, groups, data) {

  df_clean <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  
  out <- res$summary %>%
    mutate(
      RQ = rq,
      dv = dv,
      groups = paste(groups, collapse = " * "),
      n_total = nrow(df_clean),
      # Define the 'ok' column (TRUE if both p-values > 0.05)
      ok = (shapiro_p >= 0.05 & levene_p >= 0.05),
      recommendation = res$recommendation
    ) %>%
    select(RQ, dv, groups, scale, shapiro_p, levene_p, ok, n_total, recommendation)
  
  save_rq_table(out, "Assumptions_Verification", rq, "Assumptions_Summary")
}

# Execution
# RQ 1.1: Run Level
res_RQ11 <- check_assumptions(run_level, "energy_j_mean", "generation", "RQ1.1")
save_assessment("RQ1.1", res_RQ11, "energy_j_mean", "generation", run_level)

# RQ 1.2: Run Level
res_RQ12 <- check_assumptions(run_level, "J_per_B", c("generation", "model_size_b"), "RQ1.2")
save_assessment("RQ1.2", res_RQ12, "J_per_B", c("generation", "model_size_b"), run_level)

# RQ 2: Repetition Level
res_RQ2 <- check_assumptions(repetition_level, "energy_j_mean", c("generation", "task"), "RQ2")
save_assessment("RQ2", res_RQ2, "energy_j_mean", c("generation", "task"), repetition_level)

# RQ 3.1: Run Level
res_RQ31 <- check_assumptions(run_level, "e_per_tok_mean", "generation", "RQ3.1")
save_assessment("RQ3.1", res_RQ31, "e_per_tok_mean", "generation", run_level)

# RQ 3.2: Repetition Level
res_RQ32 <- check_assumptions(repetition_level, "power_w", "generation", "RQ3.2")
save_assessment("RQ3.2", res_RQ32, "power_w", "generation", repetition_level)

# RQ 3.3: Repetition Level
res_RQ33 <- check_assumptions(repetition_level, "accuracy_val", "generation", "RQ3.3")
save_assessment("RQ3.3", res_RQ33, "accuracy_val", "generation", repetition_level)


# --- STATISTICAL TESTS ---

# ======================================================================
# RQ1.1 — GENERATION (KRUSKAL - Run)
# ======================================================================

rq11 <- run_level %>%
  select(generation, energy_j_mean) %>%
  drop_na() %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# 1) Omnibus Test: Kruskal-Wallis
kw_fit <- kruskal.test(energy_j_mean ~ generation, data = rq11)

# Calculate Epsilon Squared (Effect Size for Kruskal-Wallis)
# Formula: epsilon^2 = H / ((n^2 - 1) / (n + 1))
h_stat <- kw_fit$statistic
n_total <- nrow(rq11)
eps_sq <- h_stat / ((n_total^2 - 1) / (n_total + 1))

omnibus_kw_csv <- tibble(
  test    = "Kruskal-Wallis",
  df      = kw_fit$parameter,
  chi_sq  = kw_fit$statistic,
  p_value = kw_fit$p.value,
  epsilon_sq = eps_sq,
  interpretation = "Non-parametric comparison of medians"
)

save_rq_table(omnibus_kw_csv, "Statistical_Tests", "RQ1.1", "Omnibus_Kruskal_Wallis_Energy_Test")

# 2) Post-hoc: Dunn's Test (Successive)
rq11_plain <- rq11 %>%
  mutate(generation = as.factor(as.character(generation)))
dunn_res <- FSA::dunnTest(energy_j_mean ~ generation, 
                          data = rq11_plain, 
                          method = "bh")
dunn_df  <- dunn_res$res

# Robus filtering 
successive_steps <- c("2 - 3", "3 - 3.1", "3.1 - 3.2", "3 - 2", "3.1 - 3", "3.2 - 3.1")
dunn_successive <- dunn_df %>%
  filter(Comparison %in% successive_steps)

save_rq_table(dunn_successive, "Statistical_Tests", "RQ1.1", "Pairwise_Dunn_Successive_Energy_Comparisons")

# ==============================================================================
# RQ 1.2 — MODEL-SIZE (KRUSKAL - Run)
# ==============================================================================

# 1. Create a "Group" variable to avoid aliasing
rq12_stat <- run_level %>%
  mutate(
    J_per_B = energy_j_mean / model_size_b,
    # Create a unique ID for every Gen-Size combination
    group_id = factor(paste0("Gen", generation, "_", model_size_b, "B"))
  ) %>%
  drop_na(J_per_B, group_id)

# 2. Omnibus: Kruskal-Wallis on Groups
# This tests: "Is there ANY difference between any of the model configurations?"
kw_res <- kruskal.test(J_per_B ~ group_id, data = rq12_stat)

# Calculate Effect Size (Epsilon Squared)
h_stat <- kw_res$statistic
n_total <- nrow(rq12_stat)
eps_sq <- h_stat / ((n_total^2 - 1) / (n_total + 1))

omnibus_csv <- tibble(
  test = "Kruskal-Wallis",
  chi_sq = kw_res$statistic,
  df = kw_res$parameter,
  p_value = kw_res$p.value,
  epsilon_sq = eps_sq,
  note = "Grouped by Gen and Size to handle unbalanced design"
)
save_rq_table(omnibus_csv, "Statistical_Tests", "RQ1.2", "Omnibus_Kruskal_Grouped")

# 3. Post-hoc: Dunn's Test (The important part)
# This allows us to compare specific pairs (e.g., Gen 3 8B vs Gen 3.1 8B)
dunn_res <- FSA::dunnTest(J_per_B ~ group_id, data = rq12_stat, method = "bh")
dunn_df <- dunn_res$res

# 4. Extracting the 3 Key Comparisons for your paper:
# Comparison A: Evolution of the 8B model (3.0 vs 3.1)
# Comparison B: Evolution of the 7B/8B class (2 vs 3)
# Comparison C: Scaling within 3.2 (1B vs 3B)
key_comparisons <- dunn_df %>%
  filter(
    Comparison %in% c("Gen3_8B - Gen3.1_8B", "Gen3.1_8B - Gen3_8B",
                     "Gen2_7B - Gen3_8B", "Gen3_8B - Gen2_7B",
                     "Gen3.2_1B - Gen3.2_3B", "Gen3.2_3B - Gen3.2_1B")
  )

save_rq_table(key_comparisons, "Statistical_Tests", "RQ1.2", "Key_Pairwise_Efficiency_Comparisons")

# 5. Plot: Grouped Boxplot (Standardized)
p_rq12_final <- ggplot(rq12_stat, aes(x = group_id, y = J_per_B, fill = group_id)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip() + # Horizontal makes long group names easier to read
  labs(x = "Model Configuration", y = "Efficiency (J/B)") +
  theme(legend.position = "none")

save_rq_plot(p_rq12_final, "Statistical_Tests", "RQ1.2", "Efficiency_by_Model_Configuration_Grouped")

# ==============================================================================
# RQ2 — TASK TYPE (ART - Repetition)
# ==============================================================================

# 1. Data Prep
rq2_stat <- df %>%
  select(run, generation, task, energy_j) %>%
  drop_na() %>%
  mutate(
    generation = factor(as.character(generation)),
    task       = factor(task),
    run        = factor(run)
  )

# 1. Run the Omnibus
m_art_rq2 <- art(energy_j ~ generation * task + (1|run), data = rq2_stat)
rq2_anova <- anova(m_art_rq2)
int_p <- rq2_anova["generation:task", "Pr(>F)"]

# 2. Main Effect Post-hoc (Averaged across Tasks)
# Use this for the main "Energy Evolution" narrative
gen_con <- art.con(m_art_rq2, "generation", adjust = "bh") %>%
  as.data.frame() %>%
  mutate(Analysis = "Averaged Across Tasks")

# 3. Interaction Post-hoc (Per Task)
# Use this to prove that the savings hold true for EVERY task
task_con <- art.con(m_art_rq2, "generation:task", adjust = "bh") %>%
  as.data.frame() %>%
  mutate(Analysis = "Per-Task Breakdown")

# 4. Standardizing and Filtering
# Combine them so you can choose which one to report
full_posthoc <- bind_rows(gen_con, task_con)

# Apply the Robust Regex Filter we built
col_name <- if("contrast" %in% names(full_posthoc)) "contrast" else "Comparison"

successive_rq2 <- full_posthoc %>%
  filter(
    (grepl("2", .data[[col_name]]) & grepl("3", .data[[col_name]]) & !grepl("3.1", .data[[col_name]])) |
    (grepl("3", .data[[col_name]]) & grepl("3.1", .data[[col_name]])) |
    (grepl("3.1", .data[[col_name]]) & grepl("3.2", .data[[col_name]]))
  )

save_rq_table(successive_rq2, "Statistical_Tests", "RQ2", "Successive_Generation_Pairwise_Comparisons")

# 6. Plotting with Median Trend
p_rq2_stat <- ggplot(rq2_stat, aes(x = generation, y = energy_j, color = task, group = task)) +
  stat_summary(fun = median, geom = "line", linewidth = 0.8, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", size = 2) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Generation", 
       y = "Energy Consumption (Median Joules)", 
       color = "Task Type",
       caption = "Rank-based analysis accounting for random effect of run.") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Use our new standardized plot saver
save_rq_plot(p_rq2_stat, "Statistical_Tests", "RQ2", "Energy_Consumption_by_Task_Interaction_Profile")

# ==============================================================================
# RQ 3.1 — ENERGY PER TOKEN (KRUSKAL - Run)
# ==============================================================================

# 1. Prepare Run-Level Data (Ensuring tokens_mean is included)
rq31_stat <- run_level %>%
  select(generation, e_per_tok_mean, tokens_mean) %>% # Added tokens_mean here
  drop_na() %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# 2. Omnibus: Kruskal-Wallis
kw_res <- kruskal.test(e_per_tok_mean ~ generation, data = rq31_stat)
h_stat <- as.numeric(kw_res$statistic)
n_total <- nrow(rq31_stat)
k_groups <- nlevels(rq31_stat$generation)
eps_sq <- (h_stat - k_groups + 1) / (n_total - k_groups)

omnibus_rq31 <- tibble(
  test = "Kruskal-Wallis",
  chi_sq = h_stat,
  df = kw_res$parameter,
  p_value = kw_res$p.value,
  epsilon_sq = eps_sq
)
save_rq_table(omnibus_rq31, "Statistical_Tests", "RQ3.1", "Omnibus_Kruskal_Wallis")

# 3. Successive Post-hoc: Wilcoxon + Manual Robust Cliff's Delta
gens <- levels(rq31_stat$generation)
pairs <- tibble(g1 = gens[-length(gens)], g2 = gens[-1])

calc_cliffs_delta <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  m <- outer(y, x, "-")
  delta <- (sum(m > 0) - sum(m < 0)) / (length(x) * length(y))
  return(delta)
}

calc_successive <- function(g1, g2) {
  val_g1 <- rq31_stat$e_per_tok_mean[rq31_stat$generation == g1]
  val_g2 <- rq31_stat$e_per_tok_mean[rq31_stat$generation == g2]
  wt <- wilcox.test(val_g1, val_g2, exact = FALSE)
  delta <- calc_cliffs_delta(val_g1, val_g2)
  
  abs_d <- abs(delta)
  magnitude <- "negligible" 
  if (!is.na(abs_d)) {
    if (abs_d >= 0.474) magnitude <- "large"
    else if (abs_d >= 0.330) magnitude <- "medium"
    else if (abs_d >= 0.147) magnitude <- "small"
  }
  
  tibble(
    Comparison = paste(g1, "to", g2),
    W_stat = wt$statistic,
    p_raw = wt$p.value,
    cliffs_delta = delta,
    magnitude = magnitude
  )
}

pairwise_rq31 <- map_df(1:nrow(pairs), ~calc_successive(pairs$g1[.x], pairs$g2[.x])) %>%
  mutate(p_BH = p.adjust(p_raw, method = "BH"))

save_rq_table(pairwise_rq31, "Statistical_Tests", "RQ3.1", "Pairwise_Successive_Wilcoxon_CliffsDelta")

# 4. TRADE-OFF PLOT (Replaces Boxplot)
p_rq31_tradeoff <- ggplot(rq31_stat, aes(x = tokens_mean, y = e_per_tok_mean, color = generation)) +
  geom_point(size = 3, alpha = 0.8) +
  # Linear trend line per generation
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(suffix = " J/tok")) +
  labs(
    x = "Mean Output Tokens per Run",
    y = "Energy Efficiency (Joules/Token)",
    color = "Generation",
    caption = "Horizontal trend lines indicate efficiency is independent of response length."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq31_tradeoff, "Statistical_Tests", "RQ3.1", "Energy_Token_Efficiency_Tradeoff")

# ==============================================================================
# RQ 3.2 — INFERENCE DURATION (ART - Repetion)
# ==============================================================================

# 1. Prepare the Repetition-Level Data
rq32_data <- repetition_level %>%
  mutate(
    # Use the pmax to avoid division by zero
    power_w = energy_j_mean / pmax(duration_s_mean, 1e-9),
    generation = factor(generation, levels = c("2","3","3.1","3.2")),
    run = factor(run)
  ) %>%
  drop_na(power_w)

# 2. Omnibus: ART Mixed Model
m_art_32 <- art(power_w ~ generation + (1|run), data = rq32_data)
anova_32 <- anova(m_art_32)

# Save Omnibus
save_rq_table(as.data.frame(anova_32), "Statistical_Tests", "RQ3.2", "Omnibus_ART_Mixed_Model")

# 3. Post-hoc: Successive Pairwise
# We use the same successive logic to see where the power drops
gen_con_32 <- art.con(m_art_32, "generation", adjust = "BH") %>%
  as.data.frame()

# Clean naming for the helper
if ("contrast" %in% names(gen_con_32)) gen_con_32 <- rename(gen_con_32, Comparison = contrast)

# Filter for successive generations (2-3, 3-3.1, 3.1-3.2)
successive_32 <- gen_con_32 %>%
  filter(
    (grepl("2", Comparison) & grepl("3", Comparison) & !grepl("3.1", Comparison)) |
    (grepl("3", Comparison) & grepl("3.1", Comparison)) |
    (grepl("3.1", Comparison) & grepl("3.2", Comparison))
  )

save_rq_table(successive_32, "Statistical_Tests", "RQ3.2", "Pairwise_Successive_Power")

# Secondary Analysis: Coupling check
rq32_coupling <- rq32_data %>%
  group_by(generation) %>%
  summarise(
    Spearman_Rho = cor(duration_s_mean, energy_j_mean, method = "spearman"),
    p_value = suppressWarnings(
      cor.test(duration_s_mean, energy_j_mean, method = "spearman")$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(p_BH = p.adjust(p_value, method = "BH"))

save_rq_table(rq32_coupling, "Statistical_Tests", "RQ3.2", "Energy_Duration_Spearman_Coupling")

# ==============================================================================
# RQ 3.3 — ACCURACY (ART - Repetition Level)
# ==============================================================================

# 1. Filter data for accuracy analysis
rq33_data <- repetition_level %>%
  drop_na(accuracy_val)

# 2. Omnibus: ART Mixed Model 
# Accounts for non-normality of accuracy scores and random effect of "run"
m_art_33 <- art(accuracy_val ~ generation + (1|run), data = rq33_data)
anova_33 <- anova(m_art_33)

save_rq_table(as.data.frame(anova_33), "Statistical_Tests", "RQ3.3", "Omnibus_ART_Mixed_Model")

# 3. Post-hoc: Successive Pairwise
gen_con_33 <- art.con(m_art_33, "generation", adjust = "BH") %>% 
  as.data.frame()

if ("contrast" %in% names(gen_con_33)) gen_con_33 <- rename(gen_con_33, Comparison = contrast)

# Successive filter (2-3, 3-3.1, 3.1-3.2)
successive_33 <- gen_con_33 %>%
  filter(
    (grepl("2", Comparison) & grepl("3", Comparison) & !grepl("3.1", Comparison)) |
    (grepl("3", Comparison) & grepl("3.1", Comparison)) |
    (grepl("3.1", Comparison) & grepl("3.2", Comparison))
  )

save_rq_table(successive_33, "Statistical_Tests", "RQ3.3", "Pairwise_Successive_Accuracy")

# 4. Spearman Correlation: Energy-Accuracy Trade-off
# Tests if higher energy consumption correlates with higher accuracy
rq33_spearman <- rq33_data %>%
  group_by(generation) %>%
  summarise(
    rho = cor(energy_j_mean, accuracy_val, method = "spearman"),
    p_val = suppressWarnings(
      cor.test(energy_j_mean, accuracy_val, method = "spearman")$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(p_BH = p.adjust(p_val, method = "BH"))

save_rq_table(rq33_spearman, "Statistical_Tests", "RQ3.3", "Energy_Accuracy_Spearman_Coupling")