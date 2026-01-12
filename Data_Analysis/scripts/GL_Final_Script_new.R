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
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  }
}

# --- DATA LOADING & PRE-PROCESSING ---

runs <- read_csv("GL_Final_Run_Table_Edited.csv", show_col_types = FALSE)
df <- runs %>%
  rename(duration_s = `time(s)`, energy_j = `energy`) %>%
  filter(is.na(`__done`) | `__done` == "DONE") %>%
  mutate(
    run = factor(run),
    repetition = as.integer(repetition),
    generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE),
    task = factor(task),
    model_size_label = factor(model_size, levels = c("1B","3B","7B","8B")),
    model_size_b = readr::parse_number(model_size),
    tokens = as.integer(tokens),
    duration_s = as.numeric(duration_s),
    energy_j = as.numeric(energy_j),
    accuracy_raw = suppressWarnings(as.numeric(accuracy)),
    accuracy_val = ifelse(!is.na(accuracy_raw) & accuracy_raw > 1, accuracy_raw/100, accuracy_raw),
    power_w = energy_j / pmax(duration_s, 1e-9),
    energy_per_token = energy_j / pmax(tokens, 1L)
  )

repetition_level <- df %>%
  transmute(
    run, repetition, generation, task, model_size_label, model_size_b,
    energy_j, duration_s, tokens, energy_per_token, power_w, accuracy_val
  )

run_level <- repetition_level %>%
  group_by(run, generation, task, model_size_label, model_size_b) %>%
  summarise(
    energy_j_mean = mean(energy_j, na.rm = TRUE),
    e_per_tok_mean = mean(energy_per_token, na.rm = TRUE),
    duration_s_mean = mean(duration_s, na.rm = TRUE),
    tokens_mean = mean(tokens, na.rm = TRUE),
    accuracy_mean = mean(accuracy_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(J_per_B = energy_j_mean / pmax(model_size_b, 1e-9))

# --- HELPER FUNCTIONS ---

save_rq_plot <- function(plot, context, rq, description) {
  filename <- paste0(rq, "_", gsub(" ", "_", description), ".pdf")
  filepath <- file.path("plots", context, rq, filename)
  ggsave(filepath, plot, device = "pdf", width = 8, height = 6)
}

save_rq_table <- function(table, context, rq, description) {
  filename <- paste0(rq, "_", gsub(" ", "_", description), ".csv")
  filepath <- file.path("tables", context, rq, filename)
  table_clean <- table %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  write_csv(table_clean, filepath)
}

# Restoration of original function name but removing recommendation logic
check_assumptions <- function(data, dv, groups, rq_label) {
  df_sub <- data %>% select(all_of(c(groups, dv))) %>% drop_na()
  names(df_sub)[names(df_sub) == dv] <- "y"
  df_sub$cell <- interaction(df_sub[groups], drop = TRUE)
  
  rhs <- paste(groups, collapse = " * ")
  fm <- as.formula(paste("y ~", rhs))
  fit <- lm(fm, data = df_sub)
  res <- resid(fit)
  
  p_sh <- if (length(res) >= 3 && length(res) <= 5000) suppressWarnings(shapiro.test(res)$p.value) else NA_real_
  p_lv <- tryCatch(car::leveneTest(y ~ cell, data = df_sub)[["Pr(>F)"]][1], error = function(e) NA_real_)
  
  # Output table without recommendation
  tibble(RQ = rq_label, Metric = dv, shapiro_p = p_sh, levene_p = p_lv, n_total = nrow(df_sub))
}


# --- DESCRIPTIVE DATA EXPLORATION ---

# Common Caption Text
trend_caption <- "Red diamonds indicate the mean; dotted line indicates the trend of the medians."

# ======================================================================
# RQ1.1 — GENERATION (Run Level)
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
  ) %>% arrange(generation)

save_rq_table(rq11_desc, "Descriptive_Data_Exploration", "RQ1.1", "Descriptive_Summary")

# 2. Plots
p_rq11_box <- ggplot(run_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", size = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Llama Generation", 
       y = "Total Energy (J)", 
       caption = trend_caption) + 
  theme_minimal() +
  theme(legend.position = "bottom")
save_rq_plot(p_rq11_box, "Descriptive_Data_Exploration", "RQ1.1", "Energy_Consumption_Distribution_Across_Generations_with_Trendline")

p_rq11_den <- ggplot(run_level, aes(energy_j_mean, color = generation, fill = generation)) +
  geom_density(alpha = 0.15) +
  labs(x = "Energy per Run (J)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")
save_rq_plot(p_rq11_den, "Descriptive_Data_Exploration", "RQ1.1", "Residual_Density_Distribution_of_Energy_by_Generation")

# ======================================================================
# RQ1.2 — MODEL SIZE (Run Level)
# ======================================================================

# 1. Tables
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
  ) %>% arrange(generation, model_size_b)

save_rq_table(rq12_desc, "Descriptive_Data_Exploration", "RQ1.2", "Descriptive_Summary")

# 2. Plots
p_rq12_box <- ggplot(run_level, aes(generation, J_per_B, fill = model_size_label)) +
  geom_boxplot(outlier.alpha = 0.35) +
  scale_fill_discrete(limits = c("1B", "3B", "7B", "8B")) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
               position = position_dodge(width = 0.75), color = "darkred") +
  labs(x = "Generation", y = "Efficiency (J/B)", fill = "Model Size", caption = trend_caption) + theme_minimal() +
  theme(legend.position = "bottom")
save_rq_plot(p_rq12_box, "Descriptive_Data_Exploration", "RQ1.2", "Efficiency_Scaling_Trend_Across_Generations")

p_rq12_den <- ggplot(run_level, aes(J_per_B, color = model_size_label, fill = model_size_label)) +
  geom_density(alpha = 0.15) +
  facet_wrap(~ generation, scales = "free_y") +
  labs(x = "Energy Efficiency (Joules per Billion Parameters)", 
       y = "Density", 
       color = "Model Size", 
       fill = "Model Size") + theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq12_den, "Descriptive_Data_Exploration", "RQ1.2", "Density_Distribution_of_Scaling_Efficiency_by_Model_Size")

# ======================================================================
# RQ2 — TASK TYPE (Repetition Level)
# ======================================================================

# 1. Tables (repetition-level, ordered stats) + CSV
rq2_desc <- repetition_level %>%
  group_by(generation, task) %>%
  summarise(
    n_reps      = n(),
    mean_E      = mean(energy_j, na.rm = TRUE),
    median_E    = median(energy_j, na.rm = TRUE),
    var_E       = var(energy_j, na.rm = TRUE),
    sd_E        = sd(energy_j, na.rm = TRUE),
    min_E       = min(energy_j, na.rm = TRUE),
    max_E       = max(energy_j, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(task, generation)

save_rq_table(rq2_desc, "Descriptive_Data_Exploration", "RQ2", "Descriptive_Summary")

# 2. Plots
p_rq2_violin <- ggplot(repetition_level, aes(generation, energy_j, fill = generation)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  facet_wrap(~ task, scales = "fixed") +
  labs(x = "Generation", y = "Energy Consumption per Task Execution (J)", caption = trend_caption) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq2_violin, "Descriptive_Data_Exploration", "RQ2", "Task_Specific_Energy_Variability_Across_Generations")

# ======================================================================
# RQ3.1 — TOKEN EFFICIENCY (Repetition Level)
# ======================================================================

# 1. Tables 
rq31_desc <- repetition_level %>%
  group_by(generation) %>%
  summarise(
    n_runs      = n(),
    mean_Ept    = mean(energy_per_token, na.rm = TRUE),
    median_Ept  = median(energy_per_token, na.rm = TRUE),
    var_Ept     = var(energy_per_token, na.rm = TRUE),
    sd_Ept      = sd(energy_per_token, na.rm = TRUE),
    min_Ept     = min(energy_per_token, na.rm = TRUE),
    max_Ept     = max(energy_per_token, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(generation)

save_rq_table(rq31_desc, "Descriptive_Data_Exploration", "RQ3.1", "Descriptive_Summary")

# 2. Plots
p_rq31_box <- ggplot(repetition_level, aes(generation, energy_per_token, fill = generation)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Generation", y = "Energy Efficiency (Joules/Token)", caption = trend_caption) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq31_box, "Descriptive_Data_Exploration", "RQ3.1", "Comparative_Energy_Cost_per_Token_by_Generation")

p_rq31_tradeoff <- ggplot(repetition_level, aes(x = tokens, y = energy_per_token, color = generation)) +
  # Individual repetitions (the "cloud")
  geom_point(size = 1.5, alpha = 0.3) +
  # Linear trend line per generation (your original logic)
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.6) +
  scale_y_continuous(labels = scales::label_number(suffix = " J/tok")) +
  labs(
    x = "Output Tokens",
    y = "Energy Efficiency (Joules/Token)",
    color = "Generation"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_rq_plot(p_rq31_tradeoff, "Descriptive_Data_Exploration", "RQ3.1", "Energy_Token_Efficiency_Tradeoff_Repetitions")

# ======================================================================
# RQ3.2 — INFERENCE POWER (Repetition Level)
# ======================================================================

# 1. Tables
rq32_desc <- repetition_level %>%
  group_by(generation) %>%
  summarise(
    n_reps               = n(),
    # Duration Stats
    mean_duration_s      = mean(duration_s, na.rm = TRUE),
    median_duration_s    = median(duration_s, na.rm = TRUE),
    sd_duration_s        = sd(duration_s, na.rm = TRUE),
    # Energy Stats
    mean_energy_J        = mean(energy_j, na.rm = TRUE),
    median_energy_J      = median(energy_j, na.rm = TRUE),
    sd_energy_J          = sd(energy_j, na.rm = TRUE),
    # Power Stats
    mean_power_W         = mean(power_w, na.rm = TRUE),
    median_power_W       = median(power_w, na.rm = TRUE),
    var_power_W          = var(power_w, na.rm = TRUE),
    sd_power_W           = sd(power_w, na.rm = TRUE),
    min_power_W          = min(power_w, na.rm = TRUE),
    max_power_W          = max(power_w, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(generation)

save_rq_table(rq32_desc, "Descriptive_Data_Exploration", "RQ3.2", "Descriptive_Summary")

# 2. Plots: Boxplot and Tradeoff
p_rq32_power <- ggplot(repetition_level, aes(generation, power_w, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = median, geom = "line", aes(group = 1), linetype = "dotted", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(x = "Generation", y = "Average Power Consumption (Watts)", caption = trend_caption) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq32_power, "Descriptive_Data_Exploration", "RQ3.2", "Hardware_Power_Demand_Full_Repetition_Distribution")

p_rq32_tradeoff <- ggplot(repetition_level, aes(x = duration_s, y = power_w, color = generation)) +
  geom_point(alpha = 0.2, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_y_continuous(labels = scales::label_number(suffix = " W")) +
  scale_x_continuous(labels = scales::label_number(suffix = " s")) +
  labs(
    x = "Inference Duration (Seconds)",
    y = "Average Power Draw (Watts)",
    color = "Generation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq32_tradeoff, "Descriptive_Data_Exploration", "RQ3.2", "Power_Duration_Tradeoff_Repetitions")

# ======================================================================
# RQ3.3 — ACCURACY (Repetition Level)
# ======================================================================

accuracy_data <- repetition_level %>% filter(!is.na(accuracy_val))

# 1. Tables: Descriptive Summary
rq33_desc <- accuracy_data %>%
  drop_na(accuracy_val) %>%
  group_by(generation) %>%
  summarise(
    n_observations   = n(),
    mean_accuracy    = mean(accuracy_val),
    median_accuracy  = median(accuracy_val),
    sd_accuracy      = sd(accuracy_val),
    # Spearman Rho at the repetition level shows the true coupling
    rho_energy_acc   = suppressWarnings(cor(energy_j, accuracy_val, 
                                            method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) %>% arrange(generation)

save_rq_table(rq33_desc, "Descriptive_Data_Exploration",  "RQ3.3", "Descriptive_Summary")

# 2. Plots: Distribution and Trade-off
p_rq33_box <- ggplot(accuracy_data, aes(x = generation, y = accuracy_val, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = median, geom = "line", aes(group = 1), 
               linetype = "dotted", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(
    x = "Generation", 
    y = "Accuracy Score", 
    caption = trend_caption
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq33_box, "Descriptive_Data_Exploration", "RQ3.3", "Accuracy_Repetition_Distribution")

# 2b. Scatter: Accuracy vs Energy (Repetition Level)
p_rq33_scatter <- ggplot(accuracy_data, aes(x = accuracy_val, y = energy_j, color = generation)) +
  geom_jitter(alpha = 0.3, width = 0.02) + 
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(x = "Accuracy Score", y = "Energy Consumption (J)", color = "Generation") +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq33_scatter, "Descriptive_Data_Exploration", "RQ3.3", "Accuracy_Energy_Tradeoff_Repetitions")


# --- ASSUMPTIONS VERIFICATION ---

save_rq_table(check_assumptions(run_level, "energy_j_mean", "generation", "RQ1.1"), "Assumptions_Verification", "RQ1.1", "Summary")
save_rq_table(check_assumptions(run_level, "J_per_B", c("generation", "model_size_b"), "RQ1.2"), "Assumptions_Verification", "RQ1.2", "Summary")
save_rq_table(check_assumptions(repetition_level, "energy_j", c("generation", "task"), "RQ2"), "Assumptions_Verification", "RQ2", "Summary")
save_rq_table(check_assumptions(repetition_level, "energy_per_token", "generation", "RQ3.1"), "Assumptions_Verification", "RQ3.1", "Summary")
save_rq_table(check_assumptions(repetition_level, "power_w", "generation", "RQ3.2"), "Assumptions_Verification", "RQ3.2", "Summary")
save_rq_table(check_assumptions(repetition_level, "accuracy_val", "generation", "RQ3.3"), "Assumptions_Verification", "RQ3.3", "Summary")


# --- STATISTICAL TESTS ---

# Successive pairs for filtering post-hoc comparisons (Exact Matching)
successive_pairs <- list(c("2", "3"), c("3", "3.1"), c("3.1", "3.2"))
# --- UNIVERSAL STATISTICAL FILTER ---
filter_by_pairs <- function(df, col_name = "Comparison") {
  df %>%
    mutate(
      part1 = str_split_i(.data[[col_name]], " - ", 1),
      part2 = str_split_i(.data[[col_name]], " - ", 2),
      # Extract version numbers, stripping "generation" if present
      g1_num = str_extract(part1, "2|3\\.1|3\\.2|3"), 
      g2_num = str_extract(part2, "2|3\\.1|3\\.2|3"),
      # Extract task names
      t1_task = str_extract(part1, "BoolQ|CB|COPA|RTE|WiC|WSC"),
      t2_task = str_extract(part2, "BoolQ|CB|COPA|RTE|WiC|WSC")
    ) %>%
    filter(
      map2_lgl(g1_num, g2_num, ~ {
        curr_pair <- c(.x, .y)
        any(map_lgl(successive_pairs, function(p) all(p %in% curr_pair)))
      }),
      (is.na(t1_task) | is.na(t2_task) | t1_task == t2_task)
    ) %>%
    select(-part1, -part2, -g1_num, -g2_num, -t1_task, -t2_task)
}

# ======================================================================
# RQ1.1 — GENERATION (KRUSKAL - Run)
# ======================================================================

rq11 <- run_level %>%
  select(generation, energy_j_mean) %>%
  drop_na() %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# 1) Omnibus Test: Kruskal-Wallis
kw_11 <- kruskal.test(energy_j_mean ~ generation, data = rq11)

# Calculate Epsilon Squared (Effect Size for Kruskal-Wallis)
h_stat <- kw_11$statistic
n_total <- nrow(rq11)
eps_sq <- h_stat / ((n_total^2 - 1) / (n_total + 1))

omnibus_kw_csv <- tibble(
  test    = "Kruskal-Wallis",
  df      = kw_11$parameter,
  chi_sq  = kw_11$statistic,
  p_value = kw_11$p.value,
  epsilon_sq = eps_sq,
  interpretation = "Non-parametric comparison of medians"
)

save_rq_table(omnibus_kw_csv, "Statistical_Tests", "RQ1.1", "Omnibus_Kruskal_Wallis_Energy_Test")

# 2) Post-hoc: Dunn's Test (Successive)
dunn_11 <- FSA::dunnTest(energy_j_mean ~ as.factor(as.character(generation)), 
                         data = run_level, method = "bh")$res
successive_11 <- dunn_11 %>%
  filter(Comparison %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2"))

save_rq_table(successive_11, "Statistical_Tests", "RQ1.1", "Pairwise_Dunn_Successive_Energy_Comparisons")

# ==============================================================================
# RQ 1.2 — MODEL-SIZE (KRUSKAL - Run)
# ==============================================================================

# 1. Create a "Group" variable to avoid aliasing
rq12_stat <- run_level %>%
  mutate(group_id = factor(paste0("Gen", generation, "_", model_size_b, "B")))

# 2. Omnibus: Kruskal-Wallis on Groups
kw_12 <- kruskal.test(J_per_B ~ group_id, data = rq12_stat)

# Calculate Effect Size (Epsilon Squared)
h_stat <- kw_12$statistic
n_total <- nrow(rq12_stat)
eps_sq <- h_stat / ((n_total^2 - 1) / (n_total + 1))

omnibus_csv <- tibble(
  test = "Kruskal-Wallis",
  chi_sq = kw_12$statistic,
  df = kw_12$parameter,
  p_value = kw_12$p.value,
  epsilon_sq = eps_sq,
  note = "Grouped by Gen and Size to handle unbalanced design"
)
save_rq_table(omnibus_csv, "Statistical_Tests", "RQ1.2", "Omnibus_Kruskal_Grouped")


# 2) Post-hoc: Dunn's Test (Successive)
dunn_12 <- FSA::dunnTest(J_per_B ~ group_id, data = rq12_stat, method = "bh")$res

# 2) Filter using both possible alphabetical directions
key_comp_12 <- dunn_12 %>%
  filter(Comparison %in% c(
    "Gen3_8B - Gen3.1_8B", "Gen3.1_8B - Gen3_8B",   # Successive 8B transition
    "Gen2_7B - Gen3_8B", "Gen3_8B - Gen2_7B",       # Architectural jump
    "Gen3.2_1B - Gen3.2_3B", "Gen3.2_3B - Gen3.2_1B" # 1B vs 3B comparison
  ))

key_comp_12 <- key_comp_12 %>%
  mutate(Comparison = case_when(
    Comparison == "Gen3.1_8B - Gen3_8B" ~ "Gen3_8B - Gen3.1_8B",
    TRUE ~ Comparison
  ))

save_rq_table(key_comp_12, "Statistical_Tests", "RQ1.2", "Key_Pairwise_Efficiency_Comparisons")

# 4. Plot: Grouped Boxplot (Standardized)
p_rq12_final <- ggplot(rq12_stat, aes(x = group_id, y = J_per_B, fill = group_id)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip() + # Horizontal makes long group names easier to read
  labs(x = "Model Configuration", y = "Efficiency (J/B)") +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq12_final, "Statistical_Tests", "RQ1.2", "Efficiency_by_Model_Configuration_Grouped")

# ==============================================================================
# RQ2 — TASK TYPE (ART - Repetition)
# ==============================================================================

# 1. Omnibus
art_2 <- art(energy_j ~ generation * task + (1|run), data = repetition_level)
save_rq_table(as.data.frame(anova(art_2)), "Statistical_Tests", "RQ2", "Omnibus_ART_Mixed_Model")

# 2. Main Effect Post-hoc
gen_con_rq2 <- art.con(art_2, "generation", adjust = "bh") %>% as.data.frame()
if ("contrast" %in% names(gen_con_rq2)) gen_con_rq2 <- rename(gen_con_rq2, Comparison = contrast)

successive_main_rq2 <- gen_con_rq2 %>%
  mutate(Clean_Comp = str_replace_all(Comparison, "generation", "")) %>%
  filter(Clean_Comp %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2")) %>%
  mutate(Analysis_Level = "Main Effect (Across Tasks)") %>%
  select(-Clean_Comp)

save_rq_table(successive_main_rq2, "Statistical_Tests", "RQ2", "Successive_Pairwise_Main")

# 3. Interaction Post-hoc
task_con_rq2 <- art.con(art_2, "generation:task", interaction = FALSE, adjust = "bh") %>%
  as.data.frame()
if ("contrast" %in% names(task_con_rq2)) task_con_rq2 <- rename(task_con_rq2, Comparison = contrast)

successive_interaction_rq2 <- task_con_rq2 %>%
  mutate(Clean_Comp = str_replace_all(Comparison, ",(BoolQ|CB|COPA|RTE|WiC|WSC)", "")) %>%
  filter(Clean_Comp %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2")) %>%
  mutate(Analysis_Level = "Interaction (Per Task)") %>%
  select(-Clean_Comp)

save_rq_table(successive_interaction_rq2, "Statistical_Tests", "RQ2", "Successive_Pairwise_Per Task")

# 4. Plotting with Median Trend
p_rq2_stat <- ggplot(repetition_level, aes(x = generation, y = energy_j, color = task, group = task)) +
  stat_summary(fun = median, geom = "line", linewidth = 0.8, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", size = 2) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Generation", 
       y = "Energy Consumption (Median Joules)", 
       color = "Task Type",
       caption = "Rank-based analysis accounting for random effect of run.") +
  theme_minimal() +
  theme(legend.position = "bottom")

save_rq_plot(p_rq2_stat, "Statistical_Tests", "RQ2", "Energy_Consumption_by_Task_Interaction_Profile")

# ==============================================================================
# RQ 3.1 — ENERGY PER TOKEN (ART - Repetition)
# ==============================================================================

# 1. Omnibus Test
art_31 <- art(energy_per_token ~ generation + (1|run), data = repetition_level)
save_rq_table(as.data.frame(anova(art_31)), "Statistical_Tests", "RQ3.1", "Omnibus_ART_Efficiency")

# 2. Post-hoc: Successive Generation Comparisons
gen_con_31 <- art.con(art_31, "generation", adjust = "bh") %>% as.data.frame()
if ("contrast" %in% names(gen_con_31)) gen_con_31 <- rename(gen_con_31, Comparison = contrast)

successive_31 <- gen_con_31 %>%
  mutate(Clean_Comp = str_replace_all(Comparison, "generation", ""),
         Clean_Comp = str_trim(Clean_Comp)) %>%
  filter(Clean_Comp %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2")) %>%
  mutate(Analysis_Level = "Efficiency Evolution") %>%
  select(-Clean_Comp)

save_rq_table(successive_31, "Statistical_Tests", "RQ3.1", "Successive_Pairwise_Efficiency")

# 3. Spearman correlation: Test if higher energy consumption correlates with higher token count
rq31_spearman <- repetition_level %>%
  group_by(generation) %>%
  summarise(
    rho = cor(energy_j, tokens, method = "spearman"),
    p_val = suppressWarnings(
      cor.test(energy_j, tokens, method = "spearman")$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(p_BH = p.adjust(p_val, method = "BH"))

save_rq_table(rq31_spearman, "Statistical_Tests", "RQ3.1", "Energy_Token Count_Spearman_Coupling")

# ==============================================================================
# RQ 3.2 — INFERENCE DURATION (ART - Repetion)
# ==============================================================================

# 1. Omnibus: ART Mixed Model
art_32 <- art(power_w ~ generation + (1|run), data = repetition_level)
anova_32 <- anova(art_32)
save_rq_table(as.data.frame(anova_32), "Statistical_Tests", "RQ3.2", "Omnibus_ART_Mixed_Model")

# 2. Post-hoc: Successive Pairwise
gen_con_32 <- art.con(art_32, "generation", adjust = "bh") %>% as.data.frame()
if ("contrast" %in% names(gen_con_32)) gen_con_32 <- rename(gen_con_32, Comparison = contrast)

successive_32 <- gen_con_32 %>%
  mutate(Clean_Comp = str_replace_all(Comparison, "generation", ""),
         Clean_Comp = str_trim(Clean_Comp)) %>%
  filter(Clean_Comp %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2")) %>%
  mutate(Analysis_Level = "Efficiency Evolution") %>%
  select(-Clean_Comp)

save_rq_table(successive_32, "Statistical_Tests", "RQ3.2", "Pairwise_Successive_Power")

# Secondary Analysis: Coupling check
rq32_coupling <- repetition_level %>%
  group_by(generation) %>%
  summarise(
    Spearman_Rho = cor(energy_j, duration_s, method = "spearman"),
    p_value = suppressWarnings(
      cor.test(energy_j, duration_s, method = "spearman")$p.value
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
art_33 <- art(accuracy_val ~ generation + (1|run), data = rq33_data)
anova_33 <- anova(art_33)

save_rq_table(as.data.frame(anova_33), "Statistical_Tests", "RQ3.3", "Omnibus_ART_Mixed_Model")

# 3. Post-hoc: Successive Pairwise
gen_con_33 <- art.con(art_33, "generation", adjust = "bh") %>% as.data.frame()
if ("contrast" %in% names(gen_con_33)) gen_con_33 <- rename(gen_con_33, Comparison = contrast)

successive_33 <- gen_con_33 %>%
  mutate(Clean_Comp = str_replace_all(Comparison, "generation", ""),
         Clean_Comp = str_trim(Clean_Comp)) %>%
  filter(Clean_Comp %in% c("2 - 3", "3 - 3.1", "3.1 - 3.2")) %>%
  mutate(Analysis_Level = "Efficiency Evolution") %>%
  select(-Clean_Comp)

save_rq_table(successive_33, "Statistical_Tests", "RQ3.3", "Pairwise_Successive_Accuracy")

# 4. Spearman Correlation: Energy-Accuracy Trade-off
# Tests if higher energy consumption correlates with higher accuracy
rq33_spearman <- rq33_data %>%
  group_by(generation) %>%
  summarise(
    rho = cor(energy_j, accuracy_val, method = "spearman"),
    p_val = suppressWarnings(
      cor.test(energy_j, accuracy_val, method = "spearman")$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(p_BH = p.adjust(p_val, method = "BH"))

save_rq_table(rq33_spearman, "Statistical_Tests", "RQ3.3", "Energy_Accuracy_Spearman_Coupling")