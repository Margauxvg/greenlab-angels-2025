library(tidyverse)

# make a place for plots
if (!dir.exists("plots")) dir.create("plots")

# ======================================================================
# RQ1.1 — “as shipped” generation effect (ignore size)
# ======================================================================

# Descriptives
rq11_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs        = n(),
    mean_energy   = mean(energy_j_mean),
    median_energy = median(energy_j_mean),
    var_energy    = var(energy_j_mean),
    sd_energy     = sd(energy_j_mean),
    .groups = "drop"
  ) %>%
  arrange(generation)
rq11_desc

# Figures
p_box <- ggplot(run_level, aes(generation, energy_j_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35) +   # legend shows by default when fill maps to a var
  labs(x = "Generation", y = "Energy per run (J)", fill = "Generation") +
  theme_minimal()

ggsave("plots/rq11_box_energy_by_generation_runs.png",
       p_box, width = 7, height = 5, dpi = 150, bg = "white")

# Density: overlaid by generation (pooled)
p_den <- ggplot(run_level, aes(energy_j_mean, color = generation, fill = generation)) +
  geom_density(alpha = 0.15) +
  labs(x = "Energy per run (J)", y = "Density") +
  theme_minimal()
ggsave("plots/rq11_density_energy_by_generation_runs.png", p_den, width = 8, height = 5, dpi = 150,bg = "white")

# ======================================================================
# RQ1.2 — scaling with parameter size (within generation)
# ======================================================================

# Descriptives
run_level <- run_level %>%
  mutate(J_per_B = energy_j_mean / pmax(model_size_b, 1e-9))  # Joules per billion params
# If you truly want per-parameter (tiny numbers): 
# mutate(J_per_param = energy_j_mean / (model_size_b * 1e9))

# group by generation and size (so 3.2@1B and 3.2@3B are separate)
rq12_desc <- run_level %>%
  group_by(generation, model_size_b) %>%
  summarise(
    n_runs          = n(),
    mean_energy_J   = mean(energy_j_mean),
    median_energy_J = median(energy_j_mean),
    sd_energy_J     = sd(energy_j_mean),
    mean_J_per_B    = mean(J_per_B),
    median_J_per_B  = median(J_per_B),
    sd_J_per_B      = sd(J_per_B),
    .groups = "drop"
  ) %>%
  arrange(generation, model_size_b)
rq12_desc

# Figures
# Boxplot of J per B by generation; fill shows size so 3.2's 1B vs 3B are distinct
p_rq12_box <- ggplot(run_level, aes(generation, J_per_B, fill = factor(model_size_b))) +
  geom_boxplot(outlier.alpha = 0.35) +
  labs(x="Generation", y="Energy per B params (J/B)", fill="Size (B)") +
  theme_minimal()
ggsave("plots/rq12_box_JperB_by_gen_runs.png", p_rq12_box, width=8, height=5, dpi=150, bg="white")

# Density of J per B, faceted by generation; color by size (makes 3.2’s two sizes obvious)
p_rq12_den <- ggplot(run_level, aes(J_per_B, color = factor(model_size_b), fill = factor(model_size_b))) +
  geom_density(alpha = 0.15) +
  facet_wrap(~ generation, scales = "free_y") +
  labs(x="Energy per B params (J/B)", y="Density", color="Size (B)", fill="Size (B)") +
  theme_minimal()
ggsave("plots/rq12_density_JperB_by_gen_runs.png", p_rq12_den, width=9, height=6, dpi=150, bg="white")

# ======================================================================
# RQ2 — task type × generation
# ======================================================================

# Descriptives
rq2_desc <- run_level %>%
  group_by(generation, task) %>%
  summarise(
    n_runs = n(),
    mean_E = mean(energy_j_mean),
    sd_E   = sd(energy_j_mean),
    .groups = "drop"
  ) %>% arrange(task, generation)

rq2_desc

# Figures
run_level <- run_level %>%
  mutate(generation = factor(generation, levels = c("2","3","3.1","3.2"), ordered = TRUE))

# shared ranges so facets use the SAME scales
x_range <- range(run_level$energy_j_mean, na.rm = TRUE)
y_range <- x_range  # scatter uses energy on y

# 1) BOX • energy by task, colored by task (legend appears)
p_box_task <- ggplot(run_level, aes(task, energy_j_mean, fill = task)) +
  geom_boxplot(outlier.alpha = 0.35) +
  labs(x = "Task", y = "Energy per run (J)", fill = "Task") +
  theme_minimal()
ggsave("plots/rq2_box_energy_by_task.png", p_box_task,
       width = 9, height = 5, dpi = 150, bg = "white")

# 2) SCATTER • energy vs generation, faceted by task, SAME y scale
p_scatter_task_gen <- ggplot(run_level, aes(generation, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2, position = position_jitter(width = 0.12, height = 0)) +
  facet_wrap(~ task, scales = "fixed") +
  coord_cartesian(ylim = y_range) +
  labs(x = "Generation", y = "Energy per run (J)", color = "Task") +
  theme_minimal()
ggsave("plots/rq2_scatter_energy_by_generation_facet_task.png", p_scatter_task_gen,
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
    sd_Ept      = sd(e_per_tok_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(generation)

rq31_desc

# Figures
# Boxplot: energy per token by generation WITH legend
p_rq31_box <- ggplot(run_level, aes(generation, e_per_tok_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
  labs(x = "Generation", y = "Energy per token (J/token)", fill = "Generation") +
  theme_minimal()

ggsave("plots/rq31_box_Ept_by_generation_runs.png",
       p_rq31_box, width = 7, height = 5, dpi = 150, bg = "white")

# Scatter: tokens vs energy, colored by generation
p_rq31_scatter <- ggplot(run_level, aes(tokens_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean tokens per run", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/rq31_scatter_tokens_vs_energy_by_generation_runs.png",
       p_rq31_scatter, width = 8, height = 5, dpi = 150, bg = "white")

# ======================================================================
# RQ3.2 — inference duration vs energy (by generation)
# ======================================================================

# Descriptives
rq32_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs          = n(),
    mean_duration_s = mean(duration_s_mean),
    median_duration_s = median(duration_s_mean),
    sd_duration_s   = sd(duration_s_mean),
    mean_energy_J   = mean(energy_j_mean),
    median_energy_J = median(energy_j_mean),
    sd_energy_J     = sd(energy_j_mean),
    .groups = "drop"
  ) %>%
  arrange(generation)

rq32_desc

# Figures
# 1) Boxplot: duration by generation (legend included)
p_rq32_box <- ggplot(run_level, aes(generation, duration_s_mean, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
  labs(x = "Generation", y = "Inference duration per run (s)", fill = "Generation") +
  theme_minimal()

ggsave("plots/rq32_box_duration_by_generation_runs.png",
       p_rq32_box, width = 7, height = 5, dpi = 150, bg = "white")

# 2) Scatter: duration vs energy, colored by generation (simple and useful)
p_rq32_scatter <- ggplot(run_level, aes(duration_s_mean, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Inference duration per run (s)", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()

ggsave("plots/rq32_scatter_duration_vs_energy_runs.png",
       p_rq32_scatter, width = 8, height = 5, dpi = 150, bg = "white")

# ======================================================================
# RQ3.3 — energy vs accuracy (stub: run this AFTER you add accuracy)
# ======================================================================

# ---------------- RQ3.3 DESCRIPTIVES (by generation) ----------------
#TODO: ADJUST THIS SECTION!!!

rq33_desc <- run_level %>%
  group_by(generation) %>%
  summarise(
    n_runs        = n(),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    median_accuracy = median(accuracy, na.rm = TRUE),
    sd_accuracy   = sd(accuracy, na.rm = TRUE),
    # quick association at run level
    rho_E_acc     = suppressWarnings(cor(energy_j_mean, accuracy, method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) %>%
  arrange(generation)

rq33_desc

# Figures

# Boxplot: accuracy by generation (legend ON)
p_rq33_box <- ggplot(run_level, aes(generation, accuracy, fill = generation)) +
  geom_boxplot(outlier.alpha = 0.35, show.legend = TRUE) +
  labs(x = "Generation", y = "Accuracy", fill = "Generation") +
  theme_minimal()
ggsave("plots/rq33_box_accuracy_by_generation_runs.png",
       p_rq33_box, width = 7, height = 5, dpi = 150, bg = "white")

# Scatter: accuracy vs energy, colored by generation
p_rq33_scatter <- ggplot(run_level, aes(accuracy, energy_j_mean, color = generation)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Accuracy", y = "Energy per run (J)", color = "Generation") +
  theme_minimal()
ggsave("plots/rq33_scatter_accuracy_vs_energy_runs.png",
       p_rq33_scatter, width = 8, height = 5, dpi = 150, bg = "white")

