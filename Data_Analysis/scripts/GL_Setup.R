library(tidyverse)

# 1) Read with the correct name
runs <- read_csv("GL_Final_Run_Table_Edited.csv", show_col_types = FALSE)

# 2) Check column names so we know what we’re renaming
names(runs)

acc_hit <- "accuracy"

if (length(acc_hit) >= 1) {
  runs$.accuracy_raw <- suppressWarnings(as.numeric(runs[[acc_hit[1]]]))
} else if (all(c("correct","total") %in% names(runs))) {
  runs$.accuracy_raw <- suppressWarnings(as.numeric(runs$correct) / pmax(as.numeric(runs$total), 1))
} else {
  runs$.accuracy_raw <- NA_real_
}

# 3) Clean + derive
df <- runs %>%
  rename(
    duration_s = `time(s)`,
    energy_j   = `energy`      # if your header is literally "energy," then use `energy,`
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
    logE = log1p(energy_j_mean)
  )

# ---- Build repetition-level table (one row per repetition) ----
# Same column names as run_level so the rest of your code just works.
# Only difference: we include 'repetition' and set n_reps = 1, sd = NA.
repetition_level <- df %>%
  mutate(
    # keep gens ordered everywhere for consistent plots/tests
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

# Sanity checks: one row per run×repetition; no duplicates; no missing energy
stopifnot(!any(duplicated(repetition_level[, c("run","repetition")])))
stopifnot(all(!is.na(repetition_level$energy_j_mean)))

