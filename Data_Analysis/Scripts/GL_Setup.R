library(tidyverse)

# 1) Read with a non-stupid name
runs <- read_csv("GL_Final_Run_Table_Edited.csv", show_col_types = FALSE)

# 2) Check column names so we know what we’re renaming
names(runs)

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
    
    avg_power_w      = energy_j / pmax(duration_s, 1e-9),
    energy_per_token = energy_j / pmax(tokens, 1L)
  )

#I need a run-level table because we will use this one for descriptive data exploration and avoid that the number 
#of repetitions has an influence on the data.
# ---- Build run-level table (one row per run) ----
run_level <- df %>%
  group_by(run, generation, task, model_size_label, model_size_b) %>%
  summarise(
    n_reps          = n(),
    energy_j_mean   = mean(energy_j, na.rm = TRUE),
    energy_j_sd     = sd(energy_j,   na.rm = TRUE),
    duration_s_mean = mean(duration_s, na.rm = TRUE),
    tokens_mean     = mean(tokens,   na.rm = TRUE),
    e_per_tok_mean  = mean(energy_per_token, na.rm = TRUE),
    # tokens-weighted J/token (optional, sometimes more sensible)
    .groups = "drop"
  ) %>%
  mutate(
    logE = log1p(energy_j_mean)
  )

# sanity: each run once
stopifnot(!any(duplicated(run_level$run)))


