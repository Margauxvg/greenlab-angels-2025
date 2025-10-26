# GreenLab Angels 2025 – LLaMA Energy Efficiency Experiment

## Project Overview

This project provides an empirical, inference-time energy efficiency evaluation of successive **LLaMA** model generations. Using tasks from the **SuperGLUE** benchmark, we measure each model's energy consumption, inference latency, and accuracy to analyze how efficiency has evolved from one LLaMA generation to the next.

The study focuses on the inference phase (now a major part of an LLM’s carbon footprint) to test whether newer LLaMA models are more energy-efficient than their predecessors. All experiments are executed in a controlled environment with fixed hardware and repeated trials, as described in the accompanying whitepaper.

## Experimental Architecture

- **Two-Server Setup**
  - **Orchestrator (Control Node):**
    - Runs the experiment controller.
    - Deployed as a cloud VPS.
    - Schedules and randomizes all trials.
    - Sends HTTP requests to the testbed to execute runs.
    - Logs and persists results.
  - **Testbed (Inference Node):**
    - Dedicated local GPU machine (NVIDIA RTX 4090, 24 GB VRAM).
    - Hosts a lightweight Python HTTP API to receive run requests.
    - Actually runs the model inference + energy measurement.

- **Inference Engine**
  - Uses **vLLM** (v0.11.0) to load and run LLaMA models efficiently on the testbed.
  - Input prompts for each task are tokenized with HuggingFace Transformers.
  - Context length is capped (e.g. 10,240 tokens) to fit within 24 GB VRAM.

- **Energy Measurement**
  - Uses **EnergiBridge** (v0.0.7) on the testbed to record power and energy data.
  - EnergiBridge samples CPU and GPU energy (Intel RAPL + NVIDIA NVML) ~every 200ms.
  - Low-overhead logging compared to `nvidia-smi`, so it doesn't distort the measurements.
  - Metrics include:
    - CPU joules
    - GPU wattage / joules
    - Total energy per inference run

- **Execution Flow**
  1. Orchestrator picks the next `(model, task)` combination to run.
  2. Orchestrator calls the testbed’s API with that configuration.
  3. Testbed launches `model_runner`, which:
     - Loads the requested LLaMA model in vLLM.
     - Runs the prompts for that SuperGLUE task.
     - Starts EnergiBridge to measure power/energy during inference.
  4. When inference finishes, the testbed:
     - Stops EnergiBridge.
     - Bundles outputs + power logs.
     - Sends them back to the orchestrator.
  5. Orchestrator stores results and updates run progress.
  6. A fixed cool-down window (e.g. 30s) is enforced between runs to avoid thermal bias.

This architecture lets us attribute differences in energy usage to *the model generation itself*, not random hardware noise.

## Key Files and Directories

- `calculate-accuracy.ipynb`
  - Post-processing and analysis notebook.
  - Cleans and merges all raw experiment logs.
  - Produces the final metrics (accuracy, energy per run, etc.).
  - Generates the final result tables and plots used in the report.

- `runnerconfig/`
  - Experiment configuration.
  - Declares all trial definitions (which LLaMA model, which SuperGLUE task).
  - Defines the full factorial space of treatments used in the study.

- `run_table`
  - Runtime progress tracking.
  - The orchestrator uses this to record which `(model, task, repetition)` runs are done.
  - Lets you resume / verify that all repetitions completed.

- `init_env.sh`
  - Bootstrap script for the testbed (the GPU machine).
  - Installs dependencies (Python, vLLM, Transformers, EnergiBridge, etc.).
  - Downloads all required LLaMA model weights.
  - Brings up the testbed’s HTTP API server that the orchestrator talks to.

- `model_runner`
  - The testbed-side script that actually:
    - Loads the requested LLaMA model with vLLM.
    - Executes the inference prompts for the given task.
    - Interfaces with EnergiBridge to monitor energy.
    - Returns results back to the orchestrator.
  - This is what the orchestrator triggers remotely for each trial.

- `llama_profiling/experiments/llama_profiling/`
  - Ground truth dataset for the study.
  - Contains:
    - All raw energy logs.
    - All model outputs.
    - All measurement artifacts for every run.
  - This directory is the input to the analysis notebook.

## Running the Experiment

The experiment is designed to be reproducible end-to-end. High-level flow:

1. **Prepare Machines**
   - Launch two machines:
     - Orchestrator (cloud VPS, light CPU).
     - Testbed (local GPU box with RTX 4090 or equivalent).
   - Ensure the orchestrator can reach the testbed over the network (the testbed exposes an HTTP API).

2. **Initialize the Testbed**
   - SSH into the testbed.
   - Run:
     ```bash
     ./init_env.sh
     ```
   - What this does:
     - Installs all required Python packages (vLLM, Transformers, EnergiBridge, etc.).
     - Downloads all LLaMA model weights used in the experiment.
     - Sets up and starts the testbed’s API service so it can receive run commands from the orchestrator.

3. **Configure the Experiment**
   - Inspect `runnerconfig/`.
   - These configs describe:
     - Which LLaMA generation and parameter size to test (e.g. Llama 2, Llama 3, Llama 3.1, Llama 3.2; up to 10B parameters).
     - Which SuperGLUE task to run (BoolQ, CB, COPA, RTE, WiC, WSC).
   - By default, the config defines the same model/task matrix used in the paper (a full factorial design, repeated multiple times for statistical reliability).

4. **Start the Orchestrator**
   - On the orchestrator machine, start the experiment controller (Experiment Runner).
   - It will:
     - Randomize the order of all `(model, task)` treatments.
     - For each trial, send an HTTP request to the testbed telling it which model and which task to run.
     - Wait for results.
     - Log results + update `run_table`.

5. **Inference + Measurement (Happens Automatically per Trial)**
   - The testbed receives the request and runs `model_runner`.
   - `model_runner`:
     - Loads the specified LLaMA model into vLLM.
     - Runs the selected SuperGLUE task prompts (fixed set of 80 prompts per task per run).
     - Runs EnergiBridge in parallel to collect:
       - CPU energy (via Intel RAPL),
       - GPU energy (via NVIDIA NVML),
       - Total joules consumed during inference.
   - When finished, the testbed:
     - Stops measurement.
     - Sends the inference outputs, timings, and energy logs back to the orchestrator.

   - The orchestrator records the response and marks the run complete in `run_table`.
   - A cool-down delay is enforced before the next run so thermal state doesn’t bias the next energy reading.

6. **Aggregate + Analyze Results**
   - After all repetitions across all model/task combinations are done:
     - Copy the raw logs from the testbed into your working environment if needed.
     - Open `calculate-accuracy.ipynb`.
       - Load the raw data from `llama_profiling/experiments/llama_profiling/`.
       - Clean/merge all runs into a single dataset.
       - Compute:
         - Energy per run / per token,
         - Inference duration,
         - Response token count,
         - Accuracy (via LLM-as-a-judge scoring),
         - Energy vs accuracy trade-offs per generation.
       - Produce final summary tables / plots.

   - These outputs are the basis for the figures and claims in the whitepaper (e.g. "Llama 3.2 shows a large energy reduction compared to earlier generations while maintaining performance").

---
## Data Analysis

A comprehensive statistical analysis of energy consumption across Llama model generations (2, 3, 3.1, 3.2), examining the relationship between model evolution, parameter scaling, task types, and performance metrics.

### Overview

This repository contains R code for analyzing energy consumption patterns in Large Language Models (LLMs), specifically the Llama family. The analysis addresses six research questions examining energy efficiency improvements across generations, scaling effects, task-specific consumption, and trade-offs between energy and performance.

**Research Questions**

- **RQ1.1**: Energy consumption differences across Llama generations
- **RQ1.2**: Scaling effects with parameter size within generations
- **RQ2**: Generation × Task interaction effects on energy consumption
- **RQ3.1**: Energy cost per output token across generations
- **RQ3.2**: Relationship between inference duration and energy consumption
- **RQ3.3**: Trade-offs between energy consumption and model accuracy

**Requirements**

**R Version**

- R ≥ 4.0.0

**Required Packages**

r

`install.packages(c(
  "tidyverse",
  "car",
  "ggplot2",
  "emmeans",
  "effectsize",
  "lmerTest",
  "scales",
  "lme4"
))`

### Data Format

The analysis expects a CSV file named `GL_Final_Run_Table_Edited.csv` with the following columns:

- `run`: Unique run identifier
- `repetition`: Repetition number within each run
- `generation`: Model generation (2, 3, 3.1, 3.2)
- `task`: Task type identifier
- `model_size`: Parameter size (e.g., "1B", "3B", "7B", "8B")
- `tokens`: Number of output tokens
- `time(s)`: Inference duration in seconds
- `energy`: Energy consumption in Joules
- `accuracy`: Model accuracy (0-1 or 0-100)
- `__done`: Status flag (should be "DONE" for valid entries)

### Usage

**Running the Complete Analysis**

r
```
`source("GL_Final_Script.R")


The script will automatically:
1. Create directory structure for outputs
2. Load and preprocess data
3. Generate descriptive statistics
4. Verify statistical assumptions
5. Perform appropriate statistical tests
6. Create visualizations
7. Export results to CSV files

Output Structure
├── plots/
│   ├── Descriptive_Data_Exploration/
│   │   ├── RQ1.1/
│   │   ├── RQ1.2/
│   │   ├── RQ2/
│   │   ├── RQ3.1/
│   │   ├── RQ3.2/
│   │   └── RQ3.3/
│   ├── Assumptions_Verification/
│   │   └── [same RQ structure]
│   └── Statistical_Tests/
│       └── [same RQ structure]
└── tables/
    └── [same structure as plots]
```


### Analysis Workflow

**1. Data Preprocessing**

- Aggregates repetitions into run-level statistics
- Creates repetition-level dataset for mixed models
- Computes derived metrics (energy per token, power, etc.)

**2. Descriptive Analysis**

Generates for each RQ:

- Summary statistics (mean, median, SD, min, max)
- Distribution plots (boxplots, density plots, scatter plots)
- CSV tables with descriptive statistics

**3. Assumption Verification**

Tests normality and homogeneity of variance using:

- Shapiro-Wilk tests for normality
- Levene's test for homogeneity
- Q-Q plots for visual diagnostics
- Evaluates raw, log-transformed, and sqrt-transformed data
- Provides test recommendations

**4. Statistical Testing**

**Parametric Tests** (when assumptions met):**

- One-way ANOVA (RQ1.1)
- Two-way ANOVA (RQ1.2)
- Linear mixed-effects models (RQ2)

**Nonparametric Tests** (when assumptions violated):**

- Kruskal-Wallis tests (RQ3.1, RQ3.2, RQ3.3)
- Wilcoxon rank-sum tests for post-hoc comparisons

**Effect Sizes**:

- η² and ω² for ANOVA
- Hedges' g for pairwise comparisons
- ε² for Kruskal-Wallis
- Cliff's delta for Wilcoxon tests

**Multiple Comparison Correction**:

- Benjamini-Hochberg (BH) adjustment for all post-hoc tests

### Key Features

**Robust Statistical Approach**

- Automatic assumption checking with transformation suggestions
- Appropriate test selection based on data characteristics
- Planned contrasts (successive generations: 2→3, 3→3.1, 3.1→3.2)
- Polynomial trend analysis for ordered factors

**Mixed-Effects Modeling (RQ2)**

- Accounts for nested structure (repetitions within runs)
- Tests generation × task interaction
- Provides estimates on both link and response scales
- Includes ICC and convergence diagnostics

**Comprehensive Outputs**

- Publication-ready plots (PNG, 150-300 DPI)
- Detailed CSV tables for all analyses
- Console summaries with key findings
- Effect sizes for all significant results

### Interpreting Results

**Tables**

- `_desc.csv`: Descriptive statistics
- `_assumption_summary.csv`: Assumption test results and recommendations
- `_omnibus.csv`: Main effect tests
- `_consec.csv` / `_pairwise.csv`: Post-hoc comparisons with BH-adjusted p-values

**Plots**

- **Boxplots**: Distribution and outliers by group
- **Density plots**: Overlapping distributions
- **Scatter plots**: Relationships between continuous variables
- **ECDF plots**: Full distribution comparisons
- **Q-Q plots**: Normality diagnostics

### Statistical Decisions

- **Significance level**: α = 0.05
- **Two-sided tests** throughout
- **Planned contrasts**: Successive generations only (reduces multiple comparisons)
- **Effect size reporting**: Mandatory for all tests
- **Mixed models**: REML estimation with Satterthwaite degrees of freedom

---

This repository contains:
- The experiment orchestration logic,
- The measurement and logging pipeline (EnergiBridge integration),
- The raw data from all completed trials,
- And the notebooks used to build the final analysis.
