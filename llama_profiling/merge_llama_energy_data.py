# import os
# import pandas as pd

# BASE_DIR = "../llama_profiling/experiments/llama_profiling"
# OUTPUT_FILE = "merged_energibridge.csv"

# csv_files = [
#     os.path.join(root, f)
#     for root, _, files in os.walk(BASE_DIR)
#     for f in files if f.endswith(".csv")
# ]

# dfs = []
# for path in csv_files:
#     df = pd.read_csv(path)
#     df.columns = df.columns.str.strip()
#     dfs.append(df)

# merged = pd.concat(dfs, ignore_index=True)

# merged.to_csv(OUTPUT_FILE, index=False)
# print(f"Merged CSV saved: {OUTPUT_FILE}")

import os
import pandas as pd
import re

BASE_DIR = "../llama_profiling/experiments/llama_profiling"
OUTPUT_FILE = "merged_energibridge.csv"

csv_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(BASE_DIR)
    for f in files if f.endswith(".csv")
]

dfs = []

def split_run_id(val):
    match = re.match(r'(?:run_?|)(\d+)_+(repetition_\d+)', val)
    if match:
        run_num, repetition = match.groups()
        return f"run{run_num}", repetition
    else:
        parts = val.split("_", 1)
        run = parts[0] if parts else None
        rep = parts[1] if len(parts) > 1 else None
        return run, rep

for path in csv_files:
    df = pd.read_csv(path)
    df.columns = df.columns.str.strip()

    # Look for a TSV file in the same folder
    folder = os.path.dirname(path)
    tsv_files = [f for f in os.listdir(folder) if f.endswith(".tsv")]

    if tsv_files:
        tsv_path = os.path.join(folder, tsv_files[0])  # Take the first TSV found
        tsv_df = pd.read_csv(tsv_path, sep="\t")
        tsv_df.columns = tsv_df.columns.str.strip()

        if "start" in tsv_df.columns and "end" in tsv_df.columns and "time(s)" in df.columns:
            start_time = tsv_df["start"].iloc[0]
            end_time = tsv_df["end"].iloc[-1]

            # Filter rows within the time range
            df = df[(df["time(s)"] >= start_time) & (df["time(s)"] <= end_time)]

    energy_col = "PACKAGE_ENERGY(J)"
    if energy_col in df.columns:
        energies = df[energy_col].copy()
        corrected_energies = energies.copy()

        # Detect overflow: if next value < current, assume overflow
        for i in range(1, len(energies)):
            if energies.iloc[i] < energies.iloc[i - 1]:
                corrected_energies.iloc[i:] += energies.iloc[i - 1]
        
        df[energy_col] = corrected_energies

    # Add run/repetition columns if present
    if "__run_id" in df.columns:
        run_ids = df["__run_id"].astype(str)
        split_df = run_ids.apply(split_run_id).apply(pd.Series)
        split_df.columns = ["run", "repetition"]
        df = pd.concat([df, split_df], axis=1)
    else:
        df["run"] = None
        df["repetition"] = None

    dfs.append(df)

# Merge all filtered DataFrames
merged = pd.concat(dfs, ignore_index=True)

# Keep only relevant columns
final_columns = [
    "run",
    "repetition",
    "done",
    "generation",
    "model_size",
    "task",
    "tokens",
    "time(s)",
    "PACKAGE_ENERGY (J)",
]
existing_cols = [c for c in final_columns if c in merged.columns]
merged = merged[existing_cols]

# Save merged CSV
merged.to_csv(OUTPUT_FILE, index=False)
print(f"Merged CSV saved: {OUTPUT_FILE}")
