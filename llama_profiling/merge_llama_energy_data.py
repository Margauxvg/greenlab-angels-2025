import os
import pandas as pd

BASE_DIR = "../llama_profiling/experiments/llama_profiling"
OUTPUT_FILE = "merged_energibridge.csv"

csv_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(BASE_DIR)
    for f in files if f.endswith(".csv")
]

dfs = []
for path in csv_files:
    df = pd.read_csv(path)
    df.columns = df.columns.str.strip()
    dfs.append(df)

merged = pd.concat(dfs, ignore_index=True)

merged.to_csv(OUTPUT_FILE, index=False)
print(f"Merged CSV saved: {OUTPUT_FILE}")


import os
import pandas as pd

BASE_DIR = "../llama_profiling/experiments/llama_profiling"
OUTPUT_FILE = "merged_energibridge.csv"

KEEP_HEADERS = [
    "generation",
    "model_size",
    "task",
    "PACKAGE_ENERGY",
    "Time",
    "__run_id",
]

def find_tsv_in_same_folder(csv_path):
    folder = os.path.dirname(csv_path)
    for f in os.listdir(folder):
        if f.endswith(".tsv"):
            return os.path.join(folder, f)
    return None

def read_tsv_start_end(tsv_path):
    tsv_df = pd.read_csv(tsv_path, sep="\t", dtype=str, encoding="utf-8-sig")
    cols = [c.strip().lower() for c in tsv_df.columns]
    if "start" not in cols or "end" not in cols:
        raise ValueError("tsv missing start/end columns")
    col_map = {c.strip().lower(): c for c in tsv_df.columns}
    start = float(tsv_df[col_map["start"]].iloc[0])
    end = float(tsv_df[col_map["end"]].iloc[-1])
    return start, end

def detect_and_fix_time_units(df_time_series, start_time, end_time, time_col_name):
    df_time_series[time_col_name] = pd.to_numeric(df_time_series[time_col_name], errors="coerce")
    if df_time_series[time_col_name].isna().all():
        return df_time_series
    csv_max = df_time_series[time_col_name].max()
    if csv_max > 1e12 and end_time < 1e11:
        df_time_series[time_col_name] = df_time_series[time_col_name] / 1000.0
    return df_time_series

csv_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(BASE_DIR)
    for f in files if f.endswith(".csv")
]

collected = []

for path in csv_files:
    try:
        df = pd.read_csv(path, encoding="utf-8-sig")
    except Exception:
        try:
            df = pd.read_csv(path)
        except Exception as e:
            print(f"Skipping unreadable CSV: {path} ({e})")
            continue

    df.columns = [c.strip() for c in df.columns]

    if "PACKAGE_ENERGY (J)" in df.columns:
        df.rename(columns={"PACKAGE_ENERGY (J)": "PACKAGE_ENERGY"}, inplace=True)

    tsv_path = find_tsv_in_same_folder(path)
    if tsv_path is not None and "Time" in df.columns:
        try:
            start_time, end_time = read_tsv_start_end(tsv_path)
        except Exception as e:
            print(f"Warning: can't read start/end from {tsv_path}: {e}")
            start_time = end_time = None

        if start_time is not None and end_time is not None:
            df = detect_and_fix_time_units(df, start_time, end_time, "Time")
            df = df.dropna(subset=["Time"])
            df = df[(df["Time"] >= start_time) & (df["Time"] <= end_time)].copy()
            if df.empty:
                continue

    folder_name = os.path.basename(os.path.dirname(path))
    if "__run_id" not in df.columns:
        df.loc[:, "__run_id"] = folder_name

    existing_keep = [col for col in KEEP_HEADERS if col in df.columns]
    if not existing_keep:
        continue

    df_keep = df[existing_keep].copy()
    collected.append(df_keep)

if collected:
    merged = pd.concat(collected, ignore_index=True)
    merged = merged.reindex(columns=KEEP_HEADERS)

    numeric_cols = merged.select_dtypes(include="number").columns
    if numeric_cols.any():
        merged[numeric_cols] = merged[numeric_cols].round(4)

    merged.to_csv(OUTPUT_FILE, index=False)
    print(f"Merged CSV saved: {OUTPUT_FILE} ({len(merged)} rows, {len(merged.columns)} cols)")
else:
    print("No matching rows collected. Check your CSV/TSV presence and column names.")
