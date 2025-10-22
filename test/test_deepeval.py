# works for single file (single row):
    
# import pandas as pd
# from deepeval.metrics import AnswerRelevancyMetric
# from deepeval.test_case import LLMTestCase

# # ====== CONFIGURATION ======
# FILE_PATH = "../llama_profiling/experiments/llama_profiling/run_0_repetition_0/prompts_out.tsv"

# # ====== METRIC SETUP ======
# metric = AnswerRelevancyMetric(
#     threshold=0.5,
#     model="gpt-5-nano",
#     include_reason=False
# )

# # ====== LOAD DATA ======
# df = pd.read_csv(FILE_PATH, sep='\t')

# # Ensure the output column exists
# if "accuracy" not in df.columns:
#     df["accuracy"] = [None] * len(df)

# # ====== PREPARE FIRST TEST CASE ======
# test_cases = []
# for _, row in df.iterrows():
#     prompt = row.get("prompt")
#     response = row.get("response")
#     if pd.notna(prompt) and pd.notna(response):
#         test_cases.append((prompt, response))

# # ====== EVALUATE ONLY THE FIRST ITEM ======
# if test_cases:
#     prompt, response = test_cases[0]
#     print("Evaluating first test case only...")

#     test_case = LLMTestCase(input=prompt, actual_output=response)
#     metric.measure(test_case)

#     df.at[0, "accuracy"] = metric.score
#     print(f"→ [0] Score: {metric.score:.2f}")

#     # Save result
#     df.to_csv(FILE_PATH, sep='\t', index=False)
#     print(f"Saved result for first test case to {FILE_PATH}")
# else:
#     print("No valid test cases found.")


# import os
# import pandas as pd
# from concurrent.futures import ProcessPoolExecutor, as_completed
# from deepeval.metrics import AnswerRelevancyMetric
# from deepeval.test_case import LLMTestCase

# # ====== CONFIGURATION ======
# BASE_DIR = "../llama_profiling/experiments/llama_profiling/"

# # ====== METRIC SETUP ======
# metric = AnswerRelevancyMetric(
#     threshold=0.5,
#     model="gpt-5-nano",
#     include_reason=False
# )

# def find_tsv_files(base_dir):
#     """
#     Recursively find all TSV files named 'prompts_out.tsv' under the base directory.
#     """
#     tsv_paths = []
#     for root, _, files in os.walk(base_dir):
#         for f in files:
#             if f == "prompts_out.tsv":
#                 tsv_paths.append(os.path.join(root, f))
#     return tsv_paths


# def process_tsv(file_path):
#     """
#     Process a single TSV file: compute relevancy for each prompt/response,
#     skip already populated rows, and save progress after every update.
#     """
#     try:
#         df = pd.read_csv(file_path, sep='\t')

#         # Ensure output accuracy column exists
#         if "accuracy" not in df.columns:
#             df["accuracy"] = [None] * len(df)

#         total_rows = len(df)
#         print(f"Processing {os.path.basename(file_path)} ({total_rows} rows)")

#         processed = 0
#         for i, row in df.iterrows():
#             if i == 3 :
#                 break
#             # Skip rows that already have a score
#             if pd.notna(row.get("accuracy")):
#                 continue

#             prompt = row.get("prompt")
#             response = row.get("response")
#             if pd.isna(prompt) or pd.isna(response):
#                 continue

#             # Measure relevance using deepeval
#             test_case = LLMTestCase(input=prompt, actual_output=response)
#             metric.measure(test_case)
#             df.at[i, "accuracy"] = metric.score
#             processed += 1

#             # Save after every row (safe for interruption)
#             df.to_csv(file_path, sep='\t', index=False)
#             print(f"Saved row {i+1}/{total_rows} (Score: {metric.score:.3f})")

#         print(f"Done {os.path.basename(file_path)} ({processed} new rows processed)")
#         return f"{os.path.basename(file_path)}: {processed} rows processed"

#     except Exception as e:
#         return f"Error processing {file_path}: {e}"


# def main():
#     tsv_files = find_tsv_files(BASE_DIR)
#     if not tsv_files:
#         print("No TSV files found.")
#         return

#     print(f"Found {len(tsv_files)} TSV files. Starting parallel evaluation...")

#     results = []
#     with ProcessPoolExecutor(max_workers=8) as executor:
#         futures = {executor.submit(process_tsv, path): path for path in tsv_files}
#         for future in as_completed(futures):
#             results.append(future.result())

#     print("\n===== SUMMARY =====")
#     for res in results:
#         print(res)


# if __name__ == "__main__":
#     main()



## Dummy test to see if it works:

# import os
# import pandas as pd
# import random
# import time
# from concurrent.futures import ProcessPoolExecutor, as_completed

# # ====== CONFIGURATION ======
# BASE_DIR = "../test/"
# SLEEP_BETWEEN = 0.1

# def find_tsv_files(base_dir):
#     """
#     Recursively find all TSV files named 'prompts_out.tsv' under the base directory.
#     """
#     tsv_paths = []
#     for root, _, files in os.walk(base_dir):
#         for f in files:
#             if f == "prompts_out.tsv":
#                 tsv_paths.append(os.path.join(root, f))
#     return tsv_paths


# def process_tsv(file_path):
#     """
#     Process a single TSV file: simulate metric computation for each prompt/response,
#     save progress as we go, and return the result summary.
#     """
#     try:
#         df = pd.read_csv(file_path, sep='\t')

#         if "accuracy" not in df.columns:
#             df["accuracy"] = [None] * len(df)

#         total_rows = len(df)
#         print(f"Processing {os.path.basename(file_path)} ({total_rows} rows)")

#         processed = 0
#         for i, row in df.iterrows():
#             if pd.notna(row.get("accuracy")):
#                 continue

#             prompt = row.get("prompt")
#             response = row.get("response")
#             if pd.isna(prompt) or pd.isna(response):
#                 continue

#             time.sleep(SLEEP_BETWEEN)
#             dummy_score = round(random.random(), 3)
#             df.at[i, "accuracy"] = dummy_score
#             processed += 1

#             if processed % 5 == 0:
#                 df.to_csv(file_path, sep='\t', index=False)
#                 print(f"Progress saved ({processed}/{total_rows})")

#         # final save
#         df.to_csv(file_path, sep='\t', index=False)
#         print(f"Done {os.path.basename(file_path)} ({processed} new rows)")

#         return f"{os.path.basename(file_path)}: {processed} rows processed"

#     except Exception as e:
#         return f"Error processing {file_path}: {e}"


# def main():
#     tsv_files = find_tsv_files(BASE_DIR)
#     if not tsv_files:
#         print("No TSV files found.")
#         return

#     print(f"Found {len(tsv_files)} TSV files. Starting dummy evaluation...")

#     results = []
#     with ProcessPoolExecutor() as executor:
#         futures = {executor.submit(process_tsv, path): path for path in tsv_files}
#         for future in as_completed(futures):
#             results.append(future.result())

#     print("\n===== SUMMARY =====")
#     for res in results:
#         print(res)


# if __name__ == "__main__":
#     main()


# Dummy code to test statified sampling approach

# import os
# import pandas as pd
# import random
# import time
# from concurrent.futures import ProcessPoolExecutor, as_completed

# BASE_DIR = "../test/"
# SLEEP_BETWEEN = 0.1
# NUM_RANDOM_ROWS = 5


# def find_tsv_files(base_dir):
#     tsv_paths = []
#     for root, _, files in os.walk(base_dir):
#         for f in files:
#             if f == "prompts_out.tsv":
#                 tsv_paths.append(os.path.join(root, f))
#     return tsv_paths


# def process_tsv(file_path):
#     try:
#         df = pd.read_csv(file_path, sep='\t')

#         if "accuracy" not in df.columns:
#             df["accuracy"] = [None] * len(df)

#         unprocessed_indices = df[df["accuracy"].isna()].index.tolist()

#         if not unprocessed_indices:
#             print(f"All rows already processed in {os.path.basename(file_path)}.")
#             return f"{os.path.basename(file_path)}: 0 rows processed (already complete)"

#         selected_indices = random.sample(unprocessed_indices, min(NUM_RANDOM_ROWS, len(unprocessed_indices)))

#         print(f"Processing {len(selected_indices)} random rows from {os.path.basename(file_path)}...")

#         for i in selected_indices:
#             prompt = df.at[i, "prompt"]
#             response = df.at[i, "response"]

#             if pd.isna(prompt) or pd.isna(response):
#                 continue

#             time.sleep(SLEEP_BETWEEN)
#             dummy_score = round(random.random(), 3)
#             df.at[i, "accuracy"] = dummy_score

#         df.to_csv(file_path, sep='\t', index=False)
#         print(f"Done {os.path.basename(file_path)} ({len(selected_indices)} rows updated)")

#         return f"{os.path.basename(file_path)}: {len(selected_indices)} rows processed"

#     except Exception as e:
#         return f"Error processing {file_path}: {e}"


# def main():
#     tsv_files = find_tsv_files(BASE_DIR)
#     if not tsv_files:
#         print("No TSV files found.")
#         return

#     print(f"Found {len(tsv_files)} TSV files. Starting dummy evaluation...")

#     results = []
#     with ProcessPoolExecutor() as executor:
#         futures = {executor.submit(process_tsv, path): path for path in tsv_files}
#         for future in as_completed(futures):
#             results.append(future.result())

#     print("\n===== SUMMARY =====")
#     for res in results:
#         print(res)


# if __name__ == "__main__":
#     main()




# Stratified sampling approach

import os
import pandas as pd
import random
from concurrent.futures import ProcessPoolExecutor, as_completed
from deepeval.metrics import AnswerRelevancyMetric
from deepeval.test_case import LLMTestCase

# ====== CONFIGURATION ======
BASE_DIR = "../llama_profiling/experiments/llama_profiling/"
NUM_RANDOM_ROWS = 5

metric = AnswerRelevancyMetric(
    threshold=0.5,
    model="gpt-5-nano",
    include_reason=False
)


def find_tsv_files(base_dir):
    tsv_paths = []
    for root, _, files in os.walk(base_dir):
        for f in files:
            if f == "prompts_out.tsv":
                tsv_paths.append(os.path.join(root, f))
    return tsv_paths


def process_tsv(file_path):
    try:
        df = pd.read_csv(file_path, sep='\t')

        if "accuracy" not in df.columns:
            df["accuracy"] = [None] * len(df)

        total_rows = len(df)
        print(f"Processing {os.path.basename(file_path)} ({total_rows} rows)")

        unprocessed_indices = df[df["accuracy"].isna()].index.tolist()

        if not unprocessed_indices:
            print(f"All rows already processed in {os.path.basename(file_path)}.")
            return f"{os.path.basename(file_path)}: 0 rows processed (already complete)"

        selected_indices = random.sample(unprocessed_indices, min(NUM_RANDOM_ROWS, len(unprocessed_indices)))

        processed = 0
        for i in selected_indices:
            prompt = df.at[i, "prompt"]
            response = df.at[i, "response"]

            if pd.isna(prompt) or pd.isna(response):
                continue

            test_case = LLMTestCase(input=prompt, actual_output=response)
            metric.measure(test_case)
            df.at[i, "accuracy"] = metric.score
            processed += 1

            df.to_csv(file_path, sep='\t', index=False)
            print(f"Saved row {i+1}/{total_rows} (Score: {metric.score:.3f})")

        print(f"Done {os.path.basename(file_path)} ({processed} new rows processed)")
        return f"{os.path.basename(file_path)}: {processed} rows processed"

    except Exception as e:
        return f"Error processing {file_path}: {e}"


def main():
    tsv_files = find_tsv_files(BASE_DIR)
    if not tsv_files:
        print("No TSV files found.")
        return

    print(f"Found {len(tsv_files)} TSV files. Starting parallel evaluation...")

    results = []
    with ProcessPoolExecutor(max_workers=8) as executor:
        futures = {executor.submit(process_tsv, path): path for path in tsv_files}
        for future in as_completed(futures):
            results.append(future.result())

    print("\n===== SUMMARY =====")
    for res in results:
        print(res)


if __name__ == "__main__":
    main()

