# works for single file:
    
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
# if "output accuracy" not in df.columns:
#     df["output accuracy"] = [None] * len(df)

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

#     df.at[0, "output accuracy"] = metric.score
#     print(f"→ [0] Score: {metric.score:.2f}")

#     # Save result
#     df.to_csv(FILE_PATH, sep='\t', index=False)
#     print(f"✅ Saved result for first test case to {FILE_PATH}")
# else:
#     print("⚠️ No valid test cases found.")


import os
import pandas as pd
from concurrent.futures import ProcessPoolExecutor, as_completed
from deepeval.metrics import AnswerRelevancyMetric
from deepeval.test_case import LLMTestCase

# ====== CONFIGURATION ======
BASE_DIR = "../llama_profiling/experiments/llama_profiling/"

# ====== METRIC SETUP ======
metric = AnswerRelevancyMetric(
    threshold=0.5,
    model="gpt-5-nano",
    include_reason=False
)

def find_tsv_files(base_dir):
    """
    Recursively find all TSV files named 'prompts_out.tsv' under the base directory.
    """
    tsv_paths = []
    for root, _, files in os.walk(base_dir):
        for f in files:
            if f == "prompts_out.tsv":
                tsv_paths.append(os.path.join(root, f))
    return tsv_paths


def process_tsv(file_path):
    """
    Process a single TSV file: load it, compute relevancy for the first prompt/response,
    save the score, and return the result.
    """
    try:
        df = pd.read_csv(file_path, sep='\t')

        if "output accuracy" not in df.columns:
            df["output accuracy"] = [None] * len(df)

        test_cases = []
        for _, row in df.iterrows():
            prompt = row.get("prompt")
            response = row.get("response")
            if pd.notna(prompt) and pd.notna(response):
                test_cases.append((prompt, response))

        if not test_cases:
            return f"⚠️ No valid test cases found in {file_path}"

        prompt, response = test_cases[0]
        test_case = LLMTestCase(input=prompt, actual_output=response)
        metric.measure(test_case)
        df.at[0, "output accuracy"] = metric.score

        df.to_csv(file_path, sep='\t', index=False)
        return f"✅ {os.path.basename(file_path)} → Score: {metric.score:.2f}"

    except Exception as e:
        return f"❌ Error processing {file_path}: {e}"


def main():
    tsv_files = find_tsv_files(BASE_DIR)
    if not tsv_files:
        print("⚠️ No TSV files found.")
        return

    print(f"🔍 Found {len(tsv_files)} TSV files. Starting parallel evaluation...")

    results = []
    with ProcessPoolExecutor() as executor:
        futures = {executor.submit(process_tsv, path): path for path in tsv_files}
        for future in as_completed(futures):
            results.append(future.result())

    print("\n===== SUMMARY =====")
    for res in results:
        print(res)


if __name__ == "__main__":
    main()
