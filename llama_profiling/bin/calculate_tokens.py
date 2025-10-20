#!/usr/bin/env python3
import pandas as pd
from transformers import AutoTokenizer

df = pd.read_csv("./llama_profiling/experiments/llama_profiling/run_table.csv")

def get_model_path(row):
    generation = row["generation"]
    if row["generation"] == 3.0:
        generation = "3"
    if row["generation"] == 2.0:
        generation = "2"

    return f"./llama_profiling/models/{generation}-{row["model_size"]}"

for _, row in df.iterrows():
    try:
        model_path = get_model_path(row)
        prompts_out_path = f"./llama_profiling/experiments/llama_profiling/{row["__run_id"]}/prompts_out.tsv"

        tokenizer = AutoTokenizer.from_pretrained(model_path)

        prompt_df = pd.read_csv(prompts_out_path, sep="\t")

        prompt_df["response_tokens"] = prompt_df["response"].apply(
            lambda response: len(tokenizer(response)["input_ids"])
        )

        prompt_df.to_csv(prompts_out_path, sep="\t", index=False)
    except Exception as e:
        print(e)
        print(row)
        exit()
