#!/usr/bin/env python3
import sys
import os
import json
import time
from vllm import LLM, SamplingParams
from transformers import AutoTokenizer
import requests

class ModelRunner:
    def __init__(self, model, dataset, callback_url):
        self.init_model(model)
        self.init_dataset(dataset)

        self.callback_url = callback_url

    def measure(self):
        for prompt in self.dataset:
            self.run_prompt(prompt)

        self.send_callback()

    def run_prompt(self, prompt):
        messages = [{"role": "user", "content": prompt}]
        formatted_prompt = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
        sampling_params = SamplingParams(temperature=0.7, top_p=0.9)
        outputs = model.generate([formatted_prompt], sampling_params)

    def send_callback(self, max_retries=5, initial_delay=1):
        delay = initial_delay

        for attempt in range(max_retries):
            try:
                response = requests.post(self.callback_url, timeout=10)
                response.raise_for_status()
                print(f"Callback sent successfully to {self.callback_url}")
                return
            except requests.exceptions.RequestException as e:
                if attempt == max_retries - 1:
                    print(f"Failed to send callback after {max_retries} attempts: {e}")
                    raise

                print(f"Callback attempt {attempt + 1} failed: {e}. Retrying in {delay}s...")
                time.sleep(delay)
                delay *= 2

    def init_model(self, model):
        model_path = os.path.join(os.getcwd(), "llama_profiling", "models", model)
        if not os.path.exists(model_path):
            print(f"Error: Model path does not exist: {model_path}")
            sys.exit(1)

        self.model = LLM(model=model_path, max_model_len=4096, max_num_batched_tokens=4096)
        self.tokenizer = AutoTokenizer.from_pretrained(model_path)

    def init_dataset(self, dataset):
        dataset_path = os.path.join(os.getcwd(), "llama_profiling", "prompts", f"{dataset}.jsonl")
        if not os.path.exists(dataset_path):
            print(f"Error: Dataset file does not exist: {dataset_path}")
            sys.exit(1)

        self.dataset = []
        with open(dataset_path, 'r') as f:
            for line in f:
                data = json.loads(line)
                self.dataset.append(data['prompt'])

        print(f"Loaded {len(self.dataset)} prompts from {dataset}.jsonl")


if __name__ == "__main__":
    model = sys.argv[1]
    dataset = sys.argv[2]
    callback_url = sys.argv[3]

    print(f"Starting ModelRunner with model={model}, dataset={dataset}, callback_url={callback_url}")

    runner = ModelRunner(model, dataset, callback_url)
    runner.measure()