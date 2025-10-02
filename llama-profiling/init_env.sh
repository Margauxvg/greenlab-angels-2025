#!/bin/sh

# setup the data and required CLI
python llama-profiling/download_glue_data.py --data_dir llama-profiling/glue_data --tasks CoLA
pip install -U "huggingface_hub[cli]"

# Install models
hf auth login --token $HF_TOKEN

if [ ! -d "llama-profiling/models/3.2-1B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-3.2-1B-Instruct --exlude original --local-dir llama-profiling/models/3.2-1B
else
    echo "Model already exists at llama-profiling/models/3.2-1B. Skipping download."
fi
