#!/bin/sh

if [ -z "$1" ]; then
    echo "Error: HF_TOKEN not provided"
    echo "Usage: $0 <HF_TOKEN>"
    exit 1
fi

HF_TOKEN=$1

cd storage

# download the repo
if [ ! -d "greenlab-angels-2025" ]; then
    git clone https://github.com/Margauxvg/greenlab-angels-2025.git
fi

cd greenlab-angels-2025

python3 -m venv .venv
source .venv/bin/activate

# setup the data and required CLI
pip install -r requirements.txt
pip install -U "huggingface_hub[cli]"

# Install models
hf auth login --token $HF_TOKEN

if [ ! -d "llama-profiling/models/2-7B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-2-7b-chat-hf --local-dir llama-profiling/models/2-7B
else
    echo "Model already exists at llama-profiling/models/2-7B. Skipping download."
fi

if [ ! -d "llama-profiling/models/2-13B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-2-13b-chat-hf --local-dir llama-profiling/models/2-13B
else
    echo "Model already exists at llama-profiling/models/2-13B. Skipping download."
fi

if [ ! -d "llama-profiling/models/3-8B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Meta-Llama-3-8B-Instruct --local-dir llama-profiling/models/3-8B
else
    echo "Model already exists at llama-profiling/models/3-8B. Skipping download."
fi

if [ ! -d "llama-profiling/models/3.1-8B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Meta-Llama-3.1-8B-Instruct --local-dir llama-profiling/models/3.1-8B
else
    echo "Model already exists at llama-profiling/models/3.1-8B. Skipping download."
fi

if [ ! -d "llama-profiling/models/3.2-1B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-3.2-1B-Instruct --local-dir llama-profiling/models/3.2-1B
else
    echo "Model already exists at llama-profiling/models/3.2-1B. Skipping download."
fi

if [ ! -d "llama-profiling/models/3.2-3B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-3.2-3B-Instruct --local-dir llama-profiling/models/3.2-3B
else
    echo "Model already exists at llama-profiling/models/3.2-3B. Skipping download."
fi

if [ ! -d "llama-profiling/models/4-scout-17B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-4-Scout-17B-16E-Instruct --local-dir llama-profiling/models/4-scout-17B
else
    echo "Model already exists at llama-profiling/models/4-scout-17B. Skipping download."
fi

if [ ! -d "llama-profiling/models/4-maverick-17B" ]; then
    echo "Model not found locally. Downloading..."
    hf download meta-llama/Llama-4-Maverick-17B-128E-Instruct --local-dir llama-profiling/models/4-maverick-17B
else
    echo "Model already exists at llama-profiling/models/4-maverick-17B. Skipping download."
fi
