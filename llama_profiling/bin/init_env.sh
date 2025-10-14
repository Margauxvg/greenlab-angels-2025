#!/bin/sh

  HF_TOKEN="hf_xxxxx"

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

  if [ ! -d "llama_profiling/models/2-7B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Llama-2-7b --local-dir llama_profiling/models/2-7B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/2-7B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/2-13B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Llama-2-13b --local-dir llama_profiling/models/2-13B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/2-13B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/3-8B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Meta-Llama-3-8B --local-dir llama_profiling/models/3-8B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/3-8B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/3.1-8B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Meta-Llama-3.1-8B --local-dir llama_profiling/models/3.1-8B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/3.1-8B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/3.2-1B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Llama-3.2-1B --local-dir llama_profiling/models/3.2-1B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/3.2-1B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/3.2-3B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Llama-3.2-3B --local-dir llama_profiling/models/3.2-3B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/3.2-3B. Skipping download."
  fi

  if [ ! -d "llama_profiling/models/4-scout-17B" ]; then
      echo "Model not found locally. Downloading..."
      hf download meta-llama/Llama-4-Scout-17B-16E --local-dir llama_profiling/models/4-scout-17B --exclude "original/*"
  else
      echo "Model already exists at llama_profiling/models/4-scout-17B. Skipping download."
  fi