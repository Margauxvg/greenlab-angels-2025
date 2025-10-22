#!/usr/bin/env python3
"""
Script to create a clean dataset with 100 samples from each task type.

Code generated with assistance from Claude Code (Claude Sonnet 4.5)
Anthropic. (2025). Claude Code. https://docs.claude.com/en/docs/claude-code
Prompt:
could you please look at the data directory in llama_profiling.\
\
Your task is to create a new "clean_data" directory, and go over each task type in the data directory and select 100 samples from each task type, and 
put it into the new clean directory
"""
import os

# Define paths
data_dir = "llama_profiling/data"
clean_dir = "llama_profiling/clean_data"

# Get all task type directories (excluding hidden files)
task_types = [d for d in os.listdir(data_dir)
              if os.path.isdir(os.path.join(data_dir, d)) and not d.startswith('.')]

print(f"Found {len(task_types)} task types: {', '.join(task_types)}")

# Process each task type
for task_type in task_types:
    print(f"\nProcessing {task_type}...")

    # Create task directory in clean_data
    clean_task_dir = os.path.join(clean_dir, task_type)
    os.makedirs(clean_task_dir, exist_ok=True)

    # Read from train.jsonl (or first available file)
    source_dir = os.path.join(data_dir, task_type)

    # Try to read from train.jsonl first, then other files, or task-named file
    source_file = None
    for filename in ['train.jsonl', 'test.jsonl', 'val.jsonl', f'{task_type}.jsonl']:
        potential_file = os.path.join(source_dir, filename)
        if os.path.exists(potential_file):
            source_file = potential_file
            break

    if not source_file:
        print(f"  Warning: No JSONL file found for {task_type}, skipping...")
        continue

    # Read and select first 80 samples
    samples = []
    with open(source_file, 'r', encoding='utf-8') as f:
        for i, line in enumerate(f):
            if i >= 80:
                break
            samples.append(line.strip())

    # Write to clean_data
    output_file = os.path.join(clean_task_dir, 'data.jsonl')
    with open(output_file, 'w', encoding='utf-8') as f:
        for sample in samples:
            f.write(sample + '\n')

    print(f"  ✓ Created {output_file} with {len(samples)} samples")

print(f"\n✓ Done! Clean dataset created in {clean_dir}/")
