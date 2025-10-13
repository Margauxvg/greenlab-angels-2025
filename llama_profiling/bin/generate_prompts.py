#!/usr/bin/env python3
"""
Script to generate prompt files for each task type.
Creates a 'prompts' directory with JSONL files containing prompts and labels.

TODO::CITE CLAUDE

Now using the scripts you have written, could you create a new "prompt" directory which has a file for each task type and each file should contain 
the 100 prompts for the type. Use an appropriate file format.
"""
import json
import os
from prompt_templates import get_prompt, PROMPT_TEMPLATES


def generate_prompts_for_task(task_type, output_dir="prompts"):
    """
    Generate prompts for all samples in a task type.

    Args:
        task_type: The task type (e.g., "BoolQ", "COPA")
        output_dir: Directory to save the prompt files

    Returns:
        int: Number of prompts generated
    """
    input_file = f"llama_profiling/clean_data/{task_type}/data.jsonl"
    output_file = f"{output_dir}/{task_type}.jsonl"

    if not os.path.exists(input_file):
        print(f"  Warning: {input_file} not found, skipping...")
        return 0

    prompts_generated = 0

    with open(input_file, 'r', encoding='utf-8') as infile, \
         open(output_file, 'w', encoding='utf-8') as outfile:

        for line_num, line in enumerate(infile):
            data = json.loads(line)

            # Handle different task types
            if task_type == "WSC":
                # Extract span texts from target for WSC
                prompt_data = {
                    'text': data['text'],
                    'span1_text': data['target']['span1_text'],
                    'span2_text': data['target']['span2_text']
                }
                prompt = get_prompt(task_type, prompt_data)

                output_record = {
                    'task_type': task_type,
                    'idx': data['idx'],
                    'prompt': prompt,
                    'label': data['label'],
                    'original_data': data
                }
                outfile.write(json.dumps(output_record) + '\n')
                prompts_generated += 1

            elif task_type == "MultiRC":
                # For MultiRC, create one prompt per question-answer pair
                passage = data['passage']['text']

                for question_obj in data['passage']['questions']:
                    question = question_obj['question']

                    for answer_obj in question_obj['answers']:
                        answer = answer_obj['text']

                        prompt_data = {
                            'passage': passage,
                            'question': question,
                            'answer': answer
                        }
                        prompt = get_prompt(task_type, prompt_data)

                        output_record = {
                            'task_type': task_type,
                            'idx': data['idx'],
                            'question_idx': question_obj['idx'],
                            'answer_idx': answer_obj['idx'],
                            'prompt': prompt,
                            'label': answer_obj['label'],
                            'original_data': {
                                'passage': passage,
                                'question': question,
                                'answer': answer
                            }
                        }
                        outfile.write(json.dumps(output_record) + '\n')
                        prompts_generated += 1

            elif task_type == "ReCoRD":
                # For ReCoRD, create one prompt per query
                passage = data['passage']['text']

                for qa in data['qas']:
                    query = qa['query']

                    prompt_data = {
                        'passage': passage,
                        'query': query
                    }
                    prompt = get_prompt(task_type, prompt_data)

                    # Extract answer texts
                    answer_texts = [ans['text'] for ans in qa['answers']]

                    output_record = {
                        'task_type': task_type,
                        'idx': data['idx'],
                        'qa_idx': qa['idx'],
                        'prompt': prompt,
                        'answers': answer_texts,  # List of acceptable answers
                        'original_data': {
                            'passage': passage,
                            'query': query
                        }
                    }
                    outfile.write(json.dumps(output_record) + '\n')
                    prompts_generated += 1

            else:
                # Standard handling for other tasks
                prompt = get_prompt(task_type, data)

                output_record = {
                    'task_type': task_type,
                    'idx': data['idx'],
                    'prompt': prompt,
                    'label': data['label'],
                    'original_data': data
                }
                outfile.write(json.dumps(output_record) + '\n')
                prompts_generated += 1

    return prompts_generated


def main():
    """Generate prompts for all task types."""

    # Create prompts directory
    output_dir = "llama_profiling/prompts"
    os.makedirs(output_dir, exist_ok=True)
    print(f"Created directory: {output_dir}/\n")

    total_prompts = 0
    task_summary = []

    # Generate prompts for each task type
    for task_type in PROMPT_TEMPLATES.keys():
        print(f"Generating prompts for {task_type}...")

        try:
            num_prompts = generate_prompts_for_task(task_type, output_dir)

            if num_prompts > 0:
                print(f"  ✓ Generated {num_prompts} prompts -> {output_dir}/{task_type}.jsonl")
                task_summary.append((task_type, num_prompts))
                total_prompts += num_prompts

        except Exception as e:
            print(f"  ✗ Error: {e}")

    # Print summary
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)

    for task_type, num_prompts in task_summary:
        print(f"{task_type:15s} : {num_prompts:4d} prompts")

    print("-" * 70)
    print(f"{'TOTAL':15s} : {total_prompts:4d} prompts")
    print("=" * 70)

    print(f"\nAll prompts saved to {output_dir}/ directory")
    print("\nFile format: JSONL with fields:")
    print("  - task_type: The task name")
    print("  - idx: Sample index from original data")
    print("  - prompt: The formatted prompt text")
    print("  - label: Ground truth answer")
    print("  - original_data: Original sample data")
    print("\nNote: MultiRC generates multiple prompts per sample (one per answer).")
    print("      ReCoRD generates multiple prompts per sample (one per query).")


if __name__ == "__main__":
    main()
