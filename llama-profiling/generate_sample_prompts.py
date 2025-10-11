#!/usr/bin/env python3
"""
Script to generate sample prompts from the clean_data directory.
This demonstrates how to use the prompt templates with actual data.

TODO::CITE CLAUDE

Now that we have the "clean_data" directory, I would like to construct appropriate LLM prompts based on the structure of each task type. Please look 
at the structure of each task type - additionally if you have context of these tasks already that is great - and think about how they could be 
structured as an appropriate evaluation prompt for LLMs.\
\
E.g. looking at the COPA type, there are two choices, a premise and the question (cause or effect). It could be structured as:\
\
What is the {{question}} of {{premise}}. {{choice1}} or {{choice2}}?
"""
import json
import os
from prompt_templates import get_prompt, PROMPT_TEMPLATES


def load_sample(task_type, sample_index=0):
    """Load a sample from the clean_data directory."""
    file_path = f"clean_data/{task_type}/data.jsonl"

    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Data file not found: {file_path}")

    with open(file_path, 'r', encoding='utf-8') as f:
        for i, line in enumerate(f):
            if i == sample_index:
                return json.loads(line)

    raise IndexError(f"Sample index {sample_index} not found in {file_path}")


def generate_prompt_for_task(task_type, sample_index=0):
    """Generate a prompt for a specific task type."""
    data = load_sample(task_type, sample_index)

    # Special handling for different task types
    if task_type == "WSC":
        # Extract span texts from target
        data['span1_text'] = data['target']['span1_text']
        data['span2_text'] = data['target']['span2_text']

    elif task_type == "MultiRC":
        # For MultiRC, we need to create prompts for each question-answer pair
        passage = data['passage']['text']
        prompts = []

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
                prompts.append({
                    'prompt': prompt,
                    'label': answer_obj['label']
                })

        return prompts

    elif task_type == "ReCoRD":
        # For ReCoRD, use the first query
        passage = data['passage']['text']
        query = data['qas'][0]['query']

        data = {
            'passage': passage,
            'query': query
        }

    prompt = get_prompt(task_type, data)
    return prompt


def main():
    """Generate and display sample prompts for all task types."""

    print("=" * 80)
    print("SAMPLE PROMPTS FOR EACH TASK TYPE")
    print("=" * 80)
    print()

    for task_type in PROMPT_TEMPLATES.keys():
        try:
            print(f"\n{'=' * 80}")
            print(f"TASK: {task_type}")
            print(f"Description: {PROMPT_TEMPLATES[task_type]['description']}")
            print(f"{'=' * 80}\n")

            result = generate_prompt_for_task(task_type, sample_index=0)

            # Handle MultiRC which returns multiple prompts
            if task_type == "MultiRC":
                print(f"Note: MultiRC generates multiple prompts per sample.")
                print(f"Showing first 2 question-answer pairs:\n")
                for i, item in enumerate(result[:2]):
                    print(f"--- Prompt {i+1} ---")
                    print(item['prompt'])
                    print(f"\nGround Truth: {'True' if item['label'] == 1 else 'False'}")
                    print()
            else:
                print(result)

            print()

        except Exception as e:
            print(f"Error generating prompt for {task_type}: {e}")
            print()

    print("\n" + "=" * 80)
    print("To use these prompts:")
    print("1. Format the prompt with your data")
    print("2. Send to your LLM")
    print("3. Parse the response")
    print("4. Compare with the 'label' field in the data")
    print("=" * 80)


if __name__ == "__main__":
    main()
