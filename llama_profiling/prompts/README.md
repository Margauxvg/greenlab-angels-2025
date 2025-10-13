# Prompts Directory

This directory contains pre-generated prompts for LLM evaluation on SuperGLUE tasks.

## File Format

All files are in **JSONL** (JSON Lines) format, with one prompt per line.

Each line contains a JSON object with the following structure:

```json
{
  "task_type": "BoolQ",
  "idx": 0,
  "prompt": "Passage: ...\n\nQuestion: ...\n\nAnswer with only 'True' or 'False'.",
  "label": true,
  "original_data": { ... }
}
```

### Fields

| Field           | Description                                       |
| --------------- | ------------------------------------------------- |
| `task_type`     | The SuperGLUE task name (e.g., "BoolQ", "COPA")   |
| `idx`           | Sample index from the original dataset            |
| `prompt`        | The formatted prompt text ready to send to an LLM |
| `label`         | Ground truth answer for evaluation                |
| `original_data` | The original data sample with all fields          |

### Special Cases

- **MultiRC**: Generates multiple prompts per sample (one per question-answer pair). Uses `question_idx` and `answer_idx` for tracking.
- **ReCoRD**: Generates multiple prompts per sample (one per query). Uses `qa_idx` for tracking. The `label` field is replaced with `answers` (a list of acceptable entity texts).

## Files

| File            | Prompts | Description                                     |
| --------------- | ------- | ----------------------------------------------- |
| `BoolQ.jsonl`   | 100     | Boolean questions based on passages             |
| `COPA.jsonl`    | 100     | Choice of plausible alternatives (cause/effect) |
| `CB.jsonl`      | 100     | Textual entailment (3-way classification)       |
| `RTE.jsonl`     | 100     | Textual entailment (2-way classification)       |
| `WiC.jsonl`     | 100     | Word sense disambiguation                       |
| `WSC.jsonl`     | 100     | Pronoun coreference resolution                  |
| `MultiRC.jsonl` | 5,985   | Multi-sentence reading comprehension            |
| `ReCoRD.jsonl`  | 154     | Cloze-style reading comprehension               |

**Total: 6,939 prompts**

## Usage

### Python Example

```python
import json

# Load and process prompts
with open('prompts/BoolQ.jsonl', 'r') as f:
    for line in f:
        data = json.loads(line)

        # Send prompt to your LLM
        prompt = data['prompt']
        response = your_llm_function(prompt)

        # Compare with ground truth
        ground_truth = data['label']
        correct = evaluate_response(response, ground_truth)
```

### Evaluation Flow

1. **Load prompt**: Read a line from the JSONL file
2. **Send to LLM**: Use the `prompt` field as input
3. **Parse response**: Extract the answer from the LLM output
4. **Compare**: Match against the `label` field
5. **Calculate metrics**: Aggregate accuracy across all prompts

### Answer Formats by Task

| Task    | Expected Answer Format                      |
| ------- | ------------------------------------------- |
| BoolQ   | "True" or "False"                           |
| COPA    | "1" or "2"                                  |
| CB      | "entailment", "contradiction", or "neutral" |
| RTE     | "entailment" or "not_entailment"            |
| WiC     | "True" or "False"                           |
| WSC     | "True" or "False"                           |
| MultiRC | "True" or "False"                           |
| ReCoRD  | Entity text from passage                    |

## Regeneration

To regenerate all prompt files:

```bash
python3 bin/generate_prompts.py
```

This will recreate the entire `prompts/` directory from the `clean_data/` samples.

## Notes

- Prompts are designed to be concise and direct, with clear answer format instructions
- Each prompt is self-contained and includes all necessary context
- Labels are preserved from the original SuperGLUE datasets
- Use `original_data` field if you need access to additional metadata
