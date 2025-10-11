# LLM Evaluation Prompt Templates for SuperGLUE Tasks

This guide provides prompt templates for evaluating LLMs on each SuperGLUE task type in the `clean_data` directory.

## Task Types Overview

| Task | Type | Answer Format |
|------|------|---------------|
| BoolQ | Boolean QA | True/False |
| COPA | Multiple Choice | 1 or 2 |
| CB | Entailment | entailment/contradiction/neutral |
| RTE | Entailment | entailment/not_entailment |
| WiC | Boolean | True/False |
| WSC | Boolean | True/False |
| MultiRC | Boolean | True/False (per answer) |
| ReCoRD | Extraction | Entity text |
| AX-b | Entailment | entailment/not_entailment |
| AX-g | Entailment | entailment/not_entailment |

---

## 1. BoolQ (Boolean Questions)

**Description:** Answer yes/no questions based on a passage.

**Data Fields:**
- `question`: The question to answer
- `passage`: Context passage
- `label`: Ground truth (true/false)

**Prompt Template:**
```
Passage: {{passage}}

Question: {{question}}

Answer with only 'True' or 'False'.
```

**Example:**
```
Passage: Persian language -- Persian is one of the Western Iranian languages within the Indo-Iranian branch of the Indo-European language family. It is primarily spoken in Iran, Afghanistan (officially known as Dari since 1958)...

Question: do iran and afghanistan speak the same language

Answer with only 'True' or 'False'.
```

---

## 2. COPA (Choice of Plausible Alternatives)

**Description:** Select the most plausible cause or effect given a premise.

**Data Fields:**
- `premise`: The situation
- `choice1`: First option
- `choice2`: Second option
- `question`: Either "cause" or "effect"
- `label`: Ground truth (0 or 1)

**Prompt Template:**
```
What is the {{question}} of the following?

Premise: {{premise}}

Choice 1: {{choice1}}
Choice 2: {{choice2}}

Answer with only '1' or '2'.
```

**Example:**
```
What is the cause of the following?

Premise: My body cast a shadow over the grass.

Choice 1: The sun was rising.
Choice 2: The grass was cut.

Answer with only '1' or '2'.
```

---

## 3. CB (CommitmentBank)

**Description:** Determine if a hypothesis is entailed by, contradicts, or is neutral to a premise.

**Data Fields:**
- `premise`: The premise sentence
- `hypothesis`: The hypothesis sentence
- `label`: Ground truth (entailment/contradiction/neutral)

**Prompt Template:**
```
Premise: {{premise}}

Hypothesis: {{hypothesis}}

Does the premise entail the hypothesis? Answer with only 'entailment', 'contradiction', or 'neutral'.
```

---

## 4. RTE (Recognizing Textual Entailment)

**Description:** Determine if a hypothesis follows from a premise.

**Data Fields:**
- `premise`: The premise sentence
- `hypothesis`: The hypothesis sentence
- `label`: Ground truth (entailment/not_entailment)

**Prompt Template:**
```
Premise: {{premise}}

Hypothesis: {{hypothesis}}

Does the hypothesis follow from the premise? Answer with only 'entailment' or 'not_entailment'.
```

---

## 5. WiC (Words in Context)

**Description:** Determine if a word has the same meaning in two different sentences.

**Data Fields:**
- `word`: The target word
- `sentence1`: First sentence
- `sentence2`: Second sentence
- `label`: Ground truth (true/false)

**Prompt Template:**
```
Does the word "{{word}}" have the same meaning in the following two sentences?

Sentence 1: {{sentence1}}
Sentence 2: {{sentence2}}

Answer with only 'True' or 'False'.
```

**Example:**
```
Does the word "place" have the same meaning in the following two sentences?

Sentence 1: Do you want to come over to my place later?
Sentence 2: A political system with no place for the less prominent groups.

Answer with only 'True' or 'False'.
```

---

## 6. WSC (Winograd Schema Challenge)

**Description:** Determine pronoun coreference - does a pronoun refer to a specific entity?

**Data Fields:**
- `text`: The sentence with pronoun
- `target.span1_text`: The entity
- `target.span2_text`: The pronoun
- `label`: Ground truth (true/false)

**Prompt Template:**
```
{{text}}

In the sentence above, does "{{span2_text}}" refer to "{{span1_text}}"? Answer with only 'True' or 'False'.
```

**Example:**
```
Mark told Pete many lies about himself, which Pete included in his book. He should have been more skeptical.

In the sentence above, does "He" refer to "Mark"? Answer with only 'True' or 'False'.
```

---

## 7. MultiRC (Multi-sentence Reading Comprehension)

**Description:** Answer questions about a passage. Multiple answers may be correct for each question.

**Data Fields:**
- `passage.text`: The passage
- `passage.questions[].question`: The question
- `passage.questions[].answers[].text`: Individual answer
- `passage.questions[].answers[].label`: Whether this answer is correct (0/1)

**Prompt Template (per answer):**
```
Passage: {{passage}}

Question: {{question}}

Answer: {{answer}}

Is this answer correct? Answer with only 'True' or 'False'.
```

**Note:** Each question may have multiple answers. Evaluate each answer separately.

---

## 8. ReCoRD (Reading Comprehension with Commonsense Reasoning)

**Description:** Fill in the blank (@placeholder) with an entity from the passage.

**Data Fields:**
- `passage.text`: The passage
- `qas[].query`: Question with @placeholder
- `qas[].answers[].text`: Ground truth entities

**Prompt Template:**
```
Passage: {{passage}}

Question: {{query}}

Based on the passage, what word or phrase should replace @placeholder? Answer with only the entity text.
```

---

## 9. AX-b (Broad-coverage Diagnostic)

**Description:** Textual entailment covering various linguistic phenomena.

**Data Fields:**
- `sentence1`: First sentence
- `sentence2`: Second sentence
- `label`: Ground truth (entailment/not_entailment)
- `logic`: Type of linguistic phenomenon

**Prompt Template:**
```
Sentence 1: {{sentence1}}

Sentence 2: {{sentence2}}

Does Sentence 1 entail Sentence 2? Answer with only 'entailment' or 'not_entailment'.
```

---

## 10. AX-g (Winogender Diagnostic)

**Description:** Gender bias evaluation through textual entailment.

**Data Fields:**
- `premise`: The premise sentence
- `hypothesis`: The hypothesis sentence
- `label`: Ground truth (entailment/not_entailment)

**Prompt Template:**
```
Premise: {{premise}}

Hypothesis: {{hypothesis}}

Does the premise entail the hypothesis? Answer with only 'entailment' or 'not_entailment'.
```

---

## Usage Notes

### Python Implementation
See `prompt_templates.py` for a programmatic implementation:

```python
from prompt_templates import get_prompt, PROMPT_TEMPLATES

# Get prompt for a specific task
data = {"question": "...", "passage": "..."}
prompt = get_prompt("BoolQ", data)
```

### Evaluation Strategy

1. **Format the prompt** using the template for each task type
2. **Send to LLM** for inference
3. **Parse the response** (extract only the answer in the specified format)
4. **Compare with ground truth** (`label` field in the data)
5. **Calculate accuracy** across all samples

### Answer Parsing
LLMs may include extra text. Extract only:
- For boolean: Look for "True" or "False"
- For choice: Look for "1" or "2"
- For entailment: Look for specific classification labels
- For extraction (ReCoRD): Compare extracted text with entity list

### Special Considerations

- **MultiRC**: Each question-answer pair should be evaluated separately. A question may have multiple correct answers.
- **ReCoRD**: The answer should be an entity that appears in the passage. Match against the entity list.
- **WSC**: Requires understanding of pronoun resolution and context.
- **COPA**: The question field indicates whether to find a "cause" or "effect".
