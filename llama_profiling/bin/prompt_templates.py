"""
Prompt templates for each SuperGLUE task type.

Each template is designed to evaluate LLMs on the specific task.
Templates use {{variable}} syntax for field substitution.

Code generated with assistance from Claude Code (Claude Sonnet 4.5)
Anthropic. (2025). Claude Code. https://docs.claude.com/en/docs/claude-code
Prompt:
Now that we have the "clean_data" directory, I would like to construct appropriate LLM prompts based on the structure of each task type. Please look 
at the structure of each task type - additionally if you have context of these tasks already that is great - and think about how they could be 
structured as an appropriate evaluation prompt for LLMs.\
\
E.g. looking at the COPA type, there are two choices, a premise and the question (cause or effect). It could be structured as:\
\
What is the {{question}} of {{premise}}. {{choice1}} or {{choice2}}?
"""

PROMPT_TEMPLATES = {
    "BoolQ": {
        "description": "Boolean Questions - Answer yes/no questions based on a passage",
        "template": """Passage: {{passage}}

Question: {{question}}

Explain your choice.""",
        "fields": ["passage", "question"],
        "output_type": "boolean",
        "answer_format": "True or False"
    },

    "COPA": {
        "description": "Choice of Plausible Alternatives - Select the most plausible cause or effect",
        "template": """What is the {{question}} of the following?

Premise: {{premise}}

Choice 1: {{choice1}}
Choice 2: {{choice2}}

Explain your choice.""",
        "fields": ["premise", "choice1", "choice2", "question"],
        "output_type": "choice",
        "answer_format": "1 or 2"
    },

    "CB": {
        "description": "CommitmentBank - Determine if hypothesis is entailed by premise",
        "template": """Premise: {{premise}}

Hypothesis: {{hypothesis}}

Does the premise entail the hypothesis? Explain your choice.""",
        "fields": ["premise", "hypothesis"],
        "output_type": "classification",
        "answer_format": "entailment, contradiction, or neutral"
    },

    "RTE": {
        "description": "Recognizing Textual Entailment - Determine if hypothesis follows from premise",
        "template": """Premise: {{premise}}

Hypothesis: {{hypothesis}}

Does the hypothesis follow from the premise? Explain your choice.""",
        "fields": ["premise", "hypothesis"],
        "output_type": "classification",
        "answer_format": "entailment or not_entailment"
    },

    "WiC": {
        "description": "Words in Context - Determine if a word has the same meaning in two sentences",
        "template": """Does the word "{{word}}" have the same meaning in the following two sentences?

Sentence 1: {{sentence1}}
Sentence 2: {{sentence2}}

Explain your choice.""",
        "fields": ["word", "sentence1", "sentence2"],
        "output_type": "boolean",
        "answer_format": "True or False"
    },

    "WSC": {
        "description": "Winograd Schema Challenge - Determine pronoun coreference",
        "template": """{{text}}

In the sentence above, does "{{span2_text}}" refer to "{{span1_text}}"? Explain your choice.""",
        "fields": ["text", "span1_text", "span2_text"],
        "output_type": "boolean",
        "answer_format": "True or False"
    },

    "MultiRC": {
        "description": "Multi-sentence Reading Comprehension - Answer questions about a passage (multiple answers may be correct)",
        "template": """Passage: {{passage}}

Question: {{question}}

Answer: {{answer}}

Is this answer correct? Explain your choice.""",
        "fields": ["passage", "question", "answer"],
        "output_type": "boolean",
        "answer_format": "True or False",
        "note": "Each question-answer pair should be evaluated separately"
    },

    "ReCoRD": {
        "description": "Reading Comprehension with Commonsense Reasoning - Fill in the blank with entity from passage",
        "template": """Passage: {{passage}}

Question: {{query}}

Based on the passage, what word or phrase should replace @placeholder? Explain your choice.""",
        "fields": ["passage", "query"],
        "output_type": "extraction",
        "answer_format": "Entity text from passage"
    },
}


def get_prompt(task_type, data):
    """
    Generate a prompt for a given task type and data sample.

    Args:
        task_type: The task type (e.g., "BoolQ", "COPA")
        data: Dictionary containing the data fields

    Returns:
        str: The formatted prompt
    """
    if task_type not in PROMPT_TEMPLATES:
        raise ValueError(f"Unknown task type: {task_type}")

    template_info = PROMPT_TEMPLATES[task_type]
    template = template_info["template"]

    # Replace template variables with actual data
    prompt = template
    for field in template_info["fields"]:
        if field in data:
            prompt = prompt.replace(f"{{{{{field}}}}}", str(data[field]))

    return prompt


# Example usage
if __name__ == "__main__":
    import json

    # Example for BoolQ
    boolq_sample = {
        "question": "do iran and afghanistan speak the same language",
        "passage": "Persian language -- Persian is one of the Western Iranian languages..."
    }

    print("=== BoolQ Example ===")
    print(get_prompt("BoolQ", boolq_sample))
    print()

    # Example for COPA
    copa_sample = {
        "premise": "My body cast a shadow over the grass.",
        "choice1": "The sun was rising.",
        "choice2": "The grass was cut.",
        "question": "cause"
    }

    print("=== COPA Example ===")
    print(get_prompt("COPA", copa_sample))
    print()

    # Print all task descriptions
    print("=== All Task Descriptions ===")
    for task, info in PROMPT_TEMPLATES.items():
        print(f"{task}: {info['description']}")
        print(f"  Answer format: {info['answer_format']}")
        print()
