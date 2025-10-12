import sys
import os
import signal
from vllm import LLM, SamplingParams
from transformers import AutoTokenizer
from http.server import HTTPServer, BaseHTTPRequestHandler

def init_model(model_name, model_size):
    model_path = os.path.join(os.getcwd(), "llama_profiling", "models", f"{model_name}-{model_size}")

    # Check if model path exists
    if not os.path.exists(model_path):
        print(f"Error: Model path does not exist: {model_path}")
        sys.exit(1)

    print(f"Loading model from: {model_path}")

    # Initialize vLLM engine
    model = LLM(model=model_path, max_model_len=4096, max_num_batched_tokens=4096)
    tokenizer = AutoTokenizer.from_pretrained(model_path)

    return model, tokenizer

# Global variables for model and tokenizer
model = None
tokenizer = None

def run_prompt(prompt="What is the meaning of life?"):
    messages = [{"role": "user", "content": prompt}]
    formatted_prompt = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)

    sampling_params = SamplingParams(temperature=0.7, top_p=0.9)
    outputs = model.generate([formatted_prompt], sampling_params)
    return outputs[0].outputs[0].text

class PromptHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == '/prompt':
            content_length = int(self.headers['Content-Length'])
            prompt = self.rfile.read(content_length).decode('utf-8')

            try:
                response_text = run_prompt()

                self.send_response(200)
                self.send_header('Content-type', 'text/plain')
                self.end_headers()
                self.wfile.write(response_text.encode('utf-8'))

            except Exception as e:
                self.send_response(500)
                self.send_header('Content-type', 'text/plain')
                self.end_headers()
                self.wfile.write(str(e).encode('utf-8'))
        else:
            self.send_response(404)
            self.end_headers()

def start_server(port=9999):
    server_address = ('', port)
    httpd = HTTPServer(server_address, PromptHandler)

    def signal_handler(signum, frame):
        print(f"\nReceived signal {signum}, shutting down server...")
        httpd.shutdown()
        print("Server shutdown complete")
        sys.exit(0)

    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)

    print(f"Server running on http://localhost:{port}")
    print("Send POST requests to /prompt with plain text in the body")
    httpd.serve_forever()

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python run_model.py <model_name> <model_size>")
        print("Example: python run_model.py 3.2 1B")
        sys.exit(1)

    model_name = sys.argv[1]
    model_size = sys.argv[2]

    model, tokenizer = init_model(model_name, model_size)
    start_server()