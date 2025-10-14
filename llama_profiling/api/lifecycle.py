import sys
import os
import time
from pathlib import Path
import threading

# Add experiment-runner to path to access Plugins module
import requests

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "experiment-runner"))

from Plugins.Profilers.EnergiBridge import EnergiBridge

class LifecycleController:
    def __init__(self):
        self.profiler = None
        self.stop_thread = None

    def start(self, body):
        model = body["model"]
        self.cb_url = body["callback_url"]
        dataset = body["dataset"]

        self.profiler = EnergiBridge(target_program=f"python llama_profiling/bin/model_runner.py {model} {dataset} http://localhost:9999/stop",
                                     out_file=Path("energibridge.csv"))

        self.profiler.start()

    def stop(self):
        # Initiate stop in background thread, don't block the HTTP response
        self.stop_thread = threading.Thread(target=self._stop_profiler)
        self.stop_thread.start()

    def _stop_profiler(self):
        if self.profiler:
            self.profiler.stop(wait=True)
            print("Profiler stopped successfully")

        self.send_callback()

    def send_callback(self, max_retries=5, initial_delay=1):
        delay = initial_delay
        payload = {
            'prompts_out.tsv': Path('prompts_out.tsv').read_text(),
            'energibridge.csv': Path('energibridge.csv').read_text(),
            'energibridge-summary.txt': Path('energibridge-summary.txt').read_text()
        }
        callback_url = self.cb_url

        for attempt in range(max_retries):
            try:
                response = requests.post(callback_url, timeout=10, json={"files": payload})
                response.raise_for_status()
                print(f"Callback sent successfully to {callback_url}")
                return
            except requests.exceptions.RequestException as e:
                if attempt == max_retries - 1:
                    print(f"Failed to send callback after {max_retries} attempts: {e}")
                    raise

                print(f"Callback attempt {attempt + 1} failed: {e}. Retrying in {delay}s...")
                time.sleep(delay)
                delay *= 2