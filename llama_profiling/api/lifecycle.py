import sys
import os
from pathlib import Path
import threading

# Add experiment-runner to path to access Plugins module
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "experiment-runner"))

from Plugins.Profilers.EnergiBridge import EnergiBridge

class LifecycleController:
    def __init__(self):
        self.profiler = None
        self.stop_thread = None

    def start(self, body):
        model = body["model"]
        cb_url = body["callback_url"]
        dataset = body["dataset"]

        self.profiler = EnergiBridge(target_program=f"python llama_profiling/bin/model_runner.py {model} {dataset} {cb_url}",
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
