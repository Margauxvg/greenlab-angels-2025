import sys
from pathlib import Path

# Add experiment-runner to path to access Plugins module
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "experiment-runner"))

from Plugins.Profilers.EnergiBridge import EnergiBridge

class LifecycleController:
    def __init__(self):
        self.profiler = EnergiBridge(target_program=f"python llama_profiling/bin/run_model.py",
                                     out_file="energibridge.csv")

    def start(self, body):
        model = body["model"]
        cb_url = body["callback_url"]

        print(model)
        print(cb_url)