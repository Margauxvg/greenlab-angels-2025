
import sys
from pathlib import Path
from llama_profiling.api.server import APIServer

# Adds the experiment-runner module to the path
# so it can be more easily referenced.
sys.path.insert(0, str(Path(__file__).parent / "experiment-runner"))

from Plugins.Profilers.EnergiBridge import EnergiBridge

srv = APIServer()
srv.listen()