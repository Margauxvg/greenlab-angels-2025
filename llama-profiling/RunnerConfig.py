from http.server import HTTPServer, BaseHTTPRequestHandler

from EventManager.Models.RunnerEvents import RunnerEvents
from EventManager.EventSubscriptionController import EventSubscriptionController
from ConfigValidator.Config.Models.RunTableModel import RunTableModel
from ConfigValidator.Config.Models.FactorModel import FactorModel
from ConfigValidator.Config.Models.RunnerContext import RunnerContext
from ConfigValidator.Config.Models.OperationType import OperationType
from ExtendedTyping.Typing import SupportsStr
from ProgressManager.Output.OutputProcedure import OutputProcedure as output
from Plugins.Profilers.EnergiBridge import EnergiBridge

from typing import Dict, List, Any, Optional
from pathlib import Path
from os.path import dirname, realpath
import shutil
import subprocess
import signal
import requests
import time
import csv
import os


class HttpHandler(BaseHTTPRequestHandler):
    def __init__(self):
        self.webhook_called = False

    def do_POST(self):
        if self.path == '/run-finished':
            self.send_response(200)
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write("success".encode('utf-8'))
            self.webhook_called = True

class RunnerConfig:
    ROOT_DIR = Path(dirname(realpath(__file__)))

    # ================================ USER SPECIFIC CONFIG ================================
    """The name of the experiment."""
    name:                       str             = "llama_profiling"

    """The path in which Experiment Runner will create a folder with the name `self.name`, in order to store the
    results from this experiment. (Path does not need to exist - it will be created if necessary.)
    Output path defaults to the config file's path, inside the folder 'experiments'"""
    results_output_path:        Path            = ROOT_DIR / 'experiments'

    """Experiment operation type. Unless you manually want to initiate each run, use `OperationType.AUTO`."""
    operation_type:             OperationType   = OperationType.AUTO

    """The time Experiment Runner will wait after a run completes.
    This can be essential to accommodate for cooldown periods on some systems."""
    time_between_runs_in_ms:    int             = 1

    # Dynamic configurations can be one-time satisfied here before the program takes the config as-is
    # e.g. Setting some variable based on some criteria
    def __init__(self):
        """Executes immediately after program start, on config load"""

        EventSubscriptionController.subscribe_to_multiple_events([
            (RunnerEvents.BEFORE_EXPERIMENT, self.before_experiment),
            (RunnerEvents.BEFORE_RUN       , self.before_run       ),
            (RunnerEvents.START_RUN        , self.start_run        ),
            (RunnerEvents.START_MEASUREMENT, self.start_measurement),
            (RunnerEvents.INTERACT         , self.interact         ),
            (RunnerEvents.STOP_MEASUREMENT , self.stop_measurement ),
            (RunnerEvents.STOP_RUN         , self.stop_run         ),
            (RunnerEvents.POPULATE_RUN_DATA, self.populate_run_data),
            (RunnerEvents.AFTER_EXPERIMENT , self.after_experiment )
        ])
        self.run_table_model = None  # Initialized later

        output.console_log("Custom config loaded")

    def create_run_table_model(self) -> RunTableModel:
        """Create and return the run_table model here. A run_table is a List (rows) of tuples (columns),
        representing each run performed"""
        generation = FactorModel("generation", ['1', '2', '3', '3.1', '3.2', '4-scout', '4-maverick'])
        model_size = FactorModel("model_size", ['1B', '3B', '6.7B', '8B', '11B', '13B', '17B'])
        tasks = FactorModel("task", ["BoolQ", "CB", "COPA", "MultiRC", "ReCoRD", "RTE", "WiC", "WSC"])
        self.run_table_model = RunTableModel(
            factors=[generation, model_size, tasks],
            shuffle=True,
            exclude_combinations=[
                {generation: ['1'], model_size: ['1B', '3B', '8B', '11B', '17B']}, # only run 6.7B and 13B
                {generation: ['2'], model_size: ['1B', '3B', '8B', '11B', '17B']}, # only run 6.7B and 13B
                {generation: ['3'], model_size: ['1B', '3B', '6.7B', '11B', '13B', '17B']}, # only run 8B
                {generation: ['3.1'], model_size: ['1B', '3B', '6.7B', '11B', '13B', '17B']}, # only run 8B
                {generation: ['3.2'], model_size: ['6.7B', '8B', '13B', '17B']}, # only run 1B, 3B and 11B
                {generation: ['4-scout'], model_size: ['1B', '3B', '6.7B', '8B', '11B', '13B']}, # only run 17B
                {generation: ['4-maverick'], model_size: ['1B', '3B', '6.7B', '8B', '11B', '13B']}, # only run 17B
            ],
            data_columns=[]
        )
        return self.run_table_model

    def before_experiment(self) -> None:
        """Perform any activity required before starting the experiment here
        Invoked only once during the lifetime of the program."""

        hf_token = os.getenv("HF_TOKEN")
        # To run over ssh
        with open("./llama-profiling/init_env.sh") as script:
            body = script.read()
            result = subprocess.run(
                ["ssh", "angels@91.99.79.179", "bash", "-s", hf_token],
                input=body,
                text=True,
                check=True,
            )

        # Testing locally:
        # result = subprocess.run(['./llama-profiling/init_env.sh', hf_token], check=True)

    def before_run(self) -> None:
        """Perform any activity required before starting a run.
        No context is available here as the run is not yet active (BEFORE RUN)"""

        output.console_log("Config.before_run() called!")

    def start_run(self, context: RunnerContext) -> None:
        """Perform any activity required for starting the run here.
        For example, starting the target system to measure.
        Activities after starting the run should also be performed here."""

        output.console_log("Config.start_run() called!")

    def start_measurement(self, context: RunnerContext) -> None:
        """Perform any activity required for starting measurements."""

        generation = context.execute_run["generation"]
        parameters = context.execute_run["model_size"]

        self.profiler = EnergiBridge(target_program=f"python llama-profiling/run_model.py {generation} {parameters}",
                                     out_file=context.run_dir / "energibridge.csv")

        self.start_server(4448)
        self.profiler.start()


    def start_server(self, port=4448):
        server_address = ('', port)
        self.httpd = HTTPServer(server_address, HttpHandler)
        print(f"Server running on http://localhost:{port}")
        print("Send POST requests to /run-finished")
        self.httpd.serve_forever()

    def interact(self, context: RunnerContext) -> None:
        """Perform any interaction with the running target system here, or block here until the target finishes."""
        # The server isn't immediately online, this polls until it is successful
        self.wait_for_server()

        inputs = self.read_tsv("./llama-profiling/glue_data/CoLA/test.tsv")
        for e in inputs:
            #start_time = time.time() * 1000  # Convert to milliseconds
            #out = self.run_prompt(e["sentence"])
            #end_time = time.time() * 1000  # Convert to milliseconds
            #duration_ms = end_time - start_time
            #output.console_log(f"Prompt execution took {duration_ms:.2f} ms")
            #output.console_log(out)

            # Perform next run
            # call greenlab server endpoint

            # Waiting for run to complete
            while not self.httpd.webhook_called:
                time.sleep(10)
            self.httpd.webhook_called = False


    def run_prompt(self, prompt_text: str):
        # get the current unix timestamp
        response = requests.post("http://localhost:9999/prompt", data=prompt_text)
        return response.text

    def read_tsv(self, file_path: str) -> List[Dict[str, str]]:
        """Read a TSV file and return the data as a list of dictionaries."""
        data = []
        with open(file_path, 'r', encoding='utf-8') as file:
            reader = csv.DictReader(file, delimiter='\t')
            for row in reader:
                data.append(dict(row))
        return data

    def stop_measurement(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping measurements."""

        # # Send SIGTERM to server process on port 9999
        try:
            result = subprocess.run(['sudo', 'lsof', '-ti', ':9999'], capture_output=True, text=True)
            if result.stdout.strip():
                pid = int(result.stdout.strip())
                output.console_log(f"Sending SIGTERM to server process {pid}")
                subprocess.run(['sudo', 'kill', '-TERM', str(pid)])
            else:
                output.console_log("No process found on port 9999")
        except Exception as e:
            output.console_log(f"Error stopping server: {e}")

        stdout = self.profiler.stop(wait=True)

    def stop_run(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping the run.
        Activities after stopping the run should also be performed here."""

        output.console_log("Config.stop_run() called!")

    def populate_run_data(self, context: RunnerContext) -> Optional[Dict[str, SupportsStr]]:
        """Parse and process any measurement data here.
        You can also store the raw measurement data under `context.run_dir`
        Returns a dictionary with keys `self.run_table_model.data_columns` and their values populated"""

        eb_log, eb_summary = self.profiler.parse_log(self.profiler.logfile, 
                                                     self.profiler.summary_logfile)

        return {"energy": 0}

    def after_experiment(self) -> None:
        """Perform any activity required after stopping the experiment here
        Invoked only once during the lifetime of the program."""

        output.console_log("Config.after_experiment() called!")
        # shutil.rmtree("llama-profiling/experiments")

    def wait_for_server(self) -> None:
        max_attempts = 12  # 12 attempts * 5 seconds = 60 seconds
        for attempt in range(max_attempts):
            try:
                requests.post("http://localhost:9999/prompt", timeout=5)
                output.console_log("Successfully reached server")
                break
            except requests.exceptions.ConnectionError:
                if attempt < max_attempts - 1:
                    time.sleep(5)
                else:
                    output.console_log("Server not available on port 9999 after 60 seconds")

    # ================================ DO NOT ALTER BELOW THIS LINE ================================
    experiment_path:            Path             = None
