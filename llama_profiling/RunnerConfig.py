import json
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
    def do_POST(self):
        if self.path == '/run-finished':
            print('Webhook invoked, saving files...')
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)

            try:
                body = json.loads(post_data)
            except json.JSONDecodeError:
                self.send_response(400)
                self.end_headers()
                self.wfile.write(b"Invalid JSON")
                return

            files = body.get("files")
            if not isinstance(files, dict):
                self.send_response(400)
                self.end_headers()
                self.wfile.write(b"'files' must be a JSON object")
                return

            # Write each key/value pair to a file
            for filename, content in files.items():
                with open('tmp_' + filename, "w", encoding="utf-8") as f:
                    f.write(content)
                    print('Saved ', filename)

            self.send_response(200)
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write("success".encode('utf-8'))
            print('Webhook successful')
            self.server.webhook_received = True
        else:
            print("404 - incorrect endpoint")

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
    time_between_runs_in_ms:    int             = 30_000

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
        generation = FactorModel("generation", ['2', '3.2'])
        model_size = FactorModel("model_size", ['1B', '7B'])
        tasks = FactorModel("task", ["BoolQ", "CB"])
        self.run_table_model = RunTableModel(
            factors=[generation, model_size, tasks],
            shuffle=True,
            exclude_combinations=[
                {generation: ['2'], model_size: ['1B', '3B', '8B', '11B', '17B']}, # only run 7B and 13B
                {generation: ['3'], model_size: ['1B', '3B', '7B', '11B', '13B', '17B']}, # only run 8B
                {generation: ['3.1'], model_size: ['1B', '3B', '7B', '11B', '13B', '17B']}, # only run 8B
                {generation: ['3.2'], model_size: ['7B', '8B', '13B', '17B']}, # only run 1B, 3B and 11B
                {generation: ['4-scout'], model_size: ['1B', '3B', '7B', '8B', '11B', '13B']}, # only run 17B
                {generation: ['4-maverick'], model_size: ['1B', '3B', '7B', '8B', '11B', '13B']}, # only run 17B
            ],
            data_columns=['energy']
        )
        return self.run_table_model

    def before_experiment(self) -> None:
        """Perform any activity required before starting the experiment here
        Invoked only once during the lifetime of the program."""

        # hf_token = os.getenv("HF_TOKEN")
        # # To run over ssh
        # with open("./llama-profiling/bin/init_env.sh") as script:
        #     body = script.read()
        #     result = subprocess.run(
        #         ["ssh", "angels@91.99.79.179", "bash", "-s", hf_token],
        #         input=body,
        #         text=True,
        #         check=True,
        #     )

        # Testing locally:
        # result = subprocess.run(['./llama-profiling/bin/init_env.sh', hf_token], check=True)

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
        task = context.execute_run["task"]

        data = {
            "model": f"{generation}-{parameters}",
            "dataset": task,
            "callback_url": "http://138.199.215.33:4448/run-finished"
        }

        requests.post("http://localhost:8020/start", json=data)

        self.start_server(4448)


    def start_server(self, port=4448):
        server_address = ('', port)
        self.httpd = HTTPServer(server_address, HttpHandler)
        self.httpd.webhook_received = False
        print(f"Server running on http://localhost:{port}")
        print("Send POST requests to /run-finished")

    def interact(self, context: RunnerContext) -> None:
        """Perform any interaction with the running target system here, or block here until the target finishes."""

        while not self.httpd.webhook_received:
            self.httpd.handle_request()
        print("Webhook has been called, progressing to next run")
        self.httpd.webhook_received = False

    def stop_measurement(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping measurements."""

    def stop_run(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping the run.
        Activities after stopping the run should also be performed here."""

        output.console_log("Config.stop_run() called!")

    def populate_run_data(self, context: RunnerContext) -> Optional[Dict[str, SupportsStr]]:
        """Parse and process any measurement data here.
        You can also store the raw measurement data under `context.run_dir`
        Returns a dictionary with keys `self.run_table_model.data_columns` and their values populated"""

        prefix = 'tmp_'
        eb_log = 'energibridge.csv'
        eb_summary = 'energibridge-summary.txt'
        p_out = 'prompts_out.tsv'

        profiler_logfile = context.run_dir / eb_log
        profiler_summary = context.run_dir / eb_summary
        prompts_out = context.run_dir / p_out

        shutil.copyfile(prefix + eb_log, profiler_logfile)
        shutil.copyfile(prefix + eb_summary, profiler_summary)
        shutil.copyfile(prefix + p_out, prompts_out)

        eb_log, eb_summary = EnergiBridge.parse_log(profiler_logfile, 
                                                     profiler_summary)


        return {"energy": eb_summary['total_joules']}

    def after_experiment(self) -> None:
        """Perform any activity required after stopping the experiment here
        Invoked only once during the lifetime of the program."""

        output.console_log("Config.after_experiment() called!")
        # shutil.rmtree("llama-profiling/experiments")

    # ================================ DO NOT ALTER BELOW THIS LINE ================================
    experiment_path:            Path             = None
