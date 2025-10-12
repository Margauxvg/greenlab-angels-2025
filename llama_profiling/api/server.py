from http.server import HTTPServer, BaseHTTPRequestHandler
from llama_profiling.api.lifecycle import LifecycleController
import sys
import signal
import json

class Router(BaseHTTPRequestHandler):
    lc = LifecycleController()

    def do_POST(self):
        if self.path == '/start':
            self.lc.start(self.get_body())
            self.default_headers()
        else:
            self.not_found()

    def get_body(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        return json.loads(post_data.decode('utf-8'))

    def default_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps({"status": "ok"}).encode())

    def not_found(self):
        self.send_response(404)
        self.end_headers()

class APIServer:
    port: int = 9999

    def __init__(self):
        server_address = ('', self.port)
        self.httpd = HTTPServer(server_address, Router)

        signal.signal(signal.SIGTERM, self.signal_handler)
        signal.signal(signal.SIGINT, self.signal_handler)


    def listen(self):
        print(f"Server running on http://localhost:{self.port}")
        self.httpd.serve_forever()

    def signal_handler(self, signum, frame):
        print(f"\nReceived signal {signum}, shutting down server...")
        self.httpd.server_close()
        print("Server shutdown complete")
        sys.exit(0)
