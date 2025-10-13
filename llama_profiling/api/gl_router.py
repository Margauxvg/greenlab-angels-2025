from http.server import BaseHTTPRequestHandler
import json

from llama_profiling.api.lifecycle import LifecycleController

class GLRouter(BaseHTTPRequestHandler):
    lc = LifecycleController()

    def do_POST(self):
        if self.path == '/start':
            self.lc.start(self.get_body())
            self.default_headers()
        elif self.path == '/stop':
            self.lc.stop()
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
