from http.server import HTTPServer
import sys
import signal

class APIServer:
    port: int = 9999

    def __init__(self, router):
        server_address = ('', self.port)
        self.httpd = HTTPServer(server_address, router)

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
