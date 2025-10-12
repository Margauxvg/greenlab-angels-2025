from llama_profiling.api.server import APIServer, Router

srv = APIServer(Router)
srv.listen()