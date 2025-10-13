from llama_profiling.api.server import APIServer
from llama_profiling.api.gl_router import GLRouter

srv = APIServer(GLRouter)
srv.listen()