from http.server import SimpleHTTPRequestHandler, HTTPServer
import argparse

# 解析命令行参数
parser = argparse.ArgumentParser(description="Simple static server (no cache)")
parser.add_argument("--dir", default=".", help="Root directory to serve")
parser.add_argument("--port", type=int, default=8000, help="Port to listen on")
args = parser.parse_args()

# 自定义无缓存 Handler
class NoCacheHandler(SimpleHTTPRequestHandler):
    def __init__(self, *handler_args, **handler_kwargs):
        super().__init__(*handler_args, directory=args.dir, **handler_kwargs)

    def end_headers(self):
        self.send_header("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        self.send_header("Pragma", "no-cache")
        self.send_header("Expires", "0")
        super().end_headers()

# 启动服务
if __name__ == "__main__":
    print(f"Serving {args.dir} on http://0.0.0.0:{args.port}")
    HTTPServer(("0.0.0.0", args.port), NoCacheHandler).serve_forever()
