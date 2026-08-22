const { createServer } = require("node:http");
const { createReadStream, existsSync, statSync } = require("node:fs");
const { extname, resolve, sep } = require("node:path");

const root = resolve(__dirname, ".artifacts");
const port = Number(process.env.NEUROMOSAIC_E2E_PORT || 4173);
const mime = {
  ".html": "text/html; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".gz": "application/gzip",
  ".nii": "application/octet-stream",
  ".png": "image/png"
};

const server = createServer((request, response) => {
  const url = new URL(request.url, "http://127.0.0.1");
  if (url.pathname === "/health") {
    response.writeHead(200, { "content-type": "text/plain" });
    response.end("ok");
    return;
  }
  const relative = decodeURIComponent(url.pathname).replace(/^\/+/, "");
  const path = resolve(root, relative);
  if (!path.startsWith(root + sep) || !existsSync(path) || !statSync(path).isFile()) {
    response.writeHead(404, { "content-type": "text/plain" });
    response.end("not found");
    return;
  }
  response.writeHead(200, {
    "content-type": mime[extname(path)] || "application/octet-stream",
    "cache-control": "no-store"
  });
  createReadStream(path).pipe(response);
});

server.listen(port, "127.0.0.1");

function close() {
  server.close(() => process.exit(0));
}

process.on("SIGTERM", close);
process.on("SIGINT", close);
