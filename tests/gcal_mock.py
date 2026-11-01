#!/usr/bin/env python3
"""Local mock of the Google Calendar events API and OAuth token endpoint.

Used by gcal-resilience-test.el.  Binds 127.0.0.1 on an ephemeral port and
prints "PORT <n>" on stdout.  Control endpoints:

  POST /__reset  {"events": {cal: [event, ...]}, "rules": [rule, ...]}
  GET  /__log    -> [{"method", "path", "status", "t"}, ...]
  GET  /__events -> {cal: [event, ...]}

A rule is {"method", "path" (regex on the path), "status", "body", "delay",
"times", "apply"}.  The first rule whose method and path match and whose
"times" is not used up decides the response.  "delay" -1 never answers.
"apply": true performs the normal action before the rule's delay/status, which
models a write that Google applied but whose response was lost.
"""
import json
import re
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

lock = threading.Lock()
state = {"events": {}, "rules": [], "log": [], "seq": 0}


def first_wins(pairs):
    out = {}
    for k, v in pairs:
        out.setdefault(k, v)
    return out


def default(method, path, query, body):
    """Return (status, body) for the normal Calendar or token behaviour."""
    if path == "/token":
        state["seq"] += 1
        return 200, {"access_token": "fresh-%d" % state["seq"], "expires_in": 3599,
                     "token_type": "Bearer"}
    m = re.match(r"^/calendar/v3/calendars/([^/]+)/events(?:/([^/]+))?$", path)
    if not m:
        return 404, {"error": {"code": 404, "message": "no route"}}
    cal, eid = m.group(1), m.group(2)
    evs = state["events"].setdefault(cal, [])
    find = [e for e in evs if e["id"] == eid]
    if method == "GET" and eid is None:
        return 200, {"kind": "calendar#events", "items": evs,
                     "nextSyncToken": "tok-%s" % cal}
    if method == "GET":
        return (200, find[0]) if find else (404, {"error": {"code": 404, "message": "Not Found"}})
    if method == "POST":
        ev = dict(body)
        state["seq"] += 1
        ev.setdefault("id", "srv%d" % state["seq"])
        if any(e["id"] == ev["id"] for e in evs):
            return 409, {"error": {"code": 409, "message": "The requested identifier already exists.",
                                   "errors": [{"reason": "duplicate"}]}}
        ev.update(kind="calendar#event", status="confirmed", etag='"e-%d"' % state["seq"],
                  updated="2026-10-31T00:00:00.000Z")
        evs.append(ev)
        return 200, ev
    if not find:
        return 404, {"error": {"code": 404, "message": "Not Found"}}
    ev = find[0]
    if method == "PATCH":
        state["seq"] += 1
        ev.update(body)
        ev["etag"] = '"e-%d"' % state["seq"]
        return 200, ev
    if method == "DELETE":
        evs.remove(ev)
        return 204, None
    return 405, {"error": {"code": 405, "message": "method"}}


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def send(self, status, obj):
        data = b"" if obj is None else json.dumps(obj).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=UTF-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def handle_any(self):
        path, _, query = self.path.partition("?")
        n = int(self.headers.get("Content-Length") or 0)
        raw = self.rfile.read(n) if n else b""
        if path == "/__reset":
            cfg = json.loads(raw or b"{}")
            with lock:
                state.update(events=cfg.get("events", {}), rules=cfg.get("rules", []),
                             log=[], seq=0)
            return self.send(200, {"ok": True})
        if path == "/__log":
            with lock:
                return self.send(200, state["log"])
        if path == "/__events":
            with lock:
                return self.send(200, state["events"])
        try:
            # org-gcal repeats "dateTime" in start/end with a null second
            # value; Google keeps the first, so the mock does too.
            body = json.loads(raw, object_pairs_hook=first_wins) if raw and path != "/token" else {}
        except ValueError:
            body = {}
        with lock:
            entry = {"method": self.command, "path": path, "t": time.time(),
                     "query": query, "body": raw.decode("utf-8", "replace")}
            state["log"].append(entry)
            rule = None
            for r in state["rules"]:
                if r.get("method", self.command) == self.command and \
                        re.search(r.get("path", ".*"), path) and r.get("times", 1) != 0:
                    rule = r
                    if "times" in r:
                        r["times"] -= 1
                    break
            if rule is None or rule.get("apply"):
                status, out = default(self.command, path, query, body)
            if rule is not None:
                status = rule.get("status", status if rule.get("apply") else 200)
                out = rule.get("body", out if rule.get("apply") else {})
            entry["status"] = status
        delay = rule.get("delay", 0) if rule else 0
        try:
            time.sleep(3600 if delay < 0 else delay)
            self.send(status, out)
        except (BrokenPipeError, ConnectionResetError):
            pass

    do_GET = do_POST = do_PATCH = do_PUT = do_DELETE = handle_any


server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
server.daemon_threads = True
print("PORT %d" % server.server_address[1], flush=True)
server.serve_forever()
