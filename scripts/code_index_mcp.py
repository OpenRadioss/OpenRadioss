#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["mcp", "pypdf"]
# ///
"""OpenRadioss local code-index MCP server.

Indexes the OpenRadioss source tree (.F, .F90, .inc, .c, .cpp, .cxx, .h)
and Markdown documentation files (*.md)
into a SQLite database (with FTS5 full-text search) and serves it over the
Model Context Protocol (stdio) so AI tooling gets fast symbol lookup,
caller/callee navigation and code search instead of repeated greps.

Quick start (from the repo root):

    pip install mcp                                  # once (pypdf too, if
                                                     # you ingest PDF manuals)
    python3 scripts/code_index_mcp.py --build        # build the index (~1-2 min)

Note: with no flags the script runs an MCP *server* that waits silently
for a client on stdin -- it is normally launched by the AI tool (Claude
Code reads the committed .mcp.json), not by hand.

Alternatively, uv users can skip the pip install; the PEP 723 header lets
uv fetch the dependencies automatically:

    uv run --script scripts/code_index_mcp.py --build

Claude Code picks the server up automatically from the committed .mcp.json
at the repo root. Because that file stores absolute, machine-specific paths
(the `uv`/python launcher and this script's location), `--build` and
`--rebuild` regenerate its `openradioss-index` entry for the current machine
so a freshly cloned or relocated checkout works without hand-editing. Other
MCP clients register the server the same way, e.g. GitHub Copilot CLI:

    copilot mcp add openradioss-index --tools '*' -- \
        python3 /abs/path/to/OpenRadioss/scripts/code_index_mcp.py

The index is stored in .code_index.db at the repo root (gitignored).
Refresh is manual: call the `reindex` MCP tool (or rerun with --build)
after pulling or editing sources. Reindexing is incremental (mtime/size
based) unless a full rebuild is requested.

Optionally, the Radioss user documentation can be ingested into the
same database so keyword/card documentation is searchable next to the
code:

    uv run --script scripts/code_index_mcp.py --add-docs
        # crawls the starter and engine keyword pages of the online help
        # (help.altair.com), one entry per keyword, with a progress bar
    uv run --script scripts/code_index_mcp.py --add-docs \
        https://help.altair.com/hwsolvers/rad/topics/solvers/rad/starter_input_r.htm
        # crawl a single keyword index page: every page it links to in
        # its <section> blocks is fetched and indexed individually
    uv run --script scripts/code_index_mcp.py --add-docs /path/to/manual.pdf
        # ingest a manually downloaded PDF manual (PDFs are never
        # downloaded by the script; see the links in README.md)

Extracted text lives only in the local database -- nothing is committed.

MCP tools exposed:
    find_symbol      - where is SUBROUTINE/FUNCTION/MODULE/C function X defined?
    get_callers      - who calls X / uses module X? (from the CI-maintained
                       `!||` call-graph headers plus a CALL/USE line scan)
    get_callees      - what does X call / which modules does it use?
    find_references  - exact call/use sites (file:line) of X
    search_code      - FTS5 full-text search over the source
    file_outline     - all symbols defined in one file
    search_manual    - FTS5 search over the ingested manual PDFs
    get_manual_page  - full extracted text of one manual page
    index_status     - index freshness and row counts
    reindex          - incremental (or full) re-index
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import sqlite3
import sys
import time
from html.parser import HTMLParser
from pathlib import Path

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent
DB_PATH = REPO_ROOT / ".code_index.db"
SCRIPT_PATH = Path(__file__).resolve()
MCP_JSON_PATH = REPO_ROOT / ".mcp.json"
MCP_SERVER_NAME = "openradioss-index"

SCHEMA_VERSION = "1"

# Directory trees to index, relative to the repo root.
INDEX_ROOTS = ("engine", "starter", "common_source", "reader")

# Directory names pruned during the walk (build output, external libs, stubs).
_PRUNE_EXACT = frozenset({"extlib", "exec", "stub", "unit_test", ".git"})
_PRUNE_PREFIXES = ("cbuild", "vs_build", "CMake_Compilers", "CMake_arch")

EXT_LANG = {
    ".F": "fortran_fixed",
    ".F90": "fortran_free",
    ".f90": "fortran_free",
    ".inc": "fortran_inc",
    ".md": "markdown",
    ".c": "c",
    ".cpp": "cpp",
    ".cxx": "cpp",
    ".cc": "cpp",
    ".h": "c_header",
    ".hpp": "c_header",
}

FORTRAN_LANGS = ("fortran_fixed", "fortran_free", "fortran_inc")
C_LIKE_LANGS = ("c", "cpp", "c_header")

MAX_LIMIT = 200  # hard cap for every tool's `limit` parameter

# Default manual sources: the online help keyword indexes (clean
# per-keyword pages with titles and URLs).
DEFAULT_DOC_URLS = (
    # Starter input keywords index; every /KEYWORD page is linked from it.
    "https://help.altair.com/hwsolvers/rad/topics/solvers/rad/starter_input_r.htm",
    # Engine input keywords index.
    "https://help.altair.com/hwsolvers/rad/topics/solvers/rad/engine_input_r.htm",
)
CRAWL_WORKERS = 4        # parallel page fetches (keep modest: shared site)
CRAWL_MAX_PAGES = 3000   # safety cap per index page
FETCH_TIMEOUT_S = 30     # per-request timeout (connect + stalled reads)


def log(msg: str) -> None:
    """Log to stderr only -- stdout is the MCP stdio transport."""
    print(msg, file=sys.stderr, flush=True)


def _mcp_launch_command() -> list[str]:
    """Build the launch command for this server, resolved for *this* machine.

    Prefers ``uv run --script`` when a ``uv`` binary is on PATH (so the PEP 723
    dependency header is honoured automatically); otherwise falls back to the
    current Python interpreter running the script directly.
    """
    uv = shutil.which("uv")
    if not uv:
        candidate = Path.home() / ".local" / "bin" / "uv"
        if candidate.is_file() and os.access(candidate, os.X_OK):
            uv = str(candidate)
    if uv:
        return [uv, "run", "--script", str(SCRIPT_PATH)]
    python = sys.executable or shutil.which("python3") or "python3"
    return [python, str(SCRIPT_PATH)]


def write_mcp_config(path: Path = MCP_JSON_PATH) -> None:
    """Create/refresh the ``.mcp.json`` entry with paths for this machine.

    The committed ``.mcp.json`` carries whatever absolute paths belonged to the
    machine that last built the index, which are meaningless after a clone or a
    move. Regenerating it as part of ``--build`` keeps the launcher command
    (``uv`` binary and script path) correct locally. Any other MCP servers
    already present in the file are preserved.
    """
    config: dict = {}
    if path.exists():
        try:
            with path.open(encoding="utf-8") as fh:
                loaded = json.load(fh)
            if isinstance(loaded, dict):
                config = loaded
        except (OSError, json.JSONDecodeError) as exc:
            log(f"  warning: could not read {path.name} ({exc});"
                " it will be recreated")

    servers = config.get("mcpServers")
    if not isinstance(servers, dict):
        servers = {}
    config["mcpServers"] = servers

    command, *args = _mcp_launch_command()
    servers[MCP_SERVER_NAME] = {
        "type": "stdio",
        "command": command,
        "args": args,
    }

    try:
        with path.open("w", encoding="utf-8") as fh:
            json.dump(config, fh, indent=2)
            fh.write("\n")
        log(f"  wrote MCP launcher config: {path} "
            f"(command: {command})")
    except OSError as exc:
        log(f"  warning: could not write {path.name} ({exc})")


# ---------------------------------------------------------------------------
# Fortran parsing (regexes ported from scripts/headers.py)
# ---------------------------------------------------------------------------

_DEFINITION_RE = re.compile(
    r"^\s*(?:"
    r"subroutine\s+(\w+)"                # group 1: subroutine
    r"|recursive\s+subroutine\s+(\w+)"   # group 2: recursive subroutine
    r"|function\s+(\w+)"                 # group 3: plain function
    r"|\w+\s+function\s+(\w+)"           # group 4: typed function
    r"|program\s+(\w+)"                  # group 5: program
    r"|module\s+(?!procedure\s)(\w+)"    # group 6: module
    r")",
    re.IGNORECASE,
)
_FUNCTION_END_RE = re.compile(r"^\s*END\s+function\s+\w+", re.IGNORECASE)
_BEGIN_INTERFACE_RE = re.compile(r"^\s*interface\s*$", re.IGNORECASE)
_END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\s*$", re.IGNORECASE)
_CALL_RE = re.compile(r"\bcall\s+([\w%]+)", re.IGNORECASE)
_USE_RE = re.compile(r"^\s*use\s+(\w+)", re.IGNORECASE)
_HEADER_RE = re.compile(r"^\s*!\|\|")

_DEFINITION_FIRST_CHARS = frozenset("srfpmidlcSRFPMIDLC")
_END_CHARS = frozenset("eE")
_IFACE_CHARS = frozenset("iI")

_DEF_KINDS = ("subroutine", "subroutine", "function", "function", "program", "module")

# `!||` call-graph header blocks (CI-maintained, see scripts/headers.py).
_HDR_FENCE_RE = re.compile(r"^\s*!\|\|====")
_HDR_SECTION_RE = re.compile(r"^\s*!\|\|---\s*(called by|calls|uses)")
_HDR_ENTRY_RE = re.compile(r"^\s*!\|\|\s+(\w+)\s+(\S+)")


def _definition_kind(groups) -> tuple[str, str]:
    """Return (name_lower, kind) from a _DEFINITION_RE match's groups."""
    for i, g in enumerate(groups):
        if g is not None:
            return g.strip().lower(), _DEF_KINDS[i]
    return "", ""


def parse_fortran(lines: list[str], lang: str):
    """Parse one Fortran file.

    Returns (symbols, edges, content):
      symbols: [(name, display_name, kind, line_start, line_end)]
      edges:   {(caller, callee, kind, source, line_or_None)}
      content: [(line_no, text)] lines to feed FTS (headers/copyright stripped)
    """
    fixed_form = lang in ("fortran_fixed", "fortran_inc")
    symbols: list[list] = []          # mutable so line_end can be patched
    edges: set[tuple] = set()
    content: list[tuple[int, str]] = []

    current = ""            # enclosing definition for scan-based edges
    is_interface = False
    hdr_section = None      # None | 'title' | 'called by' | 'calls' | 'uses'
    hdr_routine = ""

    for line_no, raw in enumerate(lines, 1):
        stripped = raw.strip()

        # --- `!||` call-graph header blocks -------------------------------
        if _HEADER_RE.match(raw):
            if _HDR_FENCE_RE.match(raw):
                hdr_section = "title" if hdr_section is None else None
                continue
            m = _HDR_SECTION_RE.match(raw)
            if m:
                hdr_section = m.group(1)
                continue
            m = _HDR_ENTRY_RE.match(raw)
            if m and hdr_section:
                name = m.group(1).lower()
                if hdr_section == "title":
                    hdr_routine = name
                elif hdr_routine:
                    if hdr_section == "calls":
                        edges.add((hdr_routine, name, "call", "header", None))
                    elif hdr_section == "uses":
                        edges.add((hdr_routine, name, "use", "header", None))
                    elif hdr_section == "called by":
                        edges.add((name, hdr_routine, "call", "header", None))
            continue  # header lines never reach FTS or the code scan
        hdr_section = None

        # --- FTS content filter -------------------------------------------
        if not stripped:
            continue
        if stripped.startswith(("Copyright>", "!Copyright>")) or raw.startswith("Chd"):
            continue
        content.append((line_no, stripped[:500]))

        # --- code scan ------------------------------------------------------
        code = raw.split("!")[0]
        cstr = code.lstrip()
        if not cstr:
            continue
        fc = cstr[0]

        if fc in _END_CHARS:
            if _END_INTERFACE_RE.match(code):
                is_interface = False
            continue
        if fc in _IFACE_CHARS and _BEGIN_INTERFACE_RE.match(code):
            is_interface = True
            continue

        # Fixed form: real code lines start with 4 spaces (headers.py rule);
        # this also skips column-1 comments (C/c/*) and continuation lines.
        if fixed_form and code[0:4] != "    ":
            continue

        if fc in _DEFINITION_FIRST_CHARS:
            m = _DEFINITION_RE.match(code)
            if m and not _FUNCTION_END_RE.match(code):
                name, kind = _definition_kind(m.groups())
                if name and not is_interface:
                    if symbols:
                        symbols[-1][4] = line_no - 1
                    display = code.strip().split("(")[0].split()[-1]
                    symbols.append([name, display, kind, line_no, None])
                    current = name
                continue

        m = _CALL_RE.search(code)
        if m and current:
            callee = m.group(1).lower()
            if callee not in ("this",) and "%" not in callee:
                edges.add((current, callee, "call", "scan", line_no))
            continue
        m = _USE_RE.match(code)
        if m and current:
            edges.add((current, m.group(1).lower(), "use", "scan", line_no))

    if symbols:
        symbols[-1][4] = len(lines)
    return symbols, edges, content


# ---------------------------------------------------------------------------
# C / C++ parsing (pragmatic regex; misses macro-generated and K&R functions)
# ---------------------------------------------------------------------------

_C_FUNC_RE = re.compile(
    r"^[A-Za-z_][\w\s\*&:<>,~\[\]]*?\b([A-Za-z_]\w*)\s*\("
)
_C_KEYWORDS = frozenset(
    {"if", "for", "while", "switch", "return", "sizeof", "else", "catch",
     "defined", "do", "case", "new", "delete", "throw", "using", "namespace"}
)
_VOID_FUNC_RE = re.compile(r"^\s*void\s+(\w+)", re.IGNORECASE)


def parse_c(lines: list[str], lang: str):
    """Parse one C/C++ file. Same return shape as parse_fortran."""
    symbols: list[list] = []
    edges: set[tuple] = set()
    content: list[tuple[int, str]] = []
    in_block_comment = False

    for line_no, raw in enumerate(lines, 1):
        stripped = raw.strip()
        if not stripped:
            continue
        if stripped.startswith("//Copyright>"):
            continue
        content.append((line_no, stripped[:500]))

        code = raw
        if in_block_comment:
            end = code.find("*/")
            if end < 0:
                continue
            code = code[end + 2:]
            in_block_comment = False
        code = code.split("//")[0]
        start = code.find("/*")
        if start >= 0:
            if code.find("*/", start) < 0:
                in_block_comment = True
            code = code[:start]
        if not code.strip() or code.lstrip().startswith("#"):
            continue

        # Definition candidates only at column 0 (repo style for definitions).
        if code[0].isspace():
            continue
        m = _C_FUNC_RE.match(code)
        name = None
        if m and m.group(1).lower() not in _C_KEYWORDS and "=" not in code[: m.start(1)]:
            name = m.group(1)
        else:
            m2 = _VOID_FUNC_RE.match(code)
            if m2 and ";" not in code:
                name = m2.group(1)
        if not name:
            continue
        # Prototype vs definition: a ';' before the first '{' within the
        # next few lines means declaration only.
        window = code[code.find(name):]
        for look in lines[line_no: line_no + 15]:
            window += look
            if ";" in window.split("{")[0] and "{" not in window.split(";")[0]:
                name = None
                break
            if "{" in window:
                break
        if name:
            if symbols:
                symbols[-1][4] = line_no - 1
            symbols.append([name.lower(), name, "cfunction", line_no, None])

    if symbols:
        symbols[-1][4] = len(lines)
    return symbols, edges, content


def parse_text(lines: list[str], lang: str):
    """Parse a text-only file (e.g. Markdown): no symbols/edges, FTS content only."""
    symbols: list[list] = []
    edges: set[tuple] = set()
    content: list[tuple[int, str]] = []
    for line_no, raw in enumerate(lines, 1):
        stripped = raw.strip()
        if not stripped:
            continue
        content.append((line_no, stripped[:500]))
    return symbols, edges, content


# ---------------------------------------------------------------------------
# Database
# ---------------------------------------------------------------------------

_DDL = """
CREATE TABLE IF NOT EXISTS meta(key TEXT PRIMARY KEY, value TEXT);
CREATE TABLE IF NOT EXISTS files(
  id INTEGER PRIMARY KEY, path TEXT UNIQUE NOT NULL,
  lang TEXT NOT NULL, mtime REAL NOT NULL, size INTEGER NOT NULL,
  nlines INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS symbols(
  id INTEGER PRIMARY KEY, name TEXT NOT NULL, display_name TEXT NOT NULL,
  kind TEXT NOT NULL, file_id INTEGER NOT NULL,
  line_start INTEGER NOT NULL, line_end INTEGER);
CREATE INDEX IF NOT EXISTS idx_symbols_name ON symbols(name);
CREATE INDEX IF NOT EXISTS idx_symbols_file ON symbols(file_id);
CREATE TABLE IF NOT EXISTS edges(
  src_file_id INTEGER NOT NULL, caller TEXT NOT NULL, callee TEXT NOT NULL,
  kind TEXT NOT NULL, source TEXT NOT NULL, line INTEGER);
CREATE INDEX IF NOT EXISTS idx_edges_callee ON edges(callee);
CREATE INDEX IF NOT EXISTS idx_edges_caller ON edges(caller);
CREATE INDEX IF NOT EXISTS idx_edges_file ON edges(src_file_id);
CREATE TABLE IF NOT EXISTS content(
  id INTEGER PRIMARY KEY, file_id INTEGER NOT NULL,
  line_no INTEGER NOT NULL, text TEXT NOT NULL);
CREATE INDEX IF NOT EXISTS idx_content_file ON content(file_id);
CREATE TABLE IF NOT EXISTS docs(
  id INTEGER PRIMARY KEY, source TEXT UNIQUE NOT NULL, title TEXT NOT NULL,
  pages INTEGER NOT NULL, ingested_at REAL NOT NULL);
CREATE TABLE IF NOT EXISTS doc_pages(
  id INTEGER PRIMARY KEY, doc_id INTEGER NOT NULL,
  page INTEGER NOT NULL, text TEXT NOT NULL,
  title TEXT, url TEXT);
CREATE INDEX IF NOT EXISTS idx_doc_pages_doc ON doc_pages(doc_id);
"""

_FTS_DDL = """
CREATE VIRTUAL TABLE IF NOT EXISTS fts USING fts5(
  text, content='content', content_rowid='id',
  tokenize="unicode61 tokenchars '_'");
CREATE TRIGGER IF NOT EXISTS content_ai AFTER INSERT ON content BEGIN
  INSERT INTO fts(rowid, text) VALUES (new.id, new.text); END;
CREATE TRIGGER IF NOT EXISTS content_ad AFTER DELETE ON content BEGIN
  INSERT INTO fts(fts, rowid, text) VALUES('delete', old.id, old.text); END;
CREATE VIRTUAL TABLE IF NOT EXISTS fts_docs USING fts5(
  text, content='doc_pages', content_rowid='id',
  tokenize="unicode61 tokenchars '_'");
CREATE TRIGGER IF NOT EXISTS doc_pages_ai AFTER INSERT ON doc_pages BEGIN
  INSERT INTO fts_docs(rowid, text) VALUES (new.id, new.text); END;
CREATE TRIGGER IF NOT EXISTS doc_pages_ad AFTER DELETE ON doc_pages BEGIN
  INSERT INTO fts_docs(fts_docs, rowid, text) VALUES('delete', old.id, old.text); END;
"""


def connect(db_path: Path) -> tuple[sqlite3.Connection, bool]:
    """Open (creating if needed) the index DB. Returns (conn, has_fts)."""
    fresh = not db_path.exists()
    conn = sqlite3.connect(db_path, timeout=30)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA busy_timeout=10000")
    if not fresh:
        try:
            row = conn.execute(
                "SELECT value FROM meta WHERE key='schema_version'"
            ).fetchone()
        except sqlite3.OperationalError:
            row = None
        if row is None or row[0] != SCHEMA_VERSION:
            log(f"Schema mismatch in {db_path}; rebuilding from scratch.")
            conn.close()
            for suffix in ("", "-wal", "-shm"):
                p = Path(str(db_path) + suffix)
                if p.exists():
                    p.unlink()
            conn = sqlite3.connect(db_path, timeout=30)
            conn.execute("PRAGMA journal_mode=WAL")
            conn.execute("PRAGMA synchronous=NORMAL")
            conn.execute("PRAGMA busy_timeout=10000")
    conn.executescript(_DDL)
    # Columns added after the first release; no-op on up-to-date DBs.
    for col in ("title", "url"):
        try:
            conn.execute(f"ALTER TABLE doc_pages ADD COLUMN {col} TEXT")
        except sqlite3.OperationalError:
            pass
    has_fts = True
    try:
        conn.executescript(_FTS_DDL)
    except sqlite3.OperationalError as exc:
        has_fts = False
        log(f"FTS5 unavailable ({exc}); search_code degrades to LIKE queries.")
    conn.execute(
        "INSERT OR REPLACE INTO meta(key, value) VALUES ('schema_version', ?)",
        (SCHEMA_VERSION,),
    )
    conn.commit()
    return conn, has_fts


# ---------------------------------------------------------------------------
# Indexing pipeline
# ---------------------------------------------------------------------------

def discover_files(repo_root: Path) -> dict[str, tuple[float, int, str]]:
    """Discover indexable files.

    - Source languages are discovered under INDEX_ROOTS.
    - Markdown files (*.md) are discovered across the whole repository.
    """
    found: dict[str, tuple[float, int, str]] = {}
    for root_name in INDEX_ROOTS:
        top = repo_root / root_name
        if not top.is_dir():
            continue
        for dirpath, dirnames, filenames in os.walk(top):
            dirnames[:] = [
                d for d in dirnames
                if d not in _PRUNE_EXACT and not d.startswith(_PRUNE_PREFIXES)
            ]
            for fn in filenames:
                ext = os.path.splitext(fn)[1]
                lang = EXT_LANG.get(ext)
                if lang is None:
                    continue
                full = os.path.join(dirpath, fn)
                try:
                    st = os.stat(full)
                except OSError:
                    continue
                rel = os.path.relpath(full, repo_root).replace(os.sep, "/")
                found[rel] = (st.st_mtime, st.st_size, lang)

    # Include Markdown docs from anywhere in the repository tree.
    for dirpath, dirnames, filenames in os.walk(repo_root):
        dirnames[:] = [
            d for d in dirnames
            if d not in _PRUNE_EXACT and not d.startswith(_PRUNE_PREFIXES)
        ]
        for fn in filenames:
            if os.path.splitext(fn)[1] != ".md":
                continue
            full = os.path.join(dirpath, fn)
            try:
                st = os.stat(full)
            except OSError:
                continue
            rel = os.path.relpath(full, repo_root).replace(os.sep, "/")
            found[rel] = (st.st_mtime, st.st_size, "markdown")

    return found


def index_one_file(conn: sqlite3.Connection, repo_root: Path, rel: str,
                   mtime: float, size: int, lang: str) -> None:
    """(Re)index a single file: replace its rows in every table."""
    try:
        with open(repo_root / rel, encoding="latin-1") as f:
            lines = f.read().splitlines()
    except OSError as exc:
        log(f"Skipping {rel}: {exc}")
        return

    if lang in FORTRAN_LANGS:
        parser = parse_fortran
    elif lang in C_LIKE_LANGS:
        parser = parse_c
    else:
        parser = parse_text
    symbols, edges, content = parser(lines, lang)

    row = conn.execute("SELECT id FROM files WHERE path=?", (rel,)).fetchone()
    if row:
        file_id = row[0]
        conn.execute("DELETE FROM symbols WHERE file_id=?", (file_id,))
        conn.execute("DELETE FROM edges WHERE src_file_id=?", (file_id,))
        conn.execute("DELETE FROM content WHERE file_id=?", (file_id,))
        conn.execute(
            "UPDATE files SET mtime=?, size=?, nlines=?, lang=? WHERE id=?",
            (mtime, size, len(lines), lang, file_id),
        )
    else:
        cur = conn.execute(
            "INSERT INTO files(path, lang, mtime, size, nlines) VALUES (?,?,?,?,?)",
            (rel, lang, mtime, size, len(lines)),
        )
        file_id = cur.lastrowid

    conn.executemany(
        "INSERT INTO symbols(name, display_name, kind, file_id, line_start, line_end)"
        " VALUES (?,?,?,?,?,?)",
        [(s[0], s[1], s[2], file_id, s[3], s[4]) for s in symbols],
    )
    conn.executemany(
        "INSERT INTO edges(src_file_id, caller, callee, kind, source, line)"
        " VALUES (?,?,?,?,?,?)",
        [(file_id, *e) for e in edges],
    )
    conn.executemany(
        "INSERT INTO content(file_id, line_no, text) VALUES (?,?,?)",
        [(file_id, ln, txt) for ln, txt in content],
    )


def build_index(conn: sqlite3.Connection, repo_root: Path,
                full: bool = False) -> dict:
    """Incremental (default) or full re-index. Returns a stats dict."""
    t0 = time.time()
    if full:
        log("Full rebuild: clearing existing index...")
        # Bypass the per-row FTS delete trigger: wipe fts in one command.
        conn.execute("DROP TRIGGER IF EXISTS content_ad")
        conn.execute("DELETE FROM content")
        try:
            conn.execute("INSERT INTO fts(fts) VALUES ('delete-all')")
            conn.executescript(_FTS_DDL)  # recreate the trigger
        except sqlite3.OperationalError:
            pass  # no FTS5 on this machine
        conn.execute("DELETE FROM symbols")
        conn.execute("DELETE FROM edges")
        conn.execute("DELETE FROM files")
        conn.commit()

    log("Scanning source trees...")
    found = discover_files(repo_root)
    known = {
        path: (mtime, size)
        for path, mtime, size in conn.execute("SELECT path, mtime, size FROM files")
    }

    changed = [
        rel for rel, (mtime, size, _lang) in found.items()
        if known.get(rel) != (mtime, size)
    ]
    removed = [rel for rel in known if rel not in found]

    for rel in removed:
        (file_id,) = conn.execute(
            "SELECT id FROM files WHERE path=?", (rel,)
        ).fetchone()
        conn.execute("DELETE FROM symbols WHERE file_id=?", (file_id,))
        conn.execute("DELETE FROM edges WHERE src_file_id=?", (file_id,))
        conn.execute("DELETE FROM content WHERE file_id=?", (file_id,))
        conn.execute("DELETE FROM files WHERE id=?", (file_id,))

    for i, rel in enumerate(changed):
        mtime, size, lang = found[rel]
        index_one_file(conn, repo_root, rel, mtime, size, lang)
        if (i + 1) % 200 == 0:
            conn.commit()
            log(f"  indexed {i + 1}/{len(changed)} files...")

    conn.execute(
        "INSERT OR REPLACE INTO meta(key, value) VALUES ('built_at', ?)",
        (str(time.time()),),
    )
    conn.commit()

    if full or len(changed) > 1000:
        try:
            conn.execute("INSERT INTO fts(fts) VALUES ('optimize')")
        except sqlite3.OperationalError:
            pass
        conn.execute("ANALYZE")
        conn.commit()

    stats = {
        "scanned": len(found),
        "updated": len(changed),
        "removed": len(removed),
        "seconds": round(time.time() - t0, 1),
    }
    log(f"Index up to date: {stats}")
    return stats


# ---------------------------------------------------------------------------
# Manual ingestion (online help HTML pages and PDF manuals)
# ---------------------------------------------------------------------------

class _HtmlText(HTMLParser):
    """Extract readable text, the page <h1>/<title>, and in-section links."""

    _SKIP = frozenset({"script", "style", "nav", "header", "footer", "head"})
    _BLOCK = frozenset({"p", "div", "li", "tr", "table", "section", "br",
                        "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "dt", "dd"})

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.parts: list[str] = []
        self.links: list[str] = []          # hrefs found inside <section>
        self.all_links: list[str] = []      # every href on the page
        self.title = ""
        self._skip_depth = 0
        self._section_depth = 0
        self._in_h1 = self._in_title = False

    def handle_starttag(self, tag, attrs):
        if tag in self._SKIP:
            self._skip_depth += 1
        elif tag == "section":
            self._section_depth += 1
        elif tag == "a":
            href = dict(attrs).get("href")
            if href:
                self.all_links.append(href)
                if self._section_depth:
                    self.links.append(href)
        elif tag == "h1" and not self.title:
            self._in_h1 = True
        elif tag == "title":
            self._in_title = True
        elif tag in ("td", "th"):
            self.parts.append(" ")
        if tag in self._BLOCK:
            self.parts.append("\n")

    def handle_endtag(self, tag):
        if tag in self._SKIP and self._skip_depth:
            self._skip_depth -= 1
        elif tag == "section" and self._section_depth:
            self._section_depth -= 1
        elif tag == "h1":
            self._in_h1 = False
        elif tag == "title":
            self._in_title = False

    def handle_data(self, data):
        if self._skip_depth:
            return
        if self._in_h1 or (self._in_title and not self.title):
            t = data.strip()
            if t:
                self.title = t
        self.parts.append(data)

    def text(self) -> str:
        lines = [" ".join(seg.split()) for seg in "".join(self.parts).split("\n")]
        return "\n".join(ln for ln in lines if ln)


def _fetch_url(url: str) -> str:
    """Fetch a text resource (http(s) or file://) as a decoded string."""
    import urllib.request

    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=FETCH_TIMEOUT_S) as resp:
        return resp.read().decode("utf-8", errors="replace")


def _parse_html(raw: str) -> _HtmlText:
    parser = _HtmlText()
    parser.feed(raw)
    return parser


def _replace_doc(conn: sqlite3.Connection, source: str, title: str,
                 pages: int) -> int:
    row = conn.execute("SELECT id FROM docs WHERE source=?", (source,)).fetchone()
    if row:
        conn.execute("DELETE FROM doc_pages WHERE doc_id=?", (row[0],))
        conn.execute("DELETE FROM docs WHERE id=?", (row[0],))
    cur = conn.execute(
        "INSERT INTO docs(source, title, pages, ingested_at) VALUES (?,?,?,?)",
        (source, title, pages, time.time()),
    )
    return cur.lastrowid


def ingest_html_index(conn: sqlite3.Connection, index_url: str) -> dict:
    """Crawl a keyword index page (e.g. starter_input_r.htm): fetch every
    page it links to inside <section> elements and index one row per page."""
    from urllib.parse import urljoin, urldefrag

    if not index_url.lower().startswith(("http://", "https://", "file://")):
        index_url = Path(index_url).expanduser().resolve().as_uri()

    log(f"Fetching index page {index_url} ...")
    idx = _parse_html(_fetch_url(index_url))
    # Keyword links live in the page's <section> blocks (e.g. the
    # starter_input_r__..._section_* section); fall back to every link.
    candidates = idx.links or idx.all_links
    seen: set[str] = set()
    targets: list[str] = []
    for href in candidates:
        url = urldefrag(urljoin(index_url, href))[0]
        base = url.rsplit("/", 1)[-1]
        if not base.endswith((".htm", ".html")):
            continue
        # Stay in the same directory as the index page: keyword topics are
        # siblings; anything else is site navigation.
        if url.rsplit("/", 1)[0] != index_url.rsplit("/", 1)[0]:
            continue
        if url == index_url or url in seen:
            continue
        seen.add(url)
        targets.append(url)
    targets = targets[:CRAWL_MAX_PAGES]
    if not targets:
        log(f"SKIPPED {index_url}: no keyword links found")
        return {"title": index_url, "pages": 0}

    title = index_url.rsplit("/", 1)[-1].rsplit(".", 1)[0]
    log(f"Crawling {len(targets)} pages linked from {title} "
        f"({CRAWL_WORKERS} parallel fetches, {FETCH_TIMEOUT_S}s timeout)...")
    doc_id = _replace_doc(conn, index_url, title, len(targets))

    from concurrent.futures import ThreadPoolExecutor

    def fetch(url: str):
        try:
            return url, _parse_html(_fetch_url(url)), None
        except Exception as exc:
            return url, None, exc

    n = failed = 0
    t0 = time.time()
    with ThreadPoolExecutor(max_workers=CRAWL_WORKERS) as pool:
        # executor.map keeps index order, so page numbers stay stable.
        for i, (url, page, exc) in enumerate(pool.map(fetch, targets), 1):
            if exc is not None:
                failed += 1
                log(f"  failed {url}: {exc}")
            else:
                text = page.text()
                if text:
                    n += 1
                    conn.execute(
                        "INSERT INTO doc_pages(doc_id, page, text, title, url)"
                        " VALUES (?,?,?,?,?)",
                        (doc_id, n, text,
                         page.title or url.rsplit("/", 1)[-1], url),
                    )
            _progress(i, len(targets), t0)
            if i % 25 == 0:
                conn.commit()
    conn.execute("UPDATE docs SET pages=? WHERE id=?", (n, doc_id))
    conn.commit()
    log(f"  {title}: {n} keyword pages indexed"
        + (f", {failed} failed" if failed else ""))
    return {"title": title, "pages": n}

def _local_pdf(source: str) -> Path:
    """PDFs are ingested from local files only (no downloading)."""
    if source.lower().startswith(("http://", "https://")):
        raise ValueError(
            "PDF downloading is not supported; download the file with your "
            "browser and pass its local path to --add-docs"
        )
    p = Path(source).expanduser().resolve()
    if not p.is_file():
        raise FileNotFoundError(f"no such PDF: {source}")
    return p


def _progress(done: int, total: int, t0: float, unit: str = "pages") -> None:
    """Render an in-place progress bar on a TTY; periodic lines otherwise."""
    rate = done / max(time.time() - t0, 1e-3)
    eta = int((total - done) / rate) if rate > 0 else 0
    if sys.stderr.isatty():
        width = 30
        filled = int(width * done / max(total, 1))
        bar = "#" * filled + "-" * (width - filled)
        print(f"\r  [{bar}] {done}/{total} {unit} {rate:5.1f}/s ETA {eta:4d}s",
              end="\n" if done >= total else "", file=sys.stderr, flush=True)
    elif done % 50 == 0 or done >= total:
        log(f"  {done}/{total} {unit} ({rate:.1f}/s, ~{eta}s left)")


def ingest_pdf(conn: sqlite3.Connection, source: str) -> dict:
    """Extract text from one manual PDF, one row per page."""
    pdf_path = _local_pdf(source)
    try:
        from pypdf import PdfReader
    except ImportError:
        raise SystemExit(
            "pypdf is required to ingest PDFs: pip install pypdf "
            "(or run the script via 'uv run --script')"
        )
    title = pdf_path.stem
    log(f"Extracting text from {pdf_path.name} ...")
    reader = PdfReader(pdf_path)
    doc_id = _replace_doc(conn, title, title, len(reader.pages))
    n = 0
    t0 = time.time()
    for page_no, page in enumerate(reader.pages, 1):
        try:
            text = (page.extract_text() or "").strip()
        except Exception:
            text = ""
        if text:
            conn.execute(
                "INSERT INTO doc_pages(doc_id, page, text) VALUES (?,?,?)",
                (doc_id, page_no, text),
            )
            n += 1
        _progress(page_no, len(reader.pages), t0)
        if page_no % 500 == 0:
            conn.commit()
    conn.commit()
    log(f"  {pdf_path.name}: {n} non-empty pages indexed")
    return {"title": title, "pages": n}


def ingest_docs(conn: sqlite3.Connection, sources: list[str]) -> dict:
    """Ingest manual sources: .htm/.html index pages are crawled (one row
    per linked keyword page), .pdf files are extracted page by page."""
    t0 = time.time()
    ingested = []
    for source in sources:
        is_html = source.split("#")[0].lower().endswith((".htm", ".html"))
        try:
            if is_html:
                ingested.append(ingest_html_index(conn, source))
            else:
                ingested.append(ingest_pdf(conn, source))
        except Exception as exc:
            log(f"SKIPPED {source}: {exc}")
            log("  (if the URL is unreachable from this machine, download it"
                " manually and pass a local path)")
    stats = {"ingested": ingested, "seconds": round(time.time() - t0, 1)}
    log(f"Manual ingestion done: {stats}")
    return stats


# ---------------------------------------------------------------------------
# Query helpers (shared by the CLI and the MCP tools)
# ---------------------------------------------------------------------------

def _clamp(limit: int) -> int:
    return max(1, min(int(limit), MAX_LIMIT))


def _open_ready() -> tuple[sqlite3.Connection, bool]:
    """Connection for queries; builds the index first if it doesn't exist."""
    conn, has_fts = connect(DB_PATH)
    built = conn.execute("SELECT value FROM meta WHERE key='built_at'").fetchone()
    if built is None:
        log("No index found -- building now (takes ~1-2 minutes)...")
        build_index(conn, REPO_ROOT)
    return conn, has_fts


def _symbol_location(conn: sqlite3.Connection, name: str):
    row = conn.execute(
        "SELECT f.path, s.line_start FROM symbols s JOIN files f ON f.id=s.file_id"
        " WHERE s.name=? ORDER BY s.id LIMIT 1",
        (name,),
    ).fetchone()
    return (row[0], row[1]) if row else (None, None)


def q_find_symbol(conn, name, kind=None, exact=True, path_filter=None, limit=20):
    limit = _clamp(limit)
    sql = (
        "SELECT s.display_name, s.name, s.kind, f.path, s.line_start, s.line_end"
        " FROM symbols s JOIN files f ON f.id = s.file_id WHERE "
    )
    args: list = []
    if exact:
        sql += "s.name = ?"
        args.append(name.lower())
    else:
        sql += "s.name LIKE ?"
        args.append(f"%{name.lower()}%")
    if kind:
        sql += " AND s.kind = ?"
        args.append(kind.lower())
    if path_filter:
        sql += " AND f.path LIKE ?"
        args.append(f"%{path_filter}%")
    sql += " ORDER BY s.name, f.path LIMIT ?"
    args.append(limit + 1)
    rows = conn.execute(sql, args).fetchall()
    return {
        "matches": [
            {"name": r[0], "kind": r[2], "path": r[3],
             "line_start": r[4], "line_end": r[5]}
            for r in rows[:limit]
        ],
        "truncated": len(rows) > limit,
    }


def q_get_callers(conn, name, limit=100):
    limit = _clamp(limit)
    rows = conn.execute(
        "SELECT caller, GROUP_CONCAT(DISTINCT source) FROM edges"
        " WHERE callee=? GROUP BY caller ORDER BY caller LIMIT ?",
        (name.lower(), limit + 1),
    ).fetchall()
    callers = []
    for caller, sources in rows[:limit]:
        path, line = _symbol_location(conn, caller)
        via = "both" if "," in sources else sources
        callers.append({"name": caller, "path": path, "line": line, "via": via})
    return {"symbol": name.lower(), "callers": callers,
            "truncated": len(rows) > limit}


def q_get_callees(conn, name, limit=100):
    limit = _clamp(limit)
    rows = conn.execute(
        "SELECT callee, kind, GROUP_CONCAT(DISTINCT source) FROM edges"
        " WHERE caller=? GROUP BY callee, kind ORDER BY callee LIMIT ?",
        (name.lower(), limit + 1),
    ).fetchall()
    calls, uses = [], []
    for callee, kind, sources in rows[:limit]:
        path, line = _symbol_location(conn, callee)
        via = "both" if "," in sources else sources
        entry = {"name": callee, "path": path, "line": line, "via": via}
        (uses if kind == "use" else calls).append(entry)
    return {"symbol": name.lower(), "calls": calls, "uses": uses,
            "truncated": len(rows) > limit}


def q_find_references(conn, name, limit=100):
    limit = _clamp(limit)
    rows = conn.execute(
        "SELECT f.path, e.line, e.caller, e.kind FROM edges e"
        " JOIN files f ON f.id = e.src_file_id"
        " WHERE e.callee=? AND e.source='scan' ORDER BY f.path, e.line LIMIT ?",
        (name.lower(), limit + 1),
    ).fetchall()
    return {
        "symbol": name.lower(),
        "references": [
            {"path": r[0], "line": r[1], "caller": r[2], "kind": r[3]}
            for r in rows[:limit]
        ],
        "truncated": len(rows) > limit,
    }


def q_search_code(conn, has_fts, query, path_filter=None, ext_filter=None,
                  limit=20):
    limit = _clamp(limit)
    args: list = []
    if has_fts:
        sql = (
            "SELECT f.path, c.line_no, c.text FROM fts"
            " JOIN content c ON c.id = fts.rowid"
            " JOIN files f ON f.id = c.file_id WHERE fts MATCH ?"
        )
        args.append(query)
    else:
        sql = (
            "SELECT f.path, c.line_no, c.text FROM content c"
            " JOIN files f ON f.id = c.file_id WHERE c.text LIKE ?"
        )
        args.append(f"%{query}%")
    if path_filter:
        sql += " AND f.path LIKE ?"
        args.append(f"%{path_filter}%")
    if ext_filter:
        sql += " AND f.path LIKE ?"
        args.append(f"%{ext_filter if ext_filter.startswith('.') else '.' + ext_filter}")
    if has_fts:
        sql += " ORDER BY rank"
    sql += " LIMIT ?"
    args.append(limit + 1)
    try:
        rows = conn.execute(sql, args).fetchall()
    except sqlite3.OperationalError as exc:
        return {"error": f"bad FTS5 query ({exc}); syntax: word, \"a phrase\","
                         " a AND b, a OR b, a NOT b, prefix*"}
    return {
        "hits": [
            {"path": r[0], "line": r[1], "text": r[2][:200]}
            for r in rows[:limit]
        ],
        "truncated": len(rows) > limit,
        "fts": has_fts,
    }


def q_file_outline(conn, path):
    rel = path.lstrip("./")
    row = conn.execute(
        "SELECT id, lang, nlines FROM files WHERE path=? OR path LIKE ?",
        (rel, f"%{rel}"),
    ).fetchone()
    if row is None:
        return {"error": f"file not indexed: {path}"}
    file_id, lang, nlines = row
    (real_path,) = conn.execute(
        "SELECT path FROM files WHERE id=?", (file_id,)
    ).fetchone()
    rows = conn.execute(
        "SELECT display_name, kind, line_start, line_end FROM symbols"
        " WHERE file_id=? ORDER BY line_start",
        (file_id,),
    ).fetchall()
    return {
        "path": real_path, "lang": lang, "nlines": nlines,
        "symbols": [
            {"name": r[0], "kind": r[1], "line_start": r[2], "line_end": r[3]}
            for r in rows
        ],
    }


def q_search_manual(conn, has_fts, query, doc_filter=None, limit=10):
    limit = _clamp(limit)
    args: list = []
    if has_fts:
        sql = (
            "SELECT d.title, p.page, snippet(fts_docs, 0, '>>', '<<', ' ... ', 32),"
            " p.title, p.url"
            " FROM fts_docs JOIN doc_pages p ON p.id = fts_docs.rowid"
            " JOIN docs d ON d.id = p.doc_id WHERE fts_docs MATCH ?"
        )
        args.append(query)
    else:
        sql = (
            "SELECT d.title, p.page, substr(p.text, 1, 300), p.title, p.url"
            " FROM doc_pages p JOIN docs d ON d.id = p.doc_id"
            " WHERE p.text LIKE ?"
        )
        args.append(f"%{query}%")
    if doc_filter:
        sql += " AND d.title LIKE ?"
        args.append(f"%{doc_filter}%")
    if has_fts:
        sql += " ORDER BY rank"
    sql += " LIMIT ?"
    args.append(limit + 1)
    try:
        rows = conn.execute(sql, args).fetchall()
    except sqlite3.OperationalError as exc:
        return {"error": f"bad FTS5 query ({exc}); syntax: word, \"a phrase\","
                         " a AND b, a OR b, a NOT b, prefix*"}
    ndocs = conn.execute("SELECT COUNT(*) FROM docs").fetchone()[0]
    if ndocs == 0:
        return {"error": "no manuals ingested yet -- run: "
                         "uv run --script scripts/code_index_mcp.py --add-docs"}
    hits = []
    for r in rows[:limit]:
        hit = {"doc": r[0], "page": r[1], "snippet": r[2][:400]}
        if r[3]:
            hit["title"] = r[3]
        if r[4]:
            hit["url"] = r[4]
        hits.append(hit)
    return {"hits": hits, "truncated": len(rows) > limit}


def q_get_manual_page(conn, doc, page):
    row = conn.execute(
        "SELECT p.text, d.title, p.title, p.url FROM doc_pages p"
        " JOIN docs d ON d.id = p.doc_id"
        " WHERE d.title LIKE ? AND p.page = ?",
        (f"%{doc}%", int(page)),
    ).fetchone()
    if row is None:
        titles = [t for (t,) in conn.execute("SELECT title FROM docs")]
        return {"error": f"no page {page} in a doc matching '{doc}'",
                "available_docs": titles}
    result = {"doc": row[1], "page": int(page), "text": row[0][:20000]}
    if row[2]:
        result["title"] = row[2]
    if row[3]:
        result["url"] = row[3]
    return result


def q_index_status(conn, has_fts):
    built = conn.execute("SELECT value FROM meta WHERE key='built_at'").fetchone()
    counts = {
        tbl: conn.execute(f"SELECT COUNT(*) FROM {tbl}").fetchone()[0]
        for tbl in ("files", "symbols", "edges", "content")
    }
    known = {
        path: (mtime, size)
        for path, mtime, size in conn.execute("SELECT path, mtime, size FROM files")
    }
    found = discover_files(REPO_ROOT)
    stale = sum(1 for rel, (m, s, _l) in found.items() if known.get(rel) != (m, s))
    stale += sum(1 for rel in known if rel not in found)
    return {
        "db_path": str(DB_PATH),
        "built_at": time.strftime(
            "%Y-%m-%d %H:%M:%S", time.localtime(float(built[0]))
        ) if built else None,
        "files": counts["files"],
        "symbols": counts["symbols"],
        "edges": counts["edges"],
        "indexed_lines": counts["content"],
        "stale_files": stale,
        "fts5": has_fts,
        "manuals": [
            {"title": t, "pages": p}
            for t, p in conn.execute("SELECT title, pages FROM docs")
        ],
    }


# ---------------------------------------------------------------------------
# MCP server
# ---------------------------------------------------------------------------

def make_server():
    try:
        from mcp.server.fastmcp import FastMCP
    except ImportError:
        log("ERROR: the 'mcp' Python package is not installed.")
        log("  Fix:  pip install mcp")
        log("  (or run the script with uv, which installs it automatically:")
        log("   uv run --script scripts/code_index_mcp.py)")
        sys.exit(1)

    mcp = FastMCP("openradioss-index")

    @mcp.tool()
    def find_symbol(name: str, kind: str | None = None, exact: bool = True,
                    path_filter: str | None = None, limit: int = 20) -> dict:
        """Find where a symbol is defined in the OpenRadioss source.

        Covers Fortran subroutines/functions/modules/programs and C/C++
        functions. `name` is case-insensitive. Set exact=False for substring
        matching. `kind` filters to one of: subroutine, function, module,
        program, cfunction. `path_filter` is a substring of the file path
        (e.g. 'engine/' or 'starter/') to disambiguate name collisions.
        First call after a fresh clone builds the index (~1-2 min).
        """
        conn, _ = _open_ready()
        try:
            return q_find_symbol(conn, name, kind, exact, path_filter, limit)
        finally:
            conn.close()

    @mcp.tool()
    def get_callers(name: str, limit: int = 100) -> dict:
        """List every routine that calls `name` (or uses module `name`).

        Built from the CI-maintained `!||` call-graph headers plus a
        CALL/USE line scan; `via` says which source(s) asserted the edge
        (header, scan, or both). Case-insensitive.
        """
        conn, _ = _open_ready()
        try:
            return q_get_callers(conn, name, limit)
        finally:
            conn.close()

    @mcp.tool()
    def get_callees(name: str, limit: int = 100) -> dict:
        """List everything routine `name` calls (`calls`) and every module
        it uses (`uses`). Case-insensitive; `via` gives edge provenance."""
        conn, _ = _open_ready()
        try:
            return q_get_callees(conn, name, limit)
        finally:
            conn.close()

    @mcp.tool()
    def find_references(name: str, limit: int = 100) -> dict:
        """List exact source locations (file:line) where `name` is invoked
        via CALL or pulled in via USE. Use this for impact analysis before
        changing a routine's interface. Case-insensitive."""
        conn, _ = _open_ready()
        try:
            return q_find_references(conn, name, limit)
        finally:
            conn.close()

    @mcp.tool()
    def search_code(query: str, path_filter: str | None = None,
                    ext_filter: str | None = None, limit: int = 20) -> dict:
        """Full-text search over the indexed source (SQLite FTS5).

        Matching is PER SOURCE LINE: multiple terms (a AND b) only hit when
        they appear on the same line. FTS5 syntax: bare words are ANDed;
        "quoted phrases"; a OR b; a NOT b; prefix* for prefix match.
        Identifiers keep underscores as one token (e.g. multi_fvm_mod).
        Case-insensitive.
        `path_filter` = path substring (e.g. 'engine/source/ale');
        `ext_filter` = extension (e.g. '.F90'). Copyright banners and
        `!||` call-graph headers are excluded from the index.
        """
        conn, has_fts = _open_ready()
        try:
            return q_search_code(conn, has_fts, query, path_filter, ext_filter,
                                 limit)
        finally:
            conn.close()

    @mcp.tool()
    def file_outline(path: str) -> dict:
        """List all symbols defined in one source file (repo-relative path,
        e.g. 'engine/source/ale/alemain.F'; a unique path suffix works too)."""
        conn, _ = _open_ready()
        try:
            return q_file_outline(conn, path)
        finally:
            conn.close()

    @mcp.tool()
    def search_manual(query: str, doc_filter: str | None = None,
                      limit: int = 10) -> dict:
        """Full-text search over the ingested Radioss documentation:
        the online-help keyword pages (one entry per starter/engine
        keyword, with title and URL) and/or the PDF manuals.

        Use this for input-deck keyword/card semantics (e.g. what the fields
        of /MAT/LAW36 or /INTER/TYPE7 mean), solver options and theory.
        Keywords tokenize at slashes, so query "MAT LAW36" or the phrase
        '"MAT LAW36"' rather than /MAT/LAW36. `doc_filter` narrows to one
        source by title substring (e.g. 'starter_input', 'engine_input',
        'Reference', 'Theory'). Returns snippets with page numbers (and
        keyword page titles/URLs for online-help hits); fetch full text
        with get_manual_page. Docs must be ingested once via: --add-docs
        (see the error this tool returns if nothing is ingested yet).
        """
        conn, has_fts = _open_ready()
        try:
            return q_search_manual(conn, has_fts, query, doc_filter, limit)
        finally:
            conn.close()

    @mcp.tool()
    def get_manual_page(doc: str, page: int) -> dict:
        """Return the full extracted text of one documentation page. `doc`
        is a source-title substring (e.g. 'starter_input' or 'Reference');
        `page` comes from search_manual hits. For online-help sources one
        page = one keyword topic; for PDFs fetch adjacent pages if a card
        description continues across pages."""
        conn, _ = _open_ready()
        try:
            return q_get_manual_page(conn, doc, page)
        finally:
            conn.close()

    @mcp.tool()
    def index_status() -> dict:
        """Report index freshness: row counts, build time, and `stale_files`
        (files changed on disk since indexing -- if > 0, call reindex)."""
        conn, has_fts = _open_ready()
        try:
            return q_index_status(conn, has_fts)
        finally:
            conn.close()

    @mcp.tool()
    def reindex(full: bool = False) -> dict:
        """Re-index the source tree. Incremental by default (only files whose
        mtime/size changed); full=True wipes and rebuilds everything.
        Call after pulling or editing sources (see index_status.stale_files).
        """
        conn, _ = _open_ready()
        try:
            return build_index(conn, REPO_ROOT, full=full)
        finally:
            conn.close()

    return mcp


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    global DB_PATH
    if sys.version_info < (3, 10):
        log(f"ERROR: Python >= 3.10 is required to run this server"
            f" (this is Python {sys.version.split()[0]}).")
        log("  Point the launcher at a newer interpreter, e.g. python3.11.")
        sys.exit(1)
    ap = argparse.ArgumentParser(
        description="OpenRadioss code-index MCP server "
                    "(default: serve MCP over stdio)")
    ap.add_argument("--build", action="store_true",
                    help="incrementally build/update the index and exit")
    ap.add_argument("--rebuild", action="store_true",
                    help="full rebuild of the index and exit")
    ap.add_argument("--stats", action="store_true",
                    help="print index status as JSON and exit")
    ap.add_argument("--add-docs", nargs="*", metavar="URL_OR_PDF",
                    help="ingest documentation and exit: .htm/.html keyword "
                         "index URLs are crawled, .pdf paths are extracted "
                         "locally (never downloaded); with no argument, "
                         "crawls the online-help keyword indexes")
    ap.add_argument("--db", type=Path, default=None,
                    help=f"index database path (default: {DB_PATH})")
    args = ap.parse_args()

    if args.db:
        DB_PATH = args.db.resolve()

    # CLI actions; several flags may be combined in one run.
    ran_action = False
    if args.build or args.rebuild:
        conn, _ = connect(DB_PATH)
        build_index(conn, REPO_ROOT, full=args.rebuild)
        conn.close()
        write_mcp_config()
        ran_action = True
    if args.add_docs is not None:
        conn, _ = connect(DB_PATH)
        ingest_docs(conn, args.add_docs or list(DEFAULT_DOC_URLS))
        conn.close()
        ran_action = True
    if args.stats:
        conn, has_fts = _open_ready()
        print(json.dumps(q_index_status(conn, has_fts), indent=2))
        conn.close()
        ran_action = True
    if ran_action:
        return

    # Serving mode. Everything below goes to stderr: stdout is the MCP
    # transport and must stay clean.
    built = None
    if DB_PATH.exists():
        try:
            conn, _ = connect(DB_PATH)
            built = conn.execute(
                "SELECT value FROM meta WHERE key='built_at'"
            ).fetchone()
            conn.close()
        except sqlite3.Error:
            pass
    log(f"openradioss-index MCP server: serving on stdio (db: {DB_PATH})")
    if built is None:
        log("  index not built yet -- the first tool call will build it"
            " (~1-2 min), or run with --build beforehand")
    if sys.stdin.isatty():
        log("  NOTE: this process is meant to be launched by an MCP client"
            " (e.g. Claude Code via .mcp.json).")
        log("  Started from a terminal it just waits for a client on stdin"
            " -- it is not hung. Ctrl+C to exit.")
        log("  Did you mean: --build, --stats, --add-docs or --help?")
    make_server().run()


if __name__ == "__main__":
    main()
