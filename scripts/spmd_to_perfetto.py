#!/usr/bin/env python3
"""spmd_to_perfetto.py — Convert SPMD binary trace files to Perfetto native .pftrace

Zero external dependencies — uses a built-in minimal protobuf encoder.
The output is Perfetto's native TrackEvent format which reliably renders
cross-rank flow arrows (message exchange lines) in https://ui.perfetto.dev

Usage:
  python3 spmd_to_perfetto.py spmd_timeline_rank_*.spmd.gz
  python3 spmd_to_perfetto.py -o my_run.pftrace spmd_timeline_rank_*.spmd.gz
"""

import argparse
import gzip
import os
import struct
import sys
import uuid
from collections import defaultdict, deque
from io import BytesIO

# -----------------------------------------------------------------------
# Minimal protobuf encoder (wire format only — no schemas needed)
# -----------------------------------------------------------------------

def _encode_varint(value: int) -> bytes:
    """Encode a non-negative integer as a protobuf varint."""
    buf = []
    while True:
        bits = value & 0x7F
        value >>= 7
        if value:
            buf.append(bits | 0x80)
        else:
            buf.append(bits)
            break
    return bytes(buf)

def _field_varint(field_number: int, value: int) -> bytes:
    """Wire type 0 (varint) field."""
    tag = (field_number << 3) | 0
    return _encode_varint(tag) + _encode_varint(value)

def _field_fixed64(field_number: int, value: int) -> bytes:
    """Wire type 1 (64-bit / fixed64) field — used for flow_ids in Perfetto."""
    tag = (field_number << 3) | 1
    return _encode_varint(tag) + struct.pack("<Q", value & 0xFFFFFFFFFFFFFFFF)

def _field_bytes(field_number: int, data: bytes) -> bytes:
    """Wire type 2 (length-delimited) field."""
    tag = (field_number << 3) | 2
    return _encode_varint(tag) + _encode_varint(len(data)) + data

def _field_string(field_number: int, s: str) -> bytes:
    """UTF-8 string as wire type 2."""
    return _field_bytes(field_number, s.encode("utf-8"))

# -----------------------------------------------------------------------
# Perfetto proto field numbers (verified from perfetto package protos)
# -----------------------------------------------------------------------
# TracePacket
_PKT_TIMESTAMP   = 8
_PKT_TRACK_EVENT = 11
_PKT_TRACK_DESC  = 60
_PKT_SEQ_ID      = 10
_PKT_SEQ_FLAGS   = 13

# TrackDescriptor
_TD_UUID         = 1
_TD_NAME         = 2
_TD_PARENT_UUID  = 5

# TrackEvent
_TE_NAME                  = 23
_TE_TYPE                  = 9
_TE_TRACK_UUID            = 11
_TE_FLOW_IDS              = 47
_TE_TERMINATING_FLOW_IDS  = 48

# TrackEvent.Type enum values
_TYPE_SLICE_BEGIN = 1
_TYPE_SLICE_END   = 2

# TracePacket.SequenceFlags
_SEQ_INCREMENTAL_STATE_CLEARED = 1

# -----------------------------------------------------------------------
# Packet builders
# -----------------------------------------------------------------------

def _track_descriptor_packet(uuid_val: int, name: str) -> bytes:
    """Build a TracePacket containing a TrackDescriptor."""
    desc  = _field_varint(_TD_UUID, uuid_val)
    desc += _field_string(_TD_NAME, name)
    pkt   = _field_bytes(_PKT_TRACK_DESC, desc)
    return pkt

def _slice_begin_packet(ts_ns: int, track_uuid: int, name: str,
                         seq_id: int, flow_ids=(), terminating_flow_ids=()) -> bytes:
    te  = _field_varint(_TE_TYPE, _TYPE_SLICE_BEGIN)
    te += _field_varint(_TE_TRACK_UUID, track_uuid)
    te += _field_string(_TE_NAME, name)
    for fid in flow_ids:
        te += _field_fixed64(_TE_FLOW_IDS, fid)
    for fid in terminating_flow_ids:
        te += _field_fixed64(_TE_TERMINATING_FLOW_IDS, fid)
    pkt  = _field_varint(_PKT_TIMESTAMP, ts_ns)
    pkt += _field_bytes(_PKT_TRACK_EVENT, te)
    pkt += _field_varint(_PKT_SEQ_ID, seq_id)
    return pkt

def _slice_end_packet(ts_ns: int, track_uuid: int, seq_id: int,
                       terminating_flow_ids=()) -> bytes:
    te  = _field_varint(_TE_TYPE, _TYPE_SLICE_END)
    te += _field_varint(_TE_TRACK_UUID, track_uuid)
    for fid in terminating_flow_ids:
        te += _field_fixed64(_TE_TERMINATING_FLOW_IDS, fid)  # fixed64, NOT varint
    pkt  = _field_varint(_PKT_TIMESTAMP, ts_ns)
    pkt += _field_bytes(_PKT_TRACK_EVENT, te)
    pkt += _field_varint(_PKT_SEQ_ID, seq_id)
    return pkt

def _build_trace(packets: list[bytes]) -> bytes:
    """Wrap a list of serialised TracePacket payloads into a Trace message."""
    out = BytesIO()
    for pkt in packets:
        # Trace.packet field = 1, wire type 2
        out.write(_field_bytes(1, pkt))
    return out.getvalue()

# -----------------------------------------------------------------------
# SPMD binary file reader (same as spmd_trace_convert.py)
# -----------------------------------------------------------------------
HEADER_FMT  = "<4sBxxxid"
HEADER_SIZE = struct.calcsize(HEADER_FMT)
RECORD_V1_FMT  = "<iII";      RECORD_V1_SIZE = struct.calcsize(RECORD_V1_FMT)
RECORD_V2_FMT  = "<iII32s";   RECORD_V2_SIZE = struct.calcsize(RECORD_V2_FMT)
RECORD_V3_FMT  = "<iII32sii"; RECORD_V3_SIZE = struct.calcsize(RECORD_V3_FMT)
RECORD_V4_FMT  = "<iQI32sii"; RECORD_V4_SIZE = struct.calcsize(RECORD_V4_FMT)

ARROW_ENDPOINT_TAG = -2000

TAG_NAMES = {
    -1: "MPI_Barrier",   -2: "MPI_Bcast",      -3: "MPI_Reduce",
    -4: "MPI_Allreduce", -5: "MPI_Gather",      -6: "MPI_Allgather",
    -7: "MPI_Scatter",   -8: "MPI_Wait",        -9: "MPI_Waitall",
    -10: "MPI_Waitany",  -11: "MPI_Alltoall",   -12: "MPI_Alltoallv",
    -24: "MPI_Sendrecv", -25: "MPI_Sendrecv_replace",
    -1000: "MPI_Comm_rank", -1001: "MPI_Comm_size", -1012: "MPI_Comm_size",
}

SEND_OPS = {"MPI_Send", "MPI_Isend"}

def _open_input(path):
    # Detect gzip by magic bytes (0x1f 0x8b) rather than the file name, so both
    # compressed and uncompressed traces load regardless of extension.
    with open(path, "rb") as probe:
        is_gzip = probe.read(2) == b"\x1f\x8b"
    return gzip.open(path, "rb") if is_gzip else open(path, "rb")

def _base_op_name(name):
    idx = name.find(" (tag=")
    return name[:idx] if idx != -1 else name

def _tag_to_name(tag, stored_name=""):
    if stored_name:
        return stored_name
    return TAG_NAMES.get(tag, f"tag={tag}")

def read_spmd(path):
    with _open_input(path) as fh:
        raw = fh.read(HEADER_SIZE)
        if len(raw) < HEADER_SIZE:
            raise ValueError(f"{path}: too short")
        magic, version, rank, t_origin = struct.unpack(HEADER_FMT, raw)
        if magic != b"SPMD":
            raise ValueError(f"{path}: bad magic")
        if version not in (1, 2, 3, 4):
            raise ValueError(f"{path}: unsupported version {version}")

        events = []
        if version == 1:
            while True:
                r = fh.read(RECORD_V1_SIZE)
                if len(r) < RECORD_V1_SIZE: break
                tag, tb, dur = struct.unpack(RECORD_V1_FMT, r)
                events.append(dict(tag=tag, t_begin_ns=tb * 1000, duration_ns=dur * 1000,
                                   name="", peer_rank=-2, msg_tag=-2))
        elif version == 2:
            while True:
                r = fh.read(RECORD_V2_SIZE)
                if len(r) < RECORD_V2_SIZE: break
                tag, tb, dur, nb = struct.unpack(RECORD_V2_FMT, r)
                events.append(dict(tag=tag, t_begin_ns=tb * 1000, duration_ns=dur * 1000,
                                   name=nb.rstrip(b"\x00").decode("utf-8","replace"),
                                   peer_rank=-2, msg_tag=-2))
        elif version == 3:
            while True:
                r = fh.read(RECORD_V3_SIZE)
                if len(r) < RECORD_V3_SIZE: break
                tag, tb, dur, nb, pr, mt = struct.unpack(RECORD_V3_FMT, r)
                events.append(dict(tag=tag, t_begin_ns=tb * 1000, duration_ns=dur * 1000,
                                   name=nb.rstrip(b"\x00").decode("utf-8","replace"),
                                   peer_rank=pr, msg_tag=mt))
        else:  # version 4
            while True:
                r = fh.read(RECORD_V4_SIZE)
                if len(r) < RECORD_V4_SIZE: break
                tag, tb, dur, nb, pr, mt = struct.unpack(RECORD_V4_FMT, r)
                events.append(dict(tag=tag, t_begin_ns=tb, duration_ns=dur,
                                   name=nb.rstrip(b"\x00").decode("utf-8","replace"),
                                   peer_rank=pr, msg_tag=mt))
    return rank, t_origin, events

# -----------------------------------------------------------------------
# Flow matching
# -----------------------------------------------------------------------

def _get_uuid():
    return uuid.uuid4().int & ((1 << 63) - 1)

def build_flow_map(all_rank_events):
    """Match sends to recvs; return {(rank,idx): [flow_id, ...]}."""
    recv_endpoints = defaultdict(deque)

    for dest_rank, events in all_rank_events.items():
        for i, ev in enumerate(events):
            name = _base_op_name(ev["name"])
            peer, mtag = ev["peer_rank"], ev["msg_tag"]
            if name == "MPI_Recv" and peer >= 0 and mtag >= 0:
                recv_endpoints[(peer, dest_rank, mtag)].append(i)
            elif ev["tag"] == ARROW_ENDPOINT_TAG and peer >= 0 and mtag >= 0:
                # anchor to the preceding Wait slice
                j = i - 1
                while j >= 0 and events[j]["tag"] == ARROW_ENDPOINT_TAG:
                    j -= 1
                recv_endpoints[(peer, dest_rank, mtag)].append(max(j, 0))

    # send_flows: (rank, idx) → [fid]  — used as flow_ids on sender BEGIN
    # recv_flows: (rank, idx) → [fid]  — used as terminating_flow_ids on receiver BEGIN
    send_flows = defaultdict(list)
    recv_flows = defaultdict(list)
    for src_rank, events in all_rank_events.items():
        for i, ev in enumerate(events):
            name = _base_op_name(ev["name"])
            if name not in SEND_OPS:
                continue
            peer, mtag = ev["peer_rank"], ev["msg_tag"]
            if peer < 0 or mtag < 0:
                continue
            key = (src_rank, peer, mtag)
            if not recv_endpoints[key]:
                continue
            recv_idx = recv_endpoints[key].popleft()
            fid = _get_uuid()
            send_flows[(src_rank, i)].append(fid)
            recv_flows[(peer, recv_idx)].append(fid)

    return send_flows, recv_flows

# -----------------------------------------------------------------------
# Main converter
# -----------------------------------------------------------------------

def convert(input_files, output_path, verbose=True):
    loaded = []
    for path in sorted(input_files):
        try:
            rank, t_origin, events = read_spmd(path)
            loaded.append((path, rank, t_origin, events))
        except Exception as exc:
            print(f"Warning: skipping {path}: {exc}", file=sys.stderr)

    if not loaded:
        print("No valid input files.", file=sys.stderr)
        return 0

    t0 = min(t for _, _, t, _ in loaded)

    all_rank_events = {}
    for path, rank, t_origin, events in loaded:
        offset_ns = int(round((t_origin - t0) * 1e9))
        aligned = [dict(ev, t_begin_ns=ev["t_begin_ns"] + offset_ns) for ev in events]
        all_rank_events[rank] = aligned

    flow_map = build_flow_map(all_rank_events)
    send_flows, recv_flows = flow_map

    track_uuids = {rank: _get_uuid() for _, rank, _, _ in loaded}
    SEQ_BASE = 2000

    packets = []

    # Track descriptor for each rank
    for _, rank, _, _ in loaded:
        packets.append(_track_descriptor_packet(track_uuids[rank], f"Rank {rank}"))

    # Slice events
    total = 0
    for path, rank, t_origin, _ in loaded:
        events = all_rank_events[rank]
        tuuid = track_uuids[rank]
        seq_id = SEQ_BASE + rank
        first = True

        for i, ev in enumerate(events):
            if ev["tag"] == ARROW_ENDPOINT_TAG:
                continue
            name = _tag_to_name(ev["tag"], ev["name"])
            ts_ns  = int(ev["t_begin_ns"])
            end_ns = ts_ns + int(ev["duration_ns"])
            fids      = send_flows.get((rank, i), [])
            term_fids = recv_flows.get((rank, i), [])

            packets.append(_slice_begin_packet(ts_ns, tuuid, name, seq_id,
                                               flow_ids=fids,
                                               terminating_flow_ids=term_fids))
            packets.append(_slice_end_packet(end_ns, tuuid, seq_id))
            total += 1

        if verbose:
            size = os.path.getsize(path)
            print(f"  {os.path.basename(path)}: rank {rank}, "
                  f"{len(events)} events, {size} bytes on disk")

    n_arrows = sum(len(v) for v in send_flows.values())
    if verbose and n_arrows:
        print(f"  Generated {n_arrows} message-flow arrow(s)")

    data = _build_trace(packets)
    with open(output_path, "wb") as fh:
        fh.write(data)

    size_kb = len(data) // 1024
    if verbose:
        print(f"Done: {total} events, output {size_kb} KB")
        print(f"\nOpen the trace:")
        print(f"  https://ui.perfetto.dev  →  Open trace file  →  {output_path}")

    return total


def main():
    parser = argparse.ArgumentParser(
        description="Convert SPMD binary trace files to Perfetto native format (.pftrace).")
    parser.add_argument("files", nargs="+", help="Input .spmd or .spmd.gz files")
    parser.add_argument("-o", "--output", default="spmd_timeline_all.pftrace",
                        help="Output .pftrace file (default: spmd_timeline_all.pftrace)")
    parser.add_argument("-q", "--quiet", action="store_true")
    args = parser.parse_args()

    print(f"Converting {len(args.files)} file(s) -> {args.output}")
    convert(args.files, args.output, verbose=not args.quiet)


if __name__ == "__main__":
    main()
