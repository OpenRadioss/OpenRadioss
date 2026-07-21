/*! \file spmd_profiler.cpp
 *  \brief MPI call profiler — records a compact binary timeline per MPI rank.
 *
 *  Integration
 *  -----------
 *  The Fortran wrappers bracket every MPI call with spmd_in(tag) / spmd_out(tag,ierr)
 *  from spmd_error_mod.  When compiled with -DSPMD_PROFILE those hooks call
 *  spmd_profiler_record_in / spmd_profiler_record_out here, timestamping with
 *  MPI_Wtime().
 *
 *  Output format
 *  -------------
 *  spmd_profiler_flush() writes one binary timeline file per MPI rank.
 *
 *  When compiled with -DWITH_ZLIB the file is gzip-compressed:
 *
 *    spmd_timeline_rank_NNNNN.spmd.gz
 *
 *  Without -DWITH_ZLIB (no zlib dependency) it is written uncompressed:
 *
 *    spmd_timeline_rank_NNNNN.spmd
 *
 *  Both variants share the identical binary layout below; the Python
 *  converters read either transparently.
 *
 *  Binary layout (little-endian):
 *
 *    Header (20 bytes):
 *      char[4]   magic      = "SPMD"
 *      uint8_t   version    = 1
 *      uint8_t   pad[3]     = {0,0,0}
 *      int32_t   rank
 *      double    t_origin   (MPI_Wtime() of the first recorded event)
 *
 *    Per-event record (56 bytes, repeated N times):
 *      int32_t   tag          (negative = SPMD collective; positive = user MPI tag)
 *      uint64_t  t_begin_ns   (nanoseconds since t_origin)
 *      uint32_t  duration_ns  (nanoseconds; capped at UINT32_MAX ≈ 4.3 s)
 *
 *  Convert to Chrome Trace JSON for viewing:
 *
 *    python3 spmd_trace_convert.py [--output all.json] rank_*.spmd.gz
 *    # Then open all.json in  https://ui.perfetto.dev
 *
 *  Aggregation / statistics mode
 *  ------------------------------
 *  Set env var  SPMD_PROFILE_MODE=stats  before running.
 *  Instead of per-event records, flush writes a compact JSON summary:
 *    spmd_stats_rank_NNNNN.json  (one line per unique tag: count, total, min, max)
 *
 *  Build
 *  -----
 *    mpicxx -O2 -fPIC -DOMPI_SKIP_MPICXX -DMPICH_SKIP_MPICXX \
 *           -c spmd_profiler.cpp -o build/obj/spmd_profiler.o
 *  Link with:  -lstdc++
 *
 *  Optional gzip compression of the trace files requires zlib:
 *    compile with -DWITH_ZLIB  and  link with -lz
 */

#include "spmd_profiler.h"

#define OMPI_SKIP_MPICXX  1
#define MPICH_SKIP_MPICXX 1
#include <mpi.h>
#ifdef WITH_ZLIB
#include <zlib.h>
#endif

#include <vector>
#include <unordered_map>
#include <string>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <cstdlib>  /* getenv */
#include <algorithm> /* std::min */

/* ---------------------------------------------------------------------- */
/* Binary file structures (packed, little-endian on x86/x64)              */
/* ---------------------------------------------------------------------- */

#pragma pack(push, 1)
struct SpmdFileHeader {
    char     magic[4];  /* "SPMD"                                          */
    uint8_t  version;   /* 4 (v1:12B; v2:44B; v3:52B µs; v4:56B ns+u64) */
    uint8_t  pad[3];    /* {0,0,0}                                         */
    int32_t  rank;
    double   t_origin;  /* MPI_Wtime() of first recorded event (absolute)  */
};

/* Version 4 record: tag + timing (nanoseconds) + name + peer_rank + msg_tag */
struct SpmdBinaryRecord {
    int32_t  tag;
    uint64_t t_begin_ns;    /* ns since t_origin; wraps at 2^64 ns (centuries) */
    uint32_t duration_ns;   /* nanoseconds; capped at UINT32_MAX ≈ 4.3 s    */
    char     name[32];      /* null-padded human-readable name               */
    int32_t  peer_rank;     /* dest for sends, source for recvs; -2 = N/A   */
    int32_t  msg_tag;       /* actual MPI P2P message tag; -2 = N/A         */
};
#pragma pack(pop)

/* ---------------------------------------------------------------------- */
/* Tag → human-readable name table                                        */
/* ---------------------------------------------------------------------- */

static const std::unordered_map<int, const char*> TAG_NAMES = {
    {   -6, "MPI_Allreduce"       },
    {   -7, "MPI_Reduce"          },
    {   -8, "MPI_Wait"            },
    {   -9, "MPI_Waitall"         },
    {  -10, "MPI_Waitany"         },
    {  -11, "MPI_Bcast"           },
    {  -12, "MPI_Allgather"       },
    {  -13, "MPI_Barrier"         },
    {  -14, "MPI_Alltoall"        },
    {  -15, "MPI_Alltoallv"       },
    {  -16, "MPI_Gather"          },
    {  -17, "MPI_Gatherv"         },
    {  -18, "MPI_Iallgather"      },
    {  -19, "MPI_Iallgatherv"     },
    {  -20, "MPI_Iallreduce"      },
    {  -21, "MPI_Scatter"         },
    {  -22, "MPI_Scatterv"        },
    {  -23, "MPI_Allgatherv"      },
    {  -24, "MPI_Sendrecv"        },
    {  -25, "MPI_Sendrecv_replace"},
    {  -26, "MPI_Probe"           },
    {  -27, "MPI_Iprobe"          },
    {  -28, "MPI_Get_count"       },
    {  -29, "MPI_Test"            },
    {  -30, "MPI_Testall"         },
    {  -31, "MPI_Testany"         },
    {  -32, "MPI_Testsome"        },
    {  -33, "MPI_Waitsome"        },
    {  -34, "MPI_Ibcast"          },
    {  -35, "MPI_Igather"         },
    {  -36, "MPI_Iscatter"        },
    {  -37, "MPI_Ireduce"         },
    {  -38, "MPI_Ialltoall"       },
    {  -39, "MPI_Ialltoallv"      },
    {-1011, "spmd_comm_rank"      },
    {-1012, "spmd_comm_size"      },
};

/* ---------------------------------------------------------------------- */
/* Per-event in-memory record (double precision, converted at flush)       */
/* ---------------------------------------------------------------------- */

struct SpmdEntry {
    int         tag;
    std::string name;       /* resolved at record_in time; empty = use TAG_NAMES */
    double      t_begin;
    double      t_end;
    int         peer_rank;  /* dest for sends, source for recvs; -2 = N/A        */
    int         msg_tag;    /* actual MPI P2P message tag; -2 = N/A              */
};

/* ---------------------------------------------------------------------- */
/* Per-tag aggregation record (stats mode)                                 */
/* ---------------------------------------------------------------------- */

struct TagStats {
    int         tag;      /* representative tag (first seen for this name) */
    std::string name;     /* human-readable name resolved at record_in time */
    uint64_t count;
    double   total_s;
    double   min_s;
    double   max_s;
    TagStats() : tag(0), count(0), total_s(0.0),
                 min_s(1e308), max_s(0.0) {}
};

/* ---------------------------------------------------------------------- */
/* Request registry — maps MPI_Request handle → pending P2P info          */
/* ---------------------------------------------------------------------- */

struct PendingRequest {
    int  peer_rank;
    int  msg_tag;
    bool is_recv;
};

/* ---------------------------------------------------------------------- */
/* Global state                                                            */
/* ---------------------------------------------------------------------- */

enum ProfileMode { MODE_TRACE, MODE_STATS };

static int                                         g_rank         = -1;
static double                                      g_t_begin      = 0.0;
static int                                         g_active_tag   = 0;
static std::string                                 g_active_name;
static int                                         g_active_peer  = -2;
static int                                         g_active_msgtag = -2;
static std::vector<SpmdEntry>                      g_timeline;
static std::unordered_map<std::string, TagStats>   g_stats;
static std::unordered_map<int, PendingRequest>     g_request_table;
static bool                                        g_initialized  = false;
static ProfileMode                                 g_mode         = MODE_TRACE;

/* User section state — one active section at a time, suspendable by MPI calls */
static bool        g_section_active = false;
static int         g_section_tag    = 0;
static std::string g_section_name;
static double      g_section_begin  = 0.0;  /* start of current (or resumed) segment */

static void lazy_init()
{
    if (g_initialized) return;
    MPI_Comm_rank(MPI_COMM_WORLD, &g_rank);

    const char* mode_env = std::getenv("SPMD_PROFILE_MODE");
    if (mode_env && std::strcmp(mode_env, "stats") == 0) {
        g_mode = MODE_STATS;
    } else {
        g_mode = MODE_TRACE;
        g_timeline.reserve(65536);
    }
    g_initialized = true;
}

/* ---------------------------------------------------------------------- */
/* Helper: emit one segment for the active user section                    */
/* ---------------------------------------------------------------------- */

static void emit_section_segment(double t_end)
{
    if (!g_section_active) return;
    double dur = t_end - g_section_begin;
    if (dur < 0.0) dur = 0.0;

    if (g_mode == MODE_TRACE) {
        SpmdEntry e;
        e.tag       = g_section_tag;
        e.name      = g_section_name;
        e.t_begin   = g_section_begin;
        e.t_end     = t_end;
        e.peer_rank = -2;
        e.msg_tag   = -2;
        g_timeline.push_back(e);
    } else {
        TagStats& s = g_stats[g_section_name];
        if (s.count == 0) {
            s.tag  = g_section_tag;
            s.name = g_section_name;
        }
        s.count++;
        s.total_s += dur;
        if (dur < s.min_s) s.min_s = dur;
        if (dur > s.max_s) s.max_s = dur;
    }
}

/* ---------------------------------------------------------------------- */
/* Helper: resolve a name for a tag                                        */
/* ---------------------------------------------------------------------- */

static std::string resolve_name(int tag, const std::string& stored_name)
{
    if (!stored_name.empty()) return stored_name;
    auto it = TAG_NAMES.find(tag);
    if (it != TAG_NAMES.end()) return it->second;
    if (tag >= 0) {
        char buf[32];
        std::snprintf(buf, sizeof(buf), "P2P (tag=%d)", tag);
        return buf;
    }
    char buf[32];
    std::snprintf(buf, sizeof(buf), "unknown (tag=%d)", tag);
    return buf;
}

/* ---------------------------------------------------------------------- */
/* Flush helpers                                                           */
/* ---------------------------------------------------------------------- */

static void flush_trace()
{
    if (g_timeline.empty()) return;

    char filename[256];
#ifdef WITH_ZLIB
    std::snprintf(filename, sizeof(filename),
                  "spmd_timeline_rank_%05d.spmd.gz", g_rank);
    gzFile gz = gzopen(filename, "wb6"); /* compression level 6 */
    if (!gz) {
        std::fprintf(stderr, "spmd_profiler: cannot open %s for writing\n", filename);
        return;
    }
#else
    std::snprintf(filename, sizeof(filename),
                  "spmd_timeline_rank_%05d.spmd", g_rank);
    FILE* fp = std::fopen(filename, "wb");
    if (!fp) {
        std::fprintf(stderr, "spmd_profiler: cannot open %s for writing\n", filename);
        return;
    }
#endif

    /* Header — version 4 (nanosecond resolution) */
    SpmdFileHeader hdr;
    std::memcpy(hdr.magic, "SPMD", 4);
    hdr.version  = 4;
    hdr.pad[0] = hdr.pad[1] = hdr.pad[2] = 0;
    hdr.rank     = g_rank;
    hdr.t_origin = g_timeline[0].t_begin;
#ifdef WITH_ZLIB
    gzwrite(gz, &hdr, sizeof(hdr));
#else
    std::fwrite(&hdr, sizeof(hdr), 1, fp);
#endif

    /* Records */
    for (const SpmdEntry& e : g_timeline) {
        SpmdBinaryRecord rec;
        std::memset(&rec, 0, sizeof(rec));
        rec.tag = e.tag;

        double offset_ns = (e.t_begin - hdr.t_origin) * 1.0e9;
        double dur_ns    = (e.t_end   - e.t_begin)    * 1.0e9;

        rec.t_begin_ns  = static_cast<uint64_t>(
            offset_ns < 0.0 ? 0ull :
            static_cast<uint64_t>(offset_ns));
        rec.duration_ns = static_cast<uint32_t>(
            dur_ns < 0.0 ? 0u :
            dur_ns > 4294967295.0 ? 4294967295u :
            static_cast<uint32_t>(dur_ns));

        /* Store name (null-padded, 32 bytes) */
        std::string nm = resolve_name(e.tag, e.name);
        std::size_t copy_len = std::min(nm.size(), static_cast<std::size_t>(31));
        std::memcpy(rec.name, nm.c_str(), copy_len);

        rec.peer_rank = e.peer_rank;
        rec.msg_tag   = e.msg_tag;

#ifdef WITH_ZLIB
        gzwrite(gz, &rec, sizeof(rec));
#else
        std::fwrite(&rec, sizeof(rec), 1, fp);
#endif
    }

#ifdef WITH_ZLIB
    gzclose(gz);
#else
    std::fclose(fp);
#endif
    g_timeline.clear();
}

static void flush_stats()
{
    if (g_stats.empty()) return;

    char filename[256];
    std::snprintf(filename, sizeof(filename),
                  "spmd_stats_rank_%05d.json", g_rank);

    FILE* fp = std::fopen(filename, "w");
    if (!fp) {
        std::fprintf(stderr, "spmd_profiler: cannot open %s for writing\n", filename);
        return;
    }

    std::fprintf(fp, "[\n");
    bool first = true;
    for (const auto& kv : g_stats) {
        const TagStats&  s   = kv.second;
        /* s.name is the resolved human-readable name stored at record time */
        if (!first) std::fprintf(fp, ",\n");
        first = false;
        std::fprintf(fp,
            "  {\"tag\":%d,\"name\":\"%s\","
            "\"count\":%llu,"
            "\"total_s\":%.9f,\"min_s\":%.9f,\"max_s\":%.9f,\"mean_s\":%.9f}",
            s.tag, s.name.c_str(),
            (unsigned long long)s.count,
            s.total_s, s.min_s, s.max_s,
            s.count ? s.total_s / s.count : 0.0);
    }
    std::fprintf(fp, "\n]\n");
    std::fclose(fp);
    g_stats.clear();
}

/* ---------------------------------------------------------------------- */
/* Public C interface (called via Fortran bind(c))                         */
/* ---------------------------------------------------------------------- */

extern "C" {

void spmd_profiler_init(const int* rank)
{
    if (*rank >= 0) {
        g_rank = *rank;
    } else {
        MPI_Comm_rank(MPI_COMM_WORLD, &g_rank);
    }

    const char* mode_env = std::getenv("SPMD_PROFILE_MODE");
    g_mode = (mode_env && std::strcmp(mode_env, "stats") == 0)
             ? MODE_STATS : MODE_TRACE;

    g_timeline.clear();
    g_stats.clear();
    g_request_table.clear();
    if (g_mode == MODE_TRACE)
        g_timeline.reserve(65536);

    g_initialized = true;
}

void spmd_profiler_record_in(const int* tag, const char* name, const int* name_len,
                              const int* peer_rank, const int* msg_tag)
{
    lazy_init();

    /* Suspend active user section: emit its segment up to now */
    if (g_section_active) {
        emit_section_segment(MPI_Wtime());
    }

    g_active_tag     = *tag;
    g_active_peer    = peer_rank ? *peer_rank : -2;
    g_active_msgtag  = msg_tag  ? *msg_tag   : -2;
    if (name && *name_len > 0) {
        g_active_name.assign(name, static_cast<std::size_t>(*name_len));
    } else {
        g_active_name = resolve_name(*tag, "");
    }
    /* For point-to-point calls the tag is the MPI message tag (>= 0). Include
       both tag and peer rank (when available) so sections are unambiguous:
       e.g. MPI_Send (tag=42, peer=3) / MPI_Recv (tag=42, peer=0). */
    if (*tag >= 0) {
        char suffix[48];
        if (g_active_peer >= 0) {
            std::snprintf(suffix, sizeof(suffix), " (tag=%d, peer=%d)", *tag, g_active_peer);
        } else {
            std::snprintf(suffix, sizeof(suffix), " (tag=%d)", *tag);
        }
        g_active_name += suffix;
    }
    g_t_begin = MPI_Wtime();
}

void spmd_profiler_record_out(const int* tag)
{
    const double t_end = MPI_Wtime();
    if (!g_initialized) return;

    if (g_mode == MODE_TRACE) {
        SpmdEntry e;
        e.tag       = g_active_tag;
        e.name      = g_active_name;
        e.t_begin   = g_t_begin;
        e.t_end     = t_end;
        e.peer_rank = g_active_peer;
        e.msg_tag   = g_active_msgtag;
        g_timeline.push_back(e);
    } else {
        double dur = t_end - g_t_begin;
        TagStats& s = g_stats[g_active_name];
        if (s.count == 0) {
            s.tag  = g_active_tag;
            s.name = g_active_name;
        }
        s.count++;
        s.total_s += dur;
        if (dur < s.min_s) s.min_s = dur;
        if (dur > s.max_s) s.max_s = dur;
    }

    /* Resume active user section: restart its segment from now */
    if (g_section_active) {
        g_section_begin = MPI_Wtime();
    }

    (void)tag;
}

void spmd_profiler_section_begin(const int* tag, const char* name, const int* name_len)
{
    lazy_init();
    const double t_now = MPI_Wtime();

    /* Auto-close previous section if still active */
    if (g_section_active) {
        emit_section_segment(t_now);
    }

    /* Start new section */
    g_section_active = true;
    g_section_tag    = *tag;
    g_section_begin  = t_now;

    if (name && *name_len > 0) {
        g_section_name.assign(name, static_cast<std::size_t>(*name_len));
    } else {
        char buf[48];
        std::snprintf(buf, sizeof(buf), "section (tag=%d)", *tag);
        g_section_name = buf;
    }
}

void spmd_profiler_section_end(const int* tag)
{
    if (!g_initialized || !g_section_active) return;
    emit_section_segment(MPI_Wtime());
    g_section_active = false;
    (void)tag;
}

void spmd_profiler_register_request(const int* request, const int* peer_rank,
                                    const int* msg_tag,  const int* is_recv)
{
    if (!g_initialized) lazy_init();
    PendingRequest pr;
    pr.peer_rank = *peer_rank;
    pr.msg_tag   = *msg_tag;
    pr.is_recv   = (*is_recv != 0);
    g_request_table[*request] = pr;
}

/* Helper: emit one arrow-endpoint entry (a zero-duration event) at t_end. */
static void emit_arrow_endpoint(const PendingRequest& pr, double t_end)
{
    if (!pr.is_recv) return;  /* only recv-completions are arrow endpoints */

    SpmdEntry e;
    e.tag       = -2000;       /* synthetic tag for arrow endpoints */
    e.t_begin   = t_end;
    e.t_end     = t_end;
    e.peer_rank = pr.peer_rank;
    e.msg_tag   = pr.msg_tag;

    /* Build a label: "arrow_endpoint (tag=T)" */
    char buf[64];
    std::snprintf(buf, sizeof(buf), "arrow_endpoint (tag=%d)", pr.msg_tag);
    e.name = buf;

    if (g_mode == MODE_TRACE) {
        g_timeline.push_back(e);
    }
    /* stats mode: arrow endpoints are not aggregated */
}

void spmd_profiler_complete_request(const int* request, const double* t_end)
{
    if (!g_initialized) return;
    auto it = g_request_table.find(*request);
    if (it == g_request_table.end()) return;
    emit_arrow_endpoint(it->second, *t_end);
    g_request_table.erase(it);
}

void spmd_profiler_complete_requests(const int* requests, const int* count,
                                     const double* t_end)
{
    if (!g_initialized) return;
    for (int i = 0; i < *count; ++i) {
        auto it = g_request_table.find(requests[i]);
        if (it == g_request_table.end()) continue;
        emit_arrow_endpoint(it->second, *t_end);
        g_request_table.erase(it);
    }
}

void spmd_profiler_flush(void)
{
    if (!g_initialized || g_rank < 0) return;
    if (g_mode == MODE_TRACE)
        flush_trace();
    else
        flush_stats();
}

} /* extern "C" */
