/*! \file spmd_profiler.h
 *  \brief Public C interface for the SPMD MPI profiler.
 *
 *  Called from Fortran via ISO_C_BINDING (bind(c)) interfaces declared
 *  in spmd_error_mod and spmd_profiler_mod.
 *
 *  Trace mode (default, SPMD_PROFILE_MODE unset or "trace"):
 *    One binary timeline file per rank.  With -DWITH_ZLIB it is
 *    gzip-compressed:
 *      spmd_timeline_rank_NNNNN.spmd.gz
 *    Without -DWITH_ZLIB it is uncompressed:
 *      spmd_timeline_rank_NNNNN.spmd
 *    Convert for viewing (reads either form):
 *      python3 spmd_trace_convert.py rank_*.spmd*
 *
 *  Stats mode (SPMD_PROFILE_MODE=stats):
 *    One compact JSON summary per rank (count/total/min/max per tag):
 *      spmd_stats_rank_NNNNN.json
 */
#ifndef SPMD_PROFILER_H
#define SPMD_PROFILER_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
 * \brief Initialize the profiler.
 *
 * Optional: the profiler auto-initialises on the first
 * spmd_profiler_record_in() call.  Call this explicitly (after MPI_Init)
 * only when you want to pre-allocate the buffer or override the rank.
 *
 * \param rank  MPI rank to use as the process ID in the trace file.
 *              Pass a negative value to query MPI_COMM_WORLD automatically.
 */
void spmd_profiler_init(const int* rank);

/*!
 * \brief Record the beginning of an MPI (or user) call.
 *
 * Called from spmd_in() just before the actual MPI function.
 * \param tag        SPMD tag identifying the operation.
 * \param name       Human-readable name (e.g. "MPI_Send").  May be NULL or
 *                   empty; falls back to the static TAG_NAMES table.
 * \param name_len   Number of valid characters in \p name (no null terminator).
 * \param peer_rank  Destination rank for sends, source rank for recvs.
 *                   Pass -2 when not applicable (collectives, etc.).
 * \param msg_tag    Actual MPI message tag for P2P operations.
 *                   Pass -2 when not applicable.
 */
void spmd_profiler_record_in(const int* tag, const char* name, const int* name_len,
                              const int* peer_rank, const int* msg_tag);

/*!
 * \brief Record the end of an MPI call.
 *
 * Called from spmd_out() immediately after the actual MPI function.
 * \param tag  SPMD tag (same value as the matching spmd_profiler_record_in).
 */
void spmd_profiler_record_out(const int* tag);

/*!
 * \brief Register a posted non-blocking request (Isend or Irecv).
 *
 * Must be called immediately after MPI_Isend / MPI_Irecv returns so the
 * request handle is valid.  Stores {peer_rank, msg_tag, is_recv} in an
 * internal table so Wait calls can later resolve per-message arrow endpoints.
 *
 * \param request    MPI request handle (Fortran integer).
 * \param peer_rank  Destination (Isend) or source (Irecv) rank.
 * \param msg_tag    MPI message tag.
 * \param is_recv    1 if this is a recv request, 0 if send.
 */
void spmd_profiler_register_request(const int* request, const int* peer_rank,
                                    const int* msg_tag,  const int* is_recv);

/*!
 * \brief Mark one request as completed (MPI_Wait / MPI_Waitany).
 *
 * If the request is a recv, emits an arrow-endpoint record into the timeline
 * so the converter can draw an arrow from the matching send.
 *
 * \param request  MPI request handle that was passed to MPI_Wait.
 * \param t_end    Wall-clock time at which the Wait returned (MPI_Wtime()).
 */
void spmd_profiler_complete_request(const int* request, const double* t_end);

/*!
 * \brief Mark N requests as completed (MPI_Waitall).
 *
 * Calls spmd_profiler_complete_request for each entry in \p requests.
 *
 * \param requests  Array of N MPI request handles.
 * \param count     Number of entries in \p requests.
 * \param t_end     Wall-clock time at which Waitall returned.
 */
void spmd_profiler_complete_requests(const int* requests, const int* count,
                                     const double* t_end);

/*!
 * \brief Write the timeline to disk and clear the in-memory buffer.
 *
 * Must be called before MPI_Finalize (MPI_Wtime becomes invalid after
 * finalization).  Output file: spmd_timeline_rank_NNNNN.json.
 */
void spmd_profiler_flush(void);

/*!
 * \brief Begin a named user section for profiling.
 *
 * If a section is already active, it is auto-closed first.
 * User sections are suspended by MPI calls (spmd_in/spmd_out) and
 * automatically resumed after the MPI call completes.
 *
 * \param tag       User-chosen tag (use values <= -3000 to avoid MPI tag collision).
 * \param name      Human-readable section name (e.g. "force_assembly").
 *                  May be NULL; falls back to "section (tag=N)".
 * \param name_len  Number of valid characters in \p name.
 */
void spmd_profiler_section_begin(const int* tag, const char* name, const int* name_len);

/*!
 * \brief End the active user section.
 *
 * Emits the final segment of the section. No-op if no section is active.
 *
 * \param tag  Must match the tag passed to section_begin (currently unused,
 *             reserved for future validation).
 */
void spmd_profiler_section_end(const int* tag);

#ifdef __cplusplus
}
#endif

#endif /* SPMD_PROFILER_H */
