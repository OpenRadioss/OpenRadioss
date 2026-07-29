!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_error_mod                    ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgather_double             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles2d          ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_int                ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints2d             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_real               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals2d            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_double            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles2d         ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int               ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints2d            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals2d           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles2d          ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_int                ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints2d             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_real               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals              ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals2d            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_alltoall_double              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles2d           ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int                 ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints2d              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals2d             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoallv_double             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles2d          ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_int                ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints2d             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_real               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals              ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals2d            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_barrier_comm                 ../engine/source/mpi/generic/spmd_barrier.F90
!||    spmd_bcast_double                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles2d              ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_int                    ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints2d                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_real                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals                  ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals2d                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_gather_double                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles2d             ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_int                   ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints2d                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_real                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals                 ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals2d               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gatherv_double               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles2d            ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_int                  ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints2d               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_real                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals                ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals2d              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_get_count_double             ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_int                ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_real               ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_iallgather_double            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles2d         ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_int               ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints2d            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_real              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals             ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals2d           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgatherv_double           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles2d        ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_int              ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints2d           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_real             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals            ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals2d          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallreduce_double            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles2d         ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_int               ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints2d            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_real              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals             ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals2d           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_ialltoall_double             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles2d          ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_int                ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints2d             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_real               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals              ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals2d            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoallv_double            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles2d         ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_int               ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints2d            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_real              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals             ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals2d           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ibcast_double                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles2d             ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_int                   ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints2d                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_real                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals                 ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals2d               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_igather_double               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles2d            ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_int                  ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints2d               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_real                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals                ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals2d              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_iprobe_basic                 ../engine/source/mpi/generic/spmd_iprobe.F90
!||    spmd_irecv_double                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles2d              ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int                    ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints2d                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals2d                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_ireduce_double               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles2d            ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_int                  ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints2d               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_real                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals                ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals2d              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_iscatter_double              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles2d           ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_int                 ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints2d              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_real                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals               ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals2d             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_isend_double                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles                ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles2d              ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int                    ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints2d                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals2d                ../engine/source/mpi/spmd_isend.F90
!||    spmd_mod                          ../engine/source/mpi/spmd_mod.F90
!||    spmd_pack_doubles                 ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints                    ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals                   ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe_basic                  ../engine/source/mpi/generic/spmd_probe.F90
!||    spmd_recv_double                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d               ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int                     ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints2d                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles2d             ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_int                   ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints2d                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_real                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals                 ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals2d               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_scatter_double               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles2d            ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_int                  ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints2d               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_real                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals                ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals2d              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatterv_double              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles2d           ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_int                 ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints2d              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_real                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals               ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals2d             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_send_double                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles                 ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d               ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int                     ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints2d                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals                   ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d                 ../engine/source/mpi/spmd_send.F90
!||    spmd_sendrecv_double              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles2d           ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_int                 ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints2d              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_real                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals               ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals2d             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_replace_double      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles2d   ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_int         ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints2d      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_real        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals       ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals2d     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_test_req                     ../engine/source/mpi/generic/spmd_test.F90
!||    spmd_testall_req                  ../engine/source/mpi/generic/spmd_testall.F90
!||    spmd_testany_req                  ../engine/source/mpi/generic/spmd_testany.F90
!||    spmd_testsome_req                 ../engine/source/mpi/generic/spmd_testsome.F90
!||    spmd_unpack_doubles               ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints                  ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals                 ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait_req                     ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall_req                  ../engine/source/mpi/generic/spmd_waitall.F90
!||    spmd_waitany_req                  ../engine/source/mpi/generic/spmd_waitany.F90
!||    spmd_waitsome_req                 ../engine/source/mpi/generic/spmd_waitsome.F90
!||====================================================================
      module spmd_error_mod
        use, intrinsic :: iso_c_binding
        implicit none

        interface
          function c_getpid() bind(c, name="getpid")
            import :: c_int
            integer(c_int) :: c_getpid
          end function c_getpid

          function c_system(command) bind(c, name="system")
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: command(*)
            integer(c_int) :: c_system
          end function c_system
        end interface

        !> Profiler hooks — implemented in spmd_profiler.cpp
        interface
          subroutine spmd_profiler_record_in_c(tag, name, name_len, peer_rank, msg_tag) &
            bind(c, name="spmd_profiler_record_in")
            import :: c_int, c_char
            integer(c_int), intent(in) :: tag
            character(kind=c_char), dimension(*), intent(in) :: name
            integer(c_int), intent(in) :: name_len
            integer(c_int), intent(in) :: peer_rank
            integer(c_int), intent(in) :: msg_tag
          end subroutine spmd_profiler_record_in_c

          subroutine spmd_profiler_record_out_c(tag) &
            bind(c, name="spmd_profiler_record_out")
            import :: c_int
            integer(c_int), intent(in) :: tag
          end subroutine spmd_profiler_record_out_c

          subroutine spmd_profiler_register_request_c(request, peer_rank, msg_tag, is_recv) &
            bind(c, name="spmd_profiler_register_request")
            import :: c_int
            integer(c_int), intent(in) :: request, peer_rank, msg_tag, is_recv
          end subroutine spmd_profiler_register_request_c

          subroutine spmd_profiler_complete_request_c(request, t_end) &
            bind(c, name="spmd_profiler_complete_request")
            import :: c_int, c_double
            integer(c_int), intent(in) :: request
            real(c_double), intent(in) :: t_end
          end subroutine spmd_profiler_complete_request_c

          subroutine spmd_profiler_complete_requests_c(requests, count, t_end) &
            bind(c, name="spmd_profiler_complete_requests")
            import :: c_int, c_double
            integer(c_int), intent(in) :: requests(*)
            integer(c_int), intent(in) :: count
            real(c_double), intent(in) :: t_end
          end subroutine spmd_profiler_complete_requests_c
        end interface

      contains

#ifdef DEBUG_SPMD
!||====================================================================
!||    print_traceback   ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine print_traceback()
          implicit none
          integer(c_int) :: pid
          character(len=200) :: command
          character(len=200, kind=c_char) :: c_command
          integer :: i, result

          write(*,*) "=== TRACEBACK START ==="

          pid = c_getpid()
          write(*,*) "Process ID:", pid

          write(command, '("gdb -batch -ex ""set confirm off"" -ex ""bt"" -p ", I0, " 2>/dev/null")') pid

          ! Convert Fortran string to C string
          do i = 1, len_trim(command)
            c_command(i:i) = command(i:i)
          end do
          c_command(len_trim(command)+1:len_trim(command)+1) = c_null_char

          !write(*,*) "Executing:", trim(command)
          result = c_system(c_command)

          if (result /= 0) then
            write(*,*) "Note: gdb command returned status", result
            write(*,*) "If no backtrace appeared, gdb might not be available"
            write(*,*) "or the process might not have debug symbols."
          end if

          write(*,*) "=== TRACEBACK END ==="
        end subroutine print_traceback
#endif



!! \brief Trace Entry in MPI subroutines
!>  \brief Trace entry for an MPI (or user) call.
!>  \param tag   Integer tag identifying the call (negative = SPMD collective,
!>               positive = MPI message tag, arbitrary = user-defined region).
!>  \param name  Optional human-readable name, e.g. "MPI_Send" or "my_routine".
!>               When present, the profiler uses this string instead of the
!>               generic tag-to-name lookup, allowing Send vs Isend (etc.) to
!>               be distinguished even when they share the same message tag.
!>  \param peer  Optional peer rank: destination for sends, source for recvs.
!>               When provided, enables arrow drawing in the trace visualizer.
!||====================================================================
!||    spmd_in                           ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgather_double             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles2d          ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_int                ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints2d             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_real               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals2d            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_double            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles2d         ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int               ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints2d            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals2d           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles2d          ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_int                ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints2d             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_real               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals              ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals2d            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_alltoall_double              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles2d           ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int                 ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints2d              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals2d             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoallv_double             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles2d          ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_int                ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints2d             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_real               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals              ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals2d            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_barrier_comm                 ../engine/source/mpi/generic/spmd_barrier.F90
!||    spmd_bcast_double                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles2d              ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_int                    ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints2d                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_real                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals                  ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals2d                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_comm_rank                    ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_size                    ../engine/source/mpi/spmd_mod.F90
!||    spmd_gather_double                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles2d             ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_int                   ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints2d                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_real                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals                 ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals2d               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gatherv_double               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles2d            ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_int                  ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints2d               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_real                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals                ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals2d              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_get_count_double             ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_int                ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_real               ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_iallgather_double            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles2d         ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_int               ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints2d            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_real              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals             ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals2d           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgatherv_double           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles2d        ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_int              ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints2d           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_real             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals            ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals2d          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallreduce_double            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles2d         ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_int               ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints2d            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_real              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals             ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals2d           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_ialltoall_double             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles2d          ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_int                ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints2d             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_real               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals              ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals2d            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoallv_double            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles2d         ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_int               ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints2d            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_real              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals             ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals2d           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ibcast_double                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles2d             ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_int                   ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints2d                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_real                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals                 ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals2d               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_igather_double               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles2d            ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_int                  ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints2d               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_real                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals                ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals2d              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_iprobe_basic                 ../engine/source/mpi/generic/spmd_iprobe.F90
!||    spmd_irecv_double                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles2d              ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int                    ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints2d                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals2d                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_ireduce_double               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles2d            ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_int                  ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints2d               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_real                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals                ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals2d              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_iscatter_double              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles2d           ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_int                 ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints2d              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_real                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals               ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals2d             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_isend_double                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles                ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles2d              ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int                    ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints2d                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals2d                ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_doubles                 ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints                    ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals                   ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe_basic                  ../engine/source/mpi/generic/spmd_probe.F90
!||    spmd_recv_double                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d               ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int                     ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints2d                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles2d             ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_int                   ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints2d                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_real                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals                 ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals2d               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_scatter_double               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles2d            ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_int                  ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints2d               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_real                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals                ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals2d              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatterv_double              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles2d           ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_int                 ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints2d              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_real                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals               ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals2d             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_send_double                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles                 ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d               ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int                     ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints2d                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals                   ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d                 ../engine/source/mpi/spmd_send.F90
!||    spmd_sendrecv_double              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles2d           ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_int                 ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints2d              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_real                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals               ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals2d             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_replace_double      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles2d   ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_int         ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints2d      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_real        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals       ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals2d     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_test_req                     ../engine/source/mpi/generic/spmd_test.F90
!||    spmd_testall_req                  ../engine/source/mpi/generic/spmd_testall.F90
!||    spmd_testany_req                  ../engine/source/mpi/generic/spmd_testany.F90
!||    spmd_testsome_req                 ../engine/source/mpi/generic/spmd_testsome.F90
!||    spmd_unpack_doubles               ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints                  ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals                 ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait_req                     ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall_req                  ../engine/source/mpi/generic/spmd_waitall.F90
!||    spmd_waitany_req                  ../engine/source/mpi/generic/spmd_waitany.F90
!||    spmd_waitsome_req                 ../engine/source/mpi/generic/spmd_waitsome.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_profiler_mod                 ../engine/source/mpi/generic/spmd_profiler_mod.F90
!||====================================================================
        subroutine spmd_in(tag, name, peer)
          use spmd_profiler_mod, only: spmd_profiling_enabled
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: tag
          character(len=*), intent(in), optional :: name
          integer, intent(in), optional :: peer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer(c_int) :: tag_c, name_len_c, peer_c, msgtag_c
          character(kind=c_char), dimension(65) :: name_c
          integer :: i, n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (spmd_profiling_enabled) then
            tag_c = int(tag, c_int)
            if (present(name)) then
              n = min(len_trim(name), 64)
              do i = 1, n
                name_c(i) = name(i:i)
              end do
              name_c(n+1) = c_null_char
              name_len_c = int(n, c_int)
            else
              name_c(1) = c_null_char
              name_len_c = 0_c_int
            end if
            if (present(peer)) then
              peer_c = int(peer, c_int)
            else
              peer_c = -2_c_int
            end if
            if (tag >= 0) then
              msgtag_c = int(tag, c_int)
            else
              msgtag_c = -2_c_int
            end if
            call spmd_profiler_record_in_c(tag_c, name_c, name_len_c, peer_c, msgtag_c)
          end if
#ifdef DEBUG_SPMD
          ! call print_traceback()
          if (present(name)) then
            write(6,*) "Entering MPI call: ", trim(name), " (tag=", tag, ")"
          else
            write(6,*) "Entering MPI call: ", tag
          end if
#endif
        end subroutine spmd_in

!||====================================================================
!||    spmd_out                          ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgather_double             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_doubles2d          ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_int                ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_ints2d             ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_real               ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgather_reals2d            ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_double            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles2d         ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int               ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints2d            ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real              ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals2d           ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_doubles2d          ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_int                ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_ints2d             ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_real               ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals              ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_allreduce_reals2d            ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_alltoall_double              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles2d           ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int                 ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints2d              ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real                ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals2d             ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoallv_double             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_doubles2d          ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_int                ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_ints2d             ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_real               ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals              ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_alltoallv_reals2d            ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_barrier_comm                 ../engine/source/mpi/generic/spmd_barrier.F90
!||    spmd_bcast_double                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_doubles2d              ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_int                    ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_ints2d                 ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_real                   ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals                  ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_bcast_reals2d                ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_comm_rank                    ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_size                    ../engine/source/mpi/spmd_mod.F90
!||    spmd_gather_double                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_doubles2d             ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_int                   ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_ints2d                ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_real                  ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals                 ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gather_reals2d               ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gatherv_double               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_doubles2d            ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_int                  ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_ints2d               ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_real                 ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals                ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_gatherv_reals2d              ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_get_count_double             ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_int                ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_get_count_real               ../engine/source/mpi/generic/spmd_get_count.F90
!||    spmd_iallgather_double            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_doubles2d         ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_int               ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_ints2d            ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_real              ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals             ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgather_reals2d           ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgatherv_double           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_doubles2d        ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_int              ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_ints2d           ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_real             ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals            ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallgatherv_reals2d          ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallreduce_double            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_doubles2d         ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_int               ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_ints2d            ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_real              ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals             ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_iallreduce_reals2d           ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_ialltoall_double             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_doubles2d          ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_int                ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_ints2d             ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_real               ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals              ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoall_reals2d            ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoallv_double            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_doubles2d         ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_int               ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_ints2d            ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_real              ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals             ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ialltoallv_reals2d           ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ibcast_double                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_doubles2d             ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_int                   ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_ints2d                ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_real                  ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals                 ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_ibcast_reals2d               ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_igather_double               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_doubles2d            ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_int                  ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_ints2d               ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_real                 ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals                ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_igather_reals2d              ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_iprobe_basic                 ../engine/source/mpi/generic/spmd_iprobe.F90
!||    spmd_irecv_double                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles2d              ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int                    ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints2d                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real                   ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals2d                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_ireduce_double               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_doubles2d            ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_int                  ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_ints2d               ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_real                 ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals                ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_ireduce_reals2d              ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_iscatter_double              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_doubles2d           ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_int                 ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_ints2d              ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_real                ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals               ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_iscatter_reals2d             ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_isend_double                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles                ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles2d              ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int                    ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints2d                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real                   ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals2d                ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_doubles                 ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints                    ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals                   ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe_basic                  ../engine/source/mpi/generic/spmd_probe.F90
!||    spmd_recv_double                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d               ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int                     ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints2d                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real                    ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_doubles2d             ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_int                   ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_ints2d                ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_real                  ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals                 ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_reduce_reals2d               ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_scatter_double               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_doubles2d            ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_int                  ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_ints2d               ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_real                 ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals                ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatter_reals2d              ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatterv_double              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_doubles2d           ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_int                 ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_ints2d              ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_real                ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals               ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_scatterv_reals2d             ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_send_double                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles                 ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d               ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int                     ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints2d                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real                    ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals                   ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d                 ../engine/source/mpi/spmd_send.F90
!||    spmd_sendrecv_double              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_doubles2d           ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_int                 ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_ints2d              ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_real                ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals               ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_reals2d             ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_replace_double      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_doubles2d   ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_int         ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_ints2d      ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_real        ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals       ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sendrecv_replace_reals2d     ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_test_req                     ../engine/source/mpi/generic/spmd_test.F90
!||    spmd_testall_req                  ../engine/source/mpi/generic/spmd_testall.F90
!||    spmd_testany_req                  ../engine/source/mpi/generic/spmd_testany.F90
!||    spmd_testsome_req                 ../engine/source/mpi/generic/spmd_testsome.F90
!||    spmd_unpack_doubles               ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints                  ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals                 ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait_req                     ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall_req                  ../engine/source/mpi/generic/spmd_waitall.F90
!||    spmd_waitany_req                  ../engine/source/mpi/generic/spmd_waitany.F90
!||    spmd_waitsome_req                 ../engine/source/mpi/generic/spmd_waitsome.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod               ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_profiler_mod                 ../engine/source/mpi/generic/spmd_profiler_mod.F90
!||====================================================================
        subroutine spmd_out(tag, ierr)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_profiler_mod, only: spmd_profiling_enabled
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: tag !< Tag of the the MPI call
          integer, intent(in) :: ierr !< error of the MPI call
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierror
          integer(c_int) :: tag_c
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (spmd_profiling_enabled) then
            tag_c = int(tag, c_int)
            call spmd_profiler_record_out_c(tag_c)
          end if
#ifdef MPI
          if(ierr /= MPI_SUCCESS) then
            write(6,*) "MPI error: ", ierr," at ",tag
            call MPI_Abort(SPMD_COMM_WORLD, ierr,ierror)
          end if
#ifdef DEBUG_SPMD
          write(6,*) "Exiting MPI call: ", tag
#endif
#endif
        end subroutine spmd_out

      end module spmd_error_mod
