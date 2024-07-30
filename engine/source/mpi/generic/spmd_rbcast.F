Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.



C
      !||====================================================================
      !||    spmd_rbcast                       ../engine/source/mpi/generic/spmd_rbcast.F
      !||--- called by ------------------------------------------------------
      !||    agauge                            ../engine/source/ale/agauge.F
      !||    agauge0                           ../engine/source/ale/agauge.F
      !||    alelin                            ../engine/source/ale/grid/alelin.F
      !||    ecrit                             ../engine/source/output/ecrit.F
      !||    find_dt_for_targeted_added_mass   ../engine/source/time_step/find_dt_for_targeted_added_mass.F
      !||    get_mass_rby_spmd                 ../engine/source/coupling/rad2rad/r2r_init.F
      !||    get_u_nod_a                       ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_d                       ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_v                       ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_x                       ../engine/source/user_interface/uaccess.F
      !||    i9wal2                            ../engine/source/interfaces/int09/i9wal2.F
      !||    i9wal3                            ../engine/source/interfaces/int09/i9wal3.F
      !||    imp_buck                          ../engine/source/implicit/imp_buck.F
      !||    intstamp_ass                      ../engine/source/interfaces/int21/intstamp_ass.F
      !||    manctr                            ../engine/source/input/manctr.F
      !||    movfra1                           ../engine/source/tools/skew/movfram.F
      !||    movfra2                           ../engine/source/tools/skew/movfram.F
      !||    movfra_imp                        ../engine/source/tools/skew/movfram.F
      !||    r2r_input_init                    ../engine/source/coupling/rad2rad/r2r_input_init.F
      !||    resol                             ../engine/source/engine/resol.F
      !||    section_init                      ../engine/source/tools/sect/section_init.F
      !||    section_readp                     ../engine/source/tools/sect/section_readp.F
      !||    sensor_spmd                       ../engine/source/tools/sensor/sensor_spmd.F
      !||    sms_pcg                           ../engine/source/ams/sms_pcg.F
      !||    sms_produt3                       ../engine/source/ams/sms_proj.F
      !||    sms_produt_h                      ../engine/source/ams/sms_proj.F
      !||    spmd_i21crit                      ../engine/source/mpi/interfaces/spmd_i21crit.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_RBCAST(TABI,TABR,N1,N2,FROM,ADD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,FROM,ADD
      my_real TABI(N1*N2),TABR(N1*N2)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER INFO,MSGOFF
      INTEGER K
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

C     si FROM = 0 ADD = 2 broadcast depuis 0 sur les autres
C     si FROM = NSPMD  ADD = 1 de NSPMD sur les autres

      IF(ISPMD==FROM)THEN
        call MPI_BCAST(TABI,N1*N2,REAL,FROM,
     .                 MPI_COMM_WORLD,ierror)
      ELSE
        call MPI_BCAST(TABR,N1*N2,REAL,FROM,
     .                 MPI_COMM_WORLD,ierror)
      ENDIF

#endif
      RETURN
      END
