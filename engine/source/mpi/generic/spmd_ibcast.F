Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    spmd_ibcast         ../engine/source/mpi/generic/spmd_ibcast.F
      !||--- called by ------------------------------------------------------
      !||    drbe2cnt            ../engine/source/output/anim/generate/drbe2cnt.F
      !||    drbe3cnt            ../engine/source/output/anim/generate/drbe3cnt.F
      !||    drbycnt             ../engine/source/output/anim/generate/drbycnt.F
      !||    fr_rlink1           ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fxbypid             ../engine/source/constraints/fxbody/fxbypid.F
      !||    genani              ../engine/source/output/anim/generate/genani.F
      !||    get_mass_rby_spmd   ../engine/source/coupling/rad2rad/r2r_init.F
      !||    i9wal2              ../engine/source/interfaces/int09/i9wal2.F
      !||    i9wal3              ../engine/source/interfaces/int09/i9wal3.F
      !||    lectur              ../engine/source/input/lectur.F
      !||    manctr              ../engine/source/input/manctr.F
      !||    mpp_init            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    r2r_exchange        ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    r2r_init            ../engine/source/coupling/rad2rad/r2r_init.F
      !||    r2r_input_init      ../engine/source/coupling/rad2rad/r2r_input_init.F
      !||    rbyonf              ../engine/source/constraints/general/rbody/rbyonf.F
      !||    rbypid              ../engine/source/constraints/general/rbody/rbypid.F
      !||    resol               ../engine/source/engine/resol.F
      !||    sensor_spmd         ../engine/source/tools/sensor/sensor_spmd.F
      !||    sms_build_diag      ../engine/source/ams/sms_build_diag.F
      !||    sms_ini_kin_1       ../engine/source/ams/sms_init.F
      !||    sms_ini_kin_2       ../engine/source/ams/sms_init.F
      !||    spmd_init_ebcs      ../engine/source/mpi/fluid/spmd_cfd.F
      !||    sz_print            ../engine/source/output/restart/arralloc.F
      !||    tagoff3n            ../engine/source/interfaces/interf/chkstfn3.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_IBCAST(TABI,TABR,N1,N2,FROM,ADD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,FROM,ADD
      INTEGER TABI(N1*N2),TABR(N1*N2)
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
       call MPI_BCAST(TABI,N1*N2,MPI_INTEGER,FROM,
     .                 SPMD_COMM_WORLD,ierror)
      ELSE
        call MPI_BCAST(TABR,N1*N2,MPI_INTEGER,FROM,
     .                 SPMD_COMM_WORLD,ierror)
      ENDIF

#endif
      RETURN
      END
