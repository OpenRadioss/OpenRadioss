Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|-- called by -----------
Chd|        DRBE2CNT                      source/output/anim/generate/drbe2cnt.F
Chd|        DRBE3CNT                      source/output/anim/generate/drbe3cnt.F
Chd|        DRBYCNT                       source/output/anim/generate/drbycnt.F
Chd|        DSCALN                        source/implicit/dsolve/dscaln.F
Chd|        DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|        DSRESOL_BUCK                  source/implicit/dsolve/dsresol.F
Chd|        DSRESOL_ITER                  source/implicit/dsolve/dsresol.F
Chd|        FR_RLINK1                     source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        LECTUR                        source/input/lectur.F         
Chd|        MANCTR                        source/input/manctr.F         
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|        R2R_INPUT_INIT                source/coupling/rad2rad/r2r_input_init.F
Chd|        RBYONF                        source/constraints/general/rbody/rbyonf.F
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SENSOR0                       source/tools/sensor/sensor0.F 
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|        SMS_INI_KIN_2                 source/ams/sms_init.F         
Chd|        SPMD_INIT_EBCS                source/mpi/fluid/spmd_cfd.F   
Chd|        SZ_PRINT                      source/output/restart/arralloc.F
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_IBCAST(TABI,TABR,N1,N2,FROM,ADD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
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
     .                 MPI_COMM_WORLD,ierror)
      ELSE
        call MPI_BCAST(TABR,N1*N2,MPI_INTEGER,FROM,
     .                 MPI_COMM_WORLD,ierror)
      ENDIF

#endif
      RETURN
      END
