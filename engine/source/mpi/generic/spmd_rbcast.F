Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|-- called by -----------
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        AGAUGE0                       source/ale/agauge.F           
Chd|        ALELIN                        source/ale/grid/alelin.F      
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|        GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|        MANCTR                        source/input/manctr.F         
Chd|        MOVFRA1                       source/tools/skew/movfram.F   
Chd|        MOVFRA2                       source/tools/skew/movfram.F   
Chd|        MOVFRA_IMP                    source/tools/skew/movfram.F   
Chd|        R2R_INPUT_INIT                source/coupling/rad2rad/r2r_input_init.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SECT_INI                      source/tools/sect/sectio.F    
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|        SENSOR_SPMD                   source/tools/sensor/sensor_spmd.F
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|        SPMD_I21CRIT                  source/mpi/interfaces/spmd_i21crit.F
Chd|        GET_U_NOD_A                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_D                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_V                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_X                   source/user_interface/uaccess.F
Chd|-- calls ---------------
Chd|====================================================================
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
