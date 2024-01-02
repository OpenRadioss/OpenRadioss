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
Chd|====================================================================
Chd|  SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|-- called by -----------
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        IMP_STIF24                    source/implicit/imp_solv.F    
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_INI_ERR                   source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|        SPCLASV                       source/elements/sph/spclasv.F 
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_ALLGLOB_ISUM9(V,LEN)
C gather tableau V de taille LEN de type integer et broadcast sur tous les processeurs
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,IERROR,
     .        VTMP(LEN),STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_ALLREDUCE(V,VTMP,LEN,
     .                  MPI_INTEGER,MPI_SUM,
     .                  MPI_COMM_WORLD,IERROR)
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
