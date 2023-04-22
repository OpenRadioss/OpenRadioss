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
Chd|====================================================================
Chd|  TSHCDCOM_DIM                  source/elements/thickshell/solidec/tshcdcom_dim.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        SPMD_EXCH_TAG_TSH             source/mpi/elements/spmd_exch_dttsh.F
Chd|====================================================================
      SUBROUTINE TSHCDCOM_DIM(IENUNL,FR_ELEM,IAD_ELEM,NBDDS,NBDDR ,
     .                        ISEND ,IRECV )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IENUNL(2,*),FR_ELEM(*),IAD_ELEM(2,*),NBDDS,NBDDR,
     .        IRECV(*),ISEND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IP,ITAGS(NUMNOD),LEN
C=======================================================================
       ITAGS(1:NUMNOD)=0
       DO I = 1, NTSHEG
         N = IENUNL(1,I)
         ITAGS(N) = I
         N = IENUNL(2,I)
         ITAGS(N) = I
       END DO
C--------
       NBDDS = 0
       DO IP = 1,NSPMD
         DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N = FR_ELEM(J)
          IF (ITAGS(N)>0) THEN
            NBDDS = NBDDS + 1
            ISEND(J) = 1
          END IF
         END DO
       END DO
       LEN = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
       CALL SPMD_EXCH_TAG_TSH(IAD_ELEM ,FR_ELEM ,ISEND,IRECV ,LEN)
       NBDDR = 0
       DO IP = 1,NSPMD
         DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          IF (IRECV(J)>0) NBDDR = NBDDR + 1
         END DO
       END DO
C
      RETURN
      END
