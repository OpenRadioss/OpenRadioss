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
Chd|  W_BUFEL                       source/restart/ddsplit/w_bufel.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_BUFEL(IPARG,PROC,LBUFEL_L,ELBUF,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LBUFEL_L, LEN_AM,
     .        IPARG(NPARG,*)
      my_real
     .        ELBUF(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, P, LB_L, LEN, J, IAD
      my_real,
     .    DIMENSION(:),ALLOCATABLE :: ELBUF_L

      ALLOCATE(ELBUF_L(LBUFEL_L))
C
      LB_L = 0
      DO NG=1,NGROUP
        P   = IPARG(32,NG)
        IF(P==PROC) THEN
          IAD = IPARG(4,NG)
          IF(NG<NGROUP) THEN
            LEN = IPARG(4,NG+1) - IPARG(4,NG)
          ELSE
             LEN =LBUFEL + 1 - IPARG(4,NG)
          ENDIF
          DO J = 1, LEN
            ELBUF_L(LB_L+J) = ELBUF(IAD+J-1)
          ENDDO
          LB_L = LB_L + LEN
        ENDIF
      ENDDO
C
      CALL WRITE_DB(ELBUF_L,LBUFEL_L)
      LEN_AM = LEN_AM + LBUFEL_L

      DEALLOCATE(ELBUF_L)
C
      RETURN
      END
