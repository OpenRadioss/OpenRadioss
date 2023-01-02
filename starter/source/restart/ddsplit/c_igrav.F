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
Chd|  C_IGRAV                       source/restart/ddsplit/c_igrav.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_IGRAV(IGRAV,LGRAV,LLGRAV_L,PROC,NODLOCAL,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LLGRAV_L,
     .        IGRAV(NIGRV,*), LGRAV(*) ,NUMNOD_L    
      INTEGER, DIMENSION(*), INTENT(IN) :: NODLOCAL
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IAD, NL, N, ILP
C
      ILP = 0
      DO I = 1, NGRAV
        NL = IGRAV(1,I)
        IAD= IGRAV(4,I)
        DO J = 1, NL
          N = ABS(LGRAV(IAD+J-1))
          IF (NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L)THEN
            ILP = ILP + 1
          ENDIF
        ENDDO
      ENDDO
C
      LLGRAV_L = ILP
C
      RETURN
      END
