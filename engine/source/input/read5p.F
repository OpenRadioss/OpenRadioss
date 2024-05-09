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
Chd|====================================================================
Chd|  READ5P                        source/input/read5p.F         
Chd|-- called by -----------
Chd|        RDELE                         source/input/rdele.F          
Chd|-- calls ---------------
Chd|        WCIUSC2                       source/input/wciusc2.F        
Chd|====================================================================
      SUBROUTINE READ5P(IREC,NBC,KEY0,IV2,JJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IV2(*), IREC, NBC, JJ
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IV0(40), IV1(80), K, I, N, J, NC1, NC2
C-----------------------------------------------
C
       K=0
       DO I=1,NBC
        CALL WCIUSC2(IREC+K,1,N,KEY0)
        READ(IUSC2,*)(IV0(J),J=1,N)
        DO J=1,N
          IV1(2*J - 1) = IV0(J)
          IV1(2*J)     = IV0(J)
        ENDDO
        N=2*N
        K=K+1
        NC1=1

        DO
          NC2=MIN(NC1+9-JJ,N)
          DO J=NC1,NC2
            JJ=JJ+1
            IV2(JJ)=IV1(J)
          ENDDO
          IF(JJ<10)CYCLE
          WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
          NC1=NC2+1
          JJ=0
          IF(NC2==N)EXIT
        ENDDO
      ENDDO ! I
      RETURN
      END
