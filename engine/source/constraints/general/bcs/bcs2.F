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
Chd|  BCS2                          source/constraints/general/bcs/bcs2.F
Chd|-- called by -----------
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        IQELA2                        source/ale/inter/iqela2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS2(A,B,J,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) ::  J, K
      my_real,INTENT(INOUT) :: A(3), B(LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: AA
C-----------------------------------------------
C   P r e - C o n i d i t i o n s 
C-----------------------------------------------
      IF(K==0) RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(J==1) THEN
        !=====REPERE GLOBAL
        SELECT CASE (K)
          CASE(1)
            A(3)=ZERO
          CASE(2)
            A(2)=ZERO
          CASE(3)
            A(2)=ZERO
            A(3)=ZERO
          CASE(4)
            A(1)=ZERO
          CASE(5)
            A(1)=ZERO
            A(3)=ZERO
          CASE(6)
            A(1)=ZERO
            A(2)=ZERO
          CASE(7)
            A(1)=ZERO
            A(2)=ZERO
            A(3)=ZERO
          CASE DEFAULT !same as CASE(1)
            A(3)=ZERO
          END SELECT
      ELSE
        !=====REPERE OBLIQUE
        SELECT CASE (K)
          CASE(1)
            AA  =B(7)*A(1)+B(8)*A(2)+B(9)*A(3)
            A(1)=A(1)-B(7)*AA
            A(2)=A(2)-B(8)*AA
            A(3)=A(3)-B(9)*AA
          CASE(2)
            AA  =B(4)*A(1)+B(5)*A(2)+B(6)*A(3)
            A(1)=A(1)-B(4)*AA
            A(2)=A(2)-B(5)*AA
            A(3)=A(3)-B(6)*AA
          CASE(3)
            AA  =B(7)*A(1)+B(8)*A(2)+B(9)*A(3)
            A(1)=A(1)-B(7)*AA
            A(2)=A(2)-B(8)*AA
            A(3)=A(3)-B(9)*AA
            AA  =B(4)*A(1)+B(5)*A(2)+B(6)*A(3)
            A(1)=A(1)-B(4)*AA
            A(2)=A(2)-B(5)*AA
            A(3)=A(3)-B(6)*AA
          CASE(4)
            AA  =B(1)*A(1)+B(2)*A(2)+B(3)*A(3)
            A(1)=A(1)-B(1)*AA
            A(2)=A(2)-B(2)*AA
            A(3)=A(3)-B(3)*AA
          CASE(5)
            AA  =B(7)*A(1)+B(8)*A(2)+B(9)*A(3)
            A(1)=A(1)-B(7)*AA
            A(2)=A(2)-B(8)*AA
            A(3)=A(3)-B(9)*AA
            AA  =B(1)*A(1)+B(2)*A(2)+B(3)*A(3)
            A(1)=A(1)-B(1)*AA
            A(2)=A(2)-B(2)*AA
            A(3)=A(3)-B(3)*AA
          CASE(6)
            AA  =B(1)*A(1)+B(2)*A(2)+B(3)*A(3)
            A(1)=A(1)-B(1)*AA
            A(2)=A(2)-B(2)*AA
            A(3)=A(3)-B(3)*AA
            AA  =B(4)*A(1)+B(5)*A(2)+B(6)*A(3)
            A(1)=A(1)-B(4)*AA
            A(2)=A(2)-B(5)*AA
            A(3)=A(3)-B(6)*AA
          CASE(7)
            A(1)=ZERO
            A(2)=ZERO
            A(3)=ZERO
          CASE DEFAULT !same as CASE(1)
            AA  =B(7)*A(1)+B(8)*A(2)+B(9)*A(3)
            A(1)=A(1)-B(7)*AA
            A(2)=A(2)-B(8)*AA
            A(3)=A(3)-B(9)*AA
        END SELECT
      END IF
C-----------------------------------------------
      RETURN
      END



Chd|====================================================================
Chd|  BCS2V                         source/constraints/general/bcs/bcs2.F
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS2V(NINDX,INDX,ISKEW,ICODT,A,B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT)  ::  NINDX, INDX(*),ISKEW(*),ICODT(*)
      my_real, INTENT(INOUT) :: A(3,*), B(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K, N, ISK, LCOD
      my_real :: AA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
#include "vectorize.inc"
      DO K = 1, NINDX
        N    = INDX(K)
        ISK  = ISKEW(N)
        LCOD = ICODT(N)
        IF(ISK==1) THEN
         !=====REPERE GLOBAL
         SELECT CASE (LCOD)
           CASE(1)
             A(3,N)=ZERO
           CASE(2)
             A(2,N)=ZERO
           CASE(3)
            A(2,N)=ZERO
            A(3,N)=ZERO
           CASE(4)
            A(1,N)=ZERO
           CASE(5)
            A(1,N)=ZERO
            A(3,N)=ZERO
           CASE(6)
            A(1,N)=ZERO
            A(2,N)=ZERO
           CASE(7)
            A(1,N)=ZERO
            A(2,N)=ZERO
            A(3,N)=ZERO
         END SELECT
        ELSE
         !=====REPERE OBLIQUE
         SELECT CASE (LCOD)
           CASE(1)
             AA  =B(7,ISK)*A(1,N)+B(8,ISK)*A(2,N)+B(9,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(7,ISK)*AA
             A(2,N)=A(2,N)-B(8,ISK)*AA
             A(3,N)=A(3,N)-B(9,ISK)*AA
           CASE(2)
             AA  =B(4,ISK)*A(1,N)+B(5,ISK)*A(2,N)+B(6,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(4,ISK)*AA
             A(2,N)=A(2,N)-B(5,ISK)*AA
             A(3,N)=A(3,N)-B(6,ISK)*AA
           CASE(3)
             AA  =B(7,ISK)*A(1,N)+B(8,ISK)*A(2,N)+B(9,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(7,ISK)*AA
             A(2,N)=A(2,N)-B(8,ISK)*AA
             A(3,N)=A(3,N)-B(9,ISK)*AA
             AA  =B(4,ISK)*A(1,N)+B(5,ISK)*A(2,N)+B(6,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(4,ISK)*AA
             A(2,N)=A(2,N)-B(5,ISK)*AA
             A(3,N)=A(3,N)-B(6,ISK)*AA
           CASE(4)
             AA  =B(1,ISK)*A(1,N)+B(2,ISK)*A(2,N)+B(3,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(1,ISK)*AA
             A(2,N)=A(2,N)-B(2,ISK)*AA
             A(3,N)=A(3,N)-B(3,ISK)*AA
           CASE(5)
             AA  =B(7,ISK)*A(1,N)+B(8,ISK)*A(2,N)+B(9,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(7,ISK)*AA
             A(2,N)=A(2,N)-B(8,ISK)*AA
             A(3,N)=A(3,N)-B(9,ISK)*AA
             AA  =B(1,ISK)*A(1,N)+B(2,ISK)*A(2,N)+B(3,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(1,ISK)*AA
             A(2,N)=A(2,N)-B(2,ISK)*AA
             A(3,N)=A(3,N)-B(3,ISK)*AA
           CASE(6)
             AA  =B(1,ISK)*A(1,N)+B(2,ISK)*A(2,N)+B(3,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(1,ISK)*AA
             A(2,N)=A(2,N)-B(2,ISK)*AA
             A(3,N)=A(3,N)-B(3,ISK)*AA
             AA  =B(4,ISK)*A(1,N)+B(5,ISK)*A(2,N)+B(6,ISK)*A(3,N)
             A(1,N)=A(1,N)-B(4,ISK)*AA
             A(2,N)=A(2,N)-B(5,ISK)*AA
             A(3,N)=A(3,N)-B(6,ISK)*AA
           CASE(7)
             A(1,N)=ZERO
             A(2,N)=ZERO
             A(3,N)=ZERO
         END SELECT
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
