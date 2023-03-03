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
Chd|  ICINV3                        source/interfaces/inter3d1/icinv3.F
Chd|-- called by -----------
Chd|        I12CHK3                       source/interfaces/inter3d1/i12chk3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ICINV3(IXS,IXC,IXTG,IADD,INVC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        IADD(*), INVC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------`-------------------------
      INTEGER I, J, N
C-----------------------------------------------
      IADD(1) = 1
      IADD(2) = 1
C
      DO I=3,NUMNOD+1
          IADD(I)=0
      ENDDO
C
      DO J=2,5
        DO I=1,NUMELC
          N = IXC(J,I) + 2
          IADD(N)=IADD(N)+1
        ENDDO
      ENDDO
C
      DO J=2,4
        DO I=1,NUMELTG
          N = IXTG(J,I) + 2
          IADD(N)=IADD(N)+1
        ENDDO
      ENDDO
C
      DO J=2,9
        DO I=1,NUMELS
          N = IXS(J,I) + 2
          IADD(N)=IADD(N)+1
        ENDDO
      ENDDO
C
      DO I=3,NUMNOD+1
          IADD(I)=IADD(I)+IADD(I-1)
      ENDDO
C
      DO I=1,NUMELC

          N = IXC(2,I) + 1
          INVC(IADD(N)) = I
          IADD(N) = IADD(N) + 1

          N = IXC(3,I) + 1
          INVC(IADD(N)) = I
          IADD(N) = IADD(N) + 1

          N = IXC(4,I) + 1
          INVC(IADD(N)) = I
          IADD(N) = IADD(N) + 1

          N = IXC(5,I) + 1
          INVC(IADD(N)) = I
          IADD(N) = IADD(N) + 1

      ENDDO
C
      DO I=1,NUMELTG
          N = IXTG(2,I) + 1
          INVC(IADD(N)) = I+NUMELC
          IADD(N) = IADD(N) + 1

          N = IXTG(3,I) + 1
          INVC(IADD(N)) = I+NUMELC
          IADD(N) = IADD(N) + 1

          N = IXTG(4,I) + 1
          INVC(IADD(N)) = I+NUMELC
          IADD(N) = IADD(N) + 1

      ENDDO
C
      DO I=1,NUMELS

          N = IXS(2,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(3,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(4,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(5,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(6,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(7,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(8,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1

          N = IXS(9,I) + 1
          INVC(IADD(N)) = I+NUMELC+NUMELTG
          IADD(N) = IADD(N) + 1
      ENDDO
C-------------------------------------
C     APRES DO 70 
C     IADD(1:NUMNOD) <=> IADD(2:NUMNOD+1)
C------------------------------------- 
      RETURN
      END
