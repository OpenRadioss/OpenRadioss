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
Chd|  TRANRIDRRJ33                  source/elements/thickshell/solide6c/tranridrrj33.F
Chd|-- called by -----------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/setprojks6.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANRIDRRJ33(JFT   ,JLT    ,RI  ,RD , RJ,KD  ,IS   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IS
      my_real
     .   RI(3,3,*), RJ(3,3,*),RD(3,3,*),KD(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    K(3,3)
C--------------[KD]=|RI| |RD| |RJ|^t---------------------------------
       IF (IS==1) THEN
        DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
          KD(J,I,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
       END IF
C
      RETURN
      END
