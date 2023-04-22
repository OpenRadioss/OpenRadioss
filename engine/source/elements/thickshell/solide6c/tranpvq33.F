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
Chd|  TRANPVQ33                     source/elements/thickshell/solide6c/tranpvq33.F
Chd|-- called by -----------
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/setprojks6.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANPVQ33(JFT   ,JLT    ,KD  ,PVQ ,VQ ,IS ,IT   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IS,IT
      my_real
     .   KD(3,3,*),PVQ(3,3,*),VQ(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    K(3,3)
C--------------PVQIJ=[VQ]gij-[KD][VQ]--;PVQJI=[VQ]gij-[KD]^t[VQ]-----------------------------
       IF (IT==1) THEN
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+KD(I,L,EP)*VQ(L,J,EP)
          ENDDO
          PVQ(I,J,EP)=-K(I,J)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+KD(L,I,EP)*VQ(L,J,EP)
          ENDDO
          PVQ(I,J,EP)=-K(I,J)
         ENDDO
        ENDDO
        ENDDO
       END IF !(IT==1) THEN
C       
       IF (IS==1) THEN
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          PVQ(I,J,EP)=PVQ(I,J,EP)+VQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       END IF
C       
      RETURN
      END
