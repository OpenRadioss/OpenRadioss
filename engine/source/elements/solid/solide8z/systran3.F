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
Chd|  SYSTRAN3                      source/elements/solid/solide8z/systran3.F
Chd|-- called by -----------
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SYSTRAN3(JFT   ,JLT    ,VQ   ,KK,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real 
     .   VQ(3,3,*), KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+
     1                  KK(1,2,EP)*VQ(J,2,EP)+KK(1,3,EP)*VQ(J,3,EP))+
     2              VQ(I,2,EP)*(KK(1,2,EP)*VQ(J,1,EP)+
     3                  KK(2,2,EP)*VQ(J,2,EP)+KK(2,3,EP)*VQ(J,3,EP))+
     4              VQ(I,3,EP)*(KK(1,3,EP)*VQ(J,1,EP)+
     5                  KK(2,3,EP)*VQ(J,2,EP)+KK(3,3,EP)*VQ(J,3,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO EP=JFT,JLT 
         KK(2,1,EP)= K(1,2,EP)
         KK(3,1,EP)= K(1,3,EP)
         KK(3,2,EP)= K(2,3,EP)
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+
     1                  KK(1,2,EP)*VQ(J,2,EP)+KK(1,3,EP)*VQ(J,3,EP))+
     2              VQ(I,2,EP)*(KK(2,1,EP)*VQ(J,1,EP)+
     3                  KK(2,2,EP)*VQ(J,2,EP)+KK(2,3,EP)*VQ(J,3,EP))+
     4              VQ(I,3,EP)*(KK(3,1,EP)*VQ(J,1,EP)+
     5                  KK(3,2,EP)*VQ(J,2,EP)+KK(3,3,EP)*VQ(J,3,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
