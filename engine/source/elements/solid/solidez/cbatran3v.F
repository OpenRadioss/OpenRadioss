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
Chd|  CBATRAN3V                     source/elements/solid/solidez/cbatran3v.F
Chd|-- called by -----------
Chd|        C33STIF2EL                    source/elements/solid/solidez/c33stif2el.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MSTIFORTHV                    source/elements/solid/solidez/mstiforthv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN3V(JFT   ,JLT    ,VQI   ,KK,VQJ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real 
     .   VQI(MVSIZ,3,3), VQJ(MVSIZ,3,3),KK(MVSIZ,3,3)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(MVSIZ,3,3)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K(EP,I,J)=VQI(EP,1,I)*(KK(EP,1,1)*VQJ(EP,1,J)+
     1                  KK(EP,1,2)*VQJ(EP,2,J)+KK(EP,1,3)*VQJ(EP,3,J))+
     2              VQI(EP,2,I)*(KK(EP,1,2)*VQJ(EP,1,J)+
     3                  KK(EP,2,2)*VQJ(EP,2,J)+KK(EP,2,3)*VQJ(EP,3,J))+
     4              VQI(EP,3,I)*(KK(EP,1,3)*VQJ(EP,1,J)+
     5                  KK(EP,2,3)*VQJ(EP,2,J)+KK(EP,3,3)*VQJ(EP,3,J))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(EP,I,J)= K(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(EP,I,J)=VQI(EP,1,I)*(KK(EP,1,1)*VQJ(EP,1,J)+
     1                  KK(EP,1,2)*VQJ(EP,2,J)+KK(EP,1,3)*VQJ(EP,3,J))+
     2              VQI(EP,2,I)*(KK(EP,2,1)*VQJ(EP,1,J)+
     3                  KK(EP,2,2)*VQJ(EP,2,J)+KK(EP,2,3)*VQJ(EP,3,J))+
     4              VQI(EP,3,I)*(KK(EP,3,1)*VQJ(EP,1,J)+
     5                  KK(EP,3,2)*VQJ(EP,2,J)+KK(EP,3,3)*VQJ(EP,3,J))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(EP,I,J)= K(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
