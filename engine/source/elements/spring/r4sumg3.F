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
Chd|====================================================================
Chd|  R4SUMG3                       source/elements/spring/r4sumg3.F
Chd|-- called by -----------
Chd|        R4KE3                         source/elements/spring/r4ke3.F
Chd|        RUSER32KE3                    source/elements/spring/ruser32ke3.F
Chd|        TKE3                          source/elements/truss/tke3.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R4SUMG3(JFT    ,JLT    ,AL      ,KX      ,R11    ,
     3                   R21    ,R31    ,KE11    ,KE12    ,KE22   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .    AL(*),KX(*),R11(*),R21(*),R31(*)
       my_real
     .    KE11(3,3,*),KE22(3,3,*),KE12(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, EP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------------------------------------
C   TRANS LOCAL-->GLOBAL 
C---------------------------------------
       DO EP=JFT,JLT
        KE11(1,1,EP)=KX(EP)*R11(EP)*R11(EP)
        KE11(1,2,EP)=KX(EP)*R11(EP)*R21(EP)
        KE11(1,3,EP)=KX(EP)*R11(EP)*R31(EP)
        KE11(2,2,EP)=KX(EP)*R21(EP)*R21(EP)
        KE11(2,3,EP)=KX(EP)*R21(EP)*R31(EP)
        KE11(3,3,EP)=KX(EP)*R31(EP)*R31(EP)
       ENDDO
       DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          KE22(I,J,EP)=KE11(I,J,EP)
          KE12(I,J,EP)=-KE11(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
       DO I=1,3
        DO J=I+1,3
         DO EP=JFT,JLT
          KE12(J,I,EP)=-KE11(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END

