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
Chd|  R12SUMG3                      source/elements/spring/r12sumg3.F
Chd|-- called by -----------
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R12SUMG3(JFT    ,JLT     ,KX     ,DKX     ,R11    ,
     1                    R21    ,R31     ,R12    ,R22     ,R32    ,
     2                    KE11   ,KE12    ,KE13   ,KE22    ,KE23   ,
     3                    KE33   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .    KX(*),DKX(*),R11(*),R21(*),R31(*),R12(*),R22(*),R32(*)
       my_real
     .    KE11(3,3,*),KE22(3,3,*),KE33(3,3,*),KE12(3,3,*),
     .    KE13(3,3,*),KE23(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, EP
       my_real
     .    K1(3,3,MVSIZ),K2(3,3,MVSIZ),K3(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------------------------------------
C   TRANS LOCAL-->GLOBAL 
C---------------------------------------
       DO EP=JFT,JLT
        K1(1,1,EP)=R11(EP)*R11(EP)
        K1(1,2,EP)=R11(EP)*R21(EP)
        K1(1,3,EP)=R11(EP)*R31(EP)
        K1(2,2,EP)=R21(EP)*R21(EP)
        K1(2,3,EP)=R21(EP)*R31(EP)
        K1(3,3,EP)=R31(EP)*R31(EP)
        K1(2,1,EP)=K1(1,2,EP)
        K1(3,2,EP)=K1(2,3,EP)
        K1(3,1,EP)=K1(1,3,EP)
       ENDDO
       DO EP=JFT,JLT
        K2(1,1,EP)=R12(EP)*R12(EP)
        K2(1,2,EP)=R12(EP)*R22(EP)
        K2(1,3,EP)=R12(EP)*R32(EP)
        K2(2,2,EP)=R22(EP)*R22(EP)
        K2(2,3,EP)=R22(EP)*R32(EP)
        K2(3,3,EP)=R32(EP)*R32(EP)
        K2(2,1,EP)=K2(1,2,EP)
        K2(3,2,EP)=K2(2,3,EP)
        K2(3,1,EP)=K2(1,3,EP)
       ENDDO
       DO EP=JFT,JLT
        K3(1,1,EP)=R11(EP)*R12(EP)
        K3(1,2,EP)=R11(EP)*R22(EP)
        K3(1,3,EP)=R11(EP)*R32(EP)
        K3(2,1,EP)=R21(EP)*R12(EP)
        K3(2,2,EP)=R21(EP)*R22(EP)
        K3(2,3,EP)=R21(EP)*R32(EP)
        K3(3,1,EP)=R31(EP)*R12(EP)
        K3(3,2,EP)=R31(EP)*R22(EP)
        K3(3,3,EP)=R31(EP)*R32(EP)
       ENDDO
C
       DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          KE11(I,J,EP)=KX(EP)*K1(I,J,EP)
          KE22(I,J,EP)=KX(EP)*(K1(I,J,EP)+K2(I,J,EP)+
     .                         K3(I,J,EP)+K3(J,I,EP))
          KE33(I,J,EP)=KX(EP)*K2(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
       DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          KE12(I,J,EP)=-KX(EP)*(K1(I,J,EP)+K3(I,J,EP))
          KE13(I,J,EP)=KX(EP)*K3(I,J,EP)
          KE23(I,J,EP)=-KX(EP)*(K2(I,J,EP)+K3(I,J,EP))
         ENDDO
        ENDDO
       ENDDO
C-----frotement---
       DO EP=JFT,JLT
        IF (DKX(EP)>ZERO) THEN
         DO I=1,3
          DO J=I,3
           KE11(I,J,EP)=KE11(I,J,EP)+DKX(EP)*K1(I,J,EP)
           KE22(I,J,EP)=KE22(I,J,EP)+DKX(EP)*(K1(I,J,EP)-K3(I,J,EP))
           KE33(I,J,EP)=KE33(I,J,EP)+DKX(EP)*K2(I,J,EP)
           KE22(I,J,EP)=KE22(I,J,EP)+DKX(EP)*(K2(I,J,EP)-K3(J,I,EP))
          ENDDO
         ENDDO
         DO I=1,3
          DO J=1,3
           KE12(I,J,EP)=KE12(I,J,EP)-DKX(EP)*(K1(I,J,EP)-K3(I,J,EP))
           KE13(I,J,EP)=KE13(I,J,EP)-DKX(EP)*K3(I,J,EP)
           KE23(I,J,EP)=KE23(I,J,EP)-DKX(EP)*(K2(I,J,EP)-K3(I,J,EP))
          ENDDO
         ENDDO
        ENDIF
       ENDDO
C
       DO I=1,3
        DO J=I+1,3
         DO EP=JFT,JLT
          KE11(J,I,EP)=KE11(I,J,EP)
          KE22(J,I,EP)=KE22(I,J,EP)
          KE33(J,I,EP)=KE33(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END

