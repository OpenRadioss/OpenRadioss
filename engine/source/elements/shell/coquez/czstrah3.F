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
Chd|  CZSTRAH3                      source/elements/shell/coquez/czstrah3.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CZSTRAH3(JFT    ,JLT    ,STRA_H,VHG    ,A_I   ,
     3                    MX23   ,MX34   ,MY23  ,MY34   ,DT1C  ,
     4                    EXX    ,EYY    ,EXY   ,EXZ    ,EYZ   ,  
     3                    KXX    ,KYY    ,KXY   ,NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JFT, JLT,NEL
      my_real, DIMENSION(MVSIZ),INTENT(IN) :: A_I,
     .                    DT1C,MX23,MX34,MY23,MY34,
     .                    EXX,EYY,EXY,EXZ,EYZ,  
     .                    KXX ,KYY ,KXY 
      my_real, DIMENSION(MVSIZ,6),INTENT(IN) :: VHG
      my_real, DIMENSION(NEL,8,4),INTENT(INOUT) :: STRA_H
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG,K,NPG
      MY_REAL 
     .   HX(MVSIZ),HY(MVSIZ),DHG(MVSIZ,6),PG1,PG,VPG(2,4)
      MY_REAL 
     .   A4_1(MVSIZ),A_4(MVSIZ),STRAIN(8),HXZ(MVSIZ),HYZ(MVSIZ)
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/  ! VPG(1:2,4):ksi,eta
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NPG =4
      A4_1(JFT:JLT) = FOUR*A_I(JFT:JLT)
      A_4(JFT:JLT) = FOURTH*A_I(JFT:JLT)
      DO J=1,6
        DHG(JFT:JLT,J) = VHG(JFT:JLT,J) * DT1C(JFT:JLT)
      ENDDO
      DO NG = 1,NPG
        DO I=JFT,JLT          
          HX(I) = A4_1(I)*(MY34(I)*VPG(2,NG)- MY23(I)*VPG(1,NG))
          HY(I) = A4_1(I)*(-MX34(I)*VPG(2,NG)+MX23(I)*VPG(1,NG))
          HXZ(I)= A_4(I)*(MY34(I)*VPG(2,NG)*DHG(I,5)- MY23(I)*VPG(1,NG)*DHG(I,6))
          HYZ(I)= A_4(I)*(MX23(I)*VPG(1,NG)*DHG(I,5)- MX34(I)*VPG(2,NG)*DHG(I,6))
          STRAIN(1) = EXX(I) + HX(I)*DHG(I,1)
          STRAIN(2) = EYY(I) + HY(I)*DHG(I,2)
          STRAIN(3) = EXY(I)  
          STRAIN(6) = KXX(I) + HX(I)*DHG(I,3)
          STRAIN(7) = KYY(I) + HY(I)*DHG(I,4)
          STRAIN(8) = KXY(I)  
          STRAIN(5) = EXZ(I) + HXZ(I)
          STRAIN(4) = EYZ(I) + HYZ(I)
          STRA_H(I,1,NG) = STRA_H(I,1,NG) + STRAIN(1) 
          STRA_H(I,2,NG) = STRA_H(I,2,NG) + STRAIN(2) 
          STRA_H(I,3,NG) = STRA_H(I,3,NG) + STRAIN(3) 
          STRA_H(I,4,NG) = STRA_H(I,4,NG) + STRAIN(4) 
          STRA_H(I,5,NG) = STRA_H(I,5,NG) + STRAIN(5) 
          STRA_H(I,6,NG) = STRA_H(I,6,NG) + STRAIN(6) 
          STRA_H(I,7,NG) = STRA_H(I,7,NG) + STRAIN(7) 
          STRA_H(I,8,NG) = STRA_H(I,8,NG) + STRAIN(8) 
        ENDDO
      ENDDO
C
      RETURN
      END
