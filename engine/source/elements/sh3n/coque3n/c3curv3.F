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
Chd|  C3CURV3                       source/elements/sh3n/coque3n/c3curv3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3CURV3(JFT,JLT,VRL1,VRL2,VRL3,
     .                   IXTG,WXY,ISMSTR,KXX,KYY,KXY,
     .                   PX1 ,PY1 ,PY2  ,EYZ   ,EZX  ,
     .                   E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .                   E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IXTG(NIXTG,*),ISMSTR
      my_real VRL1(MVSIZ,3), VRL2(MVSIZ,3), VRL3(MVSIZ,3),WXY(*),
     .  E1X(MVSIZ),  E1Y(MVSIZ),  E1Z(MVSIZ),  E2X(MVSIZ), 
     .  E2Y(MVSIZ),  E2Z(MVSIZ),  E3X(MVSIZ),  E3Y(MVSIZ), 
     .  E3Z(MVSIZ),  EZX(MVSIZ),  EYZ(MVSIZ),               
     .  PX1(MVSIZ),  PY1(MVSIZ),  PY2(MVSIZ),
     .  KXX(MVSIZ),  KYY(MVSIZ),  KXY(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real RX1(MVSIZ),  RX2(MVSIZ),  RX3(MVSIZ),
     .        RY1(MVSIZ),  RY2(MVSIZ),  RY3(MVSIZ)
      my_real RX12T,RX13T,RX23T,RY12T,RY13T,RY23T,RXAVT,RYAVT
C=======================================================================
      DO I=JFT,JLT
        RX1(I)=E1X(I)*VRL1(I,1)+E1Y(I)*VRL1(I,2)+E1Z(I)*VRL1(I,3)
        RY1(I)=E2X(I)*VRL1(I,1)+E2Y(I)*VRL1(I,2)+E2Z(I)*VRL1(I,3)
        RY2(I)=E2X(I)*VRL2(I,1)+E2Y(I)*VRL2(I,2)+E2Z(I)*VRL2(I,3)
        RX2(I)=E1X(I)*VRL2(I,1)+E1Y(I)*VRL2(I,2)+E1Z(I)*VRL2(I,3)
        RX3(I)=E1X(I)*VRL3(I,1)+E1Y(I)*VRL3(I,2)+E1Z(I)*VRL3(I,3)
        RY3(I)=E2X(I)*VRL3(I,1)+E2Y(I)*VRL3(I,2)+E2Z(I)*VRL3(I,3)
      ENDDO
C
      DO I=JFT,JLT
        RX12T  = RX1(I) - RX2(I)
        RX13T  = RX1(I) - RX3(I)
        RX23T  = RX2(I) - RX3(I)
C
        KYY(I)=-PY1(I) * RX13T - PY2(I) * RX23T
        KXY(I)= PX1(I) * RX12T
C
        RY12T = RY1(I) - RY2(I)
        RY13T = RY1(I) - RY3(I)
        RY23T = RY2(I) - RY3(I)
C
        KXX(I) = PX1(I) * RY12T
        KXY(I) = PY1(I) * RY13T + PY2(I) * RY23T - KXY(I)
C
        RYAVT  = PX1(I) * (
     .              PX1(I) * (-RX1(I)+RX2(I)) 
     .            + (TWO*PY1(I)+THREE*PY2(I)) * RY1(I)
     .            + (THREE*PY1(I)+TWO*PY2(I)) * RY2(I)

     .            + ( PY1(I) + PY2(I) )   * RY3(I) )
        RXAVT  = -PX1(I) * (   
     .           + (TWO*PY1(I)+PY2(I)) * RX1(I)
     .           + (PY1(I)+TWO*PY2(I)) * RX2(I)
     .           + THREE*(PY1(I)+PY2(I)) * RX3(I) )
     .           + PY1(I) * (PY1(I)+TWO*PY2(I)) * RY1(I)
     .           - PY2(I) * (TWO*PY1(I)+PY2(I)) * RY2(I)
     .           + ( PY2(I)**2 - PY1(I)**2 )   * RY3(I)
C
        EZX(I) = EZX(I) + RYAVT * THIRD
        EYZ(I) = EYZ(I) + RXAVT * THIRD   
      ENDDO
c
      IF (ISMSTR==10) THEN
        DO I=JFT,JLT
         RX12T = RX1(I) - RX2(I)
         RY13T = RY1(I) - RY3(I)
         RY23T = RY2(I) - RY3(I)
C
         WXY(I) = PY1(I) * RY13T + PY2(I) * RY23T + PX1(I) * RX12T
        ENDDO 
      ENDIF
c-----------
      RETURN
      END
