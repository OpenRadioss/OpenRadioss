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
Chd|  SCTORTH3                      source/elements/thickshell/solide8c/sctorth3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCTORTH3(
     1   JFT,     JLT,     ICSTR,   NEL,
     2   RX,      RY,      RZ,      SX,
     3   SY,      SZ,      TX,      TY,
     4   TZ,      E1X,     E1Y,     E1Z,
     5   E2X,     E2Y,     E2Z,     E3X,
     6   E3Y,     E3Z,     G1X,     G1Y,
     7   G1Z,     G2X,     G2Y,     G2Z,
     8   G3X,     G3Y,     G3Z,     GAMA,
     9   IREP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER, INTENT(IN) :: IREP
      INTEGER JFT, JLT,ICSTR,NEL
C     REAL
      my_real
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real 
     .   V1,V2,V3,VR,VS,AA,BB,SUMA,CPN(MVSIZ),SPN(MVSIZ)
C-----------------------------------------------
        IF(IREP == 0) THEN
           CPN(JFT:JLT) = GAMA(JFT:JLT,1)
           SPN(JFT:JLT) = GAMA(JFT:JLT,2)
        ELSE
         SELECT CASE (ICSTR)                                             
          CASE (1)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*RX(I) + BB*SX(I)
            V2 = AA*RY(I) + BB*SY(I)
            V3 = AA*RZ(I) + BB*SZ(I)
            VR=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            VS=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
         CASE (100)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*SX(I) + BB*TX(I)
            V2 = AA*SY(I) + BB*TY(I)
            V3 = AA*SZ(I) + BB*TZ(I)
            VR=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            VS=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
        CASE (10)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*TX(I) + BB*RX(I)
            V2 = AA*TY(I) + BB*RY(I)
            V3 = AA*TZ(I) + BB*RZ(I)
            VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
         END SELECT                                                      
        ENDIF
C
        SELECT CASE (ICSTR)                                             
        CASE (1)                                                        
            G1X(JFT:JLT)=ZERO
            G1Y(JFT:JLT)=CPN(JFT:JLT)
            G1Z(JFT:JLT)=SPN(JFT:JLT)
            G2X(JFT:JLT)=ZERO                                                 
            G2Y(JFT:JLT)=-SPN(JFT:JLT)                                                 
            G2Z(JFT:JLT)=CPN(JFT:JLT)                                               
            G3X(JFT:JLT)=ONE
            G3Y(JFT:JLT)=ZERO
            G3Z(JFT:JLT)=ZERO
        CASE (100)                                                        
            G1X(JFT:JLT)=SPN(JFT:JLT)
            G1Y(JFT:JLT)=ZERO
            G1Z(JFT:JLT)=CPN(JFT:JLT)
            G2X(JFT:JLT)=CPN(JFT:JLT)                                                 
            G2Y(JFT:JLT)=ZERO                                                 
            G2Z(JFT:JLT)=-SPN(JFT:JLT)                                               
            G3X(JFT:JLT)=ZERO
            G3Y(JFT:JLT)=ONE
            G3Z(JFT:JLT)=ZERO
        CASE (10)                                                        
            G1X(JFT:JLT)=CPN(JFT:JLT)
            G1Y(JFT:JLT)=SPN(JFT:JLT)
            G1Z(JFT:JLT)=ZERO
            G2X(JFT:JLT)=-SPN(JFT:JLT)                                                 
            G2Y(JFT:JLT)=CPN(JFT:JLT)                                                 
            G2Z(JFT:JLT)= ZERO                                               
            G3X(JFT:JLT)= ZERO
            G3Y(JFT:JLT)= ZERO
            G3Z(JFT:JLT)= ONE
       END SELECT                                                      
C----------
      RETURN
      END
