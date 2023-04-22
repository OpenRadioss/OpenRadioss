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
Chd|  SZSIGPARA                     source/elements/solid/solidez/szsigpara.F
Chd|-- called by -----------
Chd|        STRS_TENSCOR3                 source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSIGPARA(JR0  ,JS0  ,JT0  ,FHOUR  ,SIG0 ,
     .                     SIG  ,KSI  ,ETA  ,ZETA   ,NU   ,
     .                     NEL  ,I  )
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
      INTEGER NEL,I
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIG(MVSIZ,6) ,SIG0(NEL,6),NU(*),KSI  ,ETA  ,ZETA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   J
      my_real
     .   COEF,NU1,NU2,NU3,SXX,SYY,SZZ,SXY,SXZ,SYZ
C---JR0=-X1+X2+X3-X4-X5+X6+X7-X8 (coordon  s locaux)     
C---JS0=-Y1-Y2+Y3+Y4-Y5-Y6+Y7+Y8     
C---JT0=-Z1-Z2-Z3-Z4+Z5+Z6+Z7+Z8
C---NU : Poisson's Coef     
C-----------------------------------------------
        COEF=TWENTY4
c        DO I=LFT,LLT
         NU1=ONE/(ONE-NU(I))
         NU2=NU(I)*NU1
         NU3=ONE+NU(I)
         SXX=NU1*(FHOUR(I,1,2)*ZETA+FHOUR(I,1,3)*ETA)+
     .       NU2*(FHOUR(I,2,1)*ZETA+FHOUR(I,3,1)*ETA)+         
     .       NU3*FHOUR(I,1,4)*ZETA*ETA         
         SYY=NU1*(FHOUR(I,2,1)*ZETA+FHOUR(I,2,3)*KSI)+
     .       NU2*(FHOUR(I,1,2)*ZETA+FHOUR(I,3,2)*KSI)+         
     .       NU3*FHOUR(I,2,4)*ZETA*KSI         
         SZZ=NU1*(FHOUR(I,3,1)*ETA+FHOUR(I,3,2)*KSI)+
     .       NU2*(FHOUR(I,1,3)*ETA+FHOUR(I,2,3)*KSI)+         
     .       NU3*FHOUR(I,3,4)*ETA*KSI         
         SIG(I,1)=SIG0(I,1)+TWO*SXX*TWENTY4
         SIG(I,2)=SIG0(I,2)+TWO*SYY*TWENTY4
         SIG(I,3)=SIG0(I,3)+TWO*SZZ*TWENTY4
c        ENDDO
C        
c        DO I=LFT,LLT
         SXY=ZETA*(JR0(I)*FHOUR(I,1,1)/JS0(I)+
     .             JS0(I)*FHOUR(I,2,2)/JR0(I))         
         SXZ=ETA*(JR0(I)*FHOUR(I,1,1)/JT0(I)+
     .            JT0(I)*FHOUR(I,3,3)/JR0(I))
         SYZ=KSI*(JS0(I)*FHOUR(I,2,2)/JT0(I)+
     .            JT0(I)*FHOUR(I,3,3)/JS0(I))
         SIG(I,4)=SIG0(I,4)+SXY*TWENTY4         
         SIG(I,5)=SIG0(I,5)+SYZ*TWENTY4
         SIG(I,6)=SIG0(I,6)+SXZ*TWENTY4
c        ENDDO
C
      RETURN
      END
