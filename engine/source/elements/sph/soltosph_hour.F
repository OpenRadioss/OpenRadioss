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
Cbm|====================================================================
Cbm|  SZSIGPARA                     src/solidez/szhour3.F         
Cbm|-- called by -----------
Cbm|        TENSGPS3                      src/anim/tensor6.F            
Cbm|-- calls ---------------
Cbm|====================================================================
Chd|====================================================================
Chd|  SIG_HEPH1                     source/elements/sph/soltosph_hour.F
Chd|-- called by -----------
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIG_HEPH1(JR0     ,JS0  ,JT0  ,GSIG, FHOUR  , 
     .                     SIG_HEPH,PM   ,IXS  ,II  , NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),II(6),NEL
      my_real
     .   GSIG(*),FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIG_HEPH(MVSIZ,6,7),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      my_real
     .   COEF,NU1,NU2,NU3,SXX,SYY,SZZ,SXY,SXZ,SYZ
C-----------------------------------------------

C-----------------------------------------------
C
C       SIG_HEPH(,1) -> mean stress
C       SIG_HEPH(,2) -> hourglass stress component proportional to ZETA
C       SIG_HEPH(,3) -> hourglass stress component proportional to ETA 
C       SIG_HEPH(,4) -> hourglass stress component proportional to KSI 
C       SIG_HEPH(,5) -> hourglass stress component proportional to ZETA*ETA
C       SIG_HEPH(,6) -> hourglass stress component proportional to ZETA*KSI 
C       SIG_HEPH(,7) -> hourglass stress component proportional to ETA*KSI    
C
        DO I=LFT,LLT
           NU1=ONE/(ONE-PM(21,IXS(1,I)))
           NU2=PM(21,IXS(1,I))*NU1
           NU3=ONE+PM(21,IXS(1,I))
C
           SIG_HEPH(I,1,1)=GSIG(II(1)+I)
           SIG_HEPH(I,2,1)=GSIG(II(2)+I)
           SIG_HEPH(I,3,1)=GSIG(II(3)+I)
           SIG_HEPH(I,4,1)=GSIG(II(4)+I)
           SIG_HEPH(I,5,1)=GSIG(II(5)+I)
           SIG_HEPH(I,6,1)=GSIG(II(6)+I)
C
           SIG_HEPH(I,1,2)=2*TWENTY4*(NU1*FHOUR(I,1,2)+NU2*FHOUR(I,2,1))
           SIG_HEPH(I,2,2)=2*TWENTY4*(NU1*FHOUR(I,2,1)+NU2*FHOUR(I,1,2))
           SIG_HEPH(I,4,2)=TWENTY4*(JR0(I)*FHOUR(I,1,1)/JS0(I)+JS0(I)*FHOUR(I,2,2)/JR0(I))
           SIG_HEPH(I,1,3)=2*TWENTY4*(NU1*FHOUR(I,1,3)+NU2*FHOUR(I,3,1))
           SIG_HEPH(I,3,3)=2*TWENTY4*(NU1*FHOUR(I,3,1)+NU2*FHOUR(I,1,3))
           SIG_HEPH(I,6,3)=TWENTY4*(JR0(I)*FHOUR(I,1,1)/JT0(I)+JT0(I)*FHOUR(I,3,3)/JR0(I))
           SIG_HEPH(I,2,4)=2*TWENTY4*(NU1*FHOUR(I,2,3)+NU2*FHOUR(I,3,2))
           SIG_HEPH(I,3,4)=2*TWENTY4*(NU1*FHOUR(I,3,2)+NU2*FHOUR(I,2,3))
           SIG_HEPH(I,5,4)=TWENTY4*(JS0(I)*FHOUR(I,2,2)/JT0(I)+JT0(I)*FHOUR(I,3,3)/JS0(I))
           SIG_HEPH(I,1,5)=2*TWENTY4*NU3*FHOUR(I,1,4)        
           SIG_HEPH(I,2,6)=2*TWENTY4*NU3*FHOUR(I,2,4)
           SIG_HEPH(I,3,7)=2*TWENTY4*NU3*FHOUR(I,3,4)
        END DO
C
      RETURN
      END
C
