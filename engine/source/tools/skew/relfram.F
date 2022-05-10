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
Chd|  RELFRAM                       source/tools/skew/relfram.F   
Chd|-- called by -----------
Chd|        THNOD                         source/output/th/thnod.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RELFRAM(XG ,DG      ,VG      ,AG      ,VRG     ,
     2              ARG     ,XFRAME  ,XO      ,DO      ,VO      ,
     3              AO      ,XL      ,DL      ,VL      ,AL      ,
     4              VRL     ,ARL     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   XG(3), VG(3), DG(3), AG(3), VRG(3), ARG(3),
     .   XFRAME(NXFRAME), XO(3), VO(3), DO(3), AO(3), XL(3),
     .   DL(3), VL(3), AL(3), VRL(3), ARL(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K
C     REAL ou REAL*8
      my_real
     .   ROT(9),ROT0(9),ROTDT05(9),ROTT05(9),
     .   OMEGAX, OMEGAY, OMEGAZ, NN, CS, SN, UX, UY, UZ,
     .   OMEGAXX, OMEGAXY, OMEGAXZ, OMEGAYY, OMEGAYZ, OMEGAZZ,
     .  O(3), OM(3), W(3), DWDT(3), WOM(3), VE(3), AE(3), AC(3), OM2(3),
     .  DT05, DREL(3), VREL(3), AREL(3), VRREL(3), ARREL(3)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C      ACTUAL FRAME POSITION & ORIENTATION WRT GLOBAL SYSTEM
C-----------------
Clm    Frame position at time T (O == position of node N1).
       O(1)=XFRAME(10)
       O(2)=XFRAME(11)
       O(3)=XFRAME(12)
C      Frame orientation at time T.
       DO K=1,9
        ROT(K)=XFRAME(K)
       ENDDO
C-----------
Clm    LINEAR VELOCITY at time T-1/2 and acceleration at time T
C      are read from node N1.
C-----------
C-----------
C      ANGULAR VELOCITY at time T-1/2 and acceleration at time T.
C-----------
       W(1)=XFRAME(13)
       W(2)=XFRAME(14)
       W(3)=XFRAME(15)
C-----------------
C      Frame orientation at time 0.
       DO K=1,9                
         ROT0(K)=XFRAME(18+K)  
       ENDDO                   
C-----------
C      local coordinates.
C-----------
       OM(1)=XG(1)-O(1)
       OM(2)=XG(2)-O(2)
       OM(3)=XG(3)-O(3)
       XL(1)=ROT(1)*OM(1)+ROT(2)*OM(2)+ROT(3)*OM(3)
       XL(2)=ROT(4)*OM(1)+ROT(5)*OM(2)+ROT(6)*OM(3)
       XL(3)=ROT(7)*OM(1)+ROT(8)*OM(2)+ROT(9)*OM(3)
C-----------
C      relative displacement at time T.
C-----------
C      relative displacement in global frame at time T-1/2.
       DREL(1)=DG(1)-DO(1)+(ROT0(1)-ROT(1))*XL(1)
     .                    +(ROT0(4)-ROT(4))*XL(2)
     .                    +(ROT0(7)-ROT(7))*XL(3)
       DREL(2)=DG(2)-DO(2)+(ROT0(2)-ROT(2))*XL(1)
     .                    +(ROT0(5)-ROT(5))*XL(2)
     .                    +(ROT0(8)-ROT(8))*XL(3)
       DREL(3)=DG(3)-DO(3)+(ROT0(3)-ROT(3))*XL(1)
     .                    +(ROT0(6)-ROT(6))*XL(2)
     .                    +(ROT0(9)-ROT(9))*XL(3)
C      relative displacement in local frame at time T-1/2.
       DL(1)=ROT0(1)*DREL(1)+ROT0(2)*DREL(2)+ROT0(3)*DREL(3)
       DL(2)=ROT0(4)*DREL(1)+ROT0(5)*DREL(2)+ROT0(6)*DREL(3)
       DL(3)=ROT0(7)*DREL(1)+ROT0(8)*DREL(2)+ROT0(9)*DREL(3)
C-----------
C      vitesse d'entrainement (T-1/2).
C-----------
       DT05=0.5*DT1
       OM2(1)=OM(1)-DT05*(VG(1)-VO(1))
       OM2(2)=OM(2)-DT05*(VG(2)-VO(2))
       OM2(3)=OM(3)-DT05*(VG(3)-VO(3))
       WOM(1)=W(2)*OM2(3)-W(3)*OM2(2)
       WOM(2)=W(3)*OM2(1)-W(1)*OM2(3)
       WOM(3)=W(1)*OM2(2)-W(2)*OM2(1)
       VE(1)=VO(1)+WOM(1)
       VE(2)=VO(2)+WOM(2)
       VE(3)=VO(3)+WOM(3)
C-----------
C      relative velocity in global frame at time T-1/2.
C-----------
       VREL(1)=VG(1)-VE(1)
       VREL(2)=VG(2)-VE(2)
       VREL(3)=VG(3)-VE(3)
C-----------
C      relative velocity in local frame at time T-1/2.
C-----------
C       VL(1)=ROT(1)*VREL(1)+ROT(2)*VREL(2)+ROT(3)*VREL(3)
C       VL(2)=ROT(4)*VREL(1)+ROT(5)*VREL(2)+ROT(6)*VREL(3)
C       VL(3)=ROT(7)*VREL(1)+ROT(8)*VREL(2)+ROT(9)*VREL(3)
C-------
Clm    fully consistant VL would use rotation at time T-1/2 :
       OMEGAX=W(1)*DT05
       OMEGAY=W(2)*DT05
       OMEGAZ=W(3)*DT05
C
       NN=SQRT(OMEGAX*OMEGAX+OMEGAY*OMEGAY+OMEGAZ*OMEGAZ)
       CS=COS(NN)
       SN=SIN(NN)
       NN=UN/MAX(EM20,NN)
       OMEGAX=OMEGAX*NN
       OMEGAY=OMEGAY*NN
       OMEGAZ=OMEGAZ*NN
C
       OMEGAXX=OMEGAX*OMEGAX
       OMEGAXY=OMEGAX*OMEGAY
       OMEGAXZ=OMEGAX*OMEGAZ
       OMEGAYY=OMEGAY*OMEGAY
       OMEGAYZ=OMEGAY*OMEGAZ
       OMEGAZZ=OMEGAZ*OMEGAZ
C
       UX=UN - OMEGAXX
       UY=  -OMEGAXY
       UZ=  -OMEGAXZ
       ROTDT05(1)=OMEGAXX+CS*UX+SN*(OMEGAY*UZ-OMEGAZ*UY)
       ROTDT05(4)=OMEGAXY+CS*UY+SN*(OMEGAZ*UX-OMEGAX*UZ)
       ROTDT05(7)=OMEGAXZ+CS*UZ+SN*(OMEGAX*UY-OMEGAY*UX)
C
       UX=  -OMEGAXY
       UY=UN - OMEGAYY
       UZ=  -OMEGAYZ
       ROTDT05(2)=OMEGAXY+CS*UX+SN*(OMEGAY*UZ-OMEGAZ*UY)
       ROTDT05(5)=OMEGAYY+CS*UY+SN*(OMEGAZ*UX-OMEGAX*UZ)
       ROTDT05(8)=OMEGAYZ+CS*UZ+SN*(OMEGAX*UY-OMEGAY*UX)
C
       UX=  -OMEGAXZ
       UY=  -OMEGAYZ
       UZ=UN - OMEGAZZ
       ROTDT05(3)=OMEGAXZ+CS*UX+SN*(OMEGAY*UZ-OMEGAZ*UY)
       ROTDT05(6)=OMEGAYZ+CS*UY+SN*(OMEGAZ*UX-OMEGAX*UZ)
       ROTDT05(9)=OMEGAZZ+CS*UZ+SN*(OMEGAX*UY-OMEGAY*UX)
C
       NN=UN/MAX(EM20,
     .  SQRT( ROTDT05(1)*ROTDT05(1)
     .       +ROTDT05(2)*ROTDT05(2)
     .       +ROTDT05(3)*ROTDT05(3)))
       ROTDT05(1)=NN*ROTDT05(1)
       ROTDT05(2)=NN*ROTDT05(2)
       ROTDT05(3)=NN*ROTDT05(3)
       NN=UN/MAX(EM20,
     .  SQRT( ROTDT05(4)*ROTDT05(4)
     .       +ROTDT05(5)*ROTDT05(5)
     .       +ROTDT05(6)*ROTDT05(6)))
       ROTDT05(4)=NN*ROTDT05(4)
       ROTDT05(5)=NN*ROTDT05(5)
       ROTDT05(6)=NN*ROTDT05(6)
       NN=UN/MAX(EM20,
     .  SQRT( ROTDT05(7)*ROTDT05(7)
     .       +ROTDT05(8)*ROTDT05(8)
     .       +ROTDT05(9)*ROTDT05(9)))
       ROTDT05(7)=NN*ROTDT05(7)
       ROTDT05(8)=NN*ROTDT05(8)
       ROTDT05(9)=NN*ROTDT05(9)
C
       ROTT05(1)=ROTDT05(1)*ROT(1)+ROTDT05(4)*ROT(2)+ROTDT05(7)*ROT(3)
       ROTT05(2)=ROTDT05(2)*ROT(1)+ROTDT05(5)*ROT(2)+ROTDT05(8)*ROT(3)
       ROTT05(3)=ROTDT05(3)*ROT(1)+ROTDT05(6)*ROT(2)+ROTDT05(9)*ROT(3)
       ROTT05(4)=ROTDT05(1)*ROT(4)+ROTDT05(4)*ROT(5)+ROTDT05(7)*ROT(6)
       ROTT05(5)=ROTDT05(2)*ROT(4)+ROTDT05(5)*ROT(5)+ROTDT05(8)*ROT(6)
       ROTT05(6)=ROTDT05(3)*ROT(4)+ROTDT05(6)*ROT(5)+ROTDT05(9)*ROT(6)
       ROTT05(7)=ROTDT05(1)*ROT(7)+ROTDT05(4)*ROT(8)+ROTDT05(7)*ROT(9)
       ROTT05(8)=ROTDT05(2)*ROT(7)+ROTDT05(5)*ROT(8)+ROTDT05(8)*ROT(9)
       ROTT05(9)=ROTDT05(3)*ROT(7)+ROTDT05(6)*ROT(8)+ROTDT05(9)*ROT(9)
C
       VL(1)=ROTT05(1)*VREL(1)+ROTT05(2)*VREL(2)+ROTT05(3)*VREL(3)
       VL(2)=ROTT05(4)*VREL(1)+ROTT05(5)*VREL(2)+ROTT05(6)*VREL(3)
       VL(3)=ROTT05(7)*VREL(1)+ROTT05(8)*VREL(2)+ROTT05(9)*VREL(3)
Clm    end fully consistant VL.
C-----------
C      relative angular velocity at time T-1/2.
C-----------
C      relative angular velocity in global frame at time T-1/2.
       VRREL(1)=VRG(1)-W(1)
       VRREL(2)=VRG(2)-W(2)
       VRREL(3)=VRG(3)-W(3)
C      relative angular velocity in local frame at time T-1/2.
       VRL(1)=ROT(1)*VRREL(1)+ROT(2)*VRREL(2)+ROT(3)*VRREL(3)
       VRL(2)=ROT(4)*VRREL(1)+ROT(5)*VRREL(2)+ROT(6)*VRREL(3)
       VRL(3)=ROT(7)*VRREL(1)+ROT(8)*VRREL(2)+ROT(9)*VRREL(3)
C
C-----------
C      RELATIVE ACCELERATIONS.
C-----------
       DWDT(1)=XFRAME(16)
       DWDT(2)=XFRAME(17)
       DWDT(3)=XFRAME(18)
C      second order correction.
C      W(1)=W(1)+DWDT(1)*DT05
C      W(2)=W(2)+DWDT(2)*DT05
C      W(3)=W(3)+DWDT(3)*DT05
C-----------
C      acceleration d'entrainement.
C-----------
       WOM(1)=W(2)*OM(3)-W(3)*OM(2)
       WOM(2)=W(3)*OM(1)-W(1)*OM(3)
       WOM(3)=W(1)*OM(2)-W(2)*OM(1)
       AE(1)=AO(1)+DWDT(2)*OM(3)-DWDT(3)*OM(2)
     .            +W(2)*WOM(3)-W(3)*WOM(2)
       AE(2)=AO(2)+DWDT(3)*OM(1)-DWDT(1)*OM(3)
     .            +W(3)*WOM(1)-W(1)*WOM(3)
       AE(3)=AO(3)+DWDT(1)*OM(2)-DWDT(2)*OM(1)
     .            +W(1)*WOM(2)-W(2)*WOM(1)
C-----------
C      acceleration de Coriolis.
C-----------
C      second order correction.
C      vitesse d'entrainement (T).
C      VE(1)=VO(1)+WOM(1)
C      VE(2)=VO(2)+WOM(2)
C      VE(3)=VO(3)+WOM(3)
C      relative velocity in global frame at time T.
C      VREL(1)=VG(1)-VE(1)
C      VREL(2)=VG(2)-VE(2)
C      VREL(3)=VG(3)-VE(3)
       AC(1)=2.*(W(2)*VREL(3)-W(3)*VREL(2))
       AC(2)=2.*(W(3)*VREL(1)-W(1)*VREL(3))
       AC(3)=2.*(W(1)*VREL(2)-W(2)*VREL(1))
C-----------
C      relative accelaration in global frame at time T.
C-----------
       AREL(1)=AG(1)-AE(1)-AC(1)
       AREL(2)=AG(2)-AE(2)-AC(2)
       AREL(3)=AG(3)-AE(3)-AC(3)
C      relative accelaration in local frame at time T.
       AL(1)=ROT(1)*AREL(1)+ROT(2)*AREL(2)+ROT(3)*AREL(3)
       AL(2)=ROT(4)*AREL(1)+ROT(5)*AREL(2)+ROT(6)*AREL(3)
       AL(3)=ROT(7)*AREL(1)+ROT(8)*AREL(2)+ROT(9)*AREL(3)
C
C-----------
C      relative angular accelaration at time T.
C-----------
C      relative angular accelaration in global frame at time T.
       ARREL(1)=ARG(1)-DWDT(1)
       ARREL(2)=ARG(2)-DWDT(2)
       ARREL(3)=ARG(3)-DWDT(3)
C      relative angular accelaration in local frame at time T.
       ARL(1)=ROT(1)*ARREL(1)+ROT(2)*ARREL(2)+ROT(3)*ARREL(3)
       ARL(2)=ROT(4)*ARREL(1)+ROT(5)*ARREL(2)+ROT(6)*ARREL(3)
       ARL(3)=ROT(7)*ARREL(1)+ROT(8)*ARREL(2)+ROT(9)*ARREL(3)
C-----------------------------------------------
 999  CONTINUE
      RETURN
      END
