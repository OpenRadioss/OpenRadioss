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
Chd|  RELFRAM_M1                    source/tools/skew/relfram_m1.F
Chd|-- called by -----------
Chd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
Chd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RELFRAM_M1(XG ,VG      ,AREL    ,XFRAME  ,
     2                      VO      , AO      )
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
C     REAL
      my_real
     .   XG(3), VG(3), AREL(3),
     .   XFRAME(NXFRAME),  VO(3), AO(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K
C     REAL
      my_real
     .   ROT(9),ROT0(9),ROTDT05(9),ROTT05(9),
     .   OMEGAX, OMEGAY, OMEGAZ, NN, CS, SN, UX, UY, UZ,
     .   OMEGAXX, OMEGAXY, OMEGAXZ, OMEGAYY, OMEGAYZ, OMEGAZZ,
     .  O(3), OM(3), W(3), DWDT(3), WOM(3), VE(3), AE(3), AC(3), OM2(3),
     .  DT05, DREL(3), VREL(3), AG(3), VRREL(3), ARREL(3)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C      ACTUAL FRAME POSITION & ORIENTATION WRT GLOBAL SYSTEM
C-----------------
C      Frame position at time T (O == position of node N1).
       O(1)=XFRAME(10)
       O(2)=XFRAME(11)
       O(3)=XFRAME(12)
C-----------
C      ANGULAR VELOCITY at time T-1/2 and acceleration at time T.
C-----------
       W(1)=XFRAME(13)
       W(2)=XFRAME(14)
       W(3)=XFRAME(15)          
C-----------
C      local coordinates.
C-----------
       OM(1)=XG(1)-O(1)
       OM(2)=XG(2)-O(2)
       OM(3)=XG(3)-O(3)   
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
C      relative acceleration in global frame at time T.
C-----------
       AREL(1)=AREL(1)+AE(1)+AC(1)
       AREL(2)=AREL(2)+AE(2)+AC(2)
       AREL(3)=AREL(3)+AE(3)+AC(3)
C-----------------------------------------------
 999  CONTINUE
      RETURN
      END
