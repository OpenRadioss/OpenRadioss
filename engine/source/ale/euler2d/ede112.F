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
Chd|  EDE112                        source/ale/euler2d/ede112.F   
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE EDE112(PM,V,VEUL,X,IXQ,ALE_CONNECT,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT)
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IXQ(7,*)
      my_real
     .   V(3,*), VEUL(10,*),X(3,*),PM(NPROPM,*),
     .   WYZ(*), DYZ(*), DZY(*),
     .   EYY(*),EZZ(*), ETT(*), EYZ(*), EYT(*), EZT(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,IVOI,ML,NC1,NC2,NC3,NC4,J,IAD2
C     REAL
      my_real
     .   VY13, VY24, VZ13,VZ24,PY1,PY2,PZ1,PZ2,AIRE
C-----------------------------------------------
      DO I=LFT,LLT
       II=I+NFT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
       DO J=1,4
        IVOI=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
        IF(IVOI.NE.0) THEN
         ML=NINT(PM(19,IXQ(1,IVOI)))
         IF(ML.NE.11) THEN
C FP 11-dec-96   AIRE=VEUL(1,IVOI)
          NC1=IXQ(2,IVOI)
          NC2=IXQ(3,IVOI)
          NC3=IXQ(4,IVOI)
          NC4=IXQ(5,IVOI)
          PY1=HALF*(X(3,NC2)-X(3,NC4))
          PY2=HALF*(X(3,NC3)-X(3,NC1))
          PZ1=HALF*(X(2,NC4)-X(2,NC2))
          PZ2=HALF*(X(2,NC1)-X(2,NC3))	  
C Added 1 ligne FP 11-dec-96
          AIRE=TWO*(PY1*PZ2-PZ1*PY2)
          VY13=V(2,NC1)-V(2,NC3)
          VY24=V(2,NC2)-V(2,NC4)
          VZ13=V(3,NC1)-V(3,NC3)
          VZ24=V(3,NC2)-V(3,NC4)
          EYY(I)=(PY1*VY13+PY2*VY24)/AIRE
          EZZ(I)=(PZ1*VZ13+PZ2*VZ24)/AIRE
          DZY(I)=(PY1*VZ13+PY2*VZ24)/AIRE
          DYZ(I)=(PZ1*VY13+PZ2*VY24)/AIRE
          EZZ(I)=ZERO
C          DZY(I)=0.
C          DYZ(I)=0.
          WYZ(I)=HALF*DT1*(DZY(I)-DYZ(I))
         ENDIF
        ENDIF
       ENDDO
      ENDDO
C
      RETURN
      END
