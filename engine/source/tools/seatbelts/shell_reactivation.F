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
Chd|  SHELL_REACTIVATION            source/tools/seatbelts/shell_reactivation.F
Chd|-- called by -----------
Chd|        UPDATE_SLIPRING               source/tools/seatbelts/update_slipring.F
Chd|-- calls ---------------
Chd|        HENCKY_STRAIN                 source/tools/seatbelts/shell_reactivation.F
Chd|====================================================================
      SUBROUTINE SHELL_REACTIVATION(I,II,L0FRAM1,L0FRAM2,NODE_FRAM1,
     .                              NODE_FRAM2,GSTR,NEL,XL2,YL2,
     .                              XL3,YL3,XL4,YL4,OFFSET,
     .                              N_DIR2,DIRA_X,DIRA_Y,SMSTR,ISMSTR)
C----6------------------------------------------
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  I,NODE_FRAM1,NODE_FRAM2,II(6),NEL,ISMSTR
      my_real, INTENT(IN) ::  L0FRAM1,L0FRAM2,XL2,YL2,XL3,YL3,XL4,YL4,OFFSET,N_DIR2(2)
      my_real, INTENT(INOUT) ::  GSTR(NEL,8),DIRA_X,DIRA_Y,SMSTR(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODE_FRAM_S,NODE_CORES_DIR1(4)
      my_real
     .        XLL(4),YLL(4),DIST,AREA0,FF(2,2),HH(2,2),
     .        UX13,UX24,UY13,UY24,PX1B,PX2B,PY1B,PY2B,VEC(2),N_DIR1(2)
C---------------------------------------------------------
C
C---------------------------------------------------------
C     Computation of DIR1
C---------------------------------------------------------
C
      NODE_CORES_DIR1(1) = 2
      NODE_CORES_DIR1(2) = 1
      NODE_CORES_DIR1(3) = 4
      NODE_CORES_DIR1(4) = 3
C
      XLL(1) = ZERO
      XLL(2) = XL2
      XLL(3) = XL3
      XLL(4) = XL4
      YLL(1) = ZERO
      YLL(2) = YL2
      YLL(3) = YL3
      YLL(4) = YL4
C
      NODE_FRAM_S = NODE_CORES_DIR1(NODE_FRAM1)
      VEC(1) = XLL(NODE_FRAM_S)-XLL(NODE_FRAM1)
      VEC(2) = YLL(NODE_FRAM_S)-YLL(NODE_FRAM1)
      DIST = SQRT(VEC(1)**2+VEC(2)**2)
      N_DIR1(1) = VEC(1)/(DIST*TWO)
      N_DIR1(2) = VEC(2)/(DIST*TWO)
C
      NODE_FRAM_S = NODE_CORES_DIR1(NODE_FRAM2)
      VEC(1) = XLL(NODE_FRAM_S)-XLL(NODE_FRAM2)
      VEC(2) = YLL(NODE_FRAM_S)-YLL(NODE_FRAM2)
      DIST = SQRT(VEC(1)**2+VEC(2)**2)
      N_DIR1(1) = N_DIR1(1) + VEC(1)/(DIST*TWO)
      N_DIR1(2) = N_DIR1(2) + VEC(2)/(DIST*TWO)
C
C---------------------------------------------------------
C     Computation of new reference configuration
C---------------------------------------------------------
C
C--   first fram released from sliring  - node_fram_s moved in ref configuration
      NODE_FRAM_S = NODE_CORES_DIR1(NODE_FRAM1)
      XLL(NODE_FRAM1) = OFFSET*N_DIR1(1)
      YLL(NODE_FRAM1) = OFFSET*N_DIR1(2)
      XLL(NODE_FRAM_S) = XLL(NODE_FRAM1) + L0FRAM1*N_DIR1(1)
      YLL(NODE_FRAM_S) = YLL(NODE_FRAM1) + L0FRAM1*N_DIR1(2)
C
C--   second fram released from sliring  - node_fram_s moved in ref configuration
      NODE_FRAM_S = NODE_CORES_DIR1(NODE_FRAM2)
      XLL(NODE_FRAM2) = N_DIR2(1)
      YLL(NODE_FRAM2) = N_DIR2(2)
      XLL(NODE_FRAM_S) = XLL(NODE_FRAM2) + L0FRAM2*N_DIR1(1)
      YLL(NODE_FRAM_S) = YLL(NODE_FRAM2) + L0FRAM2*N_DIR1(2)

C-- origin reset to N1
      XLL(2) = XLL(2)-XLL(1)
      XLL(3) = XLL(3)-XLL(1)
      XLL(4) = XLL(4)-XLL(1)
      YLL(2) = YLL(2)-YLL(1)
      YLL(3) = YLL(3)-YLL(1)
      YLL(4) = YLL(4)-YLL(1)

C-- update of orthotropy directions
      DIRA_X = N_DIR1(1)
      DIRA_Y = N_DIR1(2)
C
C---------------------------------------------------------
C     Computation of true strain (Hencky) HH = LOG(SQRT(FT*F))
C---------------------------------------------------------
C
      IF (ISMSTR /= 11) THEN
C
        UX13=-XL3
        UX24=XL2-XL4
        UY13=-YL3
        UY24=YL2-YL4
C
        PX1B = (YLL(2)-YLL(4))*HALF
        PY1B = (XLL(4)-XLL(2))*HALF
        PX2B = YLL(3)*HALF
        PY2B = -XLL(3)*HALF
        AREA0 = HALF*((XLL(2)-XLL(4))*YLL(3)-XLL(3)*(YLL(2)-YLL(4)))
C
C--     Matrix F (gradient transformation)
        FF(1,1)=(PX1B*UX13+PX2B*UX24)/AREA0
        FF(2,2)=(PY1B*UY13+PY2B*UY24)/AREA0
        FF(1,2)=(PY1B*UX13+PY2B*UX24)/AREA0
        FF(2,1)=(PX1B*UY13+PX2B*UY24)/AREA0

C--     computation of LOG(SQRT(FT*F))
        CALL HENCKY_STRAIN(FF,HH)
C
        GSTR(I,1)=HH(1,1)
        GSTR(I,2)=HH(2,2)
        GSTR(I,3)=HH(1,2)*TWO
        GSTR(I,4)=ZERO
        GSTR(I,5)=ZERO
        GSTR(I,6)=ZERO
        GSTR(I,7)=ZERO
        GSTR(I,8)=ZERO
C
      ELSE

        SMSTR(II(1)+I-1)=XLL(2)
        SMSTR(II(2)+I-1)=YLL(2)
        SMSTR(II(3)+I-1)=XLL(3)
        SMSTR(II(4)+I-1)=YLL(3)
        SMSTR(II(5)+I-1)=XLL(4)
        SMSTR(II(6)+I-1)=YLL(4)
C
      ENDIF
C
C----------------------------------------------------------
C----------------------------------------------------------
C----------------------------------------------------------      
C----------------------------------------------------------
      RETURN
C                
      END
C
Chd|====================================================================
Chd|  HENCKY_STRAIN                 source/tools/seatbelts/shell_reactivation.F
Chd|-- called by -----------
Chd|        SHELL_REACTIVATION            source/tools/seatbelts/shell_reactivation.F
Chd|-- calls ---------------
Chd|        JACOBIEW                      source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
      SUBROUTINE HENCKY_STRAIN(FF,HH)
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
      my_real, INTENT(IN) ::   FF(2,2)
      my_real, INTENT(OUT) ::  HH(2,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NROT
      my_real
     .        FTF(2,2),FTF_VAL(2),FTF_VEC(2,2),HHB(2,2)
C------------------------------------------------
C
      FTF(1,1) = FF(1,1)**2 + FF(2,1)**2
      FTF(2,2) = FF(2,2)**2 + FF(1,2)**2
      FTF(1,2) = FF(1,1)*FF(1,2) + FF(2,1)*FF(2,2)
      FTF(2,1) = FF(1,1)*FF(1,2) + FF(2,1)*FF(2,2)
C
      CALL JACOBIEW(FTF,2,FTF_VAL,FTF_VEC,NROT)
C
      FTF_VAL(1) = LOG(SQRT(FTF_VAL(1)))
      FTF_VAL(2) = LOG(SQRT(FTF_VAL(2)))
C
      HH = ZERO
      HHB = ZERO
C     
      DO I=1,2
        DO J=1,2
          HHB(I,J) = HHB(I,J) + FTF_VEC(I,J) * FTF_VAL(J)
        ENDDO
      ENDDO
C
      DO I=1,2
        DO J=1,2
          DO K=1,2
            HH(I,J) = HH(I,J) + HHB(I,K) * FTF_VEC(J,K)
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
