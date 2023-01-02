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
Chd|  KINE_SEATBELT_VEL             source/tools/seatbelts/kine_seatbelt_vel.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE KINE_SEATBELT_VEL(A,V,X,XDP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE SEATBELT_MOD
C----6---------------------------------------------------------------7---------8
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
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) ::   A(3,NUMNOD),V(3,NUMNOD),X(3,NUMNOD)
      DOUBLE PRECISION, INTENT(INOUT) :: XDP(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NODE1,NODE2,NODE3,ANCHOR_NODE,NODE2_N,ORIENTATION_NODE,SWIP,INDEX
      my_real NN(3),NORM,SCAL,N1(3),N2(3),N3(3)
C---------------------------------------------------------
C
C----------------------------------------------------------
C-    KINEMATIC CONDITION OF SLIPRING - FORCE TRANSFER
C----------------------------------------------------------

      DO I=1,NSLIPRING
C
        DO J=1,SLIPRING(I)%NFRAM
C
           ANCHOR_NODE = SLIPRING(I)%FRAM(J)%ANCHOR_NODE
           ORIENTATION_NODE = SLIPRING(I)%FRAM(J)%ORIENTATION_NODE
           NODE1 = SLIPRING(I)%FRAM(J)%NODE(1)
           NODE2 = SLIPRING(I)%FRAM(J)%NODE(2)
           NODE3 = SLIPRING(I)%FRAM(J)%NODE(3)
C
           IF (ORIENTATION_NODE > 0) THEN
C---         update of orientation angle
             NN(1) = X(1,ORIENTATION_NODE) - X(1,ANCHOR_NODE)
             NN(2) = X(2,ORIENTATION_NODE) - X(2,ANCHOR_NODE)
             NN(3) = X(3,ORIENTATION_NODE) - X(3,ANCHOR_NODE)
             NORM = SQRT(MAX(EM30,NN(1)*NN(1)+NN(2)*NN(2)+NN(3)*NN(3)))
             NN(1) = NN(1) / NORM
             NN(2) = NN(2) / NORM
             NN(3) = NN(3) / NORM
C
             N1(1) = X(1,NODE1) - X(1,NODE2)
             N1(2) = X(2,NODE1) - X(2,NODE2)
             N1(3) = X(3,NODE1) - X(3,NODE2)
             NORM = SQRT(MAX(EM30,N1(1)*N1(1)+N1(2)*N1(2)+N1(3)*N1(3)))
             N1(1) = N1(1) / NORM
             N1(2) = N1(2) / NORM
             N1(3) = N1(3) / NORM
C
             N2(1) = X(1,NODE3) - X(1,NODE2)
             N2(2) = X(2,NODE3) - X(2,NODE2)
             N2(3) = X(3,NODE3) - X(3,NODE2)
             NORM = SQRT(MAX(EM30,N2(1)*N2(1)+N2(2)*N2(2)+N2(3)*N2(3)))
             N2(1) = N2(1) / NORM
             N2(2) = N2(2) / NORM
             N2(3) = N2(3) / NORM
C
             N3(1) = N1(2)*N2(3)-N1(3)*N2(2)
             N3(2) = N1(3)*N2(1)-N1(1)*N2(3)
             N3(3) = N1(1)*N2(2)-N1(2)*N2(1)
             NORM = SQRT(MAX(EM30,N3(1)*N3(1)+N3(2)*N3(2)+N3(3)*N3(3)))
             N3(1) = N3(1) / NORM
             N3(2) = N3(2) / NORM
             N3(3) = N3(3) / NORM
C
             SCAL = ABS(N3(1)*NN(1)+N3(2)*NN(2)+N3(3)*NN(3))
             SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE = ACOS(SCAL)
           ENDIF
C
           IF(SLIPRING(I)%FRAM(J)%UPDATE < ZERO) THEN
C
             V(1,NODE2)=V(1,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(1)
             V(2,NODE2)=V(2,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(2)
             V(3,NODE2)=V(3,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(3)     
C
             V(1,NODE3)=V(1,ANCHOR_NODE)
             V(2,NODE3)=V(2,ANCHOR_NODE)
             V(3,NODE3)=V(3,ANCHOR_NODE)
C
             A(1,NODE3)=A(1,ANCHOR_NODE)
             A(2,NODE3)=A(2,ANCHOR_NODE)
             A(3,NODE3)=A(3,ANCHOR_NODE)
C
             X(1,NODE3)=X(1,ANCHOR_NODE)
             X(2,NODE3)=X(2,ANCHOR_NODE)
             X(3,NODE3)=X(3,ANCHOR_NODE)
C
             IF (IRESP == 1) THEN
               XDP(1,NODE3)=XDP(1,ANCHOR_NODE)
               XDP(2,NODE3)=XDP(2,ANCHOR_NODE)
               XDP(3,NODE3)=XDP(3,ANCHOR_NODE)  
             ENDIF    
C
         ELSEIF(SLIPRING(I)%FRAM(J)%UPDATE > ZERO) THEN

             V(1,NODE2)=V(1,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(4)
             V(2,NODE2)=V(2,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(5)
             V(3,NODE2)=V(3,ANCHOR_NODE)-SLIPRING(I)%FRAM(J)%MATERIAL_FLOW*SLIPRING(I)%FRAM(J)%VECTOR(6)
C
             V(1,NODE1)=V(1,ANCHOR_NODE)
             V(2,NODE1)=V(2,ANCHOR_NODE)
             V(3,NODE1)=V(3,ANCHOR_NODE)

             A(1,NODE1)=A(1,ANCHOR_NODE)
             A(2,NODE1)=A(2,ANCHOR_NODE)
             A(3,NODE1)=A(3,ANCHOR_NODE)

             X(1,NODE1)=X(1,ANCHOR_NODE)
             X(2,NODE1)=X(2,ANCHOR_NODE)
             X(3,NODE1)=X(3,ANCHOR_NODE)
C
             IF (IRESP == 1) THEN
               XDP(1,NODE1)=XDP(1,ANCHOR_NODE)
               XDP(2,NODE1)=XDP(2,ANCHOR_NODE)
               XDP(3,NODE1)=XDP(3,ANCHOR_NODE)  
             ENDIF
C
           ELSE

             V(1,NODE2)=V(1,ANCHOR_NODE)
             V(2,NODE2)=V(2,ANCHOR_NODE)
             V(3,NODE2)=V(3,ANCHOR_NODE)

             A(1,NODE2)=A(1,ANCHOR_NODE)
             A(2,NODE2)=A(2,ANCHOR_NODE)
             A(3,NODE2)=A(3,ANCHOR_NODE)

           ENDIF
C
        ENDDO
C
      ENDDO

C----------------------------------------------------------
C-    KINEMATIC CONDITION OF RETRACTOR - FORCE TRANSFER
C----------------------------------------------------------

      DO I=1,NRETRACTOR
C
         ANCHOR_NODE = RETRACTOR(I)%ANCHOR_NODE
         NODE1 = RETRACTOR(I)%NODE(1)
         NODE2 = RETRACTOR(I)%NODE(2)
         NODE2_N = RETRACTOR(I)%NODE_NEXT(2)
C
         IF (RETRACTOR(I)%UPDATE > 0) THEN
C--        release of new node
           V(1,NODE2)=V(1,ANCHOR_NODE)+RETRACTOR(I)%MATERIAL_FLOW*RETRACTOR(I)%VECTOR(1)
           V(2,NODE2)=V(2,ANCHOR_NODE)+RETRACTOR(I)%MATERIAL_FLOW*RETRACTOR(I)%VECTOR(2)
           V(3,NODE2)=V(3,ANCHOR_NODE)+RETRACTOR(I)%MATERIAL_FLOW*RETRACTOR(I)%VECTOR(3)
C
           V(1,NODE2_N)=V(1,ANCHOR_NODE)
           V(2,NODE2_N)=V(2,ANCHOR_NODE)
           V(3,NODE2_N)=V(3,ANCHOR_NODE)

           A(1,NODE2_N)=A(1,ANCHOR_NODE)
           A(2,NODE2_N)=A(2,ANCHOR_NODE)
           A(3,NODE2_N)=A(3,ANCHOR_NODE)
C
           SWIP = 0
           DO K=1,RETRACTOR(I)%INACTI_NNOD
             IF (RETRACTOR(I)%INACTI_NODE(K)==NODE2) SWIP = 1           
             IF (SWIP == 1) RETRACTOR(I)%INACTI_NODE(K) = RETRACTOR(I)%INACTI_NODE(K+1) 
           ENDDO          
           RETRACTOR(I)%INACTI_NNOD = RETRACTOR(I)%INACTI_NNOD - 1
C
         ELSEIF (RETRACTOR(I)%UPDATE < 0) THEN
C--        node will enter retractor
           V(1,NODE2)=V(1,ANCHOR_NODE)
           V(2,NODE2)=V(2,ANCHOR_NODE)
           V(3,NODE2)=V(3,ANCHOR_NODE)
C
           V(1,NODE1)=V(1,ANCHOR_NODE)
           V(2,NODE1)=V(2,ANCHOR_NODE)
           V(3,NODE1)=V(3,ANCHOR_NODE)

           A(1,NODE1)=A(1,ANCHOR_NODE)
           A(2,NODE1)=A(2,ANCHOR_NODE)
           A(3,NODE1)=A(3,ANCHOR_NODE)

           X(1,NODE1)=X(1,ANCHOR_NODE)
           X(2,NODE1)=X(2,ANCHOR_NODE)
           X(3,NODE1)=X(3,ANCHOR_NODE)
C
           IF (IRESP == 1) THEN
             XDP(1,NODE1)=XDP(1,ANCHOR_NODE)
             XDP(2,NODE1)=XDP(2,ANCHOR_NODE)
             XDP(3,NODE1)=XDP(3,ANCHOR_NODE)  
           ENDIF
C
           INDEX = RETRACTOR(I)%INACTI_NNOD
           RETRACTOR(I)%INACTI_NODE(INDEX+1) = NODE2
           RETRACTOR(I)%INACTI_NNOD = RETRACTOR(I)%INACTI_NNOD + 1
C
         ELSE

           V(1,NODE2)=V(1,ANCHOR_NODE)
           V(2,NODE2)=V(2,ANCHOR_NODE)
           V(3,NODE2)=V(3,ANCHOR_NODE)

           A(1,NODE2)=A(1,ANCHOR_NODE)
           A(2,NODE2)=A(2,ANCHOR_NODE)
           A(3,NODE2)=A(3,ANCHOR_NODE)

         ENDIF
C
C        Temporary
C
         DO K=1,RETRACTOR(I)%INACTI_NNOD
            L = RETRACTOR(I)%INACTI_NODE(K)
            V(1,L)=V(1,ANCHOR_NODE)
            V(2,L)=V(2,ANCHOR_NODE)
            V(3,L)=V(3,ANCHOR_NODE)

            A(1,L)=A(1,ANCHOR_NODE)
            A(2,L)=A(2,ANCHOR_NODE)
            A(3,L)=A(3,ANCHOR_NODE)
         ENDDO
C
      ENDDO

C----------------------------------------------------------
C

C----------------------------------------------------------      
C
      RETURN
                
      END
