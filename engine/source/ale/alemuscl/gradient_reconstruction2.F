Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    gradient_reconstruction2         ../engine/source/ale/alemuscl/gradient_reconstruction2.F
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod                     ../common_source/modules/ale/alemuscl_mod.F
!||    segvar_mod                       ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE GRADIENT_RECONSTRUCTION2(IXQ, X, ALE_CONNECT, NV46, ITRIMAT, SEGVAR)
C-----------------------------------------------
C  D e s c r i p t i o n
C  This subroutine computes a gradient of the scalar field value in each 
C  element:
C        mean square approximation of the gradient in the face related 
C        neighborhood of the element
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE ALEMUSCL_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "vect01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NV46
      INTEGER, INTENT(IN) :: IXQ(NIXQ, NUMELQ)
      my_real, INTENT(IN) :: X(3, NUMNOD)
      INTEGER, INTENT(IN) :: ITRIMAT
      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, KK, IAD2, LGTH
      my_real ::  YK, ZK, YL, ZL,YF, ZF
      my_real :: VALK, VALL
      my_real :: mat(2, 2), rhs(2), sol(2)
      INTEGER :: VOIS_ID
      INTEGER :: FACE_TO_NODE_LOCAL_ID(4, 2), NODEID1, NODEID2
      my_real :: det, undet
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
!!! Once for all, associate node local id to a face number
!!! Face 1
      FACE_TO_NODE_LOCAL_ID(1, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(1, 2) = 2
!!! Face 2
      FACE_TO_NODE_LOCAL_ID(2, 1) = 2 ; FACE_TO_NODE_LOCAL_ID(2, 2) = 3
!!! Face 3
      FACE_TO_NODE_LOCAL_ID(3, 1) = 3 ; FACE_TO_NODE_LOCAL_ID(3, 2) = 4
!!! Face 4
      FACE_TO_NODE_LOCAL_ID(4, 1) = 4 ; FACE_TO_NODE_LOCAL_ID(4, 2) = 1

      DO I = LFT, LLT
         II = I + NFT
         !!! Reset mat, rhs
         mat(1:2, 1:2) = ZERO ; rhs(1:2) = ZERO
         !!! Value of the target function in the element
         VALK = ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT)
         YK = ALEMUSCL_Buffer%ELCENTER(II,2) ;
         ZK = ALEMUSCL_Buffer%ELCENTER(II,3)
         !!! IXS(2:9, II) : Node global ID
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
         DO KK = 1, NV46
            VOIS_ID = ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1)
            IF (VOIS_ID > 0) THEN
               !!! Value of the target function in the current neighbor
               VALL = ALEMUSCL_Buffer%VOLUME_FRACTION(VOIS_ID,ITRIMAT)
               YL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,2) ;
               ZL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,3) ;
            ELSE
               IF(VOIS_ID == 0) THEN
                 VALL = VALK
               ELSE
                 !vois_id<0 : means EBCS), -vois_id is seg_id
                 VALL = SEGVAR%PHASE_ALPHA(ITRIMAT,-VOIS_ID)
               ENDIF

               NODEID1 = IXQ(1 + FACE_TO_NODE_LOCAL_ID(KK, 1), II)
               NODEID2 = IXQ(1 + FACE_TO_NODE_LOCAL_ID(KK, 2), II)

               YF = HALF * (X(2, NODEID1) + X(2, NODEID2))
               ZF = HALF * (X(3, NODEID1) + X(3, NODEID2))

               YL = TWO * YF - ALEMUSCL_Buffer%ELCENTER(II,2)
               ZL = TWO * ZF - ALEMUSCL_Buffer%ELCENTER(II,3)
            ENDIF

            !!! Incrementing mat and rhs
            rhs(1) = rhs(1) + (VALK - VALL) * (YL - YK)
            rhs(2) = rhs(2) + (VALK - VALL) * (ZL - ZK)
            mat(1, 1) = mat(1, 1) + (YL - YK) * (YL - YK)
            mat(1, 2) = mat(1, 2) + (YL - YK) * (ZL - ZK)
            mat(2, 1) = mat(2, 1) + (ZL - ZK) * (YL - YK)
            mat(2, 2) = mat(2, 2) + (ZL - ZK) * (ZL - ZK)
         ENDDO
         
         det = mat(1, 1) * mat(2, 2) - mat(2, 1) * mat(1, 2)
         IF (det == 0) THEN
            PRINT*, "OUPS"
         ENDIF
         undet = ONE / det
         sol(1) = undet * (rhs(1) * mat(2,2) - rhs(2) * mat(1,2))
         sol(2) = undet * (- mat(2,1) * rhs(1) + mat(1, 1) * rhs(2))
         !!! Solution goes to the gradient
         ALEMUSCL_Buffer%GRAD(II,2,ITRIMAT) = -sol(1)
         ALEMUSCL_Buffer%GRAD(II,3,ITRIMAT) = -sol(2)
      ENDDO  ! I = LFT, LLT
C-----------------------------------------------      
      END SUBROUTINE GRADIENT_RECONSTRUCTION2
