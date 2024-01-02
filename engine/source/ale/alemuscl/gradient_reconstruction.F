Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  GRADIENT_RECONSTRUCTION       source/ale/alemuscl/gradient_reconstruction.F
Chd|-- called by -----------
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|-- calls ---------------
Chd|        CG                            source/ale/alemuscl/conjugate_gradient.F
Chd|        ALEMUSCL_MOD                  ../common_source/modules/ale/alemuscl_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE GRADIENT_RECONSTRUCTION(ITASK, IXS, X, ALE_CONNECT, NV46, ITRIMAT,SEGVAR)
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
      INTEGER, INTENT(IN) :: ITASK
      INTEGER, INTENT(IN) :: IXS(NIXS,NUMELS)
      my_real, INTENT(IN) :: X(3,NUMNOD)
      INTEGER, INTENT(IN) :: NV46
      INTEGER, INTENT(IN) :: ITRIMAT
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, KK, IAD2, LGTH
      my_real :: XK, YK, ZK, XL, YL, ZL, XF, YF, ZF
      my_real :: VALK, VALL, VALNODE
      INTEGER :: NODE_ID
      my_real :: mat(3, 3), rhs(3), sol(3)
      INTEGER :: VOIS_ID
      INTEGER :: FACE_TO_NODE_LOCAL_ID(6, 4)
      my_real :: NORM(3), A(3), B(3), C(3), SURF, SURF1, SURF2
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
!!! Once for all, associate node local id to a face number
!!! Face 1
      FACE_TO_NODE_LOCAL_ID(1, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(1, 2) = 4
      FACE_TO_NODE_LOCAL_ID(1, 3) = 3 ; FACE_TO_NODE_LOCAL_ID(1, 4) = 2
!!! Face 2
      FACE_TO_NODE_LOCAL_ID(2, 1) = 3 ; FACE_TO_NODE_LOCAL_ID(2, 2) = 4
      FACE_TO_NODE_LOCAL_ID(2, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(2, 4) = 7
!!! Face 3
      FACE_TO_NODE_LOCAL_ID(3, 1) = 5 ; FACE_TO_NODE_LOCAL_ID(3, 2) = 6
      FACE_TO_NODE_LOCAL_ID(3, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(3, 4) = 8
!!! Face 4
      FACE_TO_NODE_LOCAL_ID(4, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(4, 2) = 2
      FACE_TO_NODE_LOCAL_ID(4, 3) = 6 ; FACE_TO_NODE_LOCAL_ID(4, 4) = 5
!!! Face 5
      FACE_TO_NODE_LOCAL_ID(5, 1) = 2 ; FACE_TO_NODE_LOCAL_ID(5, 2) = 3
      FACE_TO_NODE_LOCAL_ID(5, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(5, 4) = 6
!!! Face 6
      FACE_TO_NODE_LOCAL_ID(6, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(6, 2) = 5
      FACE_TO_NODE_LOCAL_ID(6, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(6, 4) = 4  

      DO I = LFT, LLT
         II = I + NFT
         !!! Reset mat, rhs
         mat(1:3, 1:3) = ZERO ; rhs(1:3) = ZERO
         !!! Value of the target function in the element
         VALK = ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT)
         XK = ALEMUSCL_Buffer%ELCENTER(II,1) ;
         YK = ALEMUSCL_Buffer%ELCENTER(II,2) ;
         ZK = ALEMUSCL_Buffer%ELCENTER(II,3)
         !!! IXS(2:9, II) : Node global ID
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
         DO KK = 1, LGTH
            VOIS_ID = ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1)
            !IF (VOIS_ID /= 0 .AND. VOIS_ID <= NUMELS) THEN
            IF (VOIS_ID > 0) THEN
               !!! Value of the target function in the current neighbor
               VALL = ALEMUSCL_Buffer%VOLUME_FRACTION(VOIS_ID,ITRIMAT)
               XL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,1) ;
               YL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,2) ; 
               ZL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,3) ;
            ELSE
               IF(VOIS_ID == 0) THEN
                 VALL = VALK
               ELSE
                 !vois_id<0 : means EBCS), -vois_id is seg_id
                 VALL = SEGVAR%PHASE_ALPHA(ITRIMAT,-VOIS_ID)
               ENDIF
               XF = ZERO
               YF = ZERO
               ZF = ZERO

               A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
               B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 2) + 1, II))
               C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))

               NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
               NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
               NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

               SURF1 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
               XF = SURF1 * THIRD * (A(1) + B(1) + C(1))
               YF = SURF1 * THIRD * (A(2) + B(2) + C(2))
               ZF = SURF1 * THIRD * (A(3) + B(3) + C(3))

               A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
               B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))
               C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 4) + 1, II))

               NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
               NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
               NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

               SURF2 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
               XF = XF + SURF2 * THIRD * (A(1) + B(1) + C(1))
               YF = YF + SURF2 * THIRD * (A(2) + B(2) + C(2))
               ZF = ZF + SURF2 * THIRD * (A(3) + B(3) + C(3)) 
               
               SURF = SURF1 + SURF2
               XF = XF / SURF
               YF = YF / SURF
               ZF = ZF / SURF
               !!! Build face centroid
c$$$               DO JJ = 1, 4
c$$$                  NODE_ID = IXS(FACE_TO_NODE_LOCAL_ID(KK, JJ) + 1, II)
c$$$                  XF = XF + FOURTH * X(1, NODE_ID)
c$$$                  YF = YF + FOURTH * X(2, NODE_ID)
c$$$                  ZF = ZF + FOURTH * X(3, NODE_ID)
c$$$               ENDDO
               XL = TWO * XF - ALEMUSCL_Buffer%ELCENTER(II,1)
               YL = TWO * YF - ALEMUSCL_Buffer%ELCENTER(II,2)
               ZL = TWO * ZF - ALEMUSCL_Buffer%ELCENTER(II,3)
            ENDIF

            !!! Incrementing mat and rhs
            rhs(1) = rhs(1) + (VALK - VALL) * (XL - XK)
            rhs(2) = rhs(2) + (VALK - VALL) * (YL - YK)
            rhs(3) = rhs(3) + (VALK - VALL) * (ZL - ZK)
            mat(1, 1) = mat(1, 1) + (XL - XK) * (XL - XK)
            mat(1, 2) = mat(1, 2) + (XL - XK) * (YL - YK)
            mat(1, 3) = mat(1, 3) + (XL - XK) * (ZL - ZK)
            mat(2, 1) = mat(2, 1) + (YL - YK) * (XL - XK)
            mat(2, 2) = mat(2, 2) + (YL - YK) * (YL - YK)
            mat(2, 3) = mat(2, 3) + (YL - YK) * (ZL - ZK)
            mat(3, 1) = mat(3, 1) + (ZL - ZK) * (XL - XK)
            mat(3, 2) = mat(3, 2) + (ZL - ZK) * (YL - YK)
            mat(3, 3) = mat(3, 3) + (ZL - ZK) * (ZL - ZK)
         ENDDO

         CALL CG(3, mat, rhs, sol, 3, EM10)
         !CALL DIRECT_SOLVE(mat, rhs ,sol)
         !!! Solution goes to the gradient
         ALEMUSCL_Buffer%GRAD(II,1,ITRIMAT) = -sol(1)
         ALEMUSCL_Buffer%GRAD(II,2,ITRIMAT) = -sol(2)
         ALEMUSCL_Buffer%GRAD(II,3,ITRIMAT) = -sol(3)
      ENDDO  ! I = LFT, LLT
C-----------------------------------------------      
      END SUBROUTINE GRADIENT_RECONSTRUCTION
