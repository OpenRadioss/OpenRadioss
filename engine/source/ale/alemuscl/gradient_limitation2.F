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
Chd|  GRADIENT_LIMITATION2          source/ale/alemuscl/gradient_limitation2.F
Chd|-- called by -----------
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- calls ---------------
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|====================================================================
      SUBROUTINE GRADIENT_LIMITATION2(IXQ, X, TRIMAT)
C-----------------------------------------------
C  D e s c r i p t i o n   
C        limits the amplitude of this gradient in such a way that 
C        extrapolated values on the nodes of the element lie between
C        local minimum and maximum values from the neighboring elements
C        -> maximum principle purpose
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE ALEMUSCL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "vect01_c.inc"
#include "com04_c.inc"
#include "alemuscl.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXQ(NIXQ, NUMELQ), TRIMAT
      my_real, INTENT(IN) :: X(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, JJ
      INTEGER :: NODE_ID
      my_real :: reduc_factor(TRIMAT), nodal_reduc_factor, YN, ZN, VALNODE
      INTEGER :: ITRIMAT
      INTEGER :: NNUM
      my_real :: YK, ZK
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------   
      !!! Limiting process for the computed gradient -> maximum principle
      !!! and stability purposes
      DO I = LFT, LLT
         II = I + NFT
         !!! Element centroid
         YK = ELCENTER(II,2) ; ZK = ELCENTER(II,3)
         reduc_factor = EP30
         NNUM = 0
         DO ITRIMAT = 1, TRIMAT
            IF(ABS(GRAD(II,2,ITRIMAT)) + 
     .           ABS(GRAD(II,3,ITRIMAT)) > ZERO) THEN
               NNUM = NNUM + 1
               !!! Check the nodes of the element
               DO JJ = 1, 4
                  NODE_ID = IXQ(JJ+1, II)
                  !!! Get the node coordinates
                  YN = X(2, NODE_ID) ; ZN = X(3, NODE_ID) 
                  !!! Interpolate the function at the node
                  VALNODE = VOLUME_FRACTION(II,ITRIMAT) 
     .                 + GRAD(II,2,ITRIMAT) * (YN - YK)
     .                 + GRAD(II,3,ITRIMAT) * (ZN - ZK)
                  nodal_reduc_factor = ONE
                  IF (VALNODE - VOLUME_FRACTION(II,ITRIMAT) > ZERO) THEN
                     nodal_reduc_factor = 
     .                    MIN((NODE_MAX_VALUE(NODE_ID,ITRIMAT) - VOLUME_FRACTION(II,ITRIMAT))
     .                    / (VALNODE - VOLUME_FRACTION(II,ITRIMAT)), I_BETA)
                  ELSE IF (VALNODE - VOLUME_FRACTION(II,ITRIMAT) < ZERO) THEN
                     nodal_reduc_factor = 
     .                    MIN((NODE_MIN_VALUE(NODE_ID,ITRIMAT) - VOLUME_FRACTION(II,ITRIMAT)) 
     .                    / (VALNODE - VOLUME_FRACTION(II,ITRIMAT)), I_BETA)
                  ENDIF
                  reduc_factor(ITRIMAT) = MIN(reduc_factor(ITRIMAT), nodal_reduc_factor)
               ENDDO  ! JJ = 1, 8
            ELSE
               reduc_factor(ITRIMAT) = ZERO
            ENDIF 
         ENDDO  ! ITRIMAT = 1, TRIMAT
        
         DO ITRIMAT = 1, TRIMAT
            IF(ABS(GRAD(II,2,ITRIMAT)) + 
     .           ABS(GRAD(II,3,ITRIMAT)) > ZERO) THEN
            !!!   Limitation of the gradient
               GRAD(II,2,ITRIMAT) = reduc_factor(ITRIMAT) * GRAD(II,2,ITRIMAT)
               GRAD(II,3,ITRIMAT) = reduc_factor(ITRIMAT) * GRAD(II,3,ITRIMAT)
            ENDIF
         ENDDO
      ENDDO  ! I = LFT, LLT
C-----------------------------------------------      
      END SUBROUTINE GRADIENT_LIMITATION2
