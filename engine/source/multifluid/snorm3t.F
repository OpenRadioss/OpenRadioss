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
Chd|  SNORM3T                       source/multifluid/snorm3t.F   
Chd|-- called by -----------
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE SNORM3T (NEL, NFT, JALE, IXS, XGRID, WGRID, 
     .     NORM, WFAC, SURF)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Computes normal vector to the faces of each element in a group
C     for a 3d solid element (hence Snorm3)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
!     NIXS
#include      "param_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL, NFT, JALE, IXS(NIXS, *)
      my_real, INTENT(IN) :: 
     .     XGRID(3, *), WGRID(*)
      my_real, INTENT(OUT) :: WFAC(3, 6, NEL), SURF(6, NEL)
      my_real, INTENT(OUT), TARGET :: NORM(3, 6, NEL) 
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, NODE1, NODE2, NODE3, NODE4, KFACE
      my_real
     .     X1(3), X2(3), X3(3), X4(3),
     .     W1(3), W2(3), W3(3), W4(3), XC(3), XF(3)
      my_real, POINTER :: NX, NY, NZ

      DO II = 1, NEL
C     Nodes of the element locally stored in NODE* for memory access
         NODE1 = IXS(2, II + NFT)
         NODE2 = IXS(4, II + NFT)
         NODE3 = IXS(7, II + NFT)
         NODE4 = IXS(6, II + NFT)
C     Node coordinates
         X1(1:3) = XGRID(1:3, NODE1)
         X2(1:3) = XGRID(1:3, NODE2)
         X3(1:3) = XGRID(1:3, NODE3)
         X4(1:3) = XGRID(1:3, NODE4)

         XC(1:3) = QUART * (X1(1:3) + X2(1:3) + X3(1:3) + X4(1:3))
         IF (JALE .NE. 0) THEN
C     Node grid velocities
            W1(1:3) = WGRID(3 * (NODE1 - 1) + 1 : 3 * (NODE1 - 1) + 3)
            W2(1:3) = WGRID(3 * (NODE2 - 1) + 1 : 3 * (NODE2 - 1) + 3)
            W3(1:3) = WGRID(3 * (NODE3 - 1) + 1 : 3 * (NODE3 - 1) + 3)
            W4(1:3) = WGRID(3 * (NODE4 - 1) + 1 : 3 * (NODE4 - 1) + 3)
         ELSE
!     Euler
            W1(1:3) = ZERO
            W2(1:3) = ZERO
            W3(1:3) = ZERO
            W4(1:3) = ZERO
         ENDIF
C     Face normal
C     Face 1 -> nodes 1 2 3
         KFACE = 5
         NORM(1, KFACE, II) = UNDEMI * ((X3(2) - X1(2)) * (X2(3) - X1(3)) - 
     .        (X3(3) - X1(3)) * (X2(2) - X1(2)))
         NORM(2, KFACE, II) = UNDEMI * ((X3(3) - X1(3)) * (X2(1) - X1(1)) - 
     .        (X3(1) - X1(1)) * (X2(3) - X1(3)))
         NORM(3, KFACE, II) = UNDEMI * ((X3(1) - X1(1)) * (X2(2) - X1(2)) - 
     .        (X3(2) - X1(2)) * (X2(1) - X1(1)))
         NX => NORM(1, KFACE, II)
         NY => NORM(2, KFACE, II)
         NZ => NORM(3, KFACE, II)
         SURF(KFACE, II) = SQRT(NX * NX + NY * NY + NZ * NZ)
         NX = -NX / SURF(KFACE, II)
         NY = -NY / SURF(KFACE, II)
         NZ = -NZ / SURF(KFACE, II)
         XF(1:3) = TIERS * (X1 + X2 + X3)
         IF ((XF(1) - XC(1)) * NX + (XF(2) - XC(2)) * NY + (XF(3) - XC(3)) * NZ .LT. ZERO) THEN
            CALL ARRET(2)
         ENDIF
C     Face 2
         KFACE = 6
         NORM(1, KFACE, II) = UNDEMI * ((X1(2) - X4(2)) * (X2(3) - X4(3)) - 
     .        (X1(3) - X4(3)) * (X2(2) - X4(2)))
         NORM(2, KFACE, II) = UNDEMI * ((X1(3) - X4(3)) * (X2(1) - X4(1)) - 
     .        (X1(1) - X4(1)) * (X2(3) - X4(3)))
         NORM(3, KFACE, II) = UNDEMI * ((X1(1) - X4(1)) * (X2(2) - X4(2)) - 
     .        (X1(2) - X4(2)) * (X2(1) - X4(1)))
         NX => NORM(1, KFACE, II)
         NY => NORM(2, KFACE, II)
         NZ => NORM(3, KFACE, II)
         SURF(KFACE, II) = SQRT(NX * NX + NY * NY + NZ * NZ)
         NX = -NX / SURF(KFACE, II)
         NY = -NY / SURF(KFACE, II)
         NZ = -NZ / SURF(KFACE, II) 
         XF(1:3) = TIERS * (X1 + X2 + X4)
         IF ((XF(1) - XC(1)) * NX + (XF(2) - XC(2)) * NY + (XF(3) - XC(3)) * NZ .LT. ZERO) THEN
            CALL ARRET(2)
         ENDIF
C     Face 3
         KFACE = 2
         NORM(1, KFACE, II) = UNDEMI * ((X2(2) - X4(2)) * (X3(3) - X4(3)) - 
     .        (X2(3) - X4(3)) * (X3(2) - X4(2)))
         NORM(2, KFACE, II) = UNDEMI * ((X2(3) - X4(3)) * (X3(1) - X4(1)) - 
     .        (X2(1) - X4(1)) * (X3(3) - X4(3)))
         NORM(3, KFACE, II) = UNDEMI * ((X2(1) - X4(1)) * (X3(2) - X4(2)) - 
     .        (X2(2) - X4(2)) * (X3(1) - X4(1)))
         NX => NORM(1, KFACE, II)
         NY => NORM(2, KFACE, II)
         NZ => NORM(3, KFACE, II)
         SURF(KFACE, II) = SQRT(NX * NX + NY * NY + NZ * NZ)
         NX = -NX / SURF(KFACE, II)
         NY = -NY / SURF(KFACE, II)
         NZ = -NZ / SURF(KFACE, II)
         XF(1:3) = TIERS * (X4 + X2 + X3)
         IF ((XF(1) - XC(1)) * NX + (XF(2) - XC(2)) * NY + (XF(3) - XC(3)) * NZ .LT. ZERO) THEN
            CALL ARRET(2)
         ENDIF
C     Face 4
         KFACE = 4
         NORM(1, KFACE, II) = UNDEMI * ((X3(2) - X4(2)) * (X1(3) - X4(3)) - 
     .        (X3(3) - X4(3)) * (X1(2) - X4(2)))
         NORM(2, KFACE, II) = UNDEMI * ((X3(3) - X4(3)) * (X1(1) - X4(1)) - 
     .        (X3(1) - X4(1)) * (X1(3) - X4(3)))
         NORM(3, KFACE, II) = UNDEMI * ((X3(1) - X4(1)) * (X1(2) - X4(2)) - 
     .        (X3(2) - X4(2)) * (X1(1) - X4(1)))
         NX => NORM(1, KFACE, II)
         NY => NORM(2, KFACE, II)
         NZ => NORM(3, KFACE, II)
         SURF(KFACE, II) = SQRT(NX * NX + NY * NY + NZ * NZ)
         NX = -NX / SURF(KFACE, II)
         NY = -NY / SURF(KFACE, II)
         NZ = -NZ / SURF(KFACE, II)
         XF(1:3) = TIERS * (X1 + X4 + X3)
         IF ((XF(1) - XC(1)) * NX + (XF(2) - XC(2)) * NY + (XF(3) - XC(3)) * NZ .LT. ZERO) THEN
            CALL ARRET(2)
         ENDIF
C     Face grid velocity 1
         WFAC(1:3, 5, II) = TIERS * (W1(1:3) + W2(1:3) + W3(1:3))
C     Face grid velocity 2
         WFAC(1:3, 6, II) = TIERS * (W1(1:3) + W4(1:3) + W2(1:3))
C     Face grid velocity 3
         WFAC(1:3, 2, II) = TIERS * (W2(1:3) + W4(1:3) + W3(1:3))
C     Face grid velocity 4
         WFAC(1:3, 4, II) = TIERS * (W1(1:3) + W3(1:3) + W4(1:3)) 
C     Nullify quantities for KFACE = 1, 3
         WFAC(1:3, 1, II) = ZERO
         WFAC(1:3, 3, II) = ZERO
         SURF(1, II) = ZERO
         SURF(3, II) = ZERO
         NORM(1:3, 1, II) = ZERO
         NORM(1:3, 3, II) = ZERO
      ENDDO
      END SUBROUTINE SNORM3T
