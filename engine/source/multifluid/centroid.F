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
Chd|  CENTROID3                     source/multifluid/centroid.F  
Chd|-- called by -----------
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CENTROID3(NEL, LENGTH, NFT, IXS, XGRID, 
     .     ELEM_CENTROID, FACE_CENTROID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Computes the centroids of the elements
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
      INTEGER, INTENT(IN) :: NEL, NFT, IXS(NIXS, *), LENGTH
      my_real, INTENT(IN) :: XGRID(3, *)
      my_real, INTENT(OUT) :: ELEM_CENTROID(3, NEL), FACE_CENTROID(3, 6, NEL)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8
      my_real
     .     X1(3), X2(3), X3(3), X4(3), X5(3), X6(3), X7(3), X8(3)
      DO II = 1, NEL
         I = II + NFT
C     Nodes of the element locally stored in NODE* for memory access
         NODE1 = IXS(2, I)
         NODE2 = IXS(3, I)
         NODE3 = IXS(4, I)
         NODE4 = IXS(5, I)
         NODE5 = IXS(6, I)
         NODE6 = IXS(7, I)
         NODE7 = IXS(8, I)
         NODE8 = IXS(9, I)  
C     Node coordinates
         X1(1:3) = XGRID(1:3, NODE1)
         X2(1:3) = XGRID(1:3, NODE2)
         X3(1:3) = XGRID(1:3, NODE3)
         X4(1:3) = XGRID(1:3, NODE4)
         X5(1:3) = XGRID(1:3, NODE5)
         X6(1:3) = XGRID(1:3, NODE6)
         X7(1:3) = XGRID(1:3, NODE7)
         X8(1:3) = XGRID(1:3, NODE8)
C     Centroid
         ELEM_CENTROID(1:3, II) = ONE_OVER_8 * (X1(1:3) + X2(1:3) + X3(1:3) + X4(1:3) + 
     .        X5(1:3) + X6(1:3) + X7(1:3) + X8(1:3))
C     Face centroid
C     Face 1 
         FACE_CENTROID(1:3, 1, II) = FOURTH * (X1(1:3) + X2(1:3) + X3(1:3) + X4(1:3))
C     Face 2 
         FACE_CENTROID(1:3, 2, II) = FOURTH * (X3(1:3) + X4(1:3) + X8(1:3) + X7(1:3))
C     Face 3 
         FACE_CENTROID(1:3, 3, II) = FOURTH * (X5(1:3) + X6(1:3) + X7(1:3) + X8(1:3))
C     Face 4 
         FACE_CENTROID(1:3, 4, II) = FOURTH * (X1(1:3) + X2(1:3) + X6(1:3) + X5(1:3))
C     Face 5 
         FACE_CENTROID(1:3, 5, II) = FOURTH * (X2(1:3) + X3(1:3) + X7(1:3) + X6(1:3))
C     Face 6 
         FACE_CENTROID(1:3, 6, II) = FOURTH * (X1(1:3) + X4(1:3) + X8(1:3) + X5(1:3))
      ENDDO
      END SUBROUTINE CENTROID3

Chd|====================================================================
Chd|  CENTROID3T                    source/multifluid/centroid.F  
Chd|-- called by -----------
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|-- calls ---------------
Chd|        GEOM                          source/ale/alemuscl/geom.F    
Chd|====================================================================
      SUBROUTINE CENTROID3T(NEL, LENGTH, NFT, IXS, XGRID, 
     .     ELEM_CENTROID, FACE_CENTROID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Computes the centroids of the elements
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
      INTEGER, INTENT(IN) :: NEL, NFT, IXS(NIXS, *), LENGTH
      my_real, INTENT(IN) :: XGRID(3, *)
      my_real, INTENT(OUT) :: ELEM_CENTROID(3, NEL), FACE_CENTROID(3, 6, NEL)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8
      my_real
     .     X1(3), X2(3), X3(3), X4(3), VOL
      my_real 
     .     A(3), B(3), C(3)
      DO II = 1, NEL
         I = II + NFT
C     Nodes of the element locally stored in NODE* for memory access
         NODE1 = IXS(2, I)
         NODE2 = IXS(4, I)
         NODE3 = IXS(7, I)
         NODE4 = IXS(6, I)
C     Node coordinates
         X1(1:3) = XGRID(1:3, NODE1)
         X2(1:3) = XGRID(1:3, NODE2)
         X3(1:3) = XGRID(1:3, NODE3)
         X4(1:3) = XGRID(1:3, NODE4)
         ELEM_CENTROID(1:3, II) = ZERO
C     Centroid
         VOl = ZERO
         A(1:3) = X1(1:3)
         B(1:3) = X2(1:3)
         C(1:3) = X4(1:3)
         CALL GEOM(A, B, C, ELEM_CENTROID(1:3, II), VOL)

         A(1:3) = X1(1:3)
         B(1:3) = X3(1:3)
         C(1:3) = X2(1:3)
         CALL GEOM(A, B, C, ELEM_CENTROID(1:3, II), VOL)

         A(1:3) = X1(1:3)
         B(1:3) = X4(1:3)
         C(1:3) = X3(1:3)
         CALL GEOM(A, B, C, ELEM_CENTROID(1:3, II), VOL)

         A(1:3) = X2(1:3)
         B(1:3) = X3(1:3)
         C(1:3) = X4(1:3)
         CALL GEOM(A, B, C, ELEM_CENTROID(1:3, II), VOL)
         
         VOL = VOL / 6.D0
         ELEM_CENTROID(1:3, II) = ELEM_CENTROID(1:3, II) / 12.D0 / VOL

         !ELEM_CENTROID(1:3, II) = FOURTH * (X1(1:3) + X2(1:3) + X3(1:3) + X4(1:3))
C     Face centroid
C     Face 1 
         FACE_CENTROID(1:3, 5, II) = THIRD * (X1(1:3) + X2(1:3) + X3(1:3))
C     Face 2 
         FACE_CENTROID(1:3, 6, II) = THIRD * (X1(1:3) + X2(1:3) + X4(1:3))
C     Face 3 
         FACE_CENTROID(1:3, 2, II) = THIRD * (X2(1:3) + X3(1:3) + X4(1:3))
C     Face 4 
         FACE_CENTROID(1:3, 4, II) = THIRD * (X1(1:3) + X3(1:3) + X4(1:3))
      ENDDO
      END SUBROUTINE CENTROID3T

Chd|====================================================================
Chd|  CENTROID2                     source/multifluid/centroid.F  
Chd|-- called by -----------
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CENTROID2(NEL, LENGTH, NFT, IXQ, XGRID, 
     .     ELEM_CENTROID, FACE_CENTROID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Computes the centroids of the elements
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
      INTEGER, INTENT(IN) :: NEL, NFT, IXQ(NIXQ, *), LENGTH
      my_real, INTENT(IN) :: XGRID(3, *)
      my_real, INTENT(OUT) :: ELEM_CENTROID(3, NEL), FACE_CENTROID(3, 6, NEL)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, NODE1, NODE2, NODE3, NODE4
      my_real
     .     X1(3), X2(3), X3(3), X4(3)
      DO II = 1, NEL
         I = II + NFT
C     Nodes of the element locally stored in NODE* for memory access
         NODE1 = IXQ(2, I)
         NODE2 = IXQ(3, I)
         NODE3 = IXQ(4, I)
         NODE4 = IXQ(5, I)
C     Node coordinates
         X1(1:3) = XGRID(1:3, NODE1)
         X2(1:3) = XGRID(1:3, NODE2)
         X3(1:3) = XGRID(1:3, NODE3)
         X4(1:3) = XGRID(1:3, NODE4)
C     Centroid
         ELEM_CENTROID(1:3, II) = FOURTH * (X1(1:3) + X2(1:3) + X3(1:3) + X4(1:3))
C     Face centroid
C     Face 1 
         FACE_CENTROID(1:3, 1, II) = HALF * (X1(1:3) + X2(1:3))
C     Face 2 
         FACE_CENTROID(1:3, 2, II) = HALF * (X2(1:3) + X3(1:3))
C     Face 3 
         FACE_CENTROID(1:3, 3, II) = HALF * (X3(1:3) + X4(1:3))
C     Face 4 
         FACE_CENTROID(1:3, 4, II) = HALF * (X1(1:3) + X4(1:3))

      ENDDO
      END SUBROUTINE CENTROID2

Chd|====================================================================
Chd|  CENTROID2T                    source/multifluid/centroid.F  
Chd|-- called by -----------
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CENTROID2T(NEL, LENGTH, NFT, IXTG, XGRID, 
     .     ELEM_CENTROID, FACE_CENTROID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Computes the centroids of the elements
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
      INTEGER, INTENT(IN) :: NEL, NFT, IXTG(NIXTG, *), LENGTH
      my_real, INTENT(IN) :: XGRID(3, *)
      my_real, INTENT(OUT) :: ELEM_CENTROID(3, NEL), FACE_CENTROID(3, 6, NEL)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8
      my_real
     .     X1(3), X2(3), X3(3), X4(3), X5(3), X6(3), X7(3), X8(3)
      DO II = 1, NEL
         I = II + NFT
C     Nodes of the element locally stored in NODE* for memory access
         NODE1 = IXTG(2, I)
         NODE2 = IXTG(3, I)
         NODE3 = IXTG(4, I)
C     Node coordinates
         X1(1:3) = XGRID(1:3, NODE1)
         X2(1:3) = XGRID(1:3, NODE2)
         X3(1:3) = XGRID(1:3, NODE3)
C     Centroid
         ELEM_CENTROID(1:3, II) = THIRD * (X1(1:3) + X2(1:3) + X3(1:3))
C     Face centroid
C     Face 1 
         FACE_CENTROID(1:3, 1, II) = HALF * (X1(1:3) + X2(1:3))
C     Face 2 
         FACE_CENTROID(1:3, 2, II) = HALF * (X2(1:3) + X3(1:3))
C     Face 3 
         FACE_CENTROID(1:3, 3, II) = HALF * (X3(1:3) + X1(1:3))

      ENDDO
      END SUBROUTINE CENTROID2T
