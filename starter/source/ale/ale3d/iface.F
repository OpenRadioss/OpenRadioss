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
!||    iface       ../starter/source/ale/ale3d/iface.F
!||--- called by ------------------------------------------------------
!||    boxassem4   ../starter/source/model/box/bigbox.F
!||    c_iebcs     ../starter/source/restart/ddsplit/c_iebcs.F
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||    findele     ../starter/source/boundary_conditions/ebcs/findele.F
!||    fvbric01    ../starter/source/airbag/fvbric01.F
!||    i12sol3     ../starter/source/interfaces/inter3d1/insol3.F
!||    iniebcs     ../starter/source/boundary_conditions/ebcs/iniebcs.F
!||====================================================================
      INTEGER FUNCTION IFACE(IP,N)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This function is searching for face identifier
C  3D case (8 nodes)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IP(*), N(4)
C-----------------------------------------------
C     S o u r c e   L i n e s
C----------------------------------------------- 
      IF(IP(N(1)) > 0)THEN
       IF(IP(N(2)) > 0)THEN
        IF(IP(N(3)) > 0)THEN
         IFACE=1
        ELSE
         IFACE=4
        ENDIF
       ELSE
         IFACE=6
       ENDIF
      ELSE
       IF(IP(N(3)) > 0)THEN
        IF(IP(N(4)) > 0)THEN
         IFACE=2
        ELSE
         IFACE=5
        ENDIF
       ELSE
         IFACE=3
       ENDIF
      ENDIF
      RETURN
      END

!||====================================================================
!||    iface2                   ../starter/source/ale/ale3d/iface.F
!||--- called by ------------------------------------------------------
!||    c_ixfloc                 ../starter/source/restart/ddsplit/c_ixfloc.F
!||    ddsplit                  ../starter/source/restart/ddsplit/ddsplit.F
!||    findele                  ../starter/source/boundary_conditions/ebcs/findele.F
!||    multi_unplug_neighbors   ../starter/source/multifluid/multi_unplug_neighbors.F
!||====================================================================
      INTEGER FUNCTION IFACE2(IP, N) 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This function is searching for face identifier
C  2D case (4 nodes : QUADS)  
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  IP(*), N(2)      
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------      
      IF (IP(N(1))  >  0) THEN
         IF (IP(N(2))  >  0) THEN
            IFACE2 = 1
         ELSE
            IFACE2 = 4
         ENDIF
      ELSE IF (IP(N(2))  >  0) THEN
         IFACE2 = 2
      ELSE
         IFACE2 = 3
      ENDIF
      END 

!||====================================================================
!||    iface2t   ../starter/source/ale/ale3d/iface.F
!||--- called by ------------------------------------------------------
!||    findele   ../starter/source/boundary_conditions/ebcs/findele.F
!||====================================================================
      INTEGER FUNCTION IFACE2T(IP, N) 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This function is searching for face identifier
C  2D case (3 nodes : TRIANGLES)
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  IP(*), N(3)      
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------       
      IFACE2T = 0
      IF (IP(N(1))  >  0) THEN
         IF (IP(N(2))  >  0) THEN
            IFACE2T = 1
         ELSE
            IFACE2T = 3
         ENDIF
      ELSE IF (IP(N(3))  >  0) THEN
         IFACE2T = 2
      ENDIF
      END 
