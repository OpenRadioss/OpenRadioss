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
!||    w_avuply   ../starter/source/restart/ddsplit/w_avuply.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_AVUPLY( NNODPXFE_L,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NNODPXFE_L,LEN_AM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      my_real
     .  VELTMP1(3,NNODPXFE_L),VELTMP2(4,NNODPXFE_L)
C
      
         DO J = 1, NNODPXFE_L
            VELTMP1(1,J)= ZERO
            VELTMP1(2,J)= ZERO
            VELTMP1(3,J)= ZERO 
            VELTMP2(1,J) = ZERO
            VELTMP2(2,J) = ZERO
            VELTMP2(3,J) = ZERO
            VELTMP2(4,J) = ZERO
         ENDDO        
         DO J = 1, NPLYMAX
C deplacement
            CALL WRITE_DB(VELTMP1,3*NNODPXFE_L)
               LEN_AM = LEN_AM + 3*NNODPXFE_L
C vitesse
            CALL WRITE_DB(VELTMP1,3*NNODPXFE_L)
               LEN_AM = LEN_AM + 3*NNODPXFE_L
cc            CALL WRITE_DB(VELTMP2,4*NNODPXFE_L)
cc               LEN_AM = LEN_AM + 4*NNODPXFE_L 
         ENDDO        
C
      RETURN
      END
