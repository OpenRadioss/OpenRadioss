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
C
Chd|====================================================================
Chd|  SPMD_TRI22VOX0                source/mpi/interfaces/spmd_tri22vox0.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI22VOX0(
     1   X      ,BMINMA_FLU_L ,NBRIC  , IXS   , BUFBRIC, 
     2   ITASK  ,ITAB         ,XMINS  , YMINS , ZMINS  ,
     3   XMAXS  ,YMAXS        ,ZMAXS  , BMINMA_LAG_R ,IS_CONTACT,
     4   BMINMA_AND ) 
C-----------------------------------------------
C   D e s  c r i p t i o n
C-----------------------------------------------
C This subroutine is marking SPMD voxel if brick is inside
C lagrangian domain (from local domain)
C X       : Node coordinates
C BMINMAL : Fluid Bounds for local domain
C IXS     : Brick node ids
C BUFBRIC : Brick Group for interface for current domain
C ITASK   : Current Thread
C CRVOXEL : SPMD Voxel
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, intent(in) ::   NBRIC, IXS(NIXS,*), ITAB(*)
      my_real, intent(in) ::
     .                         X(3,*), 
     .                         BMINMA_FLU_L(6), BMINMA_LAG_R(6)
      my_real, intent(inout) ::
     .                         BMINMA_AND(6)
      my_real
     .        XMAXS(NBRIC), YMAXS(NBRIC), ZMAXS(NBRIC),
     .        XMINS(NBRIC), YMINS(NBRIC), ZMINS(NBRIC)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,IB,NE,I,
     .        NBX,NBY,NBZ,NEDG,M1,M2,M3,M4,M5,M6,M7,M8,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ,
     .        BUFBRIC(NBRIC),NBF,NBL,ITASK
      my_real
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,XX5, XX6, XX7, XX8,
     .        YY1,YY2,YY3,YY4,YY5, YY6, YY7, YY8,
     .        ZZ1,ZZ2,ZZ3,ZZ4,ZZ5, ZZ6, ZZ7, ZZ8,
     .        DX,DY,DZ
      LOGICAL :: IS_CONTACT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      ! Pour chercher les facettes candidats distante,
      ! On regarde dans l intersection du domaine fluide local avec les domaines lagrangien remote
      ! Cest ce domaine que l on voxelise pour rechercher les facettes remotes
      XMAXB = MIN ( BMINMA_FLU_L(1), BMINMA_LAG_R(1) )
      YMAXB = MIN ( BMINMA_FLU_L(2), BMINMA_LAG_R(2) )
      ZMAXB = MIN ( BMINMA_FLU_L(3), BMINMA_LAG_R(3) )
      XMINB = MAX ( BMINMA_FLU_L(4), BMINMA_LAG_R(4) )
      YMINB = MAX ( BMINMA_FLU_L(5), BMINMA_LAG_R(5) )
      ZMINB = MAX ( BMINMA_FLU_L(6), BMINMA_LAG_R(6) )

      DX = XMAXB-XMINB
      DY = YMAXB-YMINB
      DZ = ZMAXB-ZMINB
 
      !test interection entre domaines (FLUIDE LOCAL / LAG DISTANTS)    
      IF ( (DX<=ZERO).OR.(DY<=ZERO).OR.(DZ<=ZERO) ) THEN
        IS_CONTACT = .FALSE.
        RETURN
      ELSE
        IS_CONTACT = .TRUE.
      END IF

      BMINMA_AND(1)=XMAXB
      BMINMA_AND(2)=YMAXB
      BMINMA_AND(3)=ZMAXB
      BMINMA_AND(4)=XMINB
      BMINMA_AND(5)=YMINB
      BMINMA_AND(6)=ZMINB

      NBF = 1+ITASK*NBRIC/NTHREAD
      NBL = (ITASK+1)*NBRIC/NTHREAD

      DO IB=NBF,NBL
         !-------------------------------------------!
         !  VOXEL OCCUPIED BY THE BRICK              !
         !-------------------------------------------!
         !Voxel_lower_left_bound for this element---+
         IX1=INT(NBX*(XMINS(IB)-XMINB)/DX)
         IY1=INT(NBY*(YMINS(IB)-YMINB)/DY)
         IZ1=INT(NBZ*(ZMINS(IB)-ZMINB)/DZ)
         IX1=MAX(IX1,0) !pos for ibset must be correctly defined
         IY1=MAX(IY1,0) !CRVOXEL (0:LRVOXEL,...
         IZ1=MAX(IZ1,0) !CRVOXEL (0:LRVOXEL,...
         !Voxel_upper_right_bound for this element---+
         IX2=INT(NBX*(XMAXS(IB)-XMINB)/DX)
         IY2=INT(NBY*(YMAXS(IB)-YMINB)/DY)
         IZ2=INT(NBZ*(ZMAXS(IB)-ZMINB)/DZ)
         IX2=MIN(IX2,NBX) !pos for ibset must be correctly defined
         IY2=MIN(IY2,NBY) !CRVOXEL (0:LRVOXEL,...
         IZ2=MIN(IZ2,NBZ) !CRVOXEL (0:LRVOXEL,...
         !------------------------------------------------------!
         ! VOXEL MARKING TO SPOT BRIC LOCATIONS (LOCAL DOMAIN)  !
         !------------------------------------------------------!
#include "lockon.inc"
         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             DO IX = IX1, IX2
               CRVOXEL(IY,IZ,LOC_PROC)=IBSET(CRVOXEL(IY,IZ,LOC_PROC),IX)
             END DO
           END DO
         END DO
#include "lockoff.inc"     
      ENDDO !IB=NBF,NBL

C
      RETURN
      END
