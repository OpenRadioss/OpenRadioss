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
Chd|  DIST_NODE_SEG3N               source/tools/sensor/dist_node_seg3n.F
Chd|-- called by -----------
Chd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE DIST_NODE_SEG3N (
     .             DIST,DMIN,DMAX,NOD_X,NOD_Y,NOD_Z,
     .             AX,AY,AZ,BX,BY,BZ,CX,CY,CZ)
c-----------------------------------------------------------------------
c Calculates distance between NODE(NOD_X,NOD_Y,NOD_Z) and a 3 node segment (A-B-C)
c returns distance to plane if node projection is inside the segment
c otherwise : check closest distance to edges and segment points
c-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real , INTENT(IN)  :: DMIN,DMAX,NOD_X,NOD_Y,NOD_Z,
     .                         AX,AY,AZ,BX,BY,BZ,CX,CY,CZ
      my_real , INTENT(OUT) :: DIST
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      my_real :: ABX,ABY,ABZ,ACX,ACY,ACZ,BCX,BCY,BCZ,VECX,VECY,VECZ,NX,NY,NZ,
     .   UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,QX,QY,QZ,QA_X,QA_Y,QA_Z,QB_X,QB_Y,QB_Z,
     .   QC_X,QC_Y,QC_Z,AREA,A1,A2,SX,SY,SZ,ALPHA,ALPHA2,D1,D2,D3,LEN,NORM
C=======================================================================
c     vect AB = P1-P2
c     vect AC = P1-P3
      ABX = BX - AX
      ABY = BY - AY
      ABZ = BZ - AZ
      ACX = CX - AX
      ACY = CY - AY
      ACZ = CZ - AZ
      BCX = CX - BX
      BCY = CY - BY
      BCZ = CZ - BZ
      UX = NOD_X - AX
      UY = NOD_Y - AY
      UZ = NOD_Z - AZ
c     normal to segment plane
      NX = ABY*ACZ - ABZ*ACY
      NY = ABZ*ACX - ABX*ACZ
      NZ = ABX*ACY - ABY*ACX
      NORM = SQRT(NX*NX + NY*NY + NZ*NZ)
c     distance to plane
      DIST = ABS(NX*UX + NY*UY + NZ*UZ) / NORM
      
      IF (DIST > DMAX .or. DIST < DMIN) RETURN  ! distance is outside of valid range
c---------------------------------------------------
c     Check if projection on segment plane is inside
c     Q = projection of NODE on segment plane using normal(NPX,NPY,NPZ)
c---------------------------------------------------
      QX = NOD_X - DIST*NX / NORM
      QY = NOD_Y - DIST*NY / NORM
      QZ = NOD_Z - DIST*NZ / NORM
      QA_X = AX - QX
      QA_Y = AY - QY
      QA_Z = AZ - QZ
      QB_X = BX - QX
      QB_Y = BY - QY
      QB_Z = BZ - QZ
      QC_X = CX - QX
      QC_Y = CY - QY
      QC_Z = CZ - QZ
c
c     Surfaces de sous-triangles
c
      SX = QA_Y * QB_Z - QA_Z * QB_Y
      SY = QA_Z * QB_X - QA_X * QB_Z
      SZ = QA_X * QB_Y - QA_Y * QB_X
      A1 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
      SX = QC_Y * QA_Z - QC_Z * QA_Y
      SY = QC_Z * QA_X - QC_X * QA_Z
      SZ = QC_X * QA_Y - QC_Y * QA_X
      A2 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
      AREA = HALF*NORM   ! total segment area
c
      IF (A1 + A2 > AREA) THEN
        ! Point projection on plane is outside the segment
        ! calculate distance to segment nodes
        D1 = SQRT(UX**2 + UY**2 + UZ**2)
        VX = NOD_X - BX
        VY = NOD_Y - BY
        VZ = NOD_Z - BZ
        D2   = SQRT(VX**2 + VY**2 + VZ**2)
        DIST = MIN(D1, D2)
        WX = NOD_X - CX
        WY = NOD_Y - CY
        WZ = NOD_Z - CZ
        D3   = SQRT(WX**2 + WY**2 + WZ**2)
        DIST = MIN(DIST, D2)
        ! calculate distance to edges
        LEN   = SQRT(ABX**2 + ABY**2 + ABZ**2)
        ALPHA = (UX*ABX + UY*ABY + UZ*ABZ) / LEN
        IF (ALPHA > 0 .and. ALPHA < ONE) THEN
          QX = AX + ALPHA * ABX
          QY = AY + ALPHA * ABY
          QZ = AZ + ALPHA * ABZ
          D1 = SQRT((NOD_X - QX)**2 + (NOD_Y - QY)**2 + (NOD_Z - QZ)**2)
          DIST = MIN(DIST, D1)
        END IF
c
        LEN   = SQRT(BCX**2 + BCY**2 + BCZ**2)
        ALPHA = (VX*BCX + VY*BCY + VZ*BCZ) / LEN
        IF (ALPHA > 0 .and. ALPHA < ONE) THEN
          QX = BX + ALPHA * BCX
          QY = BY + ALPHA * BCY
          QZ = BZ + ALPHA * BCZ
          D2 = SQRT((NOD_X - QX)**2 + (NOD_Y - QY)**2 + (NOD_Z - QZ)**2)
          DIST = MIN(DIST, D2)
        END IF
c
        LEN   = SQRT(ACX**2 + ACY**2 + ACZ**2)
        ALPHA = (UX*ACX + UY*ACY + UZ*ACZ) / LEN
        IF (ALPHA > 0 .and. ALPHA < ONE) THEN
          QX = AX + ALPHA * ACX
          QY = AY + ALPHA * ACY
          QZ = AZ + ALPHA * ACZ
          D3 = SQRT((NOD_X - QX)**2 + (NOD_Y - QY)**2 + (NOD_Z - QZ)**2)
          DIST = MIN(DIST, D3)
        END IF
c        
      END IF
c-----------
      RETURN
      END SUBROUTINE

