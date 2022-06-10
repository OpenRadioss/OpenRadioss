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
Chd|  DIST_NODE_SEG4N               source/tools/sensor/dist_node_seg4n.F
Chd|-- called by -----------
Chd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE DIST_NODE_SEG4N (
     .             DIST,DMIN,DMAX,NOD_X,NOD_Y,NOD_Z,
     .             AX,AY,AZ,BX,BY,BZ,CX,CY,CZ,DX,DY,DZ)
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
     .                         AX,AY,AZ,BX,BY,BZ,CX,CY,CZ,DX,DY,DZ
      my_real , INTENT(OUT) :: DIST
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: OK
      my_real :: ABX,ABY,ABZ,ACX,ACY,ACZ,BCX,BCY,BCZ,CDX,CDY,CDZ,DAX,DAY,DAZ,
     .   UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,QX,QY,QZ,QA_X,QA_Y,QA_Z,QB_X,QB_Y,QB_Z,
     .   QC_X,QC_Y,QC_Z,MX,MY,MZ,MPX,MPY,MPZ,
     .   AMX,AMY,AMZ,BMX,BMY,BMZ,CMX,CMY,CMZ,DMX,DMY,DMZ, 
     .   N1X,N1Y,N1Z,N2X,N2Y,N2Z,N3X,N3Y,N3Z,N4X,N4Y,N4Z,
     .   SX,SY,SZ,D,D1,D2,D3,D4,NORM1,A1,A2,NORM2,NORM3,NORM4
C=======================================================================
c     segment barycenter
      MX = FOURTH*(AX + BX + CX + DX)
      MY = FOURTH*(AY + BY + CY + DY)
      MZ = FOURTH*(AZ + BZ + CZ + DZ)
      MPX = NOD_X - MX
      MPY = NOD_Y - MY
      MPZ = NOD_Z - MZ
c     Divide 4N segment into 4 segments 3N with common node in barycenter (possible warping)
c     A-B-M, B-C-M,C-D-M, D-M-A
c     calculate distance to 4 planes defined by each segment
      ABX = BX - AX
      ABY = BY - AY
      ABZ = BZ - AZ
      AMX = MX - AX
      AMY = MY - AY
      AMZ = MZ - AZ
      N1X = AMY*ABZ - AMZ*ABY
      N1Y = AMZ*ABX - AMX*ABZ
      N1Z = AMX*ABY - AMY*ABX
      NORM1 = SQRT(N1X*N1X + N1Y*N1Y + N1Z*N1Z)
      D1    = ABS(N1X*MPX + N1Y*MPY + N1Z*MPZ) / NORM1
c      
      BCX = CX - BX
      BCY = CY - BY
      BCZ = CZ - BZ
      BMX = MX - BX
      BMY = MY - BY
      BMZ = MZ - BZ
      N2X = BMY*BCZ - BMZ*BCY
      N2Y = BMZ*BCX - BMX*BCZ
      N2Z = BMX*BCY - BMY*BCX
      NORM2 = SQRT(N2X*N2X + N2Y*N2Y + N2Z*N2Z)
      D2    = ABS(N2X*MPX + N2Y*MPY + N2Z*MPZ) / NORM2
c      
      CDX = DX - CX
      CDY = DY - CY
      CDZ = DZ - CZ
      CMX = MX - CX
      CMY = MY - CY
      CMZ = MZ - CZ
      N3X = CMY*CDZ - CMZ*CDY
      N3Y = CMZ*CDX - CMX*CDZ
      N3Z = CMX*CDY - CMY*CDX
      NORM3 = SQRT(N3X*N3X + N3Y*N3Y + N3Z*N3Z)
      D3    = ABS(N3X*MPX + N3Y*MPY + N3Z*MPZ) / NORM3
c      
      DAX = AX - DX
      DAY = AY - DY
      DAZ = AZ - DZ
      DMX = MX - DX
      DMY = MY - CY
      DMZ = MZ - CZ
      N4X = DMY*DAZ - DMZ*DAY
      N4Y = DMZ*DAX - DMX*DAZ
      N4Z = DMX*DAY - DMY*DAX
      NORM4 = SQRT(N4X*N4X + N4Y*N4Y + N4Z*N4Z)
      D4    = ABS(N4X*MPX + N4Y*MPY + N4Z*MPZ) / NORM4
c---------------------------------------------------     
      DIST = MIN(D1,D2)
      DIST = MIN(DIST,D3)
      DIST = MIN(DIST,D4)      
      IF (DIST > DMAX .or. DIST < DMIN) RETURN  ! distance is outside of valid range
c---------------------------------------------------
c     check if projection is inside sub-segments
c     Q = projection of NODE on segment plane using normal(NPX,NPY,NPZ)
c---------------------------------------------------
c     1st sub-segment :       
      QX = NOD_X - D1*N1X / NORM1
      QY = NOD_Y - D1*N1Y / NORM1
      QZ = NOD_Z - D1*N1Z / NORM1
c
      SX = (QY - MY) * (AZ - MZ) - (QZ - MZ) * (AY - MY)
      SY = (QZ - MZ) * (AX - MX) - (QX - MX) * (AZ - MZ)
      SZ = (QX - MX) * (AY - MY) - (QY - MY) * (AX - MX)
      A1 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
      SX = (BY - MY) * (QZ - MZ) - (BZ - MZ) * (QY - MY)
      SY = (BZ - MZ) * (QX - MX) - (BX - MX) * (QZ - MZ)
      SZ = (BX - MX) * (QY - MY) - (BY - MY) * (QX - MX)
      A2 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
      OK = 0
      IF (A1 + A2 < HALF * NORM1) THEN
        DIST = D1
        OK   = 1
      END IF
c      
      IF (OK == 0) THEN
c       Test 2nd sub-segment :       
        QX = NOD_X - D2*N2X / NORM2
        QY = NOD_Y - D2*N2Y / NORM2
        QZ = NOD_Z - D2*N2Z / NORM2
c
        SX = (QY - MY) * (BZ - MZ) - (QZ - MZ) * (BY - MY)
        SY = (QZ - MZ) * (BX - MX) - (QX - MX) * (BZ - MZ)
        SZ = (QX - MX) * (BY - MY) - (QY - MY) * (BX - MX)
        A1 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        SX = (CY - MY) * (QZ - MZ) - (CZ - MZ) * (QY - MY)
        SY = (CZ - MZ) * (QX - MX) - (CX - MX) * (QZ - MZ)
        SZ = (CX - MX) * (QY - MY) - (CY - MY) * (QX - MX)
        A2 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        IF (A1 + A2 < HALF * NORM2) THEN
          DIST = D2
          OK   = 1
        END IF
      END IF
      IF (OK == 0) THEN
c       Test 3rd sub-segment :       
        QX = NOD_X - D3*N3X / NORM3
        QY = NOD_Y - D3*N3Y / NORM3
        QZ = NOD_Z - D3*N3Z / NORM3
c
        SX = (QY - MY) * (CZ - MZ) - (QZ - MZ) * (CY - MY)
        SY = (QZ - MZ) * (CX - MX) - (QX - MX) * (CZ - MZ)
        SZ = (QX - MX) * (CY - MY) - (QY - MY) * (CX - MX)
        A1 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        SX = (DY - MY) * (QZ - MZ) - (DZ - MZ) * (QY - MY)
        SY = (DZ - MZ) * (QX - MX) - (DX - MX) * (QZ - MZ)
        SZ = (DX - MX) * (QY - MY) - (DY - MY) * (QX - MX)
        A2 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        IF (A1 + A2 < HALF * NORM3) THEN
          DIST = D3
          OK   = 1
        END IF
      END IF
      IF (OK == 0) THEN
c       Test 4rd sub-segment :       
        QX = NOD_X - D3*N3X / NORM4
        QY = NOD_Y - D3*N3Y / NORM4
        QZ = NOD_Z - D3*N3Z / NORM4
c
        SX = (QY - MY) * (DZ - MZ) - (QZ - MZ) * (DY - MY)
        SY = (QZ - MZ) * (DX - MX) - (QX - MX) * (DZ - MZ)
        SZ = (QX - MX) * (DY - MY) - (QY - MY) * (DX - MX)
        A1 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        SX = (AY - MY) * (QZ - MZ) - (AZ - MZ) * (QY - MY)
        SY = (AZ - MZ) * (QX - MX) - (AX - MX) * (QZ - MZ)
        SZ = (AX - MX) * (QY - MY) - (AY - MY) * (QX - MX)
        A2 = SQRT(SX*SX + SY*SY + SZ*SZ) * HALF
        IF (A1 + A2 < HALF * NORM4) THEN
          DIST = D3
          OK   = 1
        END IF
      END IF
c---------------------------------------------------
c     check distance to all nodes including barycenter
c---------------------------------------------------
      IF (OK == 0) THEN
        DIST  = SQRT(MPX**2 + MPY**2 + MPZ**2)
        D1 = SQRT((NOD_X - AX)**2 + (NOD_Y - AY)**2 + (NOD_Z - AZ)**2)
        D2 = SQRT((NOD_X - BX)**2 + (NOD_Y - BY)**2 + (NOD_Z - BZ)**2)
        D3 = SQRT((NOD_X - CX)**2 + (NOD_Y - CY)**2 + (NOD_Z - CZ)**2)
        D4 = SQRT((NOD_X - DX)**2 + (NOD_Y - DY)**2 + (NOD_Z - DZ)**2)
        DIST = MIN(DIST, D1)
        DIST = MIN(DIST, D2)
        DIST = MIN(DIST, D3)
        DIST = MIN(DIST, D4)
      END IF
c
c     we skip checking distance to external edges (for the moment)
c
c-----------
      RETURN
      END SUBROUTINE

