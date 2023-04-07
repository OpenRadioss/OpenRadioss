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
Chd|  SPLOAD3                       source/elements/sph/spload3.F 
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPLOAD3(
     1   SPBUF,   WA,      RHOA,    RHON,
     2   DELTAX,  OFFG,    DXX,     DXY,
     3   DXZ,     DYX,     DYY,     DYZ,
     4   DZX,     DZY,     DZZ,     MUMAX,
     5   LFT,     LLT,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: NFT
      my_real
     .   SPBUF(NSPBUF,*) ,WA(KWASPH,*)  ,
     .   RHOA(*) ,RHON(*) ,DELTAX(*) ,OFFG(*) ,
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),
     .   MUMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I
C     REAL
      my_real
     .        OFF_L
C-----------------------------------------------
      DO 100  I=LFT,LLT
         N=NFT+I
         RHON(I)  =SPBUF(2,N)
 100  CONTINUE
C
      OFF_L = ZERO
      DO I=LFT,LLT
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
C-----
      IF(OFF_L>=ZERO)THEN
      DO 200  I=LFT,LLT
         N=NFT+I
         DXX(I)   =WA(1,N)
         DYY(I)   =WA(2,N)
         DZZ(I)   =WA(3,N)
         DXY(I)   =WA(4,N)
         DYZ(I)   =WA(5,N)
         DXZ(I)   =WA(6,N)
         DYX(I)   =WA(7,N)
         DZY(I)   =WA(8,N)
         DZX(I)   =WA(9,N)
         RHOA(I)  =WA(10,N)
C        for stability time step computation:
         DELTAX(I)=WA(11,N)
         MUMAX(I) =WA(12,N)
 200  CONTINUE
      ELSE
      DO 300  I=LFT,LLT
       N=NFT+I
       RHOA(I)  =WA(10,N)
       IF(OFFG(I)>=ZERO)THEN
         DXX(I)   =WA(1,N)
         DYY(I)   =WA(2,N)
         DZZ(I)   =WA(3,N)
         DXY(I)   =WA(4,N)
         DYZ(I)   =WA(5,N)
         DXZ(I)   =WA(6,N)
         DYX(I)   =WA(7,N)
         DZY(I)   =WA(8,N)
         DZX(I)   =WA(9,N)
C        for stability time step computation:
         DELTAX(I)=WA(11,N)
         MUMAX(I) =WA(12,N)
       ELSE
         DXX(I)   =ZERO
         DYY(I)   =ZERO
         DZZ(I)   =ZERO
         DXY(I)   =ZERO
         DYZ(I)   =ZERO
         DXZ(I)   =ZERO
         DYX(I)   =ZERO
         DZY(I)   =ZERO
         DZX(I)   =ZERO	 
C        for stability time step computation:
         DELTAX(I)=EP20
         MUMAX(I) =ZERO
       ENDIF
 300  CONTINUE
      ENDIF
C----------------------------------
      RETURN
      END
