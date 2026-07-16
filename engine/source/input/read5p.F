Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    read5p           ../engine/source/input/read5p.F
!||--- called by ------------------------------------------------------
!||    rdele            ../engine/source/input/rdele.F
!||--- calls      -----------------------------------------------------
!||    wciusc2          ../engine/source/input/wciusc2.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE READ5P(IREC,NBC,KEY0,IV2,JJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IV2(*), IREC, NBC, JJ
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, I, N, J, NC1, NC2, NTOT
      INTEGER, DIMENSION(:), ALLOCATABLE :: IV0,IV1
C-----------------------------------------------
C
C     First pass - count total number of elements
c
      NTOT=0
      K=0
      DO I=1,NBC
        CALL WCIUSC2(IREC+K,1,N,KEY0)
        K=K+1
        NTOT = NTOT + N
      ENDDO ! I

      IF(NTOT==0)RETURN ! No elements to read

C     Second pass - read all lines of elements as a single big line
      call my_alloc(IV0, NTOT, "IV0")
      call my_alloc(IV1, 2*NTOT, "IV1")
      IV0(1:NTOT) = 0
      IV1(1:2*NTOT) = 0
C
      NTOT = 0
      K=0
      DO I=1,NBC
        CALL WCIUSC2(IREC+K,1,N,KEY0)
        READ(IUSC2,*)(IV0(J),J=1,N)
        DO J=1,N
           IV1(2*(NTOT+J) - 1) = IV0(J)
           IV1(2*(NTOT+J))     = IV0(J)
        ENDDO
        NTOT = NTOT + N
        K=K+1
      ENDDO  
C      
C     Write the elements in groups of 10 - last line completed with last element
      N=2*NTOT
      NC1=1
      NC2 = 0
      DO WHILE(NC2 < N)
        JJ = 0
        NC2=MIN(NC1+9-JJ,N)
        DO J=NC1,NC2
          JJ=JJ+1
          IV2(JJ)=IV1(J)
        ENDDO
        DO K=JJ+1,10,2
          IV2(K)=IV2(JJ-1)
          IV2(K+1)=IV2(JJ)
        ENDDO
        WRITE(IIN,'(10I10)')(IV2(J),J=1,10)
        NC1=NC2+1
      ENDDO
      JJ = 0

      call my_dealloc(IV0)
      call my_dealloc(IV1)

      RETURN
      END
