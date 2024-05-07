Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  UNIFY_ABSCISSA_2D             source/materials/tools/unify_abscissas_2d.F
Chd|-- called by -----------
Chd|        LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|====================================================================
       SUBROUTINE UNIFY_ABSCISSA_2D(NFUNC,LEN,LMAX,NPT,XI,XF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      ! create abscissa vector common to all input functions, in ascending order
      ! and calculate new interpolated values of all functions
c-------------------------------------------------------------
      ! XI : INPUT   - initial X coordinates of all functions
      ! XF : OUTPUT  - common abscissa coordinates
      ! NPT : Input  => Sum of all function point numbers
      !       Output => Final length of common abscissa vector
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)                         :: NFUNC ! number of input functions
      INTEGER ,INTENT(IN)                         :: LMAX  ! max X length in input
      INTEGER ,INTENT(INOUT)                      :: NPT   ! length of X vector
      INTEGER ,DIMENSION(NFUNC)      ,INTENT(IN)  :: LEN   ! NPTS by function
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN)  :: XI    ! initial abscissa coordinates
      my_real ,DIMENSION(NPT)        ,INTENT(OUT) :: XF    ! output abscissa vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,LTOT,LL,IDX,IERROR
      my_real :: X1,X2,XC,Y1,Y2,DX,DX1,DERI
      my_real ,DIMENSION(:) ,ALLOCATABLE :: XTMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERM
c=======================================================================
      LTOT = NPT
      ALLOCATE (PERM(LTOT))
      ALLOCATE (XTMP(LTOT))
      XF(:)   = ZERO
      XTMP(:) = ZERO
      IDX = 0
      DO I = 1,NFUNC
        LL = LEN(I)
        XTMP(IDX+1:IDX+LL) = XI(1:LL,I)
        IDX = IDX + LL
      END DO
c     
      CALL MYQSORT(LTOT,XTMP,PERM,IERROR)
c
c     create common abscissa vector XF
c
      NPT = 1
      XF(1) = XTMP(1)
      DO I = 2,LTOT
        IF (XTMP(I) > XF(NPT)) THEN
          NPT = NPT + 1
          XF(NPT) = XTMP(I)
        END IF
      END DO
c
      DEALLOCATE (XTMP)
      DEALLOCATE (PERM)
c-----------
      RETURN
      END SUBROUTINE UNIFY_ABSCISSA_2D
