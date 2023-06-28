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
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE UNIFY_ABSCISSA_2D(NFUNC,LMAX,NPT,LEN,XI,YI,XF,YF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      ! create abscissa vector common to all input functions, in ascending order
      ! and calculate new interpolated values of all functions
c-------------------------------------------------------------
      ! XI : INPUT   - initial X coordinates of all functions
      ! YI : INPUT   - initial Y values of all functions
      ! XF : OUTPUT  - common abscissa coordinates
      ! YF : OUTPUT  - interpolated values of all function in common abscissa points
      ! NPT : Input  => Sum of all function point numbers
      !       Output => Final length of common abscissa vector
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)                          :: NFUNC ! number of input functions
      INTEGER ,INTENT(IN)                          :: LMAX  ! max abscissa dimension
      INTEGER ,INTENT(OUT)                         :: NPT   ! length of abscissa vectors
      INTEGER ,DIMENSION(NFUNC)     ,INTENT(INOUT) :: LEN   ! NPTS by function
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN)   :: XI    ! initial abscissa coordinates
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN)   :: YI    ! input function values
      my_real ,DIMENSION(LMAX)       ,INTENT(OUT)  :: XF    ! output abscissa vector
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(OUT)  :: YF    ! output function values
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IPT,IPOS,LL,LTOT,WORK_SIZE,IERROR
      my_real :: X1,X2,XC,Y1,Y2,DX,DX1,DERI
      my_real ,DIMENSION(:) ,ALLOCATABLE :: XTMP,PERM
c=======================================================================
      WORK_SIZE = NFUNC * LMAX
      ALLOCATE (PERM(WORK_SIZE))
      ALLOCATE (XTMP(WORK_SIZE))
      XF(:)   = ZERO
      XTMP(:) = ZERO
      LTOT = 0
      DO I = 1,NFUNC
        LL = LEN(I)
        XTMP(LTOT+1:LTOT+LL) = XI(1:LL,I)
        LTOT = LTOT + LL
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
      DO I = 1,NFUNC
        IPOS = 1
        YF(1,I)= YI(1,I)
        DO J = 2,NPT-1
          IF (XF(J) >= XI(IPOS+1,I)) IPOS = IPOS + 1
          X1 = XI(IPOS,I)
          Y1 = YI(IPOS,I)
          X2 = XI(IPOS+1,I)
          Y2 = YI(IPOS+1,I)
          DX = X2 - XF(J)
          DERI  = (Y2 - Y1) / (X2 - X1)
          YF(J,I) = Y1 + DERI * (XF(J) - X1)
        END DO
        YF(NPT,I) = Y2
        LEN(I) = NPT
      END DO      
c
      DEALLOCATE (XTMP)
      DEALLOCATE (PERM)
c-----------
      RETURN
      END SUBROUTINE UNIFY_ABSCISSA_2D
