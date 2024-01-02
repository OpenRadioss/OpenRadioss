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
Chd|  LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|-- called by -----------
Chd|        LAW70_UPD                     source/materials/mat/mat070/law70_upd.F
Chd|-- calls ---------------
Chd|        FUNC2D_DEINTERSECT            source/materials/tools/func2d_deintersect.F
Chd|        TABLE_VALUES_2D               source/materials/tools/table_values_2d.F
Chd|        UNIFY_ABSCISSA_2D             source/materials/tools/unify_abscissas_2d.F
Chd|        VW_SMOOTH                     source/tools/curve/vw_smooth.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE LAW70_TABLE(TABLE  ,NFUNC  ,LEN     ,LMAX   ,RATE   ,
     .                       XI     ,YI     )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE TABLE4D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NFUNC
      INTEGER ,INTENT(INOUT) :: LMAX
      INTEGER ,DIMENSION(NFUNC)   :: LEN
      my_real ,DIMENSION(NFUNC) ,INTENT(IN) :: RATE
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN) :: XI
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN) :: YI
      TYPE(TABLE_4D_) ,INTENT(INOUT) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IPT,NPTX,IPOS,NDIM,FUNC_ID,IERROR,STAT
      INTEGER ,DIMENSION(NFUNC) :: PERM
      INTEGER ,PARAMETER :: NPTMAX = 100   ! max number of function points
      my_real :: X1,X2,Y1,Y2,DERI
      my_real ,DIMENSION(:)   ,ALLOCATABLE :: XF
      my_real ,DIMENSION(:,:) ,ALLOCATABLE :: YF
C=======================================================================
c     create X,Y vectors for all curves before unifying all abscissas
c--------------------------------------------------------
      NPTX = 0
      DO I = 1,NFUNC
        NPTX = NPTX + LEN(I)
      END DO
      ALLOCATE (XF(NPTX))
c
      IF (NFUNC == 1) THEN
        ALLOCATE (YF(NPTX,NFUNC))
        XF(1:NPTX)   = XI(1:NPTX,1)
        YF(1:NPTX,1) = YI(1:NPTX,1)
c
      ELSE
c--------------------------------------------------------
c       unify abscissas
c--------------------------------------------------------
c
        CALL UNIFY_ABSCISSA_2D(NFUNC,LEN,LMAX,NPTX ,XI  ,XF  )
c
        ALLOCATE (YF(NPTX,NFUNC))
        DO I = 1,NFUNC
          CALL TABLE_VALUES_2D(LEN(I) ,NPTX ,XI(1,I) ,YI(1,I) ,XF ,YF(1,I) )
        END DO      
        LEN(1:NFUNC) = NPTX
c
c--------------------------------------------------------
c       check and correct intersections
c--------------------------------------------------------
c
        CALL FUNC2D_DEINTERSECT(NPTX, NFUNC  ,YF   )
c
      END IF
c--------------------------------------------------------
c     d) check and correct monotonicity
c--------------------------------------------------------
      DO I = 1,NFUNC
        DO IPT = 2,NPTX
          IF (YF(IPT,I) < YF(IPT-1,I)) YF(IPT,I) = YF(IPT-1,I)
        END DO
      END DO
c--------------------------------------------------------
c     second reduction of number of points of the 1st curve
c     and reinterpolate all table functions based on its abscissa distribution
c--------------------------------------------------------
      IF (NPTX > NPTMAX) THEN
        CALL VW_SMOOTH(NPTX,NPTMAX,XF,YF(1:NPTX,1))
c
        DO I = 2,NFUNC
          CALL TABLE_VALUES_2D(LEN(I) ,NPTX ,XI(1,I) ,YI(1,I) ,XF ,YF(1,I) )
        END DO      
      END IF
c--------------------------------------------------------------------------
c     e) create 2D function table
c--------------------------------------------------------
      NDIM = MIN(2,NFUNC)
      TABLE%NDIM  = NDIM
      ALLOCATE (TABLE%X(NDIM)              ,STAT=stat)            
      ALLOCATE (TABLE%X(1)%VALUES(NPTX)     ,STAT=stat)      
      IF (NFUNC == 1) THEN
        ALLOCATE (TABLE%Y1D(NPTX)           ,STAT=stat)
        TABLE%X(1)%VALUES(1:NPTX)   = XF(1:NPTX)   
        TABLE%Y1D(1:NPTX) = YF(1:NPTX,1)
      ELSE
        ALLOCATE (TABLE%X(2)%VALUES(NFUNC) ,STAT=stat)
        ALLOCATE (TABLE%Y2D(NPTX,NFUNC)     ,STAT=stat)
        TABLE%X(1)%VALUES(1:NPTX)   = XF(1:NPTX)   
        TABLE%X(2)%VALUES(1:NFUNC) = RATE(1:NFUNC)
        DO I = 1,NFUNC
          TABLE%Y2D(1:NPTX,I) = YF(1:NPTX,I)
        END DO
      END IF      
c
c--------------------
      DEALLOCATE (XF)
      DEALLOCATE (YF)
c--------------------
      RETURN
      END
