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
Chd|  LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|-- called by -----------
Chd|        LAW70_UPD                     source/materials/mat/mat070/law70_upd.F
Chd|-- calls ---------------
Chd|        FUNC2D_DEINTERSECT            source/materials/tools/func2d_deintersect.F
Chd|        UNIFY_ABSCISSA_2D             source/materials/tools/unify_abscissas_2d.F
Chd|        VW_SMOOTH                     source/tools/curve/vw_smooth.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE LAW70_TABLE(TABLE  ,LMAX   ,NFUNC  ,LEN     ,RATE  ,
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
      INTEGER  ,INTENT(IN) :: NFUNC
      INTEGER  ,INTENT(INOUT) :: LMAX
      INTEGER ,DIMENSION(NFUNC)   :: LEN
      my_real ,DIMENSION(NFUNC) ,INTENT(IN) :: RATE
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN) :: XI
      my_real ,DIMENSION(LMAX,NFUNC) ,INTENT(IN) :: YI
      TYPE(TABLE_4D_) ,INTENT(INOUT) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IPT,NPT,NPT0,IPOS,NDIM,FUNC_ID,IERROR,STAT
      INTEGER ,DIMENSION(NFUNC) :: PERM
      INTEGER ,PARAMETER :: NPTMAX = 100   ! max number of function points
      my_real :: X1,X2,Y1,Y2,DERI
      my_real ,DIMENSION(LMAX) :: XTMP,YTMP
      my_real ,DIMENSION(:)   ,ALLOCATABLE :: XF
      my_real ,DIMENSION(:,:) ,ALLOCATABLE :: YF
C=======================================================================
c     create X,Y vectors for all curves before unifying all abscissas
c--------------------------------------------------------
      ALLOCATE (XF(LMAX))
      ALLOCATE (YF(LMAX,NFUNC))
c
      IF (NFUNC == 1) THEN
        NPT = LEN(1)
        XF(1:NPT)   = XI(1:NPT,1)
        YF(1:NPT,1) = YI(1:NPT,1)
c
      ELSE
c--------------------------------------------------------
c       unify abscissas
c--------------------------------------------------------
c
        CALL UNIFY_ABSCISSA_2D(NFUNC,LMAX,NPT,LEN,XI,YI,XF,YF)
c
c--------------------------------------------------------
c       check and correct intersections
c--------------------------------------------------------
c
        CALL FUNC2D_DEINTERSECT(NPT, LMAX ,NFUNC  ,YF   )
c
      END IF
c--------------------------------------------------------
c     d) check and correct monotonicity
c--------------------------------------------------------
      DO I = 1,NFUNC
        DO IPT = 2,NPT
          IF (YF(IPT,I) < YF(IPT-1,I)) YF(IPT,I) = YF(IPT-1,I)
        END DO
      END DO
      XTMP(1:NPT) = XF(1:NPT)
      NPT0 = NPT
c--------------------------------------------------------
c     second reduction of number of points of the 1st curve
c     and reinterpolate all table functions based on its abscissa distribution
c--------------------------------------------------------
      IF (NPT > NPTMAX) THEN
        CALL VW_SMOOTH(NPT,NPTMAX,XF,YF(1:NPT,1))
c
        DO I = 2,NFUNC
          IPOS = 1
          YTMP(1:NPT0) = YF(1:NPT0,I)
          LEN(I) = NPT
          DO J = 2,NPT-1
            DO WHILE (XF(J) > XTMP(IPOS))
              IPOS = IPOS + 1
            END DO
            X1 = XTMP(IPOS)
            X2 = XTMP(IPOS+1)
            Y1 = YTMP(IPOS)
            Y2 = YTMP(IPOS+1)
            DERI  = (Y2 - Y1) / (X2 - X1)
            YF(J,I) = Y1 + DERI * (XF(J) - X1)
          END DO
          YF(NPT,I) = Y2
        END DO      
      END IF
c--------------------------------------------------------------------------
c     e) create 2D function table
c--------------------------------------------------------
      NDIM = MIN(2,NFUNC)
      TABLE%NDIM  = NDIM
      ALLOCATE (TABLE%X(NDIM)              ,STAT=stat)            
      ALLOCATE (TABLE%X(1)%VALUES(NPT)     ,STAT=stat)      
      IF (NFUNC == 1) THEN
        ALLOCATE (TABLE%Y1D(NPT)           ,STAT=stat)
        TABLE%X(1)%VALUES(1:NPT)   = XF(1:NPT)   
        TABLE%Y1D(1:NPT) = YF(1:NPT,1)
      ELSE
        ALLOCATE (TABLE%X(2)%VALUES(NFUNC) ,STAT=stat)
        ALLOCATE (TABLE%Y2D(NPT,NFUNC)     ,STAT=stat)
        TABLE%X(1)%VALUES(1:NPT)   = XF(1:NPT)   
        TABLE%X(2)%VALUES(1:NFUNC) = RATE(1:NFUNC)
        DO I = 1,NFUNC
          TABLE%Y2D(1:NPT,I) = YF(1:NPT,I)
        END DO
      END IF      
c
c--------------------
      DEALLOCATE (XF)
      DEALLOCATE (YF)
c--------------------
      RETURN
      END
