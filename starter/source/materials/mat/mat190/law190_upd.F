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
      !||====================================================================
      !||    law190_upd_mod   ../starter/source/materials/mat/mat190/law190_upd.F
      !||--- called by ------------------------------------------------------
      !||    updmat           ../starter/source/materials/updmat.F
      !||====================================================================
      MODULE LAW190_UPD_MOD
      CONTAINS 
      !||====================================================================
      !||    law190_upd         ../starter/source/materials/mat/mat190/law190_upd.F
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE LAW190_UPD(MATPARAM ,NUMTABL ,ITABLE   ,TABLE       ,PM    )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------      
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
#include  "com04_c.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUMTABL
      INTEGER, DIMENSION(NUMTABL) :: ITABLE
      TYPE(MATPARAM_STRUCT_), TARGET :: MATPARAM
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(INOUT) ::  TABLE
      my_real, DIMENSION(NPROPM), INTENT(INOUT) :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,KK,NDIM,NPT,FUN_1,FUNC_ID,FUNC_T,FUNC_C,FUNC_S,ICONV,
     .           SIZETOZERO,NPTMAX,IFX,IFY,STAT,LEN2,LEN3
      my_real :: XFAC,EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX,
     .           XINT,YINT,X_I,X_II,Y_I,Y_II,ENER,SCALEFAC,DX,DY,DYDX,STIFFINI,
     .           NU,G,C1
      my_real ,DIMENSION(:)  ,ALLOCATABLE :: X_ENER,Y_ENER
      TYPE(TABLE_4D_), DIMENSION(:) ,POINTER ::  TABLE_MAT
C=======================================================================
c
c--------------------------------------------------------------------------
c     Copy global functions/tables to matparam data structure
c--------------------------------------------------------------------------
c      
      ! Loading table 
      FUN_1 = ITABLE(1)                       ! Identifier
      NDIM  = TABLE(FUN_1)%NDIM               ! Number of dimensions
      NPT   = SIZE(TABLE(FUN_1)%X(1)%VALUES)  ! Number of points
      MATPARAM%NTABLE = NUMTABL               ! Number of tables
      ALLOCATE (MATPARAM%TABLE(NUMTABL))      ! Allocate table array
      TABLE_MAT =>  MATPARAM%TABLE(1:NUMTABL) ! Material table pointer    
      TABLE_MAT(1:NUMTABL)%NOTABLE = 0        ! Initialization of table identifiers array
c
      ! Scale factor
      SCALEFAC = MATPARAM%UPARAM(5)  
c
      ! Table number 1 - Loading function
      IF (FUN_1 > 0) THEN
        TABLE_MAT(1)%NOTABLE = FUN_1
        NDIM = TABLE(FUN_1)%NDIM
        TABLE_MAT(1)%NDIM = NDIM
        ALLOCATE (TABLE_MAT(1)%X(NDIM),STAT=stat)      
        ! Loop over dimensions to copy abscissa 
        DO I = 1,NDIM
          NPT = SIZE(TABLE(FUN_1)%X(I)%VALUES)
          ALLOCATE (TABLE_MAT(1)%X(I)%VALUES(NPT),STAT=stat)
          TABLE_MAT(1)%X(I)%VALUES(1:NPT) = TABLE(FUN_1)%X(I)%VALUES(1:NPT)
        END DO
        ! 1 dimension, stress vs strain loading
        IF (NDIM == 1) THEN
          NPT = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y1D(NPT),STAT=stat)
          TABLE_MAT(1)%Y1D(1:NPT) = TABLE(FUN_1)%Y%VALUES(1:NPT)
        ! 2 dimensions, stress vs strain vs strain rate 
        ELSE IF (NDIM == 2) THEN
          NPT  = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          LEN2 = SIZE(TABLE(FUN_1)%X(2)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y2D(NPT,LEN2),STAT=stat)
          DO I=1,NPT
            DO J=1,LEN2
              TABLE_MAT(1)%Y2D(I,J) = TABLE(FUN_1)%Y%VALUES((J-1)*NPT+I)
            END DO
          END DO
        ! 3 dimensions, stress vs strain vs strain rate vs volume fraction
        ELSE IF (NDIM == 3) THEN
          NPT  = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          LEN2 = SIZE(TABLE(FUN_1)%X(2)%VALUES)
          LEN3 = SIZE(TABLE(FUN_1)%X(3)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y3D(NPT,LEN2,LEN3),STAT=stat)
          DO I=1,NPT
            DO J=1,LEN2
              DO K=1,LEN3
                TABLE_MAT(1)%Y3D(I,J,K) = TABLE(FUN_1)%Y%VALUES((K-1)*NPT*LEN2+(J-1)*NPT+I)
              END DO
            END DO
          END DO
        END IF
c
        ! Compute initial slope
        STIFFINI = ZERO
        DO I = 1,NPT-1
          DX = TABLE_MAT(1)%X(1)%VALUES(I+1) - TABLE_MAT(1)%X(1)%VALUES(I)
          IF (NDIM == 1) THEN
            DY = TABLE_MAT(1)%Y1D(I+1) - TABLE_MAT(1)%Y1D(I)  
          ELSEIF (NDIM == 2) THEN
            DY = TABLE_MAT(1)%Y2D(I+1,1) - TABLE_MAT(1)%Y2D(I,1) 
          ELSEIF (NDIM == 3) THEN
            DY = TABLE_MAT(1)%Y3D(I+1,1,1) - TABLE_MAT(1)%Y3D(I,1,1) 
          ENDIF
          DYDX = SCALEFAC*DY/DX
          IF (TABLE_MAT(1)%X(1)%VALUES(I+1) == ZERO)THEN             
            STIFFINI = MAX(STIFFINI,DYDX)
          ELSEIF (TABLE_MAT(1)%X(1)%VALUES(I) == ZERO) THEN           
            STIFFINI = MAX(STIFFINI,DYDX)
          ELSEIF (TABLE_MAT(1)%X(1)%VALUES(1) >= ZERO) THEN
            DX = TABLE_MAT(1)%X(1)%VALUES(2) - TABLE_MAT(1)%X(1)%VALUES(1)
            IF (NDIM == 1) THEN
              DY = TABLE_MAT(1)%Y1D(2) - TABLE_MAT(1)%Y1D(1)     
            ELSEIF (NDIM == 2) THEN
              DY = TABLE_MAT(1)%Y2D(2,1) - TABLE_MAT(1)%Y2D(1,1) 
            ELSEIF (NDIM == 3) THEN
              DY = TABLE_MAT(1)%Y3D(2,1,1) - TABLE_MAT(1)%Y3D(1,1,1) 
            ENDIF      
            STIFFINI = MAX(STIFFINI, SCALEFAC*DY/DX)
          ENDIF
        ENDDO
      END IF
c
      STIFFINI = MAX(STIFFINI,MATPARAM%UPARAM(1))
      NU = ZERO
      C1 = STIFFINI/THREE/(ONE - TWO*NU)
      G  = HALF*STIFFINI/(ONE + NU) 
      MATPARAM%UPARAM(1) = STIFFINI
      MATPARAM%UPARAM(2) = C1
      MATPARAM%UPARAM(3) = G
      PM(20) = STIFFINI
      PM(22) = G
      PM(24) = STIFFINI
      PM(32) = C1     
c
      ! Table number 2 - Loading function
      TABLE_MAT(2)%NOTABLE = 2
      TABLE_MAT(2)%NDIM = 1
      NPT = SIZE(TABLE(FUN_1)%X(1)%VALUES)
      ALLOCATE(X_ENER(NPT),STAT=stat)
      ALLOCATE(Y_ENER(NPT),STAT=stat)
      X_ENER(1:NPT) = ZERO
      Y_ENER(1:NPT) = ZERO
      SIZETOZERO = 1
      DO I = 1, NPT-1 
        X_I  = TABLE(FUN_1)%X(1)%VALUES(I+1) 
        X_II = TABLE(FUN_1)%X(1)%VALUES(I) 
        IF(X_I >= ZERO .AND. X_II < ZERO) THEN
          SIZETOZERO = I+1 
          EXIT
        ENDIF
      ENDDO
      DO K = 1, NPT
        X_ENER(K) = TABLE(FUN_1)%X(1)%VALUES(K)
      ENDDO
      Y_ENER(SIZETOZERO) = ZERO
      DO K = SIZETOZERO+1, NPT
        X_I  = TABLE(FUN_1)%X(1)%VALUES(K) 
        X_II = TABLE(FUN_1)%X(1)%VALUES(K-1) 
        Y_I  = TABLE(FUN_1)%Y%VALUES(K)*SCALEFAC
        Y_II = TABLE(FUN_1)%Y%VALUES(K-1)*SCALEFAC
        ENER = (X_I-X_II)*(Y_I + Y_II)/TWO
        Y_ENER(K) = Y_ENER(K-1) + ENER
      ENDDO
      DO K = 1, SIZETOZERO-1 
        KK   = SIZETOZERO - K
        X_I  = TABLE(FUN_1)%X(1)%VALUES(KK) 
        X_II = TABLE(FUN_1)%X(1)%VALUES(KK+1) 
        Y_I  = TABLE(FUN_1)%Y%VALUES(KK)*SCALEFAC
        Y_II = TABLE(FUN_1)%Y%VALUES(K+1)*SCALEFAC
        ENER = (X_I-X_II)*(Y_I + Y_II)/TWO
        Y_ENER(KK) = Y_ENER(KK+1) + ENER
      ENDDO
      ALLOCATE (TABLE_MAT(2)%X(NDIM)          ,STAT=stat)      
      ALLOCATE (TABLE_MAT(2)%X(1)%VALUES(NPT) ,STAT=stat)
      ALLOCATE (TABLE_MAT(2)%Y1D(NPT)         ,STAT=stat)
      DO I = 1,NPT
        TABLE_MAT(2)%X(1)%VALUES(I) = X_ENER(I)
        TABLE_MAT(2)%Y1D(I) = Y_ENER(I)
      END DO
      DEALLOCATE(X_ENER,Y_ENER)
c
      END
      END MODULE

