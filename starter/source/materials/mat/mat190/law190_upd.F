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
Chd|  LAW190_UPD_MOD                source/materials/mat/mat190/law190_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|====================================================================
      MODULE LAW190_UPD_MOD
      CONTAINS 
Chd|====================================================================
Chd|  LAW190_UPD                    source/materials/mat/mat190/law190_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW190_UPD(MATPARAM ,NUMTABL ,ITABLE   ,TABLE       )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------      
      USE MATPARAM_DEF_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMTABL
      INTEGER ,DIMENSION(NUMTABL) :: ITABLE
      TYPE(MATPARAM_STRUCT_)  ,TARGET :: MATPARAM
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(INOUT) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,KK,NDIM,NPT,FUN_1,FUNC_ID,FUNC_T,FUNC_C,FUNC_S,ICONV,
     .           SIZETOZERO,NPTMAX,IFX,IFY,STAT,LEN2,LEN3
      my_real :: XFAC,EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX,
     .           XINT,YINT,X_I,X_II,Y_I,Y_II,ENER
      my_real ,DIMENSION(:)  ,ALLOCATABLE :: X_ENER,Y_ENER
      TYPE(TABLE_4D_), DIMENSION(:) ,POINTER ::  TABLE_MAT
C=======================================================================
      FUN_1    = ITABLE(1)
c
      NDIM = TABLE(FUN_1)%NDIM

      NPT  = SIZE(TABLE(FUN_1)%X(1)%VALUES)
c--------------------------------------------------------------------------
c     copy global function tables to local private storage for material law
c--------------------------------------------------------------------------
      ALLOCATE (MATPARAM%TABLE(NUMTABL))
      TABLE_MAT =>  MATPARAM%TABLE(1:NUMTABL)      
      TABLE_MAT(1:NUMTABL)%NOTABLE = 0
c       
c     copy original input table to matparam table structure      
c       
      ! table (1)
      IF (FUN_1 > 0) THEN
        TABLE_MAT(1)%NOTABLE = FUN_1
        NDIM = TABLE(FUN_1)%NDIM
        TABLE_MAT(1)%NDIM  = NDIM
        ALLOCATE (TABLE_MAT(1)%X(NDIM)    ,STAT=stat)      
c        
        DO I = 1,NDIM
          NPT = SIZE(TABLE(FUN_1)%X(I)%VALUES)
          ALLOCATE (TABLE_MAT(1)%X(I)%VALUES(NPT) ,STAT=stat)
          TABLE_MAT(1)%X(I)%VALUES(1:NPT) = TABLE(FUN_1)%X(I)%VALUES(1:NPT)
        END DO
        IF (NDIM == 1) THEN
          NPT = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y1D(NPT) ,STAT=stat)
          TABLE_MAT(1)%Y1D(1:NPT) = TABLE(FUN_1)%Y%VALUES(1:NPT)
        ELSE IF (NDIM == 2) THEN
          NPT  = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          LEN2 = SIZE(TABLE(FUN_1)%X(2)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y2D(NPT,LEN2) ,STAT=stat)
          DO I=1,NPT
            DO J=1,LEN2
              TABLE_MAT(1)%Y2D(I,J) = TABLE(FUN_1)%Y%VALUES((J-1)*NPT+I)
            END DO
          END DO
        ELSE IF (NDIM == 3) THEN
          NPT  = SIZE(TABLE(FUN_1)%X(1)%VALUES)
          LEN2 = SIZE(TABLE(FUN_1)%X(2)%VALUES)
          LEN3 = SIZE(TABLE(FUN_1)%X(3)%VALUES)
          ALLOCATE (TABLE_MAT(1)%Y3D(NPT,LEN2,LEN3) ,STAT=stat)
          DO I=1,NPT
            DO J=1,LEN2
              DO K=1,LEN3
                TABLE_MAT(1)%Y3D(I,J,K) = TABLE(FUN_1)%Y%VALUES((K-1)*NPT*LEN2+(J-1)*NPT+I)
              END DO
            END DO
          END DO
        END IF
      END IF

!     create energy function

      TABLE_MAT(2)%NOTABLE = 2
      TABLE_MAT(2)%NDIM = 1
      NPT = SIZE(TABLE(FUN_1)%X(1)%VALUES)
      ALLOCATE(X_ENER(NPT), STAT=stat)
      ALLOCATE(Y_ENER(NPT), STAT=stat)
      X_ENER(1:NPT) = ZERO
      Y_ENER(1:NPT) = ZERO
      !  construct the table for energy

      SIZETOZERO = 1
      DO I = 1, NPT-1 
          X_I  =  TABLE(FUN_1)%X(1)%VALUES(I+1) 
          X_II =  TABLE(FUN_1)%X(1)%VALUES(I) 
 
          IF(X_I >= ZERO .AND. X_II < ZERO) THEN
            SIZETOZERO = I+1 
            EXIT
          ENDIF
      ENDDO

      DO K = 1, NPT
          X_ENER (K) = TABLE(FUN_1)%X(1)%VALUES(K)
      ENDDO
        !calculate Y_ENER (K)
      Y_ENER (SIZETOZERO) = ZERO
      DO K = SIZETOZERO+1, NPT
          X_I  =  TABLE(FUN_1)%X(1)%VALUES(K) 
          X_II =  TABLE(FUN_1)%X(1)%VALUES(K-1) 
          Y_I  =  TABLE(FUN_1 )%Y%VALUES(K)
          Y_II =  TABLE(FUN_1 )%Y%VALUES(K-1)
          ENER = (X_I-X_II)*(Y_I + Y_II)/TWO
          Y_ENER(K) = Y_ENER(K-1) + ENER
      ENDDO
      DO K = 1, SIZETOZERO-1 
          KK   = SIZETOZERO - K
          X_I  =  TABLE(FUN_1)%X(1)%VALUES(KK) 
          X_II =  TABLE(FUN_1)%X(1)%VALUES(KK+1) 
          Y_I  =  TABLE(FUN_1)%Y%VALUES(KK)
          Y_II =  TABLE(FUN_1)%Y%VALUES(K+1)
          ENER       = (X_I-X_II)*(Y_I + Y_II)/TWO
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

c------------------------------------------
      RETURN
      END
      END MODULE

