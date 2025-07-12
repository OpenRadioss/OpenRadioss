Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    table_mat_vinterp_mod         ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- called by ------------------------------------------------------
      !||    asso_plas76                   ../engine/source/materials/mat/mat076/asso_plas76.F
      !||    asso_qplas76c                 ../engine/source/materials/mat/mat076/asso_qplas76c.F
      !||    compaction_tab                ../common_source/eos/compaction_tab.F90
      !||    compaction_tab_init           ../common_source/eos/compaction_tab.F90
      !||    condamage                     ../engine/source/materials/mat/mat190/condamage.F
      !||    conversion                    ../engine/source/materials/mat/mat190/conversion.F
      !||    granular51                    ../engine/source/materials/mat/mat051/granular51.F90
      !||    mat87c_tabulated              ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
      !||    mat87c_tabulated_3dir_ortho   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    no_asso_lplas76c              ../engine/source/materials/mat/mat076/no_asso_lplas76c.F
      !||    no_asso_plas76                ../engine/source/materials/mat/mat076/no_asso_plas76.F
      !||    no_asso_qplas76c              ../engine/source/materials/mat/mat076/no_asso_qplas76c.F
      !||    sigeps128c                    ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||    sigeps128s                    ../engine/source/materials/mat/mat128/sigeps128s.F90
      !||    sigeps129s                    ../engine/source/materials/mat/mat129/sigeps129s.F90
      !||    sigeps133                     ../engine/source/materials/mat/mat133/sigeps133.F90
      !||    sigeps163                     ../engine/source/materials/mat/mat163/sigeps163.F90
      !||    sigeps50s                     ../engine/source/materials/mat/mat050/sigeps50s.F90
      !||    sigeps57c                     ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||    sigeps70                      ../engine/source/materials/mat/mat070/sigeps70.F
      !||====================================================================
      MODULE TABLE_MAT_VINTERP_MOD
      CONTAINS
      !||====================================================================
      !||    table_mat_vinterp             ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- called by ------------------------------------------------------
      !||    asso_plas76                   ../engine/source/materials/mat/mat076/asso_plas76.F
      !||    asso_qplas76c                 ../engine/source/materials/mat/mat076/asso_qplas76c.F
      !||    compaction_tab                ../common_source/eos/compaction_tab.F90
      !||    compaction_tab_init           ../common_source/eos/compaction_tab.F90
      !||    condamage                     ../engine/source/materials/mat/mat190/condamage.F
      !||    conversion                    ../engine/source/materials/mat/mat190/conversion.F
      !||    granular51                    ../engine/source/materials/mat/mat051/granular51.F90
      !||    mat87c_tabulated              ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
      !||    mat87c_tabulated_3dir_ortho   ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    no_asso_lplas76c              ../engine/source/materials/mat/mat076/no_asso_lplas76c.F
      !||    no_asso_plas76                ../engine/source/materials/mat/mat076/no_asso_plas76.F
      !||    no_asso_qplas76c              ../engine/source/materials/mat/mat076/no_asso_qplas76c.F
      !||    sigeps128c                    ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||    sigeps128s                    ../engine/source/materials/mat/mat128/sigeps128s.F90
      !||    sigeps129s                    ../engine/source/materials/mat/mat129/sigeps129s.F90
      !||    sigeps133                     ../engine/source/materials/mat/mat133/sigeps133.F90
      !||    sigeps163                     ../engine/source/materials/mat/mat163/sigeps163.F90
      !||    sigeps50s                     ../engine/source/materials/mat/mat050/sigeps50s.F90
      !||    sigeps57c                     ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||    sigeps70                      ../engine/source/materials/mat/mat070/sigeps70.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                        ../engine/source/output/message/message.F
      !||    arret                         ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                   ../engine/share/message_module/message_mod.F
      !||    table4d_mod                   ../common_source/modules/table4d_mod.F
      !||====================================================================
      SUBROUTINE TABLE_MAT_VINTERP(TABLE,DIMX,NEL,IPOS,XX,YY,DYDX, OPT_EXTRAPOLATE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is proceeding to table interpolation.
C   example with case dim=1 (table <=> function)
C    nel is number interpolatation
C    ipos is index backup to prevent from starting the loop from 1 to npt during each cycle (input/output)
C    XX(nel) are abscissa on which the interpolation is required (input)
C    YY(nel) are the interpolated value (output)
C    DYDX(nel) is the slope (output)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE4D_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TABLE_4D_)                    ,INTENT(IN)    :: TABLE
      INTEGER, VALUE                     ,INTENT(IN)    :: DIMX
      INTEGER                            ,INTENT(IN)    :: NEL
      my_real, DIMENSION(DIMX,TABLE%NDIM),INTENT(IN)    :: XX
      INTEGER, DIMENSION(DIMX,TABLE%NDIM),INTENT(INOUT) :: IPOS
      my_real, DIMENSION(DIMX)           ,INTENT(INOUT) :: YY
      my_real, DIMENSION(DIMX)           ,INTENT(INOUT) :: DYDX
      LOGICAL, OPTIONAL, INTENT(IN)                     :: OPT_EXTRAPOLATE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: NEED_TO_COMPUTE
      INTEGER  I,J,K,M,N,I1,I2,J1,J2,K1,K2,L1,L2,NDIM
      INTEGER :: NINDX_1,NINDX_2
      INTEGER, DIMENSION(NEL) :: INDX_1,INDX_2
      INTEGER, DIMENSION(4)   :: LDIM
      my_real :: DX,DY,ALPHA,ALPHAI,BETA,BETAI,GAMMA,GAMMAI,DELTA,DELTAI
      my_real, DIMENSION(NEL,4) :: FAC
      LOGICAL DO_EXTRAPOLATION
C-----------------------------------------------
C   Source Lines
C-----------------------------------------------
      DO_EXTRAPOLATION = .TRUE.
      IF(PRESENT(OPT_EXTRAPOLATE)) THEN
        DO_EXTRAPOLATION = OPT_EXTRAPOLATE
      ENDIF

      NDIM = TABLE%NDIM
      IF (SIZE(XX,2) < NDIM ) THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF

      DO K=1,NDIM
        LDIM(K) = SIZE(TABLE%X(K)%VALUES)
      END DO

      DO K=1,NDIM
        IPOS(1:NEL,K) = MAX(IPOS(1:NEL,K),1)
        NINDX_1 = 0
        NINDX_2 = 0
#include "vectorize.inc"
        DO I=1,NEL
          M  = IPOS(I,K) 
          DX = TABLE%X(K)%VALUES(M) - XX(I,K)
          IF (DX >= ZERO)THEN
            NINDX_1 = NINDX_1 + 1
            INDX_1(NINDX_1) = I
          ELSE
            NINDX_2 = NINDX_2 + 1
            INDX_2(NINDX_2) = I
          ENDIF
        ENDDO

        DO J=1,NINDX_1
          I = INDX_1(J)
          M = IPOS(I,K)
          NEED_TO_COMPUTE = .TRUE.
          DO WHILE (NEED_TO_COMPUTE )
             DX = TABLE%X(K)%VALUES(M) - XX(I,K)
             IF (DX < ZERO .OR. M <= 1 ) THEN
                IPOS(I,K) = MAX(M,1)
                NEED_TO_COMPUTE = .FALSE.
             ELSE
                M=M-1
             ENDIF
          ENDDO
        ENDDO

        DO J=1,NINDX_2
          I = INDX_2(J)
          M = IPOS(I,K) 
          NEED_TO_COMPUTE = .TRUE.
          DO WHILE (NEED_TO_COMPUTE )
             DX = TABLE%X(K)%VALUES(M) - XX(I,K)
             IF (DX >= ZERO .OR. M == LDIM(K)) THEN
                IPOS(I,K) = M-1
                NEED_TO_COMPUTE = .FALSE.
             ELSE
                M=M+1
             ENDIF
          ENDDO
        ENDDO
      ENDDO ! K=1,NDIM

      DO K=1,NDIM
#include "vectorize.inc"
        DO I=1,NEL
          N = IPOS(I,K)
          FAC(I,K) = (TABLE%X(K)%VALUES(N+1) - XX(I,K)) / (TABLE%X(K)%VALUES(N+1) - TABLE%X(K)%VALUES(N))
        END DO
      END DO

      IF(.NOT. DO_EXTRAPOLATION)THEN
      DO K=1,NDIM
#include "vectorize.inc"
        DO I=1,NEL
          N = IPOS(I,K)
          FAC(I,K) = MIN(ONE,MAX(FAC(I,K),ZERO))
        END DO
      END DO
      ENDIF
c----------------------------------------------

      SELECT CASE(NDIM)

       CASE(4)
#include "vectorize.inc"
        DO I=1,NEL                                                                         
          I1 = IPOS(I,1)                                                                   
          I2 = I1 + 1                                                                      
          J1 = IPOS(I,2)                                                                   
          J2 = J1 + 1                                                                      
          K1 = IPOS(I,3)                                                                   
          K2 = K1 + 1                                                                      
          L1 = IPOS(I,4)                                                                   
          L2 = K1 + 1
          ALPHA  = FAC(I,1)
          BETA   = FAC(I,2)
          GAMMA  = FAC(I,3)
          DELTA  = FAC(I,4)
          ALPHAI = ONE - ALPHA                                                                 
          BETAI  = ONE - BETA                                                                 
          GAMMAI = ONE - GAMMA                                                                 
          DELTAI = ONE - DELTA
          YY(I)  =                                                                          
     .        DELTA* (GAMMA*(BETA * (ALPHA  * TABLE%Y4D(I1,J1,K1,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K1,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K1,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K1,L1)) )                                                                                          
     .              +GAMMAI*( BETA* (ALPHA  * TABLE%Y4D(I1,J1,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K2,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K2,L1))))
                      
     .      + DELTAI*(GAMMA *(BETA * (ALPHA  * TABLE%Y4D(I1,J1,K1,L2)                    
     .                              + ALPHAI * TABLE%Y4D(I2,J1,K1,L2))                   
     .                      + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K1,L2)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K1,L2)))                  
     .              +GAMMAI* (BETA* (ALPHA  * TABLE%Y4D(I1,J1,K2,L2)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K2,L2))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K2,L2)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K2,L2))))
!
          DY =  DELTA * (GAMMA *(BETA *(TABLE%Y4D(I2,J1,K1,L1)-TABLE%Y4D(I1,J1,K1,L1))                                                                                 
     .                         + BETAI*(TABLE%Y4D(I2,J2,K1,L1)-TABLE%Y4D(I1,J2,K1,L1)))   
     .                + GAMMAI *(BETA *(TABLE%Y4D(I2,J1,K2,L1)-TABLE%Y4D(I1,J1,K2,L1))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K2,L1)-TABLE%Y4D(I1,J2,K2,L1))))  
     .       + DELTAI * (GAMMA *(BETA *(TABLE%Y4D(I2,J1,K1,L2)-TABLE%Y4D(I1,J1,K1,L2))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K1,L2)-TABLE%Y4D(I1,J2,K1,L2)))   
     .                + GAMMAI *(BETA *(TABLE%Y4D(I2,J1,K2,L2)-TABLE%Y4D(I1,J1,K2,L2))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K2,L2)-TABLE%Y4D(I1,J2,K2,L2))))
          DX = TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1)
          DYDX(I) = DY / DX                                                                
        END DO                                                                             
C-----
      CASE(3)
#include "vectorize.inc"
        DO I=1,NEL
          I1 = IPOS(I,1)
          I2 = I1 + 1
          J1 = IPOS(I,2)
          J2 = J1 + 1
          K1 = IPOS(I,3)
          K2 = K1 + 1
          ALPHA  = FAC(I,1)
          BETA   = FAC(I,2)
          GAMMA  = FAC(I,3)
          ALPHAI = ONE - ALPHA                                                                 
          BETAI  = ONE - BETA                                                                 
          GAMMAI = ONE - GAMMA
          YY(I)=(GAMMA * (BETA * (ALPHA*TABLE%Y3D(I1,J1,K1) + ALPHAI*TABLE%Y3D(I2,J1,K1))
     .                  + BETAI* (ALPHA*TABLE%Y3D(I1,J2,K1) + ALPHAI*TABLE%Y3D(I2,J2,K1)) )
     .       + GAMMAI *  (BETA * (ALPHA*TABLE%Y3D(I1,J1,K2) + ALPHAI*TABLE%Y3D(I2,J1,K2))
     .                  + BETAI* (ALPHA*TABLE%Y3D(I1,J2,K2) + ALPHAI*TABLE%Y3D(I2,J2,K2))))

          DY =  GAMMA * ( BETA*(TABLE%Y3D(I2,J1,K1)  - TABLE%Y3D(I1,J1,K1))
     .                  + BETAI*(TABLE%Y3D(I2,J2,K1) - TABLE%Y3D(I1,J2,K1)))
     .       + GAMMAI * ( BETA*(TABLE%Y3D(I2,J1,K2)  - TABLE%Y3D(I1,J1,K2))
     .                  + BETAI*(TABLE%Y3D(I2,J2,K2) - TABLE%Y3D(I1,J2,K2)))
          DX = TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1)
     .                           
          DYDX(I) = DY / DX                                                                
        END DO

      CASE(2)
#include "vectorize.inc"
        DO I=1,NEL
          I1 = IPOS(I,1)
          I2 = I1 + 1
          J1 = IPOS(I,2)
          J2 = J1 + 1
          ALPHA  = FAC(I,1)
          BETA   = FAC(I,2)
          ALPHAI = ONE - ALPHA                                                                 
          BETAI  = ONE - BETA
          YY(I)  = (BETA * (ALPHA*TABLE%Y2D(I1,J1) + ALPHAI*TABLE%Y2D(I2,J1))
     .           + BETAI * (ALPHA*TABLE%Y2D(I1,J2) + ALPHAI*TABLE%Y2D(I2,J2)) )
          DYDX(I) = (BETA *(TABLE%Y2D(I2,J1) - TABLE%Y2D(I1,J1))
     .            + BETAI *(TABLE%Y2D(I2,J2) - TABLE%Y2D(I1,J2)))
     .                   / (TABLE%X(1)%VALUES(I2)-TABLE%X(1)%VALUES(I1))
        END DO

      CASE(1)
#include "vectorize.inc"
        DO I=1,NEL
          I1 = IPOS(I,1)
          I2 = I1 + 1
          ALPHA  = FAC(I,1)
          ALPHAI = ONE - ALPHA
          YY(I)   = ALPHA*TABLE%Y1D(I1) + ALPHAI*TABLE%Y1D(I2)
          DYDX(I) = (TABLE%Y1D(I2) - TABLE%Y1D(I1)) / (TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1))
        END DO

      END SELECT
c-----------
      RETURN
      END SUBROUTINE TABLE_MAT_VINTERP
c-----------
      END MODULE TABLE_MAT_VINTERP_MOD     
