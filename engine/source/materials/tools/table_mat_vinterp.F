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
Chd|  TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|-- called by -----------
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        CONDAMAGE                     source/materials/mat/mat190/condamage.F
Chd|        CONVERSION                    source/materials/mat/mat190/conversion.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE TABLE_MAT_VINTERP_MOD
      CONTAINS
Chd|====================================================================
Chd|  TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|-- called by -----------
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        CONDAMAGE                     source/materials/mat/mat190/condamage.F
Chd|        CONVERSION                    source/materials/mat/mat190/conversion.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE TABLE_MAT_VINTERP(TABLE,DIMX,NEL,IPOS,XX,YY,DYDX)
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL, DIMENSION(NEL) :: NEED_TO_COMPUTE
      INTEGER  I,J,K,L,M,N,I1,I2,J1,J2,K1,K2,L1,L2,NDIM
      INTEGER :: MINDX_1,MINDX_2
      INTEGER :: NINDX_1,NINDX_2
      INTEGER, DIMENSION(NEL) :: INDX_1,INDX_2
      INTEGER, DIMENSION(4)   :: LDIM
      my_real :: DX,DY,ALPHA,ALPHAI,BETA,BETAI,GAMMA,GAMMAI,DELTA,DELTAI
      my_real, DIMENSION(NEL,4) :: FAC
c=======================================================================
      NDIM = TABLE%NDIM
      IF (SIZE(XX,2) < NDIM ) THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .              C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
C-----
      DO K=1,NDIM
        LDIM(K) = SIZE(TABLE%X(K)%VALUES)
      END DO
c
      DO K=1,NDIM
        IPOS(1:NEL,K) = MAX(IPOS(1:NEL,K),1)
        NINDX_1 = 0
        MINDX_1 = 0
        NINDX_2 = 0
        MINDX_2 = LDIM(K) + 1
#include "vectorize.inc"
        DO I=1,NEL
          M  = IPOS(I,K) 
          DX = TABLE%X(K)%VALUES(M) - XX(I,K)
          IF (DX >= ZERO)THEN
            NINDX_1 = NINDX_1 + 1
            INDX_1(NINDX_1) = I
            MINDX_1 = MAX(MINDX_1,M)
          ELSE
            NINDX_2 = NINDX_2 + 1
            INDX_2(NINDX_2) = I
            MINDX_2 = MIN(MINDX_2,M)
          ENDIF
        ENDDO

        NEED_TO_COMPUTE(1:NINDX_1) = .TRUE.
        DO N = MINDX_1,1,-1
#include "vectorize.inc"
          DO J=1,NINDX_1
            IF(NEED_TO_COMPUTE(J)) THEN
              I = INDX_1(J)
              M = IPOS(I,K)
              DX = TABLE%X(K)%VALUES(N) - XX(I,K)
              IF (DX < ZERO .OR. N <= 1) THEN 
                IPOS(I,K)=MAX(N,1)
                NEED_TO_COMPUTE(J) = .FALSE.
              ENDIF
            ENDIF
          ENDDO
        ENDDO
c
        NEED_TO_COMPUTE(1:NINDX_2) = .TRUE.
c
        DO N=MINDX_2,LDIM(K)
#include "vectorize.inc"
          DO J=1,NINDX_2
            IF (NEED_TO_COMPUTE(J)) THEN
              I = INDX_2(J)
              M = IPOS(I,K) 
              DX = TABLE%X(K)%VALUES(N) - XX(I,K)
              IF (DX >= ZERO .OR. N == LDIM(K)) THEN
                IPOS(I,K) = N-1
                NEED_TO_COMPUTE(J) = .FALSE.
              ENDIF
            ENDIF
          ENDDO
        ENDDO

      ENDDO ! K=1,NDIM
c
      DO K=1,NDIM
#include "vectorize.inc"
        DO I=1,NEL
          N = IPOS(I,K)
          FAC(I,K) = (TABLE%X(K)%VALUES(N+1) - XX(I,K))
     .             / (TABLE%X(K)%VALUES(N+1) - TABLE%X(K)%VALUES(N))
        END DO
      END DO
c----------------------------------------------
      SELECT CASE(NDIM)

       CASE(4)
C
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
c
          YY(I)  =                                                                          
     .        DELTA* (GAMMA*(BETA * (ALPHA  * TABLE%Y4D(I1,J1,K1,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K1,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K1,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K1,L1)) )                  
                                                                        
     .              +GAMMAI*( BETA* (ALPHA  * TABLE%Y4D(I1,J1,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K2,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K2,L1))))                 
     .      +DELTAI*(GAMMA *( BETA* (ALPHA  * TABLE%Y4D(I1,J1,K1,L1)                    
     .                              +ALPHAI * TABLE%Y4D(I2,J1,K1,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K1,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K1,L1)))                  
     .              +GAMMAI*(BETA * (ALPHA  * TABLE%Y4D(I1,J1,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J1,K2,L1))                   
     .                     + BETAI* (ALPHA  * TABLE%Y4D(I1,J2,K2,L1)                    
     .                            +  ALPHAI * TABLE%Y4D(I2,J2,K2,L1))))                 
c
          DY =  DELTA * (GAMMA *(BETA *(TABLE%Y4D(I2,J1,K1,L1)-TABLE%Y4D(I1,J1,K1,L1))                                                                                 
     .                         + BETAI*(TABLE%Y4D(I2,J2,K1,L1)-TABLE%Y4D(I1,J2,K1,L1)))   
     .                + GAMMAI *(BETA *(TABLE%Y4D(I2,J1,K2,L1)-TABLE%Y4D(I1,J1,K2,L1))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K2,L1)-TABLE%Y4D(I1,J2,K2,L1))))  
     .       + DELTAI * (GAMMA *(BETA *(TABLE%Y4D(I2,J1,K1,L1)-TABLE%Y4D(I1,J1,K1,L1))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K1,L1)-TABLE%Y4D(I1,J2,K1,L1)))   
     .                + GAMMAI *(BETA *(TABLE%Y4D(I2,J1,K2,L1)-TABLE%Y4D(I1,J1,K2,L1))    
     .                         + BETAI*(TABLE%Y4D(I2,J2,K2,L1)-TABLE%Y4D(I1,J2,K2,L1))))
     .    
     .                                                                                     
          DX = TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1)
          DYDX(I) = DY / DX                                                                
        END DO                                                                             
C-----
      CASE(3)
C
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
C
          YY(I)=(GAMMA * (BETA* (ALPHA*TABLE%Y3D(I1,J1,K1) + ALPHAI*TABLE%Y3D(I2,J1,K1))
     .                  + BETAI* (ALPHA*TABLE%Y3D(I1,J2,K1) + ALPHAI*TABLE%Y3D(I2,J2,K1)) )
     .       + GAMMAI *  (BETA* (ALPHA*TABLE%Y3D(I1,J1,K2) + ALPHAI*TABLE%Y3D(I2,J1,K2))
     .                  + BETAI* (ALPHA*TABLE%Y3D(I1,J2,K2) + ALPHAI*TABLE%Y3D(I2,J2,K2))))
c
          DY =  GAMMA * ( BETA*(TABLE%Y3D(I2,J1,K1) - TABLE%Y3D(I1,J1,K1))
     .                  + BETAI*(TABLE%Y3D(I2,J2,K1) - TABLE%Y3D(I1,J2,K1)))
     .       + GAMMAI * ( BETA*(TABLE%Y3D(I2,J1,K2) - TABLE%Y3D(I1,J1,K2))
     .                  + BETAI*(TABLE%Y3D(I2,J2,K2) - TABLE%Y3D(I1,J2,K2)))
          DX = TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1)
     .                           
          DYDX(I) = DY / DX                                                                
        END DO
C-----
      CASE(2)
C
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
c
          YY(I)  = (BETA * (ALPHA*TABLE%Y2D(I1,J1) + ALPHAI*TABLE%Y2D(I2,J1))
     .           + BETAI * (ALPHA*TABLE%Y2D(I1,J2) + ALPHAI*TABLE%Y2D(I2,J2)) )
c
          DYDX(I) = (BETA *(TABLE%Y2D(I2,J1) - TABLE%Y2D(I1,J1))
     .            + BETAI *(TABLE%Y2D(I2,J2) - TABLE%Y2D(I1,J2)))
     .                   / (TABLE%X(1)%VALUES(I2)-TABLE%X(1)%VALUES(I1))
        END DO
C-----
      CASE(1)
c
#include "vectorize.inc"
        DO I=1,NEL
          I1 = IPOS(I,1)
          I2 = I1 + 1
          ALPHA  = FAC(I,1)
          ALPHAI = ONE - ALPHA                                                                 
c
          YY(I)   = ALPHA*TABLE%Y1D(I1) + ALPHAI*TABLE%Y1D(I2)
          DYDX(I) = (TABLE%Y1D(I2) - TABLE%Y1D(I1)) 
     .            / (TABLE%X(1)%VALUES(I2) - TABLE%X(1)%VALUES(I1))
        END DO
C-----
      END SELECT
c-----------
      RETURN
      END SUBROUTINE TABLE_MAT_VINTERP
c-----------
      END MODULE TABLE_MAT_VINTERP_MOD     
