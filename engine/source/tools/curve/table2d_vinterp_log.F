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
Chd|  TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|-- called by -----------
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        MAT107C_NEWTON                source/materials/mat/mat107/mat107c_newton.F
Chd|        MAT107C_NICE                  source/materials/mat/mat107/mat107c_nice.F
Chd|        MAT107_NEWTON                 source/materials/mat/mat107/mat107_newton.F
Chd|        MAT107_NICE                   source/materials/mat/mat107/mat107_nice.F
Chd|        MAT112C_XIA_NEWTON            source/materials/mat/mat112/mat112c_xia_newton.F
Chd|        MAT112C_XIA_NICE              source/materials/mat/mat112/mat112c_xia_nice.F
Chd|        MAT112_XIA_NEWTON             source/materials/mat/mat112/mat112_xia_newton.F
Chd|        MAT112_XIA_NICE               source/materials/mat/mat112/mat112_xia_nice.F
Chd|        SIGEPS109                     source/materials/mat/mat109/sigeps109.F
Chd|        SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|        SIGEPS110C_LITE_NEWTON        source/materials/mat/mat110/sigeps110c_lite_newton.F
Chd|        SIGEPS110C_LITE_NICE          source/materials/mat/mat110/sigeps110c_lite_nice.F
Chd|        SIGEPS110C_NEWTON             source/materials/mat/mat110/sigeps110c_newton.F
Chd|        SIGEPS110C_NICE               source/materials/mat/mat110/sigeps110c_nice.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE TABLE2D_VINTERP_LOG(TABLE,ISMOOTH,NEL,IPOS,XX,YY,DYDX1,DYDX2)
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
c-----------------------------------------------
c     vectorized 2D table interpolation
c     dependency on second variable may be interpolated using following algorithms :
c            ISMOOTH = 1  => linear interpolation   
c            ISMOOTH = 2  => logarythmic interpolation base 10  
c            ISMOOTH = 3  => logarythmic interpolation base n
c   OUTPUT :
c            YY            interpolated function value
c            DYDX1         partial derivative vs 1st independent variable
c            DYDX2         partial derivative vs 2nd independent variable
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TTABLE)            :: TABLE
      INTEGER                 :: ISMOOTH,NEL
      INTEGER ,DIMENSION(:,:) :: IPOS
      my_real ,DIMENSION(:,:) :: XX
      my_real ,DIMENSION(NEL) :: YY, DYDX1, DYDX2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NXK(2), IB(2,2,NEL) 
      INTEGER :: I,I1,I2,J1,J2,K,N,M,L,IN,IM,IL,N1,IL1,IL2,NDIM
      my_real :: DX1,DX2,YA1,YA2,YB1,YB2,X1_1,X1_2,X2_1,X2_2,
     .           X1,X2,Y1,Y2,R1,R2,UNR1,UNR2
      TYPE(TTABLE_XY)                ,POINTER :: TY
      TYPE(TTABLE_XY), DIMENSION(:)  ,POINTER :: TX
C=======================================================================
      NDIM = TABLE%NDIM
      IF (SIZE(XX,2) < NDIM .or. NDIM > 2) THEN
        CALL ANCMSG(MSGID=36,ANMODE=ANINFO,C1='TABLE INTERPOLATION')
        CALL ARRET(2)
      END IF
c-----
      IF (NCYCLE == 0) THEN
        IPOS(1:NEL,1:NDIM) = 1
      END IF
c-----
      TX => TABLE%X
      TY => TABLE%Y
c--------------------------------------
      DO K=1,NDIM
        NXK(K) = SIZE(TX(K)%VALUES)
        DO I=1,NEL
          IPOS(I,K) = MAX(IPOS(I,K),1)
          IPOS(I,K) = MIN(IPOS(I,K),NXK(K))
          M = IPOS(I,K) 
          DX2 = TX(K)%VALUES(M) - XX(I,K)
          IF (DX2 >= ZERO)THEN
            DO N = M-1,1,-1
              DX2 = TX(K)%VALUES(N) - XX(I,K)
              IF (DX2 < ZERO .OR. N <=1)THEN 
                IPOS(I,K) = MAX(N,1)
                EXIT
              ENDIF
            END DO
          ELSE
            DO N = M+1,NXK(K)
              DX2 = TX(K)%VALUES(N) - XX(I,K)
              IF (DX2 >= ZERO .OR. N == NXK(K)) THEN
                IPOS(I,K) = N-1
                EXIT
              ENDIF
            END DO
          END IF
        END DO
      END DO
c---------------------
      SELECT CASE(NDIM)
c---------------------
      CASE(1)
c-----
        DO I=1,NEL        
          N  = IPOS(I,1)
          X1 = TX(1)%VALUES(N)
          X2 = TX(1)%VALUES(N+1)
          Y1 = TY%VALUES(N)
          Y2 = TY%VALUES(N+1)
          R1 = (X2 - XX(I,1)) / (X2 - X1)
          UNR1 = ONE - R1
          YY(I)   =  R1*Y1  + UNR1*Y2
          DYDX1(I)= (Y2 - Y1) / (X2 - X1)
         END DO
c-----
      CASE(2)
c-----
        N1 = NXK(1)                                                         
        DO I=1,NEL                                                          
          IL1 = IPOS(I,1)                                                   
          IL2 = IPOS(I,2)                                                   
          DO M=0,1                                                          
            IM = N1*(IL2 - 1 + M)                                           
            IB(1,M+1,I) = IM + IL1                                          
            IB(2,M+1,I) = IM + IL1 + 1                                      
          END DO                                                            
        END DO                                                              
c
        IF (ISMOOTH == 1) THEN       ! linear interpolation                   
c                                                                           
          DO I=1,NEL
            I1 = IPOS(I,1)
            I2 = I1 + 1
            J1 = IPOS(I,2)
            J2 = J1 + 1
            YA1  = TY%VALUES(IB(1,1,I))                                     
            YB1  = TY%VALUES(IB(2,1,I))                                     
            YA2  = TY%VALUES(IB(1,2,I))                                     
            YB2  = TY%VALUES(IB(2,2,I))                                     
            X1_1 = TX(1)%VALUES(I1)                                  
            X1_2 = TX(1)%VALUES(I2)                                
            X2_1 = TX(2)%VALUES(J1)                                  
            X2_2 = TX(2)%VALUES(J2)
c            
            R1   = (X1_2 - XX(I,1)) / (X1_2 - X1_1)
            R2   = (X2_2 - XX(I,2)) / (X2_2 - X2_1)
            UNR1 = ONE - R1                                                  
            UNR2 = ONE - R2                                                              
c                                         
            Y1   = R1*YA1 + UNR1*YB1                                
            Y2   = R1*YA2 + UNR1*YB2                                
c                                                                           
            YY(I)    = R2*Y1 + UNR2*Y2                                         
            DYDX1(I) = (R2*(YB1 - YA1) + UNR2*(YB2 - YA2)) / (X1_2 - X1_1)  
            DYDX2(I) = (Y2 - Y1) / (X2_2 - X2_1)                            
          END DO                                                            
c                                                                            
        ELSE IF (ISMOOTH == 2) THEN       ! logarythmic interpolation base 10                                    
c                                                                           
          DO I=1,NEL
            I1 = IPOS(I,1)
            I2 = I1 + 1
            J1 = IPOS(I,2)
            J2 = J1 + 1
            YA1  = TY%VALUES(IB(1,1,I))                                     
            YB1  = TY%VALUES(IB(2,1,I))                                     
            YA2  = TY%VALUES(IB(1,2,I))                                     
            YB2  = TY%VALUES(IB(2,2,I))                                     
            X1_1 = TX(1)%VALUES(I1)                                  
            X1_2 = TX(1)%VALUES(I2)                                
            X2_1 = TX(2)%VALUES(J1)                                  
            X2_2 = TX(2)%VALUES(J2)
c            
            R1   = (X1_2 - XX(I,1)) / (X1_2 - X1_1)
            R2   = (LOG10(X2_2) - LOG10(XX(I,2))) / (LOG10(X2_2) - LOG10(X2_1))
            UNR1 = ONE - R1                                                  
            UNR2 = ONE - R2                                                              
c                                         
            Y1   = R1*YA1 + UNR1*YB1                                
            Y2   = R1*YA2 + UNR1*YB2                                
c                                                                           
            YY(I)    = R2*Y1 + UNR2*Y2                                         
            DYDX1(I) = (R2*(YB1 - YA1) + UNR2*(YB2 - YA2)) / (X1_2 - X1_1)  
            DYDX2(I) = (Y2 - Y1) / (X2_2 - X2_1)                            
          END DO                                                            
c
        ELSE IF (ISMOOTH == 3) THEN       ! logarythmic interpolation base n                                   
c                                                                           
          DO I=1,NEL
            I1 = IPOS(I,1)
            I2 = I1 + 1
            J1 = IPOS(I,2)
            J2 = J1 + 1
            YA1  = TY%VALUES(IB(1,1,I))                                     
            YB1  = TY%VALUES(IB(2,1,I))                                     
            YA2  = TY%VALUES(IB(1,2,I))                                     
            YB2  = TY%VALUES(IB(2,2,I))                                     
            X1_1 = TX(1)%VALUES(I1)                                  
            X1_2 = TX(1)%VALUES(I2)                                
            X2_1 = TX(2)%VALUES(J1)                                  
            X2_2 = TX(2)%VALUES(J2)
c            
            R1   = (X1_2 - XX(I,1)) / (X1_2 - X1_1)
            R2   = (LOG(X2_2) - LOG(XX(I,2))) / (LOG(X2_2) - LOG(X2_1))
            UNR1 = ONE - R1                                                  
            UNR2 = ONE - R2                                                              
c                                         
            Y1   = R1*YA1 + UNR1*YB1                                
            Y2   = R1*YA2 + UNR1*YB2                                
c                                                                           
            YY(I)    = R2*Y1 + UNR2*Y2                                         
            DYDX1(I) = (R2*(YB1 - YA1) + UNR2*(YB2 - YA2)) / (X1_2 - X1_1)  
            DYDX2(I) = (Y2 - Y1) / (X2_2 - X2_1)                            
          END DO                                                            
c
        END IF                                                              
c-----------
      END SELECT
c-----------
      RETURN
      END SUBROUTINE TABLE2D_VINTERP_LOG
