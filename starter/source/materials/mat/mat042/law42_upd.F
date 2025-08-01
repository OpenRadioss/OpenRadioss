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
!||    law42_upd              ../starter/source/materials/mat/mat042/law42_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW42_UPD(MAT_PARAM,IOUT,TITR    ,MAT_ID,PM, GAMA_INF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE)  :: TITR
      INTEGER MAT_ID,IOUT
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM  
      my_real , INTENT(IN)  :: GAMA_INF
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NDATA,INDX,NORDER
      my_real :: MU(10),AL(10)
      my_real :: LAM_MIN,LAM_MAX,STRAIN_MIN,STRAIN_MAX,
     .   D11,D22,D12,LAM1,LAM2,LAM3,LAM12,INVD1,INVD2,GS,BULK,YOUNG,NU
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
      INTEGER , DIMENSION(:), ALLOCATABLE :: ITAB_ON_A,INDEX

C=======================================================================
!       CHECK the material stability (Drucker proguer conditions)
!====================================================================
      NDATA = 10000  ! La=0.1,10, EM3!!
      LAM_MIN = EM3                                         
      LAM_MAX = TEN
        
      NORDER = MAT_PARAM%IPARAM(1)   ! Order of Ogden model
C      
       IF (GAMA_INF <  ONE) THEN 
         GS = ZERO
         DO I=1,NORDER
           MU(I) = MAT_PARAM%UPARAM(I) / GAMA_INF
           AL(I) = MAT_PARAM%UPARAM(10+I)
           GS = GS + MU(I)*AL(I)
           MAT_PARAM%UPARAM(I) = MU(I) 
         ENDDO 
         NU = MAT_PARAM%UPARAM(22)
         BULK = GS*(ONE+NU)/MAX(EM20,THREE*(ONE-TWO*NU)) 
         MAT_PARAM%UPARAM(21) = BULK
         !! parameters 
          YOUNG   = GS*(ONE + NU)                                             
          PM(20)  = YOUNG                                               
          PM(22)  = GS !! TWO*G                                                 
          PM(24)  = YOUNG/(ONE - NU**2)                                  
          PM(32)  = BULK                                               
          PM(100) = BULK 
C     Formulation for solid elements time step computation.
         PM(105) = GS/(BULK + TWO_THIRD*GS) 
         !!
         WRITE(IOUT,2000) MAT_ID
         WRITE(IOUT,2100) HALF*GS,BULK
         WRITE(IOUT,2200) MU(1),MU(2),MU(3),MU(4),MU(5),MU(6),MU(7),MU(8),MU(9),MU(10)          
       ELSE
         DO I=1,NORDER
           MU(I) = MAT_PARAM%UPARAM(I)
           AL(I) = MAT_PARAM%UPARAM(10+I)
         ENDDO  
       ENDIF 
C
       ALLOCATE (STRETCH(NDATA))                              
       ALLOCATE (STRESS(NDATA))                                  
       ALLOCATE (ITAB_ON_A(NDATA))                                    
       ALLOCATE (INDEX(NDATA))                                                                           
       STRETCH(1)=LAM_MIN
       ITAB_ON_A = 0
       DO K= 2,NDATA
         STRETCH(K)=STRETCH(K-1) + EM3
       ENDDO                                         
       STRESS=ZERO  
C                                                            
       WRITE(IOUT,1000)MAT_ID  
C  Tension/compression      
       INDX =0                
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = ONE/SQRT(LAM1)
          LAM3 = LAM2
C            
          DO K=1,NORDER 
            LAM12 = (lAM1*LAM2)**(-AL(K))
            D11 = D11 + AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
            D22 = D22 + AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
            D12 = D12 + AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
           ENDIF  
       ENDDO 
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1010)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF  
       ENDIF
C  Biaxial      
       INDX =0                                 
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = LAM1
          LAM3 = ONE/LAM1/LAM1
C            
          DO K=1,NORDER 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11 = D11 + AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22 = D22 + AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12 = D12 + AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX + 1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I 
           ENDIF  
       ENDDO
       IF(INDX > 0 .AND. INDX < NDATA) THEN
         WRITE(IOUT,1020)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF 
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF 
       ENDIF
C shear test
       INDX =0                                 
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = ONE
          LAM3 = ONE/LAM1
C            
          DO K=1,NORDER 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11 = D11 + AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22 = D22 + AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12 = D12 + AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
           ENDIF  
       ENDDO
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1030)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF
       ENDIF 
C             
        DEALLOCATE (STRETCH)                                                 
        DEALLOCATE (STRESS)                                 
        DEALLOCATE (ITAB_ON_A)                                    
        DEALLOCATE (INDEX)  
      RETURN
 1000 FORMAT
     & (//5X, 'CHECK THE DRUCKER PRAGER STABILITY CONDITIONS   ' ,/,
     &    5X, ' -----------------------------------------------', /,
     &    5X, 'MATERIAL LAW = OGDEN (LAW42) ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1010 FORMAT(
     & 7X,'TEST TYPE = UNIXIAL  ')
 1020 FORMAT(//,
     & 7X,'TEST TYPE = BIAXIAL  ')
 1030 FORMAT(//,
     & 7X,'TEST TYPE = PLANAR (SHEAR)')
 1100 FORMAT(
     & 8X,'COMPRESSION:    UNSTABLE AT A NOMINAL STRAIN LESS THAN ',1PG20.13)
 1200 FORMAT(
     & 8X,'TENSION:        UNSTABLE AT A NOMINAL STRAIN LARGER THAN ',1PG20.13)
 2000 FORMAT
     & (//5X, 'MODIFIED  MATERIAL RIGIDITY    ' ,/,
     &    5X, ' ---------------------------', /,
     &    5X, 'MATERIAL LAW =  LAW42 ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//) 
 2100 FORMAT (
     & 5X,'INITIAL SHEAR MODULUS . . . . . . . . .=',E12.4/
     & 5X,'INITIAL BULK MODULUS. . .. . . . . . .=',E12.4//)
 2200 FORMAT
     & (5X,  'MU1 . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     &  5X,  'MU2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU4 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU5 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU6 . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     &  5X,  'MU7 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU8 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU9 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     &  5X,  'MU10. . . . . . . . . . . . . . . . . .=',1PG20.13/)
c-----------
      END
