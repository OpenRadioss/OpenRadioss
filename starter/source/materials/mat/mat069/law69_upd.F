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
!||    law69_upd              ../starter/source/materials/mat/mat069/law69_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    arret                  ../starter/source/system/arret.F
!||    law69_nlsqf            ../starter/source/materials/tools/nlsqf.F
!||    law69_nlsqf_auto       ../starter/source/materials/tools/law69_nlsqf_auto.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW69_UPD(IOUT,TITR    ,MAT_ID,UPARAM,NFUNC,NFUNCT,
     .                      IFUNC, FUNC_ID,NPC   ,PLD   ,PM,IPM, GAMA_INF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      INTEGER ,INTENT(IN) :: NFUNC
      INTEGER ,INTENT(IN) :: NFUNCT
      INTEGER NPC(*), FUNC_ID(NFUNCT) ,IPM(NPROPMI)
      my_real 
     .         UPARAM(*),PLD(*),PM(NPROPM)
      my_real , INTENT(IN)    ::  GAMA_INF
      INTEGER, DIMENSION(NFUNC):: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

c
      INTEGER N_NETWORK,N,K,ITEST,ICHECK,II,JJ,NSTART,IC1,IC2,NOGD,NDATA,NMULA,
     .        TAB,NMUAL,IFLAG,IS,I,INDX,LAWID, ITAG,IS_ENCRYPTED
      my_real
     .   MU(5),AL(5),LAM_MIN,LAM_MAX,STRAIN_MIN,STRAIN_MAX,
     .   D11,D22,D12,LAM1,LAM2,LAM3,LAM12,INVD1,INVD2
      my_real
     .   E,NU,GS,RBULK, D,YOUNG,
     .   ERRTOL,AVE_SLOPE,G,MU_MAX,MU_MIN,DX,LAM,BETA,
     .   AMULA(2),MUAL(10),DY
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
      INTEGER , DIMENSION(:), ALLOCATABLE :: ITAB_ON_A,INDEX
!             
C==================================================================== 
C=======================================================================
!       IDENTIFICATION
!====================================================================       

       IF(IFUNC(2) == 0 )  RETURN
C      
       NMUAL = 2*UPARAM(18)
       IFLAG = NINT(UPARAM(13))
       ICHECK = NINT(UPARAM(19))
       NSTART = NINT(UPARAM(20))
       IS_ENCRYPTED = NINT(UPARAM(21))
       ERRTOL = FIVEEM3 
C      
       IC1 = NPC(IFUNC(2))
       IC2 = NPC(IFUNC(2)+1)
C
       NOGD=(IC2-IC1)/2
       NDATA=NOGD
C
       ALLOCATE (STRETCH(NOGD))                              
       ALLOCATE (STRESS(NOGD))                           
C
       AVE_SLOPE = ZERO
       JJ=0                                                  
       STRETCH=ZERO                                          
       STRESS=ZERO                                           
       G=ZERO                                               
       RBULK=ZERO                                            
       GS=ZERO                                               
       LAM_MAX= ZERO                                         
       LAM_MIN= ZERO   
       ITAG  =  0   
       IS = IFUNC(2)                                   
       DO II = IC1,IC2-2,2                                   
            JJ=JJ+1                                          
            STRETCH(JJ) = PLD(II) + ONE                       
            STRESS(JJ)  = PLD(II+1)
            IF(PLD(II) < ZERO .AND. STRESS(JJ) < ZERO ) ITAG = 1
              IF(PLD(II) <=  -ONE) THEN
                CALL ANCMSG(MSGID=1175,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO,
     .                       I1=MAT_ID,
     .                       C1=TITR,
     .                       I2=FUNC_ID(IS)) ! Id_function 
                   CALL ARRET(2)
            ENDIF           
       ENDDO 
C  check if the curve is monotonic
        DO JJ =1,NOGD - 1                                 
          DX = STRETCH(JJ+1) - STRETCH(JJ)                
          DY = STRESS(JJ + 1) - STRESS(JJ) 
          IF(DX <= ZERO .OR. DY <= ZERO) THEN             
            CALL ANCMSG(MSGID=1176,                      
     .             MSGTYPE=MSGERROR,                   
     .             ANMODE=ANINFO,                      
     .             I1=MAT_ID,                          
     .             C1=TITR,                            
     .             I2=FUNC_ID(IS))                              
          ENDIF                                           
        ENDDO                                             
       AVE_SLOPE = ZERO                                   

       IF (IFLAG == 2 .AND. NMUAL /= 4) THEN              
        CALL ANCMSG(MSGID=126,                            
     .              MSGTYPE=MSGERROR,                   
     .              ANMODE=ANINFO_BLIND_1,              
     .              I1=MAT_ID,                          
     .              C1=TITR,                            
     .              I2=FUNC_ID(IS))                               
       ENDIF                                              
       DO II=1,5                                          
         MUAL(2*II-1)=FIVE                                
         MUAL(2*II)=TWO                                  
       ENDDO 
!      
        WRITE(IOUT,277)  
        WRITE(IOUT,270)TRIM(TITR),MAT_ID  
        IF(IS_ENCRYPTED > 0) WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
!       
       IF (IFLAG == 1 .OR. IFLAG == 2) THEN
           LAWID = IFLAG 
           CALL LAW69_NLSQF(LAWID,STRETCH,STRESS,10,NMUAL,NOGD,MUAL,
     $                             ICHECK, NSTART, ERRTOL,MAT_ID,TITR,IS_ENCRYPTED)
       ELSEIF(IFLAG == -1) THEN
           LAWID = 2
           IF(ITAG == 0) ICHECK = ABS(ICHECK)
           CALL LAW69_NLSQF_AUTO(LAWID,STRETCH,STRESS,10,NMUAL,NOGD,MUAL,
     $                             ICHECK, NSTART, ERRTOL,MAT_ID,TITR,IS_ENCRYPTED)
           UPARAM(18) = NMUAL / 2    
       ENDIF
       DEALLOCATE (STRETCH)                                                  
       DEALLOCATE (STRESS)                                                   
       IF(GAMA_INF < ONE) THEN
         DO II=1,5                                                           
           UPARAM(II)=MUAL(2*II-1)/GAMA_INF                                     
           UPARAM(II+5)=MUAL(2*II)                                      
           GS=GS + MUAL(2*II-1)*MUAL(2*II)                                     
         ENDDO 
         IF(IS_ENCRYPTED == 0) WRITE(IOUT,100)  MAT_ID
       ELSE 
         DO II=1,5                                                           
           UPARAM(II)=MUAL(2*II-1)                                      
           UPARAM(II+5)=MUAL(2*II)                                      
           GS=GS+MUAL(2*II-1)*MUAL(2*II)                                     
         ENDDO   
       ENDIF                                                              
       G=GS/TWO                                                            
       NU = UPARAM(14)                                   
       RBULK=GS*(ONE+UPARAM(14))                                         
     &       /MAX(EM30,THREE*(ONE-TWO*UPARAM(14)))                      
       UPARAM(11)=RBULK                                                 
       UPARAM(17)=GS                                                    
C parameters                                                                 
       YOUNG  = GS*(ONE + NU)                                                 
       PM(20) = YOUNG                                                   
       PM(21) = NU                                                      
       PM(22) = GS                                                      
       PM(24) = YOUNG/(ONE - NU**2)                                      
       PM(32) = GS                                                      
       PM(100) = RBULK 
C     Formulation for solid elements time step computation.
       IPM(252)= 2
       PM(105) = GS/(RBULK + TWO_THIRD*G)       
       
C                                                        
      IF(IS_ENCRYPTED == 0) THEN 
         IF (LAWID /= 2) THEN                                      
           WRITE(IOUT,278)                                                   
     &  UPARAM(1),UPARAM(2),UPARAM(3),UPARAM(4),         
     &  UPARAM(5),UPARAM(6),UPARAM(7),UPARAM(8),         
     &  UPARAM(9),UPARAM(10),G,RBULK                              
         ELSE                                                                
           WRITE(IOUT,279)                                                   
     &  UPARAM(1),UPARAM(2),UPARAM(6),UPARAM(7),         
     &  HALF*UPARAM(1),-HALF*UPARAM(2),G,RBULK                
         ENDIF    
       ENDIF                                                                 
c----------------
c     end of optimization loop
c----------------
C=======================================================================
!       CHECK the material stability (Drucker proguer conditions)
!====================================================================
       NDATA = 10000  ! La=0.1,10, EM3!!
       LAM_MIN= EM3                                         
       LAM_MAX= TEN 
C       
       DO I=1,5
         MU(I) = UPARAM(I)
         AL(I) = UPARAM(5+I)
       ENDDO  
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
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
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
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
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
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
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
        DEALLOCATE (STRETCH)                                                 
        DEALLOCATE (STRESS)                                
        DEALLOCATE (ITAB_ON_A)                                    
        DEALLOCATE (INDEX)    
C --------------------------------------------------------
      RETURN
 100  FORMAT
     & (//5X, 'MODIFIED  FITTED MATERIAL RIGIDITY    ' ,/,
     &    5X, ' ---------------------------', /,
     &    5X, 'MATERIAL LAW =  LAW69 ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//) 
 277  FORMAT
     & (//5X, 'FITTED PARAMETERS FOR HYPERELASTIC_MATERIAL LAW69' ,/,
     &    5X, ' ----------------------------------------')
 270  FORMAT(
     & 5X,A,/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 278  FORMAT(
     &  5X,  'OGDEN LAW PARAMETERS:',/
     & ,5X,  'MU1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU4 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU5 . . . . . . . . . . . . . . . . . .=',1PG20.13//
     & ,5X,  'AL1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL4 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL5 . . . . . . . . . . . . . . . . . .=',1PG20.13//
     & ,5X,  'INITIAL SHEAR MODULUS . . . . . . . . .=',1PG20.13/
     & ,5X,  'BULK MODULUS. . . . . . . . . . . . . .=',1PG20.13//)
 279   FORMAT(
     &  5X,  'MOONEY-RIVLIN LAW PARAMETERS:',/
     & ,5X,  'MU1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'MU2 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'AL2 . . . . . . . . . . . . . . . . . .=',1PG20.13//
     & ,5X,  'C10 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'C01 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'INITIAL SHEAR MODULUS . . . . . . . . .=',1PG20.13/
     & ,5X,  'BULK MODULUS. . . . . . . . . . . . . .=',1PG20.13//)
 1000 FORMAT
     & (//5X, 'CHECK THE DRUCKER PRAGER STABILITY CONDITIONS   ' ,/,
     &    5X, ' -----------------------------------------------', /,
     &    5X, 'MATERIAL LAW = OGDEN (LAW69) ',/,
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
     & 8X,'TENSION:        UNSTABLE AT A NOMINAL STRAIN LARGER THAN ',1PG20.13//)
c-----------
      END
