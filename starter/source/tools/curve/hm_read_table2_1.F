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
      !||    hm_read_table2_1           ../starter/source/tools/curve/hm_read_table2_1.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_table2             ../starter/source/tools/curve/hm_read_table.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||    table_mod                  ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_TABLE2_1(NTABLE, NTABLE1, TABLE ,L,NFUNCT,UNITAB, LSUBMODEL )       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTABLE,NFUNCT,NTABLE1,L
      TYPE(TTABLE) ,INTENT(INOUT) :: TABLE(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IDFUN(NTABLE)
      INTEGER :: I,J,K,N,ID, NDIM, NX(4), NY, STAT, NOK(4)
      INTEGER,DIMENSION(:)  ,ALLOCATABLE :: JPERM1,JPERM2
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITAG
      INTEGER :: IERROR,FUNC_ID,NFUN,IDEB,IFIN,IOK 
      INTEGER :: KK,LL,NN,NF,NP,N1,N11,N12,N13,KK1
      INTEGER :: I1,I2,NXX,countx
      my_real :: X0,X1,X2,X3,YY,Y1,Y2,R,XMIN,XMAX,SCALEY
      my_real :: X234(3)
      my_real,DIMENSION(:),ALLOCATABLE :: XX, XSTOR1, XSTOR2
      integer ,DIMENSION(NTABLE) :: indx1,indx2,indx3
      my_real ,DIMENSION(NTABLE) :: XX1,XX2,XX3,combinations
      my_real ,DIMENSION(:,:),ALLOCATABLE :: XV2
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C======================================================================|
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/TABLE/1')

      DO I=1,NTABLE1 

       CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                         OPTION_TITR = TITR, 
     .                         OPTION_ID = ID) 
       CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
       CALL HM_GET_INTV('ORDER', NDIM, IS_AVAILABLE, LSUBMODEL)
       IF(NDIM/=1.AND.NDIM/=2.AND.NDIM/=3.AND.NDIM/=4)THEN
         CALL ANCMSG(MSGID=777,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=LL,
     .               C1=TITR)
       END IF
       NX(1) = 0
       NX(2) = 0
       NX(3) = 0
       NX(4) = 0

       IF(NDIM==1)THEN
         CYCLE
       END IF
       L=L+1
       TABLE(L)%NOTABLE=ID
       TABLE(L)%NDIM=NDIM
       ALLOCATE(TABLE(L)%X(NDIM),STAT=stat)
       IF(STAT/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='TABLE')
       CALL HM_GET_INTV('curverows', NFUN, IS_AVAILABLE, LSUBMODEL) !number of functions
       IF (NFUN  == 1) THEN
           CALL ANCMSG(MSGID=778,  MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=NFUN)
       END IF
       NXX=0
       ! check function IDs   
       ! count sum of all X abscissa values for all functions
       NXX  = 0                                                                                       
       IDFUN(:) = 0
       DO J = 1,NFUN
          CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID,J,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('A'               ,X1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
          CALL HM_GET_FLOAT_ARRAY_INDEX('B'               ,X2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
          CALL HM_GET_FLOAT_ARRAY_INDEX('C'               ,X3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
          DO NF = 1,NTABLE
            IF (TABLE(NF)%NOTABLE == FUNC_ID) THEN
              IDFUN(J) = FUNC_ID
              XX1(J) = X1
              XX2(J) = X2
              XX3(J) = X3
              NXX = NXX + SIZE(TABLE(NF)%X(1)%VALUES)                                                                       
              EXIT
            END IF
          END DO                          
          IF (IDFUN(J) == 0) THEN
            CALL ANCMSG(MSGID=781, MSGTYPE=MSGERROR, ANMODE=ANINFO,    
     .                  I1=ID,  C1=TITR, I2=FUNC_ID)        
          END IF
       ENDDO ! J = 1,NFUN

       ALLOCATE (XX(NXX))
       ALLOCATE (XSTOR1(NXX))
       ALLOCATE (XSTOR2(NFUN))
       ALLOCATE (JPERM1(NXX))
       ALLOCATE (JPERM2(NFUN))
       ALLOCATE (XV2(NFUN,NDIM))

       NXX  = 0                                                                                       
       XMIN =  EP30                                                                                   
       XMAX = -EP30                                                                                   
       DO J = 1,NFUN                                                                                  
         CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID ,J,IS_AVAILABLE,LSUBMODEL)           
         DO NF=1,NTABLE                                                                               
           IF (TABLE(NF)%NOTABLE == FUNC_ID) THEN                                                          
             DO NP=1,SIZE(TABLE(NF)%X(1)%VALUES)                                                      
               NXX = NXX+1                                                                          
               XX(NXX) = TABLE(NF)%X(1)%VALUES(NP)                                                   
               XMIN = MIN(XMIN,XX(NXX))                                                              
               XMAX = MAX(XMAX,XX(NXX))                                                              
             END DO                                                                                   
             EXIT                                                                                     
           END IF                                                                                     
         END DO                                                                                       
       END DO  !J = 1, NFUN    
C
       CALL MYQSORT(NXX,XX,JPERM1,IERROR)     

       ! skip double abscissa values
       countx = 1                               
       X1 = XX(countx)                          
       DO K = 2,NXX                              
         X1 = XX(K)                            
         IF (XX(K) > XX(countx)) THEN                 
           countx = countx+1                         
           XX(countx) = XX(K)                    
         END IF                                  
       END DO
       NXX = countx                                   
C
       ALLOCATE(TABLE(L)%X(1)%VALUES(nxx),STAT=stat)        
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR)           

       TABLE(L)%X(1)%VALUES(1:nxx) = xx(1:nxx)                       
C         
       IF (NDIM > 0) XV2(1:NFUN,1) = XX1(1:NFUN)
       IF (NDIM > 1) XV2(1:NFUN,2) = XX2(1:NFUN)
       IF (NDIM > 2) XV2(1:NFUN,3) = XX3(1:NFUN)

       DO N=1,NDIM-1
         CALL MYQSORT(NFUN,XV2(1,N),JPERM2,IERROR)           
         DO K=1,NFUN                                         
           XSTOR2(K) = XV2(K,N)
         END DO                                               
         NX(N+1)  = 1                                           
         XV2(1,N) = XSTOR2(1)                                   
         DO K=2,NFUN                                       
           X2 = XSTOR2(K)                                       
           IF (X2 > XV2(NX(N+1),N)) THEN                        
             NX(N+1) = NX(N+1)+1                                
             XV2(NX(N+1),N) = X2                                
           END IF                                             
         END DO                                               
       END DO                                                 
C
C pas d'interpolation/extrapolation dans les autres directions.
       IF (NFUN /= NX(2)*MAX(1,NX(3))*MAX(1,NX(4))) THEN    
         CALL ANCMSG(MSGID=784, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)                            
       END IF                                            
C
       DO N=1,NDIM-1                                            
         ALLOCATE(TABLE(L)%X(N+1)%VALUES(NX(N+1)),STAT=stat)    
         IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='TABLE')                          
       END DO                                                   
       DO N=1,NDIM-1                                            
         DO K=1,NX(N+1)                                         
           TABLE(L)%X(N+1)%VALUES(K)=XV2(K,N)                   
         END DO                                                 
       END DO                                                   
!----------------------------------------------------------------------------------------------
       ! check variable combinations consistency and repeated values in all dimensions
       COMBINATIONS(:) = 0
       indx1(:) = 0
       indx2(:) = 0
       indx3(:) = 0
       DO J = 1,NFUN                                                                                  
         CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID ,J,IS_AVAILABLE,LSUBMODEL)           
         CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('B'              ,X2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('C'              ,X3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
         IDFUN(J) = FUNC_ID
         XX1(J) = X1
         XX2(J) = X2
         XX3(J) = X3
       END DO
!
       DO J=1,NFUN
         do k = 1,nx(2)
           if (XX1(J) == XV2(k,1)) indx1(j) = k
         end do
         do k = 1,nx(3)
           if (XX2(J) == XV2(k,2)) indx2(j) = k
         end do
         do k = 1,nx(4)
           if (XX3(J) == XV2(k,3)) indx3(j) = k
         end do
       END DO
       DO J=1,NFUN
         COMBINATIONS(J) = indx1(j)*nfun**2 + indx2(j)*nfun + indx3(j)
       END DO
         
        CALL MYQSORT(nfun,COMBINATIONS,JPERM2,IERROR)          

       X1 =  COMBINATIONS(1) 
       DO J=2,NFUN
         X2 = COMBINATIONS(J)
         i1 = JPERM2(j-1)
         i2 = JPERM2(j)
         if (x1 == x2) then
           CALL ANCMSG(MSGID=3081, MSGTYPE=MSGERROR, ANMODE=ANINFO, 
     .                 I1=ID,C1=TITR, I2=IDFUN(i1),I3=IDFUN(i2))                            
         end if
         x1 = x2
       END DO
!----------------------------------------------------------------------------------------------
C
       DEALLOCATE(XX,XSTOR1,JPERM1,XV2,XSTOR2,JPERM2)

       DO J = 1, NFUN
          CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID ,J,IS_AVAILABLE,LSUBMODEL)          
          CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X2      ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)   
       END DO !J = 1, NFUN
C
       ALLOCATE(TABLE(L)%Y,STAT=stat)                           
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='TABLE')                          

       NY=NX(1)                                                 
       DO N=2,NDIM                                              
         NY = NY*NX(N)                                            
       END DO                                                   
       ALLOCATE(TABLE(L)%Y%VALUES(NY),STAT=stat)                
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='TABLE')                          

       ALLOCATE(ITAG(NX(1),NX(2)*MAX(1,NX(3))*MAX(1,NX(4))), STAT=stat)                                      
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='ITAG')                           
       ITAG=0                                                   
       DO J = 1, NFUN
         CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',FUNC_ID,J,IS_AVAILABLE,LSUBMODEL)           
         CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X234(1),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('B'              ,X234(2),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('C'              ,X234(3),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('Fscale_array'   ,SCALEY ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)    

C dicho
          IF(SCALEY == ZERO) SCALEY = ONE 
          NOK=0                                                   
          DO N=2,NDIM                                             
            IDEB=1                                                
            IFIN=NX(N)                                            
            IOK=0                                                 
            DO WHILE(IOK==0)                                      
              IF(IFIN-IDEB==1)THEN                                
                K=IDEB                                            
                IF(TABLE(L)%X(N)%VALUES(K)==X234(N-1))THEN        
                ELSE                                              
                  K=K+1                                           
                END IF                                            
                IOK=1                                             
              ELSE                                                
                K=(IDEB+IFIN)/2                                   
                IF(TABLE(L)%X(N)%VALUES(K) > X234(N-1))THEN       
                  IFIN=K                                          
                ELSEIF(TABLE(L)%X(N)%VALUES(K) < X234(N-1))THEN   
                  IDEB=K                                          
                ELSE                                              
                  IOK=1                                           
                END IF                                            
              END IF                                              
              NOK(N)=K                                            
            END DO                                                
          END DO                                                  
C
C aiguille
          DO NF=1,NTABLE                                              
            IF(TABLE(NF)%NOTABLE==FUNC_ID)THEN                         
              NOK(1)=1                                                
              DO NP=1,SIZE(TABLE(NF)%X(1)%VALUES)                     
                X1=TABLE(NF)%X(1)%VALUES(NP)                          
                DO WHILE(X1 > TABLE(L)%X(1)%VALUES(NOK(1)))           
                  NOK(1)=NOK(1)+1                                     
                END DO                                                
                NN=1                                                  
                KK=NOK(1)                                             
                DO N=2,NDIM                                           
                  NN=NN*NX(N-1)                                       
                  KK=NN*(NOK(N)-1)+KK                                 
                END DO                                                
                TABLE(L)%Y%VALUES(KK)=TABLE(NF)%Y%VALUES(NP)*SCALEY   
                KK= MAX(1,NX(3))*NX(2)*(MAX(1,NOK(4))-1)              
     .             +NX(2)*(MAX(1,NOK(3))-1)                           
     .             +NOK(2)                                            
                ITAG(NOK(1),KK)=1
              END DO                                                  
              EXIT                                                    
            END IF                                                    
          END DO                                                      
       END DO!J = 1, NFUN
C-------------------------------------------------------------------------------
C interpolation
       DO KK=1,NX(2)*MAX(1,NX(3))*MAX(1,NX(4))    
         N11=1                                    
         DO WHILE(ITAG(N11,KK)==0)                
           N11=N11+1                              
         END DO                                   
         N12=N11+1                                
         DO WHILE(ITAG(N12,KK)==0)                
           N12=N12+1                              
           IF(N12 > NX(1))THEN                    
c error (fonction 1 seul pt)
           END IF                           
         END DO                             
         X1=TABLE(L)%X(1)%VALUES(N11)       
         X2=TABLE(L)%X(1)%VALUES(N12)       
         KK1=NX(1)*(KK-1)+N11               
         Y1=TABLE(L)%Y%VALUES(KK1)          
         KK1=NX(1)*(KK-1)+N12               
         Y2=TABLE(L)%Y%VALUES(KK1)          
         DO N1=1,N12                        
           IF(N1/=N11) THEN                 
             X0=TABLE(L)%X(1)%VALUES(N1)    
             R=(X2-X0)/(X2-X1)              
             YY=R*Y1+(ONE-R)*Y2              
             KK1=NX(1)*(KK-1)+N1            
             TABLE(L)%Y%VALUES(KK1)=YY      
             ITAG(N1,KK)=1                  
           END IF                           
         END DO                             
 200     CONTINUE                         
         N13=N12+1                        
c         DO WHILE(ITAG(N13,KK)==0)       
c           N13=N13+1                     
c           IF(N13 > NX(1))EXIT           
c         END DO !pmo mw                  
         DO WHILE(N13 <= NX(1))           
           IF (ITAG(N13,KK) == 0) THEN    
             N13=N13+1                    
           ELSE                           
             EXIT                         
           ENDIF                          
         END DO                           
         IF(N13 > NX(1))THEN              
            X1=TABLE(L)%X(1)%VALUES(N11)    
            X2=TABLE(L)%X(1)%VALUES(N12)    
            KK1=NX(1)*(KK-1)+N11            
            Y1=TABLE(L)%Y%VALUES(KK1)       
            KK1=NX(1)*(KK-1)+N12            
            Y2=TABLE(L)%Y%VALUES(KK1)       
            DO N1=N12+1,NX(1)               
              X0=TABLE(L)%X(1)%VALUES(N1)   
              R=(X2-X0)/(X2-X1)             
              YY=R*Y1+(ONE-R)*Y2             
              KK1=NX(1)*(KK-1)+N1           
              TABLE(L)%Y%VALUES(KK1)=YY     
              ITAG(N1,KK)=1                 
            END DO                          
         ELSE                                
             N11=N12                           
             N12=N13                           
             IF(N12 > N11+1)THEN               
               X1=TABLE(L)%X(1)%VALUES(N11)    
               X2=TABLE(L)%X(1)%VALUES(N12)    
               KK1=NX(1)*(KK-1)+N11            
               Y1=TABLE(L)%Y%VALUES(KK1)       
               KK1=NX(1)*(KK-1)+N12            
               Y2=TABLE(L)%Y%VALUES(KK1)       
               DO N1=N11+1,N12-1               
                 X0=TABLE(L)%X(1)%VALUES(N1)   
                 R=(X2-X0)/(X2-X1)             
                 YY=R*Y1+(ONE-R)*Y2             
                 KK1=NX(1)*(KK-1)+N1           
                 TABLE(L)%Y%VALUES(KK1)=YY     
                 ITAG(N1,KK)=1                 
               END DO                          
             END IF                            
             GO TO 200                         
         END IF                              
       END DO !KK
       DEALLOCATE(ITAG)

       NY=SIZE(TABLE(L)%Y%VALUES)
       IF (IS_ENCRYPTED)THEN                                     
         WRITE(IOUT,'(A)')'CONFIDENTIAL DATA'
       ELSE
         WRITE(IOUT,2000) NTABLE1
         WRITE(IOUT,2100) TABLE(L)%NOTABLE, TABLE(L)%NDIM
         DO K=1,TABLE(L)%NDIM
           NX(K)=SIZE( TABLE(L)%X(K)%VALUES )
           WRITE(IOUT,2200) K
           WRITE(IOUT,2250) (TABLE(L)%X(K)%VALUES(N),N=1,NX(K))
         END DO
         NY=SIZE(TABLE(L)%Y%VALUES)
         WRITE(IOUT,2300)
         WRITE(IOUT,2350) (TABLE(L)%Y%VALUES(N),N=1,NY)
       END IF

      END DO  
      RETURN
C-----------------------------------------------------------------
2000  FORMAT(//
     .        '    TABLES'/
     .        '    ------'/
     .        '    NUMBER OF TABLES . . . . . . . . . . =',I10/)
2100  FORMAT(/'    TABLE ID . . . . . . . . . . . . . . =',I10/
     .        '    NUMBER OF PARAMETERS . . . . . . . . =',I10/)
2200  FORMAT(/'    VALUES FOR PARAMETER NUMBER. . . . . .',I4,':'/)
2250  FORMAT((3X,5(1X,G20.13))/)
2300  FORMAT(/'    ORDINATE VALUES . . . . . . . . . . . :'/)
2350  FORMAT((3X,5(1X,G20.13))/)
C-----------------------------------------------------------------
      END
