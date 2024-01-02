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
Chd|  HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|-- called by -----------
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_TABLE2_1(NTABLE, NTABLE1, TABLE ,L,NFUNCT,UNITAB, LSUBMODEL )       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTABLE,NFUNCT,NTABLE1,L
C     REAL
      TYPE(TTABLE) ,INTENT(INOUT) :: TABLE(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real, INTENT(IN) :: UNITAB(LUNIT, NUNITS)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITYPE, IBID, IDTAB(NTABLE)
      INTEGER I, J, K, II, N, ID, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, IDS, LL, LX(2), NOK(4)
      INTEGER,DIMENSION(:),ALLOCATABLE :: JPERM1,JPERM2
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITAG
      INTEGER IERROR, NF, IDFUNC, NP, NFUN, 
     .        KK, IDEB, IFIN, IOK, NN, N1, N2, N11, N12, N13, KK1
C     REAL
      my_real
     .   BID, F5(5), XX, X1, X2, X234(3), YY, Y1, Y2, R, XMIN, XMAX,YFAC_UNIT,
     .    TIME, FUNCT,SCALEY
      my_real,
     .        DIMENSION(2) :: XD2
      my_real,
     .        DIMENSION(:),ALLOCATABLE :: XV1, XSTOR1, XSTOR2
      my_real,
     .        DIMENSION(:,:),ALLOCATABLE :: XV2
      CHARACTER TITR*nchartitle, MESS*40, KEY*ncharfield
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C======================================================================|
!     Initialization
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
       LX(1)=0
       LX(2)=0
       CALL HM_GET_INTV('curverows', NFUN, IS_AVAILABLE, LSUBMODEL) !number of functions
       IF (NFUN  == 1) THEN
           CALL ANCMSG(MSGID=778,  MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=NFUN)
       END IF
       
       DO J = 1, NFUN
          CALL HM_GET_INT_ARRAY_INDEX ('tableentityarray',IDFUNC,J,IS_AVAILABLE,LSUBMODEL)
          LX(2)=LX(2)+1
          DO NF=1,NTABLE
             IF (TABLE(NF)%NOTABLE==IDFUNC) THEN
               LX(1)=LX(1)+SIZE(TABLE(NF)%X(1)%VALUES)
               GO TO 110
             END IF                                       
          END DO                          
 100      CONTINUE                        
          CALL ANCMSG(MSGID=781,          
     .                  MSGTYPE=MSGERROR, 
     .                  ANMODE=ANINFO,    
     .                  I1=ID,            
     .                  C1=TITR,          
     .                  I2=IDFUNC)        
 110      CONTINUE                       

       ENDDO !J = 1, NFUN
       ALLOCATE(XV1(LX(1)),XSTOR1(LX(1)),JPERM1(LX(1)),
     .       XV2(LX(2),NDIM),XSTOR2(LX(2)),JPERM2(LX(2)),
     .       STAT=stat)
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                MSGTYPE=MSGERROR)

       LX(1)=0                                                                                        
       LX(2)=0                                                                                        
       XMIN =  EP30                                                                                   
       XMAX = -EP30                                                                                   
       DO J = 1, NFUN                                                                                  
         CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',IDFUNC ,J,IS_AVAILABLE,LSUBMODEL)           
         CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X234(1),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('B'              ,X234(2),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         CALL HM_GET_FLOAT_ARRAY_INDEX ('C'              ,X234(3),J,IS_AVAILABLE,LSUBMODEL,UNITAB)    
         LX(2)=LX(2)+1                                                                                
         DO N=1,NDIM-1                                                                                
          XV2(LX(2),N)=X234(N)                                                                        
         END DO                                                                                       
         DO NF=1,NTABLE                                                                               
           IF(TABLE(NF)%NOTABLE==IDFUNC)THEN                                                          
             DO NP=1,SIZE(TABLE(NF)%X(1)%VALUES)                                                      
               LX(1)=LX(1)+1                                                                          
               XV1(LX(1))=TABLE(NF)%X(1)%VALUES(NP)                                                   
               XMIN=MIN(XMIN,XV1(LX(1)))                                                              
               XMAX=MAX(XMAX,XV1(LX(1)))                                                              
             END DO                                                                                   
             EXIT                                                                                     
           END IF                                                                                     
         END DO                                                                                       
       END DO  !J = 1, NFUN    
C
       CALL MYQSORT(LX(1),XV1,JPERM1,IERROR)     
       DO K=1,LX(1)                              
        XSTOR1(K)=XV1(K)                         
       END DO                                    
       NX(1)    =1                               
       XV1(NX(1))=XSTOR1(1)                      
       DO K=2,LX(1)                              
         X1=XSTOR1(K)                            
         IF(X1 > XV1(NX(1)))THEN                 
           NX(1)=NX(1)+1                         
           XV1(NX(1))=X1                         
         END IF                                  
       END DO                                    
C
       ALLOCATE(TABLE(L)%X(1)%VALUES(NX(1)),STAT=stat)        
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,    
     .                            MSGTYPE=MSGERROR)           
       DO K=1,NX(1)                                           
         TABLE(L)%X(1)%VALUES(K)=XV1(K)                       
       END DO                                                 
C
       DO N=1,NDIM-1                                          
         CALL MYQSORT(LX(2),XV2(1,N),JPERM2,IERROR)           
         DO K=1,LX(2)                                         
          XSTOR2(K)=XV2(K,N)                                  
         END DO                                               
         NX(N+1) =1                                           
         XV2(1,N)=XSTOR2(1)                                   
         DO K=2,LX(2)                                         
           X2=XSTOR2(K)                                       
           IF(X2 > XV2(NX(N+1),N))THEN                        
             NX(N+1)=NX(N+1)+1                                
             XV2(NX(N+1),N)=X2                                
           END IF                                             
         END DO                                               
       END DO                                                 
C
C pas d'interpolation/extrapolation dans les autres directions.
       IF(LX(2)/=NX(2)*MAX(1,NX(3))*MAX(1,NX(4)))THEN    
         CALL ANCMSG(MSGID=784,                          
     .               MSGTYPE=MSGERROR,                   
     .               ANMODE=ANINFO,                      
     .               I1=ID,                              
     .               C1=TITR)                            
       END IF                                            
C
       DO N=1,NDIM-1                                            
         ALLOCATE(TABLE(L)%X(N+1)%VALUES(NX(N+1)),STAT=stat)    
         IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,    
     .                              MSGTYPE=MSGERROR,           
     .                     C1='TABLE')                          
       END DO                                                   
       DO N=1,NDIM-1                                            
         DO K=1,NX(N+1)                                         
           TABLE(L)%X(N+1)%VALUES(K)=XV2(K,N)                   
         END DO                                                 
       END DO                                                   
C
       DEALLOCATE(XV1,XSTOR1,JPERM1,XV2,XSTOR2,JPERM2)

       DO J = 1, NFUN
          CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',IDFUNC ,J,IS_AVAILABLE,LSUBMODEL)          
          CALL HM_GET_FLOAT_ARRAY_INDEX ('A'              ,X2     ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)   
       END DO !J = 1, NFUN
C
       ALLOCATE(TABLE(L)%Y,STAT=stat)                           
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,      
     .                            MSGTYPE=MSGERROR,             
     .                     C1='TABLE')                          

       NY=NX(1)                                                 
       DO N=2,NDIM                                              
         NY=NY*NX(N)                                            
       END DO                                                   
       ALLOCATE(TABLE(L)%Y%VALUES(NY),STAT=stat)                
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,      
     .                            MSGTYPE=MSGERROR,             
     .                     C1='TABLE')                          

       ALLOCATE(ITAG(NX(1),NX(2)*MAX(1,NX(3))*MAX(1,NX(4))),    
     .          STAT=stat)                                      
       IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,      
     .                            MSGTYPE=MSGERROR,             
     .                     C1='ITAG')                           
       ITAG=0                                                   
       DO J = 1, NFUN
         CALL HM_GET_INT_ARRAY_INDEX  ('tableentityarray',IDFUNC ,J,IS_AVAILABLE,LSUBMODEL)           
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
            IF(TABLE(NF)%NOTABLE==IDFUNC)THEN                         
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
C
       END DO!J = 1, NFUN
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
             XX=TABLE(L)%X(1)%VALUES(N1)    
             R=(X2-XX)/(X2-X1)              
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
              XX=TABLE(L)%X(1)%VALUES(N1)   
              R=(X2-XX)/(X2-X1)             
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
                 XX=TABLE(L)%X(1)%VALUES(N1)   
                 R=(X2-XX)/(X2-X1)             
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
      END
