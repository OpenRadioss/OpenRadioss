Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE STAT_S_STRAF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,
     3                        STAT_INDXS,X,IGLOB,IPART,SIZP0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0,IGLOB
      INTEGER IXS(NIXS,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTS(*), IPART_STATE(*), STAT_INDXS(*),IPART(LIPART1,*)
      my_real
     .   X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,II,JJ,LEN,NLAY,NPTR,NPTS,NPTT,
     .   ISOLNOD,ISTRAIN,NG, NEL, MLW, ID, IPRT0, IPRT,IE, 
     .   NPG,IPG,IPT,IL,IR,IS,IT,IPID,PID,IOFF,KK(8)
      INTEGER PTWA(STAT_NUMELS), PTWA_P0(0:MAX(1,STAT_NUMELS_G))
      my_real
     .   GAMA(6),WATMP(6)
      CHARACTER*100 DELIMIT,LINE
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C----  
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
C     8 NODES BRICK
C======================================================================|
      JJ = 0
      IF(STAT_NUMELS==0) GOTO 200

      IE=0
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        MLW   =IPARG(1,NG) 
        NEL   =IPARG(2,NG) 
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
c        JHBE    = IPARG(23,NG)
        ISTRAIN = IPARG(44,NG)          
        LFT = 1
        LLT = NEL
        IPRT=IPARTS(LFT+NFT)
        PID = IPART(2,IPRT)
        JHBE   = IGEO(10,PID)   
!
        DO I=1,8  ! lenght max of GBUF%G_STRA = 8
          KK(I) = NEL*(I-1)
        ENDDO
!
        IF (ITY == 1) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IPRT=IPARTS(LFT+NFT)
          PID = IPART(2,IPRT)
C          JHBE   = IGEO(10,PID)
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          NLAY = ELBUF_TAB(NG)%NLAY                       
          NPTR = ELBUF_TAB(NG)%NPTR                        
          NPTS = ELBUF_TAB(NG)%NPTS                        
          NPTT = ELBUF_TAB(NG)%NPTT                        
          NPT  = NPTR * NPTS * NPTT * NLAY
c
          IF (JCVT==1.AND.ISORTH/=0) JCVT=2
c          

          IF (ISOLNOD == 16) THEN
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              II = (I-1)*6                                                  
              IPRT=IPARTS(N)                                                
              IF(IPART_STATE(IPRT)==0)CYCLE                                 
              WA(JJ+ 1)= GBUF%VOL(I)                                        
              WA(JJ+ 2)= IPRT                                               
              WA(JJ+ 3)= IXS(NIXS,N)                                        
              WA(JJ+ 4)= NLAY                                               
              WA(JJ+ 5)= NPTR                                               
              WA(JJ+ 6)= NPTS                                               
              WA(JJ+ 7)= NPTT                                               
              WA(JJ+ 8)= ISOLNOD                                            
              WA(JJ+ 9)= JHBE                                               
              WA(JJ+10)= IGTYP 
              WA(JJ+11) = GBUF%OFF(I)                                             
              JJ = JJ + 11                                                  
              IF (IGLOB == 1)THEN                                           
                IF(JCVT==2)THEN                                
                  GAMA(1)=GBUF%GAMA(KK(1)+I)				       
                  GAMA(2)=GBUF%GAMA(KK(2)+I)				       
                  GAMA(3)=GBUF%GAMA(KK(3)+I)				       
                  GAMA(4)=GBUF%GAMA(KK(4)+I)				       
                  GAMA(5)=GBUF%GAMA(KK(5)+I)				       
                  GAMA(6)=GBUF%GAMA(KK(6)+I)				       
                ELSE                                                        
                  GAMA(1)=ONE                                                
                  GAMA(2)=ZERO                                              
                  GAMA(3)=ZERO                                              
                  GAMA(4)=ZERO                                              
                  GAMA(5)=ONE                                                
                  GAMA(6)=ZERO                                              
                END IF                                                      
              ENDIF                                                         
c---
              IS = 1
              DO IT=1,NPTT                                               
               DO IR=1,NPTR                                            
                 DO IL=1,NLAY                                                 
                    LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)      
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)
                    IF (IGLOB == 1)                                               
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP)   
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)
                    JJ = JJ + 6                                        
                 ENDDO                                                  
                ENDDO                                                    
               ENDDO
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            ENDDO 
          ELSEIF (ISOLNOD == 20) THEN
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              II = (I-1)*6                                                  
              IPRT=IPARTS(N)                                                
              IF(IPART_STATE(IPRT)==0)CYCLE                                 
              WA(JJ+ 1)= GBUF%VOL(I)                                        
              WA(JJ+ 2)= IPRT                                               
              WA(JJ+ 3)= IXS(NIXS,N)                                        
              WA(JJ+ 4)= NLAY                                               
              WA(JJ+ 5)= NPTR                                               
              WA(JJ+ 6)= NPTS                                               
              WA(JJ+ 7)= NPTT                                               
              WA(JJ+ 8)= ISOLNOD                                            
              WA(JJ+ 9)= JHBE                                               
              WA(JJ+10)= IGTYP
              WA(JJ+11) = GBUF%OFF(I)                                                                                           
              JJ = JJ + 11                                                  
              IF (IGLOB == 1)THEN                                           
                IF(JCVT==2)THEN                                
                  GAMA(1)=GBUF%GAMA(KK(1)+I)				       
                  GAMA(2)=GBUF%GAMA(KK(2)+I)				       
                  GAMA(3)=GBUF%GAMA(KK(3)+I)				       
                  GAMA(4)=GBUF%GAMA(KK(4)+I)				       
                  GAMA(5)=GBUF%GAMA(KK(5)+I)				       
                  GAMA(6)=GBUF%GAMA(KK(6)+I)				       
                ELSE                                                        
                  GAMA(1)=ONE                                                
                  GAMA(2)=ZERO                                              
                  GAMA(3)=ZERO                                              
                  GAMA(4)=ZERO                                              
                  GAMA(5)=ONE                                                
                  GAMA(6)=ZERO                                              
                END IF                                                      
              ENDIF                                                         
c---
              IL = 1
              DO IT=1,NPTT                                               
               DO IS=1,NPTS                                            
                 DO IR=1,NPTR                                                 
                    LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)      
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)
                    IF (IGLOB == 1)                                               
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP)   
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)
                    JJ = JJ + 6                                        
                 ENDDO                                                  
                ENDDO                                                    
               ENDDO
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            ENDDO 
  
          ELSEIF (IGTYP == 22) THEN
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              II = (I-1)*6                                                  
              IPRT=IPARTS(N)                                                
              IF(IPART_STATE(IPRT)==0)CYCLE                                 
              WA(JJ+ 1)= GBUF%VOL(I)                                        
              WA(JJ+ 2)= IPRT                                               
              WA(JJ+ 3)= IXS(NIXS,N)                                        
              WA(JJ+ 4)= NLAY                                               
              WA(JJ+ 5)= NPTR                                               
              WA(JJ+ 6)= NPTS                                               
              WA(JJ+ 7)= NPTT                                               
              WA(JJ+ 8)= ISOLNOD                                            
              WA(JJ+ 9)= JHBE                                               
              WA(JJ+10)= IGTYP       
              WA(JJ+11) = GBUF%OFF(I)                                                                                    
              JJ = JJ + 11                                                  
              IF (IGLOB == 1)THEN                                           
                IF(JCVT==2)THEN                                
                  GAMA(1)=GBUF%GAMA(KK(1)+I)				       
                  GAMA(2)=GBUF%GAMA(KK(2)+I)				       
                  GAMA(3)=GBUF%GAMA(KK(3)+I)				       
                  GAMA(4)=GBUF%GAMA(KK(4)+I)				       
                  GAMA(5)=GBUF%GAMA(KK(5)+I)				       
                  GAMA(6)=GBUF%GAMA(KK(6)+I)				       
                ELSE                                                        
                  GAMA(1)=ONE                                                
                  GAMA(2)=ZERO                                              
                  GAMA(3)=ZERO                                              
                  GAMA(4)=ZERO                                              
                  GAMA(5)=ONE                                                
                  GAMA(6)=ZERO                                              
                END IF                                                      
              ENDIF                                                         
c---
              DO IR=1,NPTR                                               
               DO IS=1,NPTS                                             
                DO IT=1,NPTT                                           
                 DO IL=1,NLAY                                                 
                    LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)      
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)
                    IF (IGLOB == 1)                                               
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP)   
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)
                    JJ = JJ + 6                                                   
                  ENDDO                                                
                 ENDDO                                                  
                ENDDO                                                    
               ENDDO
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            ENDDO                                                           
          ELSEIF (IGTYP == 43) THEN
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              II = (I-1)*3                                                  
              IPRT = IPARTS(N)                                                
              IF (IPART_STATE(IPRT)==0) CYCLE                                 
              WA(JJ+ 1)= GBUF%VOL(I)                                        
              WA(JJ+ 2)= IPRT                                               
              WA(JJ+ 3)= IXS(NIXS,N)                                        
              WA(JJ+ 4)= NLAY                                               
              WA(JJ+ 5)= NPTR                                               
              WA(JJ+ 6)= NPTS                                               
              WA(JJ+ 7)= NPTT                                               
              WA(JJ+ 8)= ISOLNOD                                            
              WA(JJ+ 9)= JHBE                                               
              WA(JJ+10)= IGTYP       
              WA(JJ+11) = GBUF%OFF(I)                                                                                    
              JJ = JJ + 11                                                  
              GAMA(1)=ONE                                                
              GAMA(2)=ZERO                                              
              GAMA(3)=ZERO                                              
              GAMA(4)=ZERO                                              
              GAMA(5)=ONE                                                
              GAMA(6)=ZERO                                              
c---
              DO IR=1,NPTR                                               
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,1,1)      
                WATMP(1) = ZERO
                WATMP(2) = ZERO
                WATMP(3) = LBUF%EPE(KK(1)+I)
                WATMP(4) = ZERO
                WATMP(5) = LBUF%EPE(KK(2)+I)
                WATMP(6) = LBUF%EPE(KK(3)+I)
                IF(IGLOB==1)CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP)
                WA(JJ + 1) = WATMP(1)
                WA(JJ + 2) = WATMP(2)
                WA(JJ + 3) = WATMP(3)
                WA(JJ + 4) = WATMP(4)
                WA(JJ + 5) = WATMP(5)
                WA(JJ + 6) = WATMP(6)
                JJ = JJ + 6                                                   
              ENDDO
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            ENDDO                                                           
          ELSEIF (ISTRAIN == 0) THEN
            DO I=LFT,LLT                                                    
              N  = I + NFT                                                  
              II = (I-1)*6                                                  
              IPRT=IPARTS(N)                                                
              IF(IPART_STATE(IPRT)==0)CYCLE                                 
              WA(JJ+ 1)= GBUF%VOL(I)                                        
              WA(JJ+ 2)= IPRT                                               
              WA(JJ+ 3)= IXS(NIXS,N)                                        
              WA(JJ+ 4)= NLAY                                               
              WA(JJ+ 5)= NPTR                                               
              WA(JJ+ 6)= NPTS                                               
              WA(JJ+ 7)= NPTT                                               
              WA(JJ+ 8)= ISOLNOD                                            
              WA(JJ+ 9)= JHBE                                               
              WA(JJ+10)= IGTYP   
              WA(JJ+11) = GBUF%OFF(I)                                                                                        
              JJ = JJ + 11                                                  
              DO IPT=1,NPT                      
                WA(JJ + 1 ) = ZERO              
                WA(JJ + 2 ) = ZERO              
                WA(JJ + 3 ) = ZERO              
                WA(JJ + 4 ) = ZERO              
                WA(JJ + 5 ) = ZERO              
                WA(JJ + 6 ) = ZERO              
                JJ = JJ + 6                     
              ENDDO                             
              IE=IE+1                                                       
C             pointeur de fin de zone dans WA                                 
              PTWA(IE)=JJ                                                   
            ENDDO                                                           
c
          ELSEIF (IGTYP == 20 .OR. IGTYP == 21) THEN
c
            DO I=LFT,LLT                                                                
              N  = I + NFT                                                              
              II = (I-1)*6                                                              
              IPRT=IPARTS(N)                                                            
              IF(IPART_STATE(IPRT)==0)CYCLE                                             
              WA(JJ+ 1)= GBUF%VOL(I)                                                    
              WA(JJ+ 2)= IPRT                                                           
              WA(JJ+ 3)= IXS(NIXS,N)                                                    
              WA(JJ+ 4)= NLAY                                                           
              WA(JJ+ 5)= NPTR                                                           
              WA(JJ+ 6)= NPTS                                                           
              WA(JJ+ 7)= NPTT                                                           
              WA(JJ+ 8)= ISOLNOD                                                        
              WA(JJ+ 9)= JHBE                                                           
              WA(JJ+10)= IGTYP    
              WA(JJ+11) = GBUF%OFF(I)                                                                                                   
              JJ = JJ + 11                                                              
              IF (IGLOB == 1)THEN                                                       
                IF(JCVT==2)THEN                                            
                  GAMA(1)=GBUF%GAMA(KK(1)+I)						   
                  GAMA(2)=GBUF%GAMA(KK(2)+I)						   
                  GAMA(3)=GBUF%GAMA(KK(3)+I)						   
                  GAMA(4)=GBUF%GAMA(KK(4)+I)						   
                  GAMA(5)=GBUF%GAMA(KK(5)+I)						   
                  GAMA(6)=GBUF%GAMA(KK(6)+I)						   
                ELSE                                                                    
                  GAMA(1)=ONE                                                            
                  GAMA(2)=ZERO                                                          
                  GAMA(3)=ZERO                                                          
                  GAMA(4)=ZERO                                                          
                  GAMA(5)=ONE                                                            
                  GAMA(6)=ZERO                                                          
                END IF                                                                  
              ENDIF                                                                     
c---
               DO IL=1,NLAY                                                             
                DO IR=1,NPTR                                                            
                 DO IS=1,NPTS                                                           
                  DO IT=1,NPTT                                                          
                    LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)    
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)		    
                    IF (IGLOB == 1)                                                     
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP)  
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)               
                    JJ = JJ + 6                                                         
                  ENDDO                                                                 
                 ENDDO                                                                  
                ENDDO                                                                   
               ENDDO                                                                    
              IE=IE+1                                                                   
C             pointeur de fin de zone dans WA                                           
              PTWA(IE)=JJ                                                               
            ENDDO                                                                       
c
c          ELSEIF (ISOLNOD == 8 .AND. (JHBE == 14 .OR. JHBE == 17) .OR.                  
c     .           (ISOLNOD == 6 .OR. ISOLNOD == 8) .AND. JHBE == 15.OR.                  
c     .           (ISOLNOD == 4 .AND. ISROT == 1 )) THEN                                 
c
          ELSEIF (JHBE == 12 .OR. JHBE == 14 .OR. JHBE == 17) THEN                          
            DO I=LFT,LLT                                                                
              N  = I + NFT                                                              
              II = (I-1)*6                                                              
              IPRT=IPARTS(N)                                                            
              IF(IPART_STATE(IPRT)==0)CYCLE                                             
              WA(JJ+ 1)= GBUF%VOL(I)                                                    
              WA(JJ+ 2)= IPRT                                                           
              WA(JJ+ 3)= IXS(NIXS,N)                                                    
              WA(JJ+ 4)= NLAY                                                           
              WA(JJ+ 5)= NPTR                                                           
              WA(JJ+ 6)= NPTS                                                           
              WA(JJ+ 7)= NPTT                                                           
              WA(JJ+ 8)= ISOLNOD                                                        
              WA(JJ+ 9)= JHBE                                                           
              WA(JJ+10)= IGTYP 
              WA(JJ+11) = GBUF%OFF(I)                                                                                                      
              IF (JHBE==17.AND.IINT==2) WA(JJ+ 9)=  18
              JJ = JJ + 11                                                              
              IF (IGLOB == 1)THEN                                                       
                IF(JCVT==2)THEN                                            
                  GAMA(1)=GBUF%GAMA(KK(1)+I)						   
                  GAMA(2)=GBUF%GAMA(KK(2)+I)						   
                  GAMA(3)=GBUF%GAMA(KK(3)+I)						   
                  GAMA(4)=GBUF%GAMA(KK(4)+I)						   
                  GAMA(5)=GBUF%GAMA(KK(5)+I)						   
                  GAMA(6)=GBUF%GAMA(KK(6)+I)						   
                ELSE                                                                    
                  GAMA(1)=ONE                                                            
                  GAMA(2)=ZERO                                                          
                  GAMA(3)=ZERO                                                          
                  GAMA(4)=ZERO                                                          
                  GAMA(5)=ONE                                                            
                  GAMA(6)=ZERO                                                          
                END IF                                                                  
              ENDIF                                                                     

              DO IL=1,NLAY                                               
               DO IT=1,NPTT
                DO IS=1,NPTS
                 DO IR=1,NPTR
                   LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)   
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)		      
                   IF (IGLOB == 1)                                                      
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP) 
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)              
                   JJ = JJ + 6                                                          
                 ENDDO                                                                  
                ENDDO                                                                   
               ENDDO                                                                    
              ENDDO                                                                     
              IE=IE+1                                                                   
c             pointeur de fin de zone dans WA                                           
              PTWA(IE)=JJ                                                               
            ENDDO                                                                       
c
          ELSE                                                                          
c
            DO I=LFT,LLT                                                                
              N  = I + NFT                                                              
              II = (I-1)*6                                                              
              IPRT=IPARTS(N)                                                            
              IF(IPART_STATE(IPRT)==0)CYCLE                                             
              WA(JJ+ 1)= GBUF%VOL(I)                                                    
              WA(JJ+ 2)= IPRT                                                           
              WA(JJ+ 3)= IXS(NIXS,N)                                                    
              WA(JJ+ 4)= NLAY                                                           
              WA(JJ+ 5)= NPTR                                                           
              WA(JJ+ 6)= NPTS                                                           
              WA(JJ+ 7)= NPTT                                                           
              WA(JJ+ 8)= ISOLNOD                                                        
              WA(JJ+ 9)= JHBE                                                           
              WA(JJ+10)= IGTYP   
              WA(JJ+11) = GBUF%OFF(I)                                                                                                    
              JJ = JJ + 11                                                              
              IF (IGLOB == 1)THEN                                                       
                IF(JCVT==2)THEN                                            
                  GAMA(1)=GBUF%GAMA(KK(1)+I)						   
                  GAMA(2)=GBUF%GAMA(KK(2)+I)						   
                  GAMA(3)=GBUF%GAMA(KK(3)+I)						   
                  GAMA(4)=GBUF%GAMA(KK(4)+I)						   
                  GAMA(5)=GBUF%GAMA(KK(5)+I)						   
                  GAMA(6)=GBUF%GAMA(KK(6)+I)						   
                ELSE                                                                    
                  GAMA(1)=ONE                                                            
                  GAMA(2)=ZERO                                                          
                  GAMA(3)=ZERO                                                          
                  GAMA(4)=ZERO                                                          
                  GAMA(5)=ONE                                                            
                  GAMA(6)=ZERO                                                          
                END IF                                                                  
              ENDIF                                                                     
c---
               DO IL=1,NLAY                                                             
                DO IR=1,NPTR                                                            
                 DO IS=1,NPTS                                                           
                  DO IT=1,NPTT                                                          
                    LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)  
                    WATMP(1) = LBUF%STRA(KK(1)+I)
                    WATMP(2) = LBUF%STRA(KK(2)+I)
                    WATMP(3) = LBUF%STRA(KK(3)+I)
                    WATMP(4) = LBUF%STRA(KK(4)+I)
                    WATMP(5) = LBUF%STRA(KK(5)+I)
                    WATMP(6) = LBUF%STRA(KK(6)+I)			
                    IF (IGLOB == 1)                                                     
     .             CALL SROTA6(X,IXS(1,N),JCVT,WATMP,GAMA,JHBE,IGTYP) 
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)               
                    JJ = JJ + 6                                                         
                  ENDDO                                                                 
                 ENDDO                                                                  
                ENDDO                                                                   
               ENDDO                                                                    
              IE=IE+1                                                                   
C             pointeur de fin de zone dans WA                                           
              PTWA(IE)=JJ                                                               
            ENDDO                                                                       
          ENDIF  ! ISOLNOD, JHBE                                                        
C                                                                                       
        ENDIF    ! ITY == 1
      ENDDO      ! NG=1,NGROUP 
 200  CONTINUE
c-----------------------------------------------------------
      IF(NSPMD == 1)THEN
Clm     recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELS
          PTWA_P0(N)=PTWA(N)
        END DO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELS,PTWA_P0,STAT_NUMELS_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF
c-----------------------------------------------------------
      IF(ISPMD == 0.AND.LEN.GT.0) THEN

        IPRT0=0
        DO N=1,STAT_NUMELS_G

C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXS(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
          IOFF    = NINT(WAP0(J + 11))      
          IPRT  = NINT(WAP0(J + 2)) 
          IF (IOFF >= 1) THEN
            IF(IPRT /= IPRT0)THEN
             IF (IZIPSTRS == 0) THEN
             WRITE(IUGEO,'(A)') DELIMIT
             IF(IGLOB == 1)THEN
               WRITE(IUGEO,'(A)')'/INIBRI/STRA_FGLO'
             ELSE
               WRITE(IUGEO,'(A)')'/INIBRI/STRA_F'
             ENDIF
             WRITE(IUGEO,'(A)')
     .      '#------------------------ REPEAT -------------------------'
             WRITE(IUGEO,'(A)')
     .      '#  BRICKID       NPT    ISOLNOD    ISOLID' 
             WRITE(IUGEO,'(A/A/A)')
     .      '# IF(NPT /= 0) REPEAT K=1,NPT ',
     .      '#    E1,  E2,  E3',
     .      '#   E12, E23, E31'
                   WRITE(IUGEO,'(A)')
     .      '#------------------------ REPEAT -------------------------'
              WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100)
               IF(IGLOB == 1)THEN 
                 WRITE(LINE,'(A)')'/INIBRI/STRA_FGLO'
                 CALL STRS_TXT50(LINE,100) 
               ELSE
                 WRITE(LINE,'(A)')'/INIBRI/STRA_F'
                 CALL STRS_TXT50(LINE,100) 
               ENDIF
               WRITE(LINE,'(A)')
     .      '#------------------------ REPEAT -------------------------'
               CALL STRS_TXT50(LINE,100)  
               WRITE(LINE,'(A)')
     .      '#  BRICKID      NPT    ISOLNOD    ISOLID'
               CALL STRS_TXT50(LINE,100)  
               WRITE(LINE,'(A)')
     .      '# IF(NPT /= 0) REPEAT K=1,NPT '
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#    E1,  E2,  E3'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')'#   E12, E23, E31'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)')
     .      '#------------------------ REPEAT -------------------------'
               CALL STRS_TXT50(LINE,100) 
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100) 
             END IF
              IPRT0=IPRT
            END IF
            ID      = NINT(WAP0(J + 3))          
            NLAY    = NINT(WAP0(J + 4))          
            NPTR    = NINT(WAP0(J + 5))          
            NPTS    = NINT(WAP0(J + 6))          
            NPTT    = NINT(WAP0(J + 7))          
            ISOLNOD = NINT(WAP0(J + 8))          
            JHBE    = NINT(WAP0(J + 9))          
            IGTYP   = NINT(WAP0(J +10))    
            NPT     = NLAY * NPTR * NPTS * NPTT  
c
            J = J + 11
c------------------------------------------------
            IF (ISOLNOD == 16) THEN
              IF (IZIPSTRS == 0) THEN 
                WRITE(IUGEO,'(8I10)') ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NPTT,NLAY
              ELSE
                WRITE(LINE,'(8I10)')  ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NPTT,NLAY
                CALL STRS_TXT50(LINE,100)
              ENDIF
              DO IPT = 1, NPT                                 
                IF (IZIPSTRS == 0) THEN                        
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6) 
                ELSE                                           
                  CALL TAB_STRS_TXT50(WAP0(1),6,J,SIZP0,3)    
                ENDIF                                          
                J = J + 6                                     
              ENDDO                  
c               
            ELSEIF (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22) THEN
              IF (IZIPSTRS == 0) THEN 
                WRITE(IUGEO,'(7I10)') ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NLAY
              ELSE
                WRITE(LINE,'(7I10)')  ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NLAY
                CALL STRS_TXT50(LINE,100)
              ENDIF
              DO IPT = 1, NPT                                 
                IF (IZIPSTRS == 0) THEN                        
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6) 
                ELSE                                           
                  CALL TAB_STRS_TXT50(WAP0(1),6,J,SIZP0,3)    
                ENDIF                                          
                J = J + 6                                     
              ENDDO                                        
c
            ELSEIF ( ((ISOLNOD == 8 .OR. NPT == 1) .AND.
     .               JHBE /= 14 .AND. JHBE /= 15)  .OR.
     .               (ISOLNOD == 4 .AND. NPT == 1)  )THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(4I10)') ID,NPT,ISOLNOD,JHBE
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6*NPT)
              ELSE
                WRITE(LINE,'(4I10)') ID,NPT,ISOLNOD,JHBE
                CALL STRS_TXT50(LINE,100)
                CALL TAB_STRS_TXT50(WAP0(1),6*NPT,J,SIZP0,3)
              ENDIF
              J = J + 6
C
            ELSEIF((ISOLNOD == 8 .AND. JHBE == 14) .OR.
     .             (ISOLNOD == 4 .AND. NPT == 4 )  .OR.
     .             (ISOLNOD == 10)                 .OR.
     .             (ISOLNOD == 20)                 .OR.
     .             ((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15).OR.
     .             ((ISOLNOD == 8) .AND. JHBE == 17) .OR.
     .             ((ISOLNOD == 8) .AND. JHBE == 18)) THEN 
              IF (IZIPSTRS == 0) THEN 
                WRITE(IUGEO,'(8I10)')ID,NPT,ISOLNOD,JHBE,
     .                               NPTR,NPTS,NPTT,NLAY
              ELSE
                WRITE(LINE,'(8I10)')ID,NPT,ISOLNOD,JHBE,
     .                              NPTR,NPTS,NPTT,NLAY
                CALL STRS_TXT50(LINE,100)
              ENDIF
c---
              DO IPT = 1, NPT                                 
                IF (IZIPSTRS == 0) THEN                        
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6) 
                ELSE                                           
                  CALL TAB_STRS_TXT50(WAP0(1),6,J,SIZP0,3)    
                ENDIF                                          
                J = J + 6                                     
              ENDDO                                           
            ENDIF
          ENDIF  !  IF (IOFF == 1)
c---
        ENDDO		    
      ENDIF
c-----------
      RETURN
      END
