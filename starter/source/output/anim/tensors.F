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
Chd|  TENSORS                       source/output/anim/tensors.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        INITBUF                       source/output/anim/initbuf.F  
Chd|        SROTA6                        source/output/anim/srota6.F   
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENSORS(ELBUF_TAB,IPARG   ,ITENS   ,IXS     ,PM      ,    
     2                   EL2FA   ,NBF     ,TENS    ,                      
     3                   X       ,IPART   ,IPARTSP ,ISPH3D  ,IPM     )                      
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"                                          
C-----------------------------------------------                        
C   C o m m o n   B l o c k s                                           
C-----------------------------------------------                        
#include      "vect01_c.inc"                                            
#include      "mvsiz_p.inc"                                            
#include      "com01_c.inc"                                             
#include      "com04_c.inc"                                             
#include      "param_c.inc"                                             
#include      "scr17_c.inc"
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   TENS(6,*),PM(NPROPM,*),X(3,*)                         
      INTEGER IPARG(NPARG,*),ITENS, ISPH3D,NBF,                                    
     .   IXS(NIXS,*),EL2FA(*),IPM(NPROPMI,*),IPART(LIPART1,*),IPARTSP(*)                         
      REAL WA(6*NBF)                                                    
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   EVAR(6,MVSIZ),OFF, FAC, A1, A2, A3, THK, GAMA(6)               
      REAL R4(18)                                                       
      INTEGER I, II,NG, NEL, IPT,MT1,NLAY,IL,                                      
     .   N, J, MLW,IALEL,NN1,NN2,NN3,IPRT, KCVT,ISOLNOD,                                       
     .   NPTR, NPTS, NPTT,NPTG, IS, IR, IT,                                  
     .   JHBE, JIVF, JCLOSE, JPLASOL, IREP, IGTYP,          
     .   ICSEN, ISORTHG, IFAILURE, IINT,JJ(6)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
C=======================================================================      
      DO J=1,18                                                       
        R4(J) = ZERO                                                  
      ENDDO                                                          
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = NN2 + NUMELS                                                
C                                                                       
      DO 490 NG=1,NGROUP                                               
        ISOLNOD = IPARG(28,NG)                                          
        CALL INITBUF (IPARG    ,NG      ,                               
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,             
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,             
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,             
     5        JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,             
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,             
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE)                      
        LFT=1                                                           
        LLT=NEL                                                         
!
        DO J=1,6  ! length max of GBUF%L_STRA = 6
          JJ(J) = (J-1)*NEL
        ENDDO
!
C-----------------------------------------------                        
C       SOLID 8N                                                        
C-----------------------------------------------                        
        IF (ITY == 1) THEN                                                
          GBUF => ELBUF_TAB(NG)%GBUF
          NLAY = ELBUF_TAB(NG)%NLAY                
          NPTR = ELBUF_TAB(NG)%NPTR                 
          NPTS = ELBUF_TAB(NG)%NPTS                 
          NPTT = ELBUF_TAB(NG)%NPTT
          NPTG = NPTT*NPTS*NPTR
          NPT  = NPTG*NLAY
          MT1  = IXS(1,1 + NFT)                                            
          IF (KCVT==1 .AND. ISORTH/=0) KCVT=2                             
C-----------------------------------------------                        
          IF (ITENS == 1)THEN                                            
C           STRESS                                                       
C-----------------------------------------------                        
            DO I=LFT,LLT
              N = I + NFT                                               
              EVAR(1,I) = GBUF%SIG(JJ(1) + I)         
              EVAR(2,I) = GBUF%SIG(JJ(2) + I)         
              EVAR(3,I) = GBUF%SIG(JJ(3) + I)         
              EVAR(4,I) = GBUF%SIG(JJ(4) + I)         
              EVAR(5,I) = GBUF%SIG(JJ(5) + I)         
              EVAR(6,I) = GBUF%SIG(JJ(6) + I)         
            ENDDO                                                       
            IF (KCVT/=0) THEN                                         
C             STRESS TENSOR IN GLOBAL SYSTEM                             
              DO I=LFT,LLT                                               
                N = I + NFT                                              
                IF(EL2FA(NN2+N)/=0)THEN                                
C                 pour JHBE=14, valeurs moyennes est dans rep. corota.
                  IF (KCVT==2.AND.JHBE/=14) THEN
                    GAMA(1)=GBUF%GAMA(JJ(1) + I)
                    GAMA(2)=GBUF%GAMA(JJ(2) + I)
                    GAMA(3)=GBUF%GAMA(JJ(3) + I)
                    GAMA(4)=GBUF%GAMA(JJ(4) + I)
                    GAMA(5)=GBUF%GAMA(JJ(5) + I)
                    GAMA(6)=GBUF%GAMA(JJ(6) + I)
                  ELSE                                                   
                    GAMA(1)=ONE                                           
                    GAMA(2)=ZERO                                         
                    GAMA(3)=ZERO                                         
                    GAMA(4)=ZERO                                         
                    GAMA(5)=ONE                                           
                    GAMA(6)=ZERO                                         
                  END IF                                                 
                  CALL SROTA6(X,IXS(1,N),KCVT,EVAR(1,I),GAMA)                                    
                ENDIF                                                    
              ENDDO                                                      
            ENDIF                                                       
C-----------------------------------------------
          ELSEIF(ITENS == 2)THEN                                        
C           STRAIN
C-----------------------------------------------
            DO I=LFT,LLT                                                
              DO J=1,6                                                   
                EVAR(J,I) = ZERO                                          
              ENDDO                                                      
            ENDDO                                                       
c
            IF (ISOLNOD == 8.AND.NPT == 8.AND.JHBE/=14.AND.             
     .          JHBE/=24.AND.JHBE/=15 )THEN                           
              IF (MLW >= 28) THEN                                           
                DO I=LFT,LLT                                               
                  N  = I + NFT                                                
                  DO J=1,8                                                  
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,J)                                        
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1)+I)*ONE_OVER_8
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2)+I)*ONE_OVER_8
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3)+I)*ONE_OVER_8
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4)+I)*ONE_OVER_8
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5)+I)*ONE_OVER_8
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6)+I)*ONE_OVER_8
                  ENDDO                                                     
                ENDDO                                                      
              ENDIF                                                        
c
            ELSEIF(ISOLNOD == 8 .AND. NPT == 1 .AND.                           
     .             JHBE/=14.AND.JHBE/=15.AND.JHBE/=24)THEN        
              IF (MLW>=28) THEN                                           
                 LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                        
                 DO I=LFT,LLT                                            
                   N  = I + NFT                                           
                   EVAR(1,I) = LBUF%STRA(JJ(1)+I)*ONE_OVER_8
                   EVAR(2,I) = LBUF%STRA(JJ(2)+I)*ONE_OVER_8
                   EVAR(3,I) = LBUF%STRA(JJ(3)+I)*ONE_OVER_8
                   EVAR(4,I) = LBUF%STRA(JJ(4)+I)*ONE_OVER_8
                   EVAR(5,I) = LBUF%STRA(JJ(5)+I)*ONE_OVER_8
                   EVAR(6,I) = LBUF%STRA(JJ(6)+I)*ONE_OVER_8
                 ENDDO                                                    
              ELSEIF(MLW == 14)THEN                                      
                 DO I=LFT,LLT                                             
                   N = I + NFT                                             
                   EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1)+I)
                   EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2)+I)
                   EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3)+I)       
                 ENDDO                                                      
              ELSEIF(MLW == 24)THEN                                      
                DO I=LFT,LLT                                             
                  N = I + NFT                                              
                  EVAR(1,I) = LBUF%STRA(JJ(1)+I)
                  EVAR(2,I) = LBUF%STRA(JJ(2)+I)
                  EVAR(3,I) = LBUF%STRA(JJ(3)+I)         
                  EVAR(4,I) = LBUF%STRA(JJ(4)+I)*HALF
                  EVAR(5,I) = LBUF%STRA(JJ(5)+I)*HALF
                  EVAR(6,I) = LBUF%STRA(JJ(6)+I)*HALF   
                ENDDO                                                    
              ENDIF                                                     
c
            ELSEIF (ISOLNOD == 16 .OR. ISOLNOD == 20 .OR.       
     .             (ISOLNOD == 8 .AND. JHBE == 14)) THEN                                    
              IF (MLW>=28) THEN                                            
                DO I=LFT,LLT                                               
                  DO IL=1,NLAY                     
                   DO IS=1,NPTS                     
                    DO IT=1,NPTT
                     DO IR=1,NPTR                      
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)       
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1)+I)/NPTG
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2)+I)/NPTG
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3)+I)/NPTG
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4)+I)/NPTG
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5)+I)/NPTG
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6)+I)/NPTG
                     ENDDO 
                    ENDDO
                   ENDDO
                  ENDDO
                ENDDO                                                   
              ELSEIF (MLW == 14) THEN                                        
                DO I=LFT,LLT                                           
                  N = I + NFT                                              
                  DO IL=1,NLAY                                             
                   DO IS=1,NPTS                                            
                    DO IT=1,NPTT                                           
                     DO IR=1,NPTR                                          
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                       EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1)+I)/NPTG        
                       EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2)+I)/NPTG        
                       EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3)+I)/NPTG        
                     ENDDO                                                 
                    ENDDO                                                  
                   ENDDO                                                   
                  ENDDO                                                    
                ENDDO                                                   
              ELSEIF(MLW == 24)THEN                                      
                DO I=LFT,LLT                                           
                  N = I + NFT                                           
                  DO IL=1,NLAY                     
                   DO IS=1,NPTS                     
                    DO IT=1,NPTT
                     DO IR=1,NPTR                      
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_STRA > 0) THEN
                       EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1)+I)/NPTG
                       EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2)+I)/NPTG
                       EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3)+I)/NPTG
                       EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4)+I)/NPTG
                       EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5)+I)/NPTG
                       EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6)+I)/NPTG
                      ENDIF
                     ENDDO 
                    ENDDO
                   ENDDO
                  ENDDO
                ENDDO                                                  
              ENDIF                                                        
c
           ELSEIF (ISOLNOD == 10) THEN                                     
             IF(MLW>=28)THEN                                            
               DO I=LFT,LLT                                                
                 N = I + NFT                                                
                 DO IPT=1,NPT                                               
                   LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                   EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1)+I)/NPT        
                   EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2)+I)/NPT        
                   EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3)+I)/NPT        
                   EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4)+I)/NPT        
                   EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5)+I)/NPT        
                   EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6)+I)/NPT        
                 ENDDO                                                      
              ENDDO                                                       
             ELSEIF(MLW == 14)THEN                                        
               DO I=LFT,LLT                                               
                 N = I + NFT                                              
                 DO IPT=1,NPT                                          
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)    
                      EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1)+I)/NPT       
                      EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2)+I)/NPT       
                      EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3)+I)/NPT       
                 ENDDO                                                 
               ENDDO                                                      
             ELSEIF (MLW == 24) THEN                                        
               DO I=LFT,LLT                                               
                 N = I + NFT                                              
                   DO IPT=1,NPT
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)        
                     EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1)+I)/NPT
                     EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2)+I)/NPT
                     EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3)+I)/NPT
                     EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4)+I)/NPT
                     EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5)+I)/NPT
                     EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6)+I)/NPT
                   ENDDO
                ENDDO                                                     
             ENDIF                                                        
           ELSEIF ((ISOLNOD == 6.OR.ISOLNOD == 8) .AND. JHBE == 15) THEN                                   
             IF (MLW>=28) THEN                                                 
                DO I=LFT,LLT                                                     
                  N  = I + NFT                                                   
                  DO IL= 1,NLAY                                                  
                    DO IPT=1,NPTG                                                
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)             
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1)+I)/NPTG        
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2)+I)/NPTG        
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3)+I)/NPTG        
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4)+I)/NPTG        
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5)+I)/NPTG        
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6)+I)/NPTG        
                    ENDDO                                                        
                  ENDDO                                                          
                ENDDO                                                           
             ELSEIF(MLW == 14)THEN                                        
               DO I=LFT,LLT                                                
                 N = I + NFT                                                
                 DO IL= 1,NLAY                                            
                   DO IPT=1,NPTG                                          
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)       
                     EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1)+I)/NPTG        
                     EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2)+I)/NPTG        
                     EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3)+I)/NPTG        
                   ENDDO                                                  
                 ENDDO                                                    
               ENDDO                                                       
             ELSEIF (MLW == 24) THEN                                        
               DO I=LFT,LLT                                               
                 N = I + NFT                                              
                 DO IL= 1,NLAY                                              
                   DO IPT=1,NPTG                                             
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)           
                     EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1)+I)/NPTG
                     EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2)+I)/NPTG
                     EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3)+I)/NPTG
                     EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4)+I)/NPTG     
                     EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5)+I)/NPTG     
                     EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6)+I)/NPTG     
                   ENDDO                                                     
                 ENDDO                                                      
               ENDDO                                                     
             ENDIF                                                        
           ENDIF                                                        
C-----------------------------------------------                        
          ELSEIF (ITENS == 4.AND.MLW == 24.AND.                              
     .            NINT(PM(56,MT1)) == 1) THEN              
C           CRACKS                                                       
C-----------------------------------------------                        
            LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
            DO I=LFT,LLT                                                 
              EVAR(1,I) = LBUF%DGLO(JJ(1)+I)        
              EVAR(2,I) = LBUF%DGLO(JJ(2)+I)        
              EVAR(3,I) = LBUF%DGLO(JJ(3)+I)        
              EVAR(4,I) = LBUF%DGLO(JJ(4)+I)        
              EVAR(5,I) = LBUF%DGLO(JJ(5)+I)        
              EVAR(6,I) = LBUF%DGLO(JJ(6)+I)        
            ENDDO                                                        
            IF (KCVT/=0) THEN                                          
C             DAMAGE IN GLOBAL SYSTEM                                    
              DO I=LFT,LLT                                               
                N = I + NFT                                              
                IF(EL2FA(NN2+N)/=0)THEN                                
                  IF(KCVT==2)THEN                                        
                    GAMA(1)= GBUF%GAMA(JJ(1) + I)  
                    GAMA(2)= GBUF%GAMA(JJ(2) + I)  
                    GAMA(3)= ZERO             
                    GAMA(4)=-GAMA(2)          
                    GAMA(5)= GAMA(1)          
                    GAMA(6)= ZERO             
                  ELSE                                                   
                    GAMA(1)=ONE                                           
                    GAMA(2)=ZERO                                         
                    GAMA(3)=ZERO                                         
                    GAMA(4)=ZERO                                         
                    GAMA(5)=ONE                                           
                    GAMA(6)=ZERO                                         
                  END IF                                                 
                  CALL SROTA6(X,IXS(1,N),KCVT,EVAR(1,I),GAMA)                                    
                ENDIF                                                    
              ENDDO                                                      
            ENDIF                                                        
C--------         
          ELSE                                                          
C--------         
            DO I=LFT,LLT                                                
              N = I + NFT                                               
              EVAR(1,I) = ZERO                                          
              EVAR(2,I) = ZERO                                          
              EVAR(3,I) = ZERO                                          
              EVAR(4,I) = ZERO                                          
              EVAR(5,I) = ZERO                                          
              EVAR(6,I) = ZERO                                          
            ENDDO                                                       
          ENDIF                                                         
C-----------------------------------------------                        
          IF(ISOLNOD == 16)THEN                                         
            DO I=LFT,LLT                                               
              N = I + NFT                                               
              IF(EL2FA(NN2+N)/=0)THEN                                 
                TENS(1,EL2FA(NN2+N)) = EVAR(1,I)                        
                TENS(2,EL2FA(NN2+N)) = EVAR(2,I)                        
                TENS(3,EL2FA(NN2+N)) = EVAR(3,I)                        
                TENS(4,EL2FA(NN2+N)) = EVAR(4,I)                        
                TENS(5,EL2FA(NN2+N)) = EVAR(5,I)                        
                TENS(6,EL2FA(NN2+N)) = EVAR(6,I)                        
                TENS(1,EL2FA(NN2+N)+1) = EVAR(1,I)                      
                TENS(2,EL2FA(NN2+N)+1) = EVAR(2,I)                      
                TENS(3,EL2FA(NN2+N)+1) = EVAR(3,I)                      
                TENS(4,EL2FA(NN2+N)+1) = EVAR(4,I)                      
                TENS(5,EL2FA(NN2+N)+1) = EVAR(5,I)                      
                TENS(6,EL2FA(NN2+N)+1) = EVAR(6,I)                      
                TENS(1,EL2FA(NN2+N)+2) = EVAR(1,I)                      
                TENS(2,EL2FA(NN2+N)+2) = EVAR(2,I)                      
                TENS(3,EL2FA(NN2+N)+2) = EVAR(3,I)                      
                TENS(4,EL2FA(NN2+N)+2) = EVAR(4,I)                      
                TENS(5,EL2FA(NN2+N)+2) = EVAR(5,I)                      
                TENS(6,EL2FA(NN2+N)+2) = EVAR(6,I)                      
                TENS(1,EL2FA(NN2+N)+3) = EVAR(1,I)                      
                TENS(2,EL2FA(NN2+N)+3) = EVAR(2,I)                      
                TENS(3,EL2FA(NN2+N)+3) = EVAR(3,I)                      
                TENS(4,EL2FA(NN2+N)+3) = EVAR(4,I)                      
                TENS(5,EL2FA(NN2+N)+3) = EVAR(5,I)                      
                TENS(6,EL2FA(NN2+N)+3) = EVAR(6,I)                      
              ENDIF                                                     
            ENDDO                                                      
          ELSE                                                          
             DO I=LFT,LLT                                               
              N = I + NFT                                               
              IF(EL2FA(NN2+N)/=0)THEN                                 
               TENS(1,EL2FA(NN2+N)) = EVAR(1,I)                         
               TENS(2,EL2FA(NN2+N)) = EVAR(2,I)                         
               TENS(3,EL2FA(NN2+N)) = EVAR(3,I)                         
               TENS(4,EL2FA(NN2+N)) = EVAR(4,I)                         
               TENS(5,EL2FA(NN2+N)) = EVAR(5,I)                         
               TENS(6,EL2FA(NN2+N)) = EVAR(6,I)                         
              ENDIF                                                     
             ENDDO                                                      
          ENDIF                                                         
C-----------------------------------------------                        
        ELSEIF(ISPH3D == 1.AND.ITY == 51)THEN                           
C-----------------------------------------------                        
C         TETRAS SPH.                                                  
C-----------------------------------------------                        
          IPRT=IPARTSP(1 + NFT)                                         
          MT1  =IPART(1,IPRT)                                           
C-----------------------------------------------                        
          IF (ITENS == 1) THEN                                            
C          STRESS                                                       
C-----------------------------------------------                        
           DO I=LFT,LLT                                                 
             N = I + NFT                                                
             IF(EL2FA(NN3+N)/=0)THEN                                  
               TENS(1,EL2FA(NN3+N)) = LBUF%SIG(JJ(1) + I)
               TENS(2,EL2FA(NN3+N)) = LBUF%SIG(JJ(2) + I)
               TENS(3,EL2FA(NN3+N)) = LBUF%SIG(JJ(3) + I)
               TENS(4,EL2FA(NN3+N)) = LBUF%SIG(JJ(4) + I)
               TENS(5,EL2FA(NN3+N)) = LBUF%SIG(JJ(5) + I)
               TENS(6,EL2FA(NN3+N)) = LBUF%SIG(JJ(6) + I)
             ENDIF                                                      
           ENDDO                                                        
C-----------------------------------------------                        
          ELSEIF(ITENS == 4.AND.MLW == 24.                              
     .                      AND.NINT(PM(56,MT1)) == 1)THEN              
C          CRACKS                                                       
C-----------------------------------------------                        
            LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
            DO I=LFT,LLT                                                
              N = I + NFT                                               
              IF (EL2FA(NN3+N)/=0) THEN                                 
                EVAR(1,I) = LBUF%DGLO(JJ(1)+I)        
                EVAR(2,I) = LBUF%DGLO(JJ(2)+I)        
                EVAR(3,I) = LBUF%DGLO(JJ(3)+I)        
                EVAR(4,I) = LBUF%DGLO(JJ(4)+I)        
                EVAR(5,I) = LBUF%DGLO(JJ(5)+I)        
                EVAR(6,I) = LBUF%DGLO(JJ(6)+I)        
              ENDIF                                                     
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = I + NFT                                               
              IF(EL2FA(NN3+N)/=0)THEN                                 
                TENS(1,EL2FA(NN3+N)) = ZERO                             
                TENS(2,EL2FA(NN3+N)) = ZERO                             
                TENS(3,EL2FA(NN3+N)) = ZERO                             
                TENS(4,EL2FA(NN3+N)) = ZERO                             
                TENS(5,EL2FA(NN3+N)) = ZERO                             
                TENS(6,EL2FA(NN3+N)) = ZERO                             
              ENDIF                                                     
            ENDDO                                                       
          ENDIF                                                         
C-----------------------------------------------                        
        ENDIF  ! ITY                                                          
C-----------------------------------------------                        
 490  CONTINUE                                                         
C-----------------------------------------------                        
      DO N=1,NBF                                                        
        R4(1) = TENS(1,N)                                               
        R4(2) = TENS(2,N)                                               
        R4(3) = TENS(3,N)                                               
        R4(4) = TENS(4,N)                                               
        R4(5) = TENS(5,N)                                               
        R4(6) = TENS(6,N)                                               
        CALL WRITE_R_C(R4,6)                                            
      ENDDO                                                             
C-----------               
      RETURN                                                            
      END                                                               
