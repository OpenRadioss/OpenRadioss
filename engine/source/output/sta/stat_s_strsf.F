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
Chd|  STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
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
      SUBROUTINE STAT_S_STRSF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXS ,
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
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr16_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0,IGLOB
      INTEGER IXS(NIXS,*),IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTS(*), IPART_STATE(*), STAT_INDXS(*),IPART(LIPART1,*)
      my_real  X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,JJ,LEN,ISOLNOD,IUS,MT,TSHELL,
     .        NLAY,NPTR,NPTS,NPTT,NPTG,NGF,NGL,NN,NG,NEL,MLW,
     .        ID, IPRT0, IPRT, NPG, IPG, IPT, IE,IP,IL,IR,IS,IT,PID,IOFF,
     .        KK(6),KHBE
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
C======================================================================|
      JJ = 0
      IF (STAT_NUMELS==0) GOTO 200

      IE=0
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
c
        IF (ITY == 1) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          LFT = 1
          LLT = NEL
          IPRT = IPARTS(LFT+NFT)
          PID  = IPART(2,IPRT)
          ISOLNOD = IPARG(28,NG)
          TSHELL  = 0
          IF (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22) TSHELL = 1
          IF (JCVT == 1 .AND. ISORTH /=0 ) JCVT=2
c
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          NLAY = ELBUF_TAB(NG)%NLAY                       
          NPTR = ELBUF_TAB(NG)%NPTR                        
          NPTS = ELBUF_TAB(NG)%NPTS                        
          NPTT = ELBUF_TAB(NG)%NPTT                        
          NPT  = NPTR * NPTS * NPTT * NLAY
!
          DO I=1,6
            KK(I) = NEL*(I-1)
          ENDDO
!
c-------------------------------
          IF (ISOLNOD == 16) THEN
c---------------   
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT                                                                                      
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                    WATMP(1) = LBUF%SIG(KK(1)+I)
                    WATMP(2) = LBUF%SIG(KK(2)+I)
                    WATMP(3) = LBUF%SIG(KK(3)+I)
                    WATMP(4) = LBUF%SIG(KK(4)+I)
                    WATMP(5) = LBUF%SIG(KK(5)+I)
                    WATMP(6) = LBUF%SIG(KK(6)+I)
                    IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),				    
     .              		    JCVT,WATMP,GAMA,JHBE,IGTYP)   
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)
                    IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN			       
                      WA(JJ + 7) = ZERO
                    ELSE
                      WA(JJ + 7) = LBUF%PLA(I)  		      
                    ENDIF
                    WA(JJ+8)= LBUF%EINT(I)			       
                    WA(JJ+9)= LBUF%RHO(I)			       
                    JJ = JJ + 9
                  ENDDO
                ENDDO
              ENDDO                                                      
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO  !  I=LFT,LLT
c---------------   
          ELSEIF (ISOLNOD == 20) THEN
c---------------   
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT                                                                                          
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                    WATMP(1) = LBUF%SIG(KK(1)+I)			
                    WATMP(2) = LBUF%SIG(KK(2)+I)			
                    WATMP(3) = LBUF%SIG(KK(3)+I)			
                    WATMP(4) = LBUF%SIG(KK(4)+I)			
                    WATMP(5) = LBUF%SIG(KK(5)+I)			
                    WATMP(6) = LBUF%SIG(KK(6)+I)			
                    IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),				    
     .              		    JCVT,WATMP,GAMA,JHBE,IGTYP)   
                    WA(JJ + 1) = WATMP(1)
                    WA(JJ + 2) = WATMP(2)
                    WA(JJ + 3) = WATMP(3)
                    WA(JJ + 4) = WATMP(4)
                    WA(JJ + 5) = WATMP(5)
                    WA(JJ + 6) = WATMP(6)
                    IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN			       
                      WA(JJ + 7) = ZERO
                    ELSE
                      WA(JJ + 7) = LBUF%PLA(I)  		      
                    ENDIF
                    WA(JJ+8)= LBUF%EINT(I)			       
                    WA(JJ+9)= LBUF%RHO(I)			       
                    JJ = JJ + 9
                  ENDDO
                ENDDO
              ENDDO                                                      
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO  !  I=LFT,LLT
c---------------          
          ELSEIF (TSHELL == 1) THEN
c---------------          
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT                                                                                 
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                      WATMP(1) = LBUF%SIG(KK(1)+I)			  
                      WATMP(2) = LBUF%SIG(KK(2)+I)			  
                      WATMP(3) = LBUF%SIG(KK(3)+I)			  
                      WATMP(4) = LBUF%SIG(KK(4)+I)			  
                      WATMP(5) = LBUF%SIG(KK(5)+I)			  
                      WATMP(6) = LBUF%SIG(KK(6)+I)			  
                      IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),                                 
     .                                JCVT,WATMP,GAMA,JHBE,IGTYP)   
                      WA(JJ + 1) = WATMP(1)
                      WA(JJ + 2) = WATMP(2)
                      WA(JJ + 3) = WATMP(3)
                      WA(JJ + 4) = WATMP(4)
                      WA(JJ + 5) = WATMP(5)
                      WA(JJ + 6) = WATMP(6)
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                               
                        WA(JJ + 7) = ZERO
                      ELSE
                        WA(JJ + 7) = LBUF%PLA(I)                        
                      ENDIF
                      WA(JJ+8)= LBUF%EINT(I)                             
                      WA(JJ+9)= LBUF%RHO(I)                              
                      JJ = JJ + 9
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO                                                      
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO  !  I=LFT,LLT
c---------------          
          ELSEIF (JHBE == 12 .OR. JHBE == 14 .OR. JHBE == 17 .OR.
     .            ISOLNOD == 4 .AND. ISROT == 1 ) THEN   
c---------------          
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT
              IF (JHBE==17.AND.IINT==2) WA(JJ+ 9)=  18
                                                                                       
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                DO IT=1,NPTT
                  DO IS=1,NPTS
                    DO IR=1,NPTR
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)    
                      WATMP(1) = LBUF%SIG(KK(1)+I)			  
                      WATMP(2) = LBUF%SIG(KK(2)+I)			  
                      WATMP(3) = LBUF%SIG(KK(3)+I)			  
                      WATMP(4) = LBUF%SIG(KK(4)+I)			  
                      WATMP(5) = LBUF%SIG(KK(5)+I)			  
                      WATMP(6) = LBUF%SIG(KK(6)+I)			    
                      IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),                                 
     .                                JCVT,WATMP,GAMA,JHBE,IGTYP)
                      WA(JJ + 1) = WATMP(1)
                      WA(JJ + 2) = WATMP(2)
                      WA(JJ + 3) = WATMP(3)
                      WA(JJ + 4) = WATMP(4)
                      WA(JJ + 5) = WATMP(5)
                      WA(JJ + 6) = WATMP(6)
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                               
                        WA(JJ + 7) = ZERO
                      ELSE
                        WA(JJ + 7) = LBUF%PLA(I)                        
                      ENDIF
                      WA(JJ+8)= LBUF%EINT(I)                             
                      WA(JJ+9)= LBUF%RHO(I)                              
                      JJ = JJ + 9
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO                                                      
c---
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO                                                      
          ELSEIF (IGTYP == 43) THEN   
c---------------          
            DO I=LFT,LLT
              N  = I + NFT
              IPRT=IPARTS(N)
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
              WA(JJ+12) = ISROT                                                                                       
              JJ = JJ + 12                                              
              GAMA(1)=ONE
              GAMA(2)=ZERO
              GAMA(3)=ZERO
              GAMA(4)=ZERO
              GAMA(5)=ONE
              GAMA(6)=ZERO
c---
              DO IR=1,NPTR                                                 
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,1,1)             
                WATMP(1) = LBUF%SIG(KK(1)+I)				      
                WATMP(2) = LBUF%SIG(KK(2)+I)				      
                WATMP(3) = LBUF%SIG(KK(3)+I)				      
                WATMP(4) = LBUF%SIG(KK(4)+I)				      
                WATMP(5) = LBUF%SIG(KK(5)+I)				      
                WATMP(6) = LBUF%SIG(KK(6)+I)				      
                IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),                                       
     .                          JCVT,WATMP,GAMA,JHBE,IGTYP)                
                WA(JJ + 1) = WATMP(1)                                      
                WA(JJ + 2) = WATMP(2)                                      
                WA(JJ + 3) = WATMP(3)                                      
                WA(JJ + 4) = WATMP(4)                                      
                WA(JJ + 5) = WATMP(5)                                      
                WA(JJ + 6) = WATMP(6)                                      
                WA(JJ + 7) = LBUF%EINT(I)
                WA(JJ + 8) = LBUF%PLA(I)
                IF (ELBUF_TAB(NG)%BUFLY(1)%L_PLA == 2) THEN
                  WA(JJ + 9) = LBUF%PLA(I+NEL)
                ELSE
                  WA(JJ + 9) = ZERO
                ENDIF                                   
                JJ = JJ + 9
              ENDDO                                                        
c---
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO                                                      
c---------------          
          ELSEIF (ISOLNOD == 8 .OR. NPT == 1) THEN
c---------------          
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT                                                                                         
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                      WATMP(1) = LBUF%SIG(KK(1)+I)			  
                      WATMP(2) = LBUF%SIG(KK(2)+I)			  
                      WATMP(3) = LBUF%SIG(KK(3)+I)			  
                      WATMP(4) = LBUF%SIG(KK(4)+I)			  
                      WATMP(5) = LBUF%SIG(KK(5)+I)			  
                      WATMP(6) = LBUF%SIG(KK(6)+I)			   
                      IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),                                 
     .                                JCVT,WATMP,GAMA,JHBE,IGTYP)
                      WA(JJ + 1) = WATMP(1)
                      WA(JJ + 2) = WATMP(2)
                      WA(JJ + 3) = WATMP(3)
                      WA(JJ + 4) = WATMP(4)
                      WA(JJ + 5) = WATMP(5)
                      WA(JJ + 6) = WATMP(6)
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                              
                        WA(JJ + 7) = ZERO
                      ELSE
                        WA(JJ + 7) = LBUF%PLA(I)                        
                      ENDIF
                      WA(JJ+8)= LBUF%EINT(I)                            
                      WA(JJ+9)= LBUF%RHO(I)  
                      JJ = JJ + 9
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO                                                      
c---
C               pointeur de fin de zone dans WA
                IE=IE+1
                PTWA(IE)=JJ
            ENDDO  !  I=LFT,LLT
c---------------          
          ELSE
c---------------          
            DO I=LFT,LLT
              N  = I + NFT
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
              WA(JJ+12) = ISROT                                                                                       
              JJ = JJ + 12                                              
              IF (IGLOB == 1)THEN
                IF (JCVT==2 ) THEN
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
                      WATMP(1) = LBUF%SIG(KK(1)+I)			  
                      WATMP(2) = LBUF%SIG(KK(2)+I)			  
                      WATMP(3) = LBUF%SIG(KK(3)+I)			  
                      WATMP(4) = LBUF%SIG(KK(4)+I)			  
                      WATMP(5) = LBUF%SIG(KK(5)+I)			  
                      WATMP(6) = LBUF%SIG(KK(6)+I)			     
                      IF (IGLOB == 1) CALL SROTA6(X,IXS(1,N),                                 
     .                                JCVT,WATMP,GAMA,JHBE,IGTYP)
                      WA(JJ + 1) = WATMP(1)
                      WA(JJ + 2) = WATMP(2)
                      WA(JJ + 3) = WATMP(3)
                      WA(JJ + 4) = WATMP(4)
                      WA(JJ + 5) = WATMP(5)
                      WA(JJ + 6) = WATMP(6)
                      IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA == 0) THEN                               
                        WA(JJ + 7) = ZERO
                      ELSE
                        WA(JJ + 7) = LBUF%PLA(I)                        
                      ENDIF
                      WA(JJ+8)= LBUF%EINT(I)                             
                      WA(JJ+9)= LBUF%RHO(I)                              
                      JJ = JJ + 9
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO                                                      
c---
C             pointeur de fin de zone dans WA
              IE=IE+1
              PTWA(IE)=JJ
            ENDDO  !  I=LFT,LLT
          ENDIF  ! ISOLNOD, JHBE                                         
        ENDIF  ! ITY = 1
      ENDDO    ! NGROUP
 200  CONTINUE
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
Clm     recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELS
          PTWA_P0(N) = PTWA(N)
        END DO
        LEN=JJ
        DO J=1,LEN
          WAP0(J) = WA(J)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELS,PTWA_P0,STAT_NUMELS_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      IF (ISPMD == 0 .AND. LEN > 0) THEN

        IPRT0=0
        DO N=1,STAT_NUMELS_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXS(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)

          IPRT    = NINT(WAP0(J + 2))
          ID      = NINT(WAP0(J + 3)) 
          NLAY    = NINT(WAP0(J + 4))          
          NPTR    = NINT(WAP0(J + 5))          
          NPTS    = NINT(WAP0(J + 6))          
          NPTT    = NINT(WAP0(J + 7))          
          ISOLNOD = NINT(WAP0(J + 8))          
          JHBE    = NINT(WAP0(J + 9))          
          IGTYP   = NINT(WAP0(J +10)) 
          IOFF    = NINT(WAP0(J + 11))   
          ISROT   = NINT(WAP0(J + 12))             
          NPT     = NLAY * NPTR * NPTS * NPTT
          NPTG    = NPT
c
          IF (IOFF >= 1) THEN
           IF (IPRT /= IPRT0) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(A)') DELIMIT
              IF(IGLOB == 1)THEN
                WRITE(IUGEO,'(A)')'/INIBRI/STRS_FGLO'
              ELSE
                WRITE(IUGEO,'(A)')'/INIBRI/STRS_F'
              ENDIF
              WRITE(IUGEO,'(A)')
     .       '#------------------------ REPEAT ------------------------'
              WRITE(IUGEO,'(A)')
     .        '#  BRICKID       NPT   ISOLNOD     JJHBE'
               WRITE(IUGEO,'(A)')
     .        '# IF (NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
               IF ((ISOLNOD == 8 .AND. 
     .         (JHBE==1.OR.JHBE==2.OR.JHBE==12.OR.JHBE==24.OR.JHBE==17 .OR. JHBE == 18)
     .         .AND.IGTYP /= 43).OR. (ISOLNOD == 4 .AND. ISROT == 0)) THEN
                 WRITE(IUGEO,'(A)') '#  EINT, RHO'
c----------------------------------------------------------------------
               WRITE(IUGEO,'(A/A)') '#    S1,  S2,  S3',
     .                              '#   S12, S23, S31'
c----------------------------------------------------------------------
                WRITE(IUGEO,'(A)') '#  EPSP'
              ELSEIF (IGTYP==43 ) THEN
                WRITE(IUGEO,'(A/A)') '#    S1,  S2,  S3',
     .                               '#   S12, S23, S31'
                WRITE(IUGEO,'(A)') '#  EINT, EPSP'
              ELSE
c----------------------------------------------------------------------
               WRITE(IUGEO,'(A/A)') '#    S1,  S2,  S3',
     .                              '#   S12, S23, S31'
c----------------------------------------------------------------------
                WRITE(IUGEO,'(A)') '#  EPSP,EINT, RHO'
              END IF
c
              WRITE(IUGEO,'(A)')
     .       '#---------------------- END REPEAT ---------------------'
              WRITE(IUGEO,'(A)') DELIMIT
c
c----------------------------------------------------------------------
            ELSE  !  IZIPSTRS /= 0
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100)
              IF(IGLOB == 1)THEN 
                WRITE(LINE,'(A)')'/INIBRI/STRS_FGLO'
                CALL STRS_TXT50(LINE,100) 
              ELSE
                WRITE(LINE,'(A)')'/INIBRI/STRS_F'
                CALL STRS_TXT50(LINE,100) 
              ENDIF
              WRITE(LINE,'(A)')
     .       '#------------------------ REPEAT -----------------------'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')
     .        '#  BRICKID       NPT   ISOLNOD     JJHBE'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .       '# IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'
              CALL STRS_TXT50(LINE,100)
              IF ((ISOLNOD == 8 .AND. 
     .         (JHBE==1.OR.JHBE==2.OR.JHBE==12.OR.JHBE==24.OR.JHBE==17 .OR. JHBE == 18)
     .         .AND.IGTYP /= 43).OR. (ISOLNOD == 4 .AND. ISROT == 0)) THEN
                WRITE(LINE,'(A)') '#  EINT, RHO'
                CALL STRS_TXT50(LINE,100) 
                IF (IGLOB == 1)THEN 
                  WRITE(LINE,'(A)')'#    SX,  SY,  SZ' 
                  CALL STRS_TXT50(LINE,100) 
                  WRITE(LINE,'(A)')'#   SXY, SYZ, SZX'
                  CALL STRS_TXT50(LINE,100) 
                ELSE
                  WRITE(LINE,'(A)')'#    S1,  S2,  S3' 
                  CALL STRS_TXT50(LINE,100) 
                  WRITE(LINE,'(A)')'#   S12, S23, S31'
                  CALL STRS_TXT50(LINE,100) 
                ENDIF 
                WRITE(LINE,'(A)') '#  EPSP'
                CALL STRS_TXT50(LINE,100) 
C
              ELSEIF (IGTYP==43 ) THEN
                IF (IGLOB == 1)THEN 
                   WRITE(LINE,'(A)')'#    SX,  SY,  SZ' 
                   CALL STRS_TXT50(LINE,100) 
                   WRITE(LINE,'(A)')'#   SXY, SYZ, SZX'
                   CALL STRS_TXT50(LINE,100) 
                ELSE
                   WRITE(LINE,'(A)')'#    S1,  S2,  S3' 
                   CALL STRS_TXT50(LINE,100) 
                   WRITE(LINE,'(A)')'#   S12, S23, S31'
                   CALL STRS_TXT50(LINE,100) 
                ENDIF 
                WRITE(LINE,'(A)') '#  EINT, EPSP'
                CALL STRS_TXT50(LINE,100) 
C
              ELSE
                IF (IGLOB == 1)THEN 
                   WRITE(LINE,'(A)')'#    SX,  SY,  SZ' 
                   CALL STRS_TXT50(LINE,100) 
                   WRITE(LINE,'(A)')'#   SXY, SYZ, SZX'
                   CALL STRS_TXT50(LINE,100) 
                ELSE
                   WRITE(LINE,'(A)')'#    S1,  S2,  S3' 
                   CALL STRS_TXT50(LINE,100) 
                   WRITE(LINE,'(A)')'#   S12, S23, S31'
                   CALL STRS_TXT50(LINE,100) 
                ENDIF 
                WRITE(LINE,'(A)') '#  EPSP,EINT, RHO'
                CALL STRS_TXT50(LINE,100) 
              END IF
c
              WRITE(LINE,'(A)')
     .       '#---------------------- END REPEAT ----------------------'
              CALL STRS_TXT50(LINE,100)  
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100)
            ENDIF
            IPRT0=IPRT
          END IF
c------------------------------------------------------------------
          IF (ISOLNOD == 16) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(8I10)')ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NPTT,NLAY
            ELSE
              WRITE(LINE,'(8I10)') ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NPTT,NLAY
              CALL STRS_TXT50(LINE,100)
            ENDIF
          ELSEIF (TSHELL == 1) THEN
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(7I10)')ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NLAY
            ELSE
              WRITE(LINE,'(7I10)') ID,NPT,ISOLNOD,JHBE,NPTR,NPTS,NLAY
              CALL STRS_TXT50(LINE,100)
            ENDIF
          ELSE
            KHBE=JHBE
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(7I10)') ID,NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT
            ELSE
              WRITE(LINE,'(7I10)')  ID,NPT,ISOLNOD,KHBE,NPTR,NPTS,NPTT
              CALL STRS_TXT50(LINE,100)
            ENDIF
          ENDIF
          J = J + 12
c-------------------
          IF ((ISOLNOD == 8 .AND. 
     .         (JHBE==1.OR.JHBE==2.OR.JHBE==12.OR.JHBE==24.OR.JHBE==17 .OR. JHBE == 18)
     .         .AND.IGTYP /= 43).OR. (ISOLNOD == 4 .AND. ISROT == 0)) THEN
            DO IPT = 1, NPTG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P2E20.13)')(WAP0(J + K),K=8,9)
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,6)
                WRITE(IUGEO,'(1P1E20.13)') WAP0(J + 7)
              ELSE
                   CALL TAB_STRS_TXT50(WAP0(8),2,J,SIZP0,2)
                   CALL TAB_STRS_TXT50(WAP0(1),6,J,SIZP0,3)
                   CALL TAB_STRS_TXT50(WAP0(7),1,J,SIZP0,1)
              ENDIF
              J = J + 9
            ENDDO
            ELSE
c---
            DO IPT = 1, NPTG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=4,6)
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=7,9)
              ELSE
                CALL TAB_STRS_TXT50(WAP0,9,J,SIZP0,3)
              ENDIF
              J = J + 9
            ENDDO
          ENDIF  
         ENDIF  !  IF (IOFF == 1)
c---
        ENDDO    !  N=1,STAT_NUMELS_G      
      ENDIF
c-----------
      RETURN
      END
