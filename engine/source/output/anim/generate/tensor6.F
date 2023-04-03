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
Chd|  TENSORS                       source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        SROTA6_S8S                    source/output/anim/generate/srota6_s8s.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE TENSORS(ELBUF_TAB,IPARG   ,ITENS   ,IXS     ,PM       ,
     2                   EL2FA    ,NBF     ,TENS    ,EPSDOT  ,
     3                   NBPART   ,X       ,IADG    ,IPART   ,
     4                   IPARTSP  ,ISPH3D  ,IPM     ,IGEO    ) 
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
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TENS(6,*),EPSDOT(6,*),PM(NPROPM,*),X(3,*)
      INTEGER IPARG(NPARG,*),ITENS, 
     .   IXS(NIXS,*),EL2FA(*),IADG(NSPMD,*),IPM(NPROPMI,*),
     .   NBF,NBPART,IPART(LIPART1,*),IPARTSP(*),
     .   ISPH3D,IGEO(NPROPGI,*)
      REAL WA(6*NBF)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,MVSIZ),OFF, FAC, A1, A2, A3, THK, GAMA(6),EVAR_TMP(6)
      REAL R4(18)
      INTEGER I,I1,I2,II,N,J,NG,NEL,IPT,MT1,MLW, ISTRAIN,TSHELL,
     .        IPID, NS1, NS2 ,IALEL, ISTRE,IPRT,PTI,IADPG,PID,
     .        NN1,NN2,NN3,NN4,ICSIG,IOR_TSH,NUVAR,BUF,L_PLA,L_STRA,KHBE,
     .        KCVT,ISOLNOD,NLAY,NPTR,NPTS,NPTT,NPTG,IL,IS,IR,IT,IVISC,IOK,
     .        JJ(6),IR0,IS0,IT0
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
C=======================================================================      
      DO J=1,18      
        R4(J) = ZERO
      ENDDO         
      NN1 = 1
      NN2 = 1
      NN3 = NN2 + NUMELS
      NN4 = NN3 + ISPH3D*(NUMSPH+MAXPJET)
C-----------------------------------------------
       DO 490 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        ISTRAIN = IPARG(44,NG)
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        CALL INITBUF(IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
       DO I=1,6
         JJ(I) = NEL*(I-1)
       ENDDO
!
       IF(MLW /= 13) THEN          
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN
          TSHELL  = 0
          IOR_TSH = 0
          IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1
          IF (IGTYP == 21.OR.IGTYP == 22) IOR_TSH = 1
          NLAY = ELBUF_TAB(NG)%NLAY                
          NPTR = ELBUF_TAB(NG)%NPTR                 
          NPTS = ELBUF_TAB(NG)%NPTS                 
          NPTT = ELBUF_TAB(NG)%NPTT
          NPTG = NPTT*NPTS*NPTR
          NPT  = NPTG*NLAY
          PID=IXS(10,1 + NFT)
          MT1=IXS(1,1 + NFT)
C
          IF (KCVT==1.AND.ISORTH/=0) KCVT=2
          NUVAR = IPM(8,MT1)
          IF (IGTYP /= 22) THEN
            IF (ISORTH > 0) ISORTHG = 0
          END IF  
          IF(MLW==0)THEN
            DO I=LFT,LLT
              N = I + NFT
              TENS(1,EL2FA(NN2+N)) = ZERO
              TENS(2,EL2FA(NN2+N)) = ZERO
              TENS(3,EL2FA(NN2+N)) = ZERO
              TENS(4,EL2FA(NN2+N)) = ZERO
              TENS(5,EL2FA(NN2+N)) = ZERO
              TENS(6,EL2FA(NN2+N)) = ZERO
            ENDDO
            GO TO 490
          END IF  
C-----------
          EVAR(1:6,LFT:LLT)=ZERO
          IF (ITENS == 1) THEN
C-----------------------------------------------
C          STRESS
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
           IF(IVISC > 0) THEN
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
              DO I=LFT,LLT
                EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                EVAR(3,I) =EVAR(3,I)+ LBUF%VISC(JJ(3) + I)
                EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                EVAR(5,I) =EVAR(5,I)+ LBUF%VISC(JJ(5) + I)
                EVAR(6,I) =EVAR(6,I)+ LBUF%VISC(JJ(6) + I)
              ENDDO
           ENDIF
c
           IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
             DO I=LFT,LLT
               EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
               EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
               EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
               EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
               EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
               EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
             ENDDO
           ENDIF
c
           IF (JHBE == 17 .AND. IINT ==3) THEN       !KCVT == 2 .AND. 
C           STRESS TENSOR IN GLOBAL SYSTEM
            DO I=LFT,LLT
             N = I + NFT
             IF(EL2FA(NN2+N) /= 0)THEN
C             pour JHBE=14, valeurs moyennes est dans rep. corota.
              IF(KCVT==2.AND.JHBE/=14.AND.JHBE/=15)THEN
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
                CALL SROTA6_S8S(
     1   KCVT,                  EVAR(1,I),             GAMA,                  JHBE,
     2   IGTYP,                 GBUF%COR_FR(9*(I-1)+1),IINT,                  ISORTH)
             ENDIF
            ENDDO
C
           ELSE IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C           STRESS TENSOR IN GLOBAL SYSTEM
            DO I=LFT,LLT
             N = I + NFT
             IF(EL2FA(NN2+N) /= 0)THEN
C             pour JHBE=14, valeurs moyennes est dans rep. corota.
              IF(KCVT==2.AND.JHBE/=14.AND.JHBE/=15)THEN
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
                CALL SROTA6(X,IXS(1,N),KCVT,EVAR(1,I),GAMA,JHBE,IGTYP)
             ENDIF
            ENDDO
           ENDIF
C
          ELSEIF (ITENS == 2)THEN
C-----------------------------------------------
C          STRAIN
C-----------------------------------------------
c-----------missed :IF (IGTYP == 22) -> cycle first IL=1,NLAY and GAMA<- LBUF%GAMA inside
            IF (ISOLNOD == 8 .AND. IGTYP == 43) THEN
c-----------
             DO I=LFT,LLT                                            
               DO IPT= 1,NPTR
                 LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)                                           
                 EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(1) + I)/NPT
                 EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT
                 EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(3) + I)/NPT
               ENDDO                                                  
             ENDDO                                                   
             DO I=LFT,LLT                        
               N = I + NFT                         
               IF(EL2FA(NN2+N) /= 0)THEN           
                 CALL SROTA6(X,IXS(1,N),KCVT,EVAR(1,I),GAMA,JHBE,IGTYP)     
               ENDIF                               
             ENDDO                                 
c-----------
            ELSEIF (ISOLNOD == 8 .AND. NPT == 8 .AND. JHBE /= 14.AND.
     .        JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17 )THEN
c-----------
              NVAUX =IPARG(18,NG)         
              IF (MLW>=28) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  DO J=1,8
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,J)                                        
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)*ONE_OVER_8
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)*ONE_OVER_8
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)*ONE_OVER_8
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*ONE_OVER_8
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*ONE_OVER_8
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*ONE_OVER_8
                  ENDDO
                ENDDO
              ENDIF
c-----------
            ELSEIF ((ISOLNOD==8.OR.(ISOLNOD==4 .AND. ISROT==0)).AND.
     .               NPT==1 .AND. JHBE /= 14 .AND. JHBE /= 15) THEN
c-----------
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                           
              IF (ISORTH > 0) ISORTHG = 1
c
              IF (MLW>=28.AND.MLW /= 49) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  EVAR(1,I) =  LBUF%STRA(JJ(1) + I)
                  EVAR(2,I) =  LBUF%STRA(JJ(2) + I)
                  EVAR(3,I) =  LBUF%STRA(JJ(3) + I)
                  EVAR(4,I) =  LBUF%STRA(JJ(4) + I)*HALF
                  EVAR(5,I) =  LBUF%STRA(JJ(5) + I)*HALF
                  EVAR(6,I) =  LBUF%STRA(JJ(6) + I)*HALF
                ENDDO   
                IF (ISORTH > 0) KCVT = 2
              ELSEIF (MLW == 12 .OR. MLW == 14)THEN
                DO I=LFT,LLT
                  N = I + NFT                                    
                  EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)
                  EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)
                  EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)	       
                ENDDO
                IF (ISORTH > 0) KCVT = 2
              ELSEIF (MLW == 24 .OR. MLW == 25)THEN
                DO I=LFT,LLT
                  N = I + NFT                                    
                  EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                  EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                  EVAR(3,I) = LBUF%STRA(JJ(3) + I)		       
                  EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                  EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                  EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF   
                ENDDO
                IF (ISORTH > 0) KCVT = 2
              ELSEIF (ISTRAIN > 0) THEN
                IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28.OR.
     .              MLW == 49) THEN 
                  DO I=LFT,LLT
                    N = I + NFT  
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)			  
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF    
                  ENDDO
                ELSE
                  DO I=LFT,LLT
                    EVAR(1,I) = ZERO
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO                    
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ENDIF
              ENDIF
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                   DO I=LFT,LLT
                     N = I + NFT
                     IF(EL2FA(NN2+N) /= 0)THEN
                      IF(KCVT==2)THEN
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
                     CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)       
                    ENDIF
                   ENDDO
                 ENDIF
c-----------
            ELSEIF(ISOLNOD == 16.OR.ISOLNOD == 20 .OR.
     .            (ISOLNOD == 8.AND.(JHBE == 14.OR.JHBE == 17)))THEN
c-----------
              IF (MLW>=28.AND.MLW /= 49)THEN        
                DO I=LFT,LLT
                 N = I + NFT   
                 DO IL=1,NLAY                     
                  DO IS=1,NPTS                     
                   DO IT=1,NPTT
                    DO IR=1,NPTR                      
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)        
                     EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)/NPT
                     EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)/NPT
                     EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)/NPT
                     EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF/NPT
                     EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF/NPT
                     EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF/NPT
                    ENDDO 
                   ENDDO
                  ENDDO
                 ENDDO
                ENDDO
              ELSEIF (MLW == 12 .OR. MLW == 14) THEN
                DO I=LFT,LLT                                              
                 N = I + NFT                                              
                 DO IL=1,NLAY                                             
                  DO IS=1,NPTS                                            
                   DO IT=1,NPTT                                           
                    DO IR=1,NPTR                                          
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
                      EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT	      
                      EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT	      
                      EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT	      
                    ENDDO                                                 
                   ENDDO                                                  
                  ENDDO                                                   
                 ENDDO                                                    
                ENDDO                                                     
              ELSEIF(MLW == 24 .OR. MLW == 25)THEN
                DO I=LFT,LLT
                 N = I + NFT   
                 DO IL=1,NLAY                     
                  DO IS=1,NPTS                     
                   DO IT=1,NPTT
                    DO IR=1,NPTR                      
                     LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT) 
                     IF (ELBUF_TAB(NG)%BUFLY(IL)%L_STRA > 0) THEN
                      EVAR_TMP(1) =  LBUF%STRA(JJ(1) + I)/NPT
                      EVAR_TMP(2) =  LBUF%STRA(JJ(2) + I)/NPT
                      EVAR_TMP(3) =  LBUF%STRA(JJ(3) + I)/NPT
                      EVAR_TMP(4) =  LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR_TMP(5) =  LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR_TMP(6) =  LBUF%STRA(JJ(6) + I)*HALF/NPT
        	      ICSIG=IPARG(17,NG)  
        	      IF (KCVT /= 0 .AND.ICSIG > 0) THEN 
        	       IF (IGTYP == 21) THEN
C       	        STRAIN TENSOR IN GLOBAL SYSTEM
        	        IF (JHBE == 14) THEN
        	          SELECT CASE (ICSIG)						 
        	          CASE (1)	 
        	              IF(EL2FA(NN2+N) /= 0)THEN
        	        	IF(KCVT==2)THEN
        	        	  GAMA(1)= ZERO
        	        	  GAMA(2)= GBUF%GAMA(JJ(1) + I)
        	        	  GAMA(3)= GBUF%GAMA(JJ(2) + I)
        	        	  GAMA(4)= ZERO
        	        	  GAMA(5)=-GAMA(2)
        	        	  GAMA(6)= GAMA(1)
        	        	ELSE
        	        	  GAMA(1)=ONE
        	        	  GAMA(2)=ZERO
        	        	  GAMA(3)=ZERO
        	        	  GAMA(4)=ZERO
        	        	  GAMA(5)=ONE
        	        	  GAMA(6)=ZERO
        	        	END IF
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          CASE (10)	  
        	              IF(EL2FA(NN2+N) /= 0)THEN
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
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          CASE (100)	 
        	              IF(EL2FA(NN2+N) /= 0)THEN
        	        	IF(KCVT==2)THEN
        	        	  GAMA(1)= GBUF%GAMA(JJ(2) + I)
        	        	  GAMA(2)= ZERO
        	        	  GAMA(3)= GBUF%GAMA(JJ(1) + I)
        	        	  GAMA(4)= GAMA(3)
        	        	  GAMA(5)= ZERO
        	        	  GAMA(6)=-GAMA(1)
        	        	ELSE
        	        	  GAMA(1)=ONE
        	        	  GAMA(2)=ZERO
        	        	  GAMA(3)=ZERO
        	        	  GAMA(4)=ZERO
        	        	  GAMA(5)=ONE
        	        	  GAMA(6)=ZERO
        	        	END IF
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          END SELECT					     
        	        ENDIF
                       ELSE
C       	        STRAIN TENSOR IN GLOBAL SYSTEM
        	        IF (JHBE == 14) THEN
        	          SELECT CASE (ICSIG)						 
        	          CASE (1)	 
        	              IF(EL2FA(NN2+N) /= 0)THEN
        	        	IF(KCVT==2)THEN
        	        	  GAMA(1)= ZERO
        	        	  GAMA(2)= LBUF%GAMA(JJ(1) + I)
        	        	  GAMA(3)= LBUF%GAMA(JJ(2) + I)
        	        	  GAMA(4)= ZERO
        	        	  GAMA(5)=-GAMA(2)
        	        	  GAMA(6)= GAMA(1)
        	        	ELSE
        	        	  GAMA(1)=ONE
        	        	  GAMA(2)=ZERO
        	        	  GAMA(3)=ZERO
        	        	  GAMA(4)=ZERO
        	        	  GAMA(5)=ONE
        	        	  GAMA(6)=ZERO
        	        	END IF
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          CASE (10)	  
        	              IF(EL2FA(NN2+N) /= 0)THEN
        	        	IF(KCVT==2)THEN
        	        	  GAMA(1)= LBUF%GAMA(JJ(1) + I)
        	        	  GAMA(2)= LBUF%GAMA(JJ(2) + I)
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
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          CASE (100)	 
        	              IF(EL2FA(NN2+N) /= 0)THEN
        	        	IF(KCVT==2)THEN
        	        	  GAMA(1)= LBUF%GAMA(JJ(2) + I)
        	        	  GAMA(2)= ZERO
        	        	  GAMA(3)= LBUF%GAMA(JJ(1) + I)
        	        	  GAMA(4)= GAMA(3)
        	        	  GAMA(5)= ZERO
        	        	  GAMA(6)=-GAMA(1)
        	        	ELSE
        	        	  GAMA(1)=ONE
        	        	  GAMA(2)=ZERO
        	        	  GAMA(3)=ZERO
        	        	  GAMA(4)=ZERO
        	        	  GAMA(5)=ONE
        	        	  GAMA(6)=ZERO
        	        	END IF
        	        	CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	    EVAR_TMP,GAMA, JHBE,IGTYP)
        	              ENDIF
        	          END SELECT					     
        	        ENDIF
        	       ENDIF
        	      ENDIF
                      EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
                      EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
                      EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
                      EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
                      EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
                      EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6) 
                     ENDIF
                    ENDDO 
                   ENDDO
                  ENDDO
                 ENDDO
                ENDDO
              ELSEIF(ISTRAIN > 0)THEN
                IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN               
                  DO I=LFT,LLT
                   N = I + NFT   
                   DO IL=1,NLAY                     
                    DO IS=1,NPTS                     
                     DO IT=1,NPTT
                      DO IR=1,NPTR                      
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT) 
                       EVAR_TMP(1) = LBUF%STRA(JJ(1) + I)/NPT
                       EVAR_TMP(2) = LBUF%STRA(JJ(2) + I)/NPT
                       EVAR_TMP(3) = LBUF%STRA(JJ(3) + I)/NPT
                       EVAR_TMP(4) = LBUF%STRA(JJ(4) + I)*HALF/NPT
                       EVAR_TMP(5) = LBUF%STRA(JJ(5) + I)*HALF/NPT
                       EVAR_TMP(6) = LBUF%STRA(JJ(6) + I)*HALF/NPT
c
        	       ICSIG=IPARG(17,NG)  
        	       IF (KCVT /= 0 .AND.ICSIG > 0) THEN
C       	         STRAIN TENSOR IN GLOBAL SYSTEM
        	         IF (JHBE == 14) THEN
        	           SELECT CASE (ICSIG)  					  
        	           CASE (1)	  
        	               IF(EL2FA(NN2+N) /= 0)THEN
        	        	 IF(KCVT==2)THEN
        	        	   GAMA(1)= ZERO
        	        	   GAMA(2)= LBUF%GAMA(JJ(1) + I)
        	        	   GAMA(3)= LBUF%GAMA(JJ(2) + I)
        	        	   GAMA(4)= ZERO
        	        	   GAMA(5)=-GAMA(2)
        	        	   GAMA(6)= GAMA(1)
        	        	 ELSE
        	        	   GAMA(1)=ONE
        	        	   GAMA(2)=ZERO
        	        	   GAMA(3)=ZERO
        	        	   GAMA(4)=ZERO
        	        	   GAMA(5)=ONE
        	        	   GAMA(6)=ZERO
        	        	 END IF
        	        	 CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	     EVAR_TMP,GAMA, JHBE,IGTYP)
        	               ENDIF
        	           CASE (10)	   
        	               IF(EL2FA(NN2+N) /= 0)THEN
        	        	 IF(KCVT==2)THEN
        	        	   GAMA(1)= LBUF%GAMA(JJ(1) + I)
        	        	   GAMA(2)= LBUF%GAMA(JJ(2) + I)
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
        	        	 CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	     EVAR_TMP,GAMA, JHBE,IGTYP)
        	               ENDIF
        	           CASE (100)	  
        	               IF(EL2FA(NN2+N) /= 0)THEN
        	        	 IF(KCVT==2)THEN
        	        	   GAMA(1)= LBUF%GAMA(JJ(2) + I)
        	        	   GAMA(2)= ZERO
        	        	   GAMA(3)= LBUF%GAMA(JJ(1) + I)
        	        	   GAMA(4)= GAMA(3)
        	        	   GAMA(5)= ZERO
        	        	   GAMA(6)=-GAMA(1)
        	        	 ELSE
        	        	   GAMA(1)=ONE
        	        	   GAMA(2)=ZERO
        	        	   GAMA(3)=ZERO
        	        	   GAMA(4)=ZERO
        	        	   GAMA(5)=ONE
        	        	   GAMA(6)=ZERO
        	        	 END IF
        	        	 CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	     EVAR_TMP,GAMA, JHBE,IGTYP)
        	               ENDIF
        	           END SELECT					      
        	         ENDIF
        	       ENDIF	   
                       EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
                       EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
                       EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
                       EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
                       EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
                       EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6)
                      ENDDO 
                     ENDDO
                    ENDDO
                   ENDDO
                  ENDDO
                ELSE
                  DO I=LFT,LLT
                    EVAR(1,I) =  ZERO
                    EVAR(2,I) =  ZERO
                    EVAR(3,I) =  ZERO                     
                    EVAR(4,I) =  ZERO
                    EVAR(5,I) =  ZERO
                    EVAR(6,I) =  ZERO
                  ENDDO
                ENDIF
              ENDIF
c--- 
              ICSIG=IPARG(17,NG) 
              IF (JHBE == 17) THEN 
                IF (MLW == 12 .OR. MLW == 14 .OR. MLW == 24 .OR.  
     .              MLW == 25 .OR.(MLW >= 28 .AND.MLW /= 49)) THEN 
                  IF (ISORTH > 0) KCVT = 2
                ENDIF
              ENDIF
              IF (KCVT /= 0 .AND.ICSIG == 0 .AND. JHBE /= 16) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT					   
                  N = I + NFT					   
                  IF(EL2FA(NN2+N) /= 0)THEN			   
                    IF(KCVT==2)THEN				   
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
                    CALL SROTA6(X,IXS(1,N),KCVT,		   
     .          	      EVAR(1,I),GAMA, JHBE,IGTYP)	   
                  ENDIF 					   
                ENDDO 
              ENDIF
c-----------
            ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
              IF (MLW>=28.AND.MLW /= 49)THEN 
                DO I=LFT,LLT
                  N = I + NFT  
                  DO IPT=1,NPT
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)        
                    EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                    EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                    EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                    EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                    EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                    EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                  ENDDO
                ENDDO
              ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                DO I=LFT,LLT
                  N = I + NFT  
                  DO IPT=1,NPT
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                       EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/NPT        
                       EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/NPT        
                       EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/NPT        
                  ENDDO
                ENDDO    
              ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0) THEN
                DO I=LFT,LLT
                  N = I + NFT  
                  DO IPT=1,NPT
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)        
                    EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                    EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                    EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                    EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                    EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                    EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                  ENDDO
                ENDDO
               ELSEIF(ISTRAIN > 0)THEN
                IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN               
                  DO I=LFT,LLT
                    N = I + NFT  
                    DO IPT=1,NPT
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)        
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/NPT
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/NPT
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/NPT
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*HALF/NPT
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*HALF/NPT
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*HALF/NPT
                    ENDDO
                  ENDDO
                ELSE                           
                  DO I=LFT,LLT                 
                    EVAR(1,I) =  ZERO          
                    EVAR(2,I) =  ZERO          
                    EVAR(3,I) =  ZERO                       
                    EVAR(4,I) =  ZERO          
                    EVAR(5,I) =  ZERO          
                    EVAR(6,I) =  ZERO          
                  ENDDO                        
                ENDIF                         
              ENDIF 
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT                              
                  N = I + NFT                             
                  IF (EL2FA(NN2+N) /= 0) THEN               
                    IF (KCVT==2) THEN                       
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
                    ENDIF                                
                    CALL SROTA6(X,IXS(1,N),KCVT,            
     .                   EVAR(1,I),GAMA, JHBE,IGTYP)             
                  ENDIF                                    
                ENDDO                                     
              ENDIF
c-----------
            ELSEIF((ISOLNOD == 6.OR.ISOLNOD == 8).AND.JHBE == 15)THEN
c-----------
               IF (MLW>=28.AND.MLW /= 49.AND.ISTRAIN > 0) THEN
                DO I=LFT,LLT                                               
                  N = I + NFT                                              
                  DO IL= 1,NLAY
                    DO IPT=1,NPTG                                            
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)          
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)		
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)		
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)		
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                                      HALF/(NPTG*NLAY)       
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                                      HALF/(NPTG*NLAY)       
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                                      HALF/(NPTG*NLAY)   
                    ENDDO                                                    
                  ENDDO                                                    
                ENDDO                                                      
              ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                DO I=LFT,LLT
                  DO IL= 1,NLAY
                    DO IPT=1,NPTG
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)
                      EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)/(NPTG*NLAY)		
                      EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)/(NPTG*NLAY)		 
                      EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)/(NPTG*NLAY)		 
                    ENDDO
                  ENDDO
                ENDDO    
              ELSEIF ((MLW == 24 .OR. MLW == 25) .and. ISTRAIN > 0)THEN
                DO I=LFT,LLT                                               
                  N = I + NFT                                              
                  DO IL= 1,NLAY
                    DO IPT=1,NPTG                                            
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)          
                      EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)	       
                      EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)	       
                      EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)   
                      EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                            HALF/(NPTG*NLAY)      
                      EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                            HALF/(NPTG*NLAY) 
                      EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                            HALF/(NPTG*NLAY)   
                    ENDDO                                                    
                  ENDDO                                                    
                ENDDO                                                      
              ELSEIF (ISTRAIN > 0) THEN                                        
                IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN                 
                  DO I=LFT,LLT                                               
                    N = I + NFT                                              
                    DO IL= 1,NLAY
                      DO IPT=1,NPTG                                            
                       LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)         
                        EVAR(1,I) = EVAR(1,I)+LBUF%STRA(JJ(1) + I)/(NPTG*NLAY)  	   
                        EVAR(2,I) = EVAR(2,I)+LBUF%STRA(JJ(2) + I)/(NPTG*NLAY)  	  
                        EVAR(3,I) = EVAR(3,I)+LBUF%STRA(JJ(3) + I)/(NPTG*NLAY)  	
                        EVAR(4,I) = EVAR(4,I)+LBUF%STRA(JJ(4) + I)*
     .                            HALF/(NPTG*NLAY)
                        EVAR(5,I) = EVAR(5,I)+LBUF%STRA(JJ(5) + I)*
     .                            HALF/(NPTG*NLAY)
                        EVAR(6,I) = EVAR(6,I)+LBUF%STRA(JJ(6) + I)*
     .                            HALF/(NPTG*NLAY)
                      ENDDO                                                    
                    ENDDO                                                    
                  ENDDO                                                      
                ELSE                                                          
                  DO I=LFT,LLT                                                
                    EVAR(1,I) =  ZERO                                         
                    EVAR(2,I) =  ZERO                                         
                    EVAR(3,I) =  ZERO                                         
                    EVAR(4,I) =  ZERO                                         
                    EVAR(5,I) =  ZERO                                         
                    EVAR(6,I) =  ZERO                                         
                  ENDDO                                                       
                ENDIF                                                        
              ENDIF                          
              IF (KCVT /= 0) THEN                     
C               STRAIN TENSOR IN GLOBAL SYSTEM        
                DO I=LFT,LLT                          
                  N = I + NFT                         
                  IF (EL2FA(NN2+N) /= 0) THEN           
                    IF (KCVT==2)THEN                   
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
                    CALL SROTA6(X,IXS(1,N),KCVT,       
     .                   EVAR(1,I),GAMA, JHBE,IGTYP)        
                  ENDIF                                
                ENDDO                                 
              ENDIF                                   
c-----------
            ENDIF  ! ISOLNOD & ......
C-----------------------------------------------
C         CRACKS
C-----------------------------------------------
          ELSEIF (ITENS == 4.AND.MLW == 24.AND.
     .            NINT(PM(56,MT1)) == 1) THEN
c-----------
            DO I=LFT,LLT
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
            ENDDO
c----------- not work for the moment, cycle avoid crash
c-----------
            IF (ISOLNOD == 8 .AND.(JHBE == 14 .OR. JHBE == 15)) THEN
c-----------
c              DO IPT=1,NPTG                                            
c                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
c                DO I=LFT,LLT                                              
c                  EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)/NPTG	      
c                  EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)/NPTG	      
c                  EVAR(3,I) = EVAR(3,I)+LBUF%DGLO(JJ(3) + I)/NPTG	      
c                  EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)/NPTG      
c                  EVAR(5,I) = EVAR(5,I)+LBUF%DGLO(JJ(5) + I)/NPTG      
c                  EVAR(6,I) = EVAR(6,I)+LBUF%DGLO(JJ(6) + I)/NPTG
c                ENDDO                                                    
c              ENDDO                                                      
            ELSE
              LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
              DO I=LFT,LLT                                                
                EVAR(1,I) = EVAR(1,I)+LBUF%DGLO(JJ(1) + I)  
                EVAR(2,I) = EVAR(2,I)+LBUF%DGLO(JJ(2) + I)	       
                EVAR(3,I) = EVAR(3,I)+LBUF%DGLO(JJ(3) + I)	       
                EVAR(4,I) = EVAR(4,I)+LBUF%DGLO(JJ(4) + I)	 
                EVAR(5,I) = EVAR(5,I)+LBUF%DGLO(JJ(5) + I)	 
                EVAR(6,I) = EVAR(6,I)+LBUF%DGLO(JJ(6) + I)	 
              ENDDO                                                      
	           ENDIF
            IF (KCVT /= 0) THEN
C             DAMAGE IN GLOBAL SYSTEM
              DO I=LFT,LLT
                N = I + NFT
                IF(EL2FA(NN2+N) /= 0)THEN
                  IF (KCVT==2) THEN
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
                  CALL SROTA6(X,IXS(1,N),KCVT,
     .               EVAR(1,I),GAMA, JHBE,IGTYP)
                ENDIF
              ENDDO
            ENDIF
!
          ELSEIF (ITENS == 5) THEN
C-----------------------------------------------
C    - START -     FULL PLASTIC STRAIN TENSOR (MEAN)
C-----------------------------------------------
            DO I=LFT,LLT
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
            ENDDO
c-----------
            IF ((ISOLNOD == 8 .OR. (ISOLNOD == 4 .AND. ISROT == 0)) .AND.
     .               NPT == 1 .AND. JHBE /= 14   .AND. JHBE /= 15) THEN
c-----------
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                           
              IF (ISORTH > 0) ISORTHG = 1
              IF (MLW == 24) THEN
                DO I=LFT,LLT
                  N = I + NFT                                    
                  EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                  EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                  EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)		       
                  EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL)*HALF
                  EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL)*HALF
                  EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL)*HALF   
                ENDDO
              ENDIF ! IF (MLW == 24)
!
              IF (KCVT /= 0) THEN
C             PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)
                      GAMA(3) = GBUF%GAMA(JJ(3) + I)
                      GAMA(4) = GBUF%GAMA(JJ(4) + I)
                      GAMA(5) = GBUF%GAMA(JJ(5) + I)
                      GAMA(6) = GBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT, EVAR(1,I),GAMA, JHBE,IGTYP)       
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (KCVT /= 0)
c-----------
            ELSEIF (ISOLNOD == 16 .OR.  ISOLNOD == 20 .OR.
     .             (ISOLNOD == 8  .AND.(JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
              IF (MLW == 24) THEN
                DO I=LFT,LLT
                  N = I + NFT   
                  DO IL=1,NLAY                     
                    DO IS=1,NPTS                     
                      DO IT=1,NPTT
                        DO IR=1,NPTR                      
                          LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT) 
                          IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
!
                            EVAR_TMP(1) = LBUF%PLA(JJ(1) + I + NEL)/NPT
                            EVAR_TMP(2) = LBUF%PLA(JJ(2) + I + NEL)/NPT
                            EVAR_TMP(3) = LBUF%PLA(JJ(3) + I + NEL)/NPT
                            EVAR_TMP(4) = LBUF%PLA(JJ(4) + I + NEL)*HALF/NPT
                            EVAR_TMP(5) = LBUF%PLA(JJ(5) + I + NEL)*HALF/NPT
                            EVAR_TMP(6) = LBUF%PLA(JJ(6) + I + NEL)*HALF/NPT
!
        	            ICSIG=IPARG(17,NG)  
        	            IF (KCVT /= 0 .AND.ICSIG > 0) THEN 
        	              IF (IGTYP == 21) THEN
!                             PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
        	                IF (JHBE == 14) THEN
        	                  SELECT CASE (ICSIG)						 
        	                CASE (1)	 
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
        	        	      GAMA(1) = ZERO
        	                      GAMA(2) = GBUF%GAMA(JJ(1) + I)
        	       	              GAMA(3) = GBUF%GAMA(JJ(2) + I)
                                      GAMA(4) = ZERO
                                      GAMA(5) =-GAMA(2)
                                      GAMA(6) = GAMA(1)
        	        	    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
        	        	    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                CASE (10)	  
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
                                      GAMA(1) = GBUF%GAMA(JJ(1) + I)
                                      GAMA(2) = GBUF%GAMA(JJ(2) + I)
                                      GAMA(3) = ZERO
                                      GAMA(4) =-GAMA(2)
                                      GAMA(5) = GAMA(1)
                                      GAMA(6) = ZERO
        	        	    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
        	        	    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                CASE (100)	 
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
                                      GAMA(1) = GBUF%GAMA(JJ(2) + I)
                                      GAMA(2) = ZERO
                                      GAMA(3) = GBUF%GAMA(JJ(1) + I)
                                      GAMA(4) = GAMA(3)
                                      GAMA(5) = ZERO
                                      GAMA(6) =-GAMA(1)
        	        	    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
        	        	    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                  END SELECT					     
        	                ENDIF ! IF (JHBE == 14)
                              ELSE
!       	              PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
        	                IF (JHBE == 14) THEN
        	                  SELECT CASE (ICSIG)						 
        	                CASE (1)	 
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
                                      GAMA(1) = ZERO
                                      GAMA(2) = LBUF%GAMA(JJ(1) + I)
                                      GAMA(3) = LBUF%GAMA(JJ(2) + I)
                                      GAMA(4) = ZERO
                                      GAMA(5) =-GAMA(2)
                                      GAMA(6) = GAMA(1)
                                    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
        	        	    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                CASE (10)	  
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
                                      GAMA(1) = LBUF%GAMA(JJ(1) + I)
                                      GAMA(2) = LBUF%GAMA(JJ(2) + I)
                                      GAMA(3) = ZERO
                                      GAMA(4) =-GAMA(2)
                                      GAMA(5) = GAMA(1)
                                      GAMA(6) = ZERO
        	        	    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
        	        	    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                CASE (100)	 
        	                  IF (EL2FA(NN2+N) /= 0) THEN
        	        	    IF (KCVT == 2) THEN
                                      GAMA(1) = LBUF%GAMA(JJ(2) + I)
                                      GAMA(2) = ZERO
                                      GAMA(3) = LBUF%GAMA(JJ(1) + I)
                                      GAMA(4) = GAMA(3)
                                      GAMA(5) = ZERO
                                      GAMA(6) =-GAMA(1)
                                    ELSE
                                      GAMA(1) = ONE
                                      GAMA(2) = ZERO
                                      GAMA(3) = ZERO
                                      GAMA(4) = ZERO
                                      GAMA(5) = ONE
                                      GAMA(6) = ZERO
                                    ENDIF ! IF (KCVT == 2)
        	        	    CALL SROTA6(X,IXS(1,N),KCVT,
     .  	        	                EVAR_TMP,GAMA, JHBE,IGTYP)
        	                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
        	                  END SELECT
        	                ENDIF ! IF (JHBE == 14)
                              ENDIF ! IF (IGTYP == 21)
                            ENDIF ! IF (KCVT /= 0 .AND.ICSIG > 0)
                            EVAR(1,I) = EVAR(1,I)+EVAR_TMP(1)
                            EVAR(2,I) = EVAR(2,I)+EVAR_TMP(2)
                            EVAR(3,I) = EVAR(3,I)+EVAR_TMP(3)
                            EVAR(4,I) = EVAR(4,I)+EVAR_TMP(4)
                            EVAR(5,I) = EVAR(5,I)+EVAR_TMP(5)
                            EVAR(6,I) = EVAR(6,I)+EVAR_TMP(6) 
                          ENDIF ! IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0)
                        ENDDO ! DO IR=1,NPTR
                      ENDDO ! DO IT=1,NPTT
                    ENDDO ! DO IS=1,NPTS
                  ENDDO ! DO IL=1,NLAY
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (MLW == 24)
c--- 
              ICSIG = IPARG(17,NG) 
              IF (KCVT /= 0 .AND. ICSIG == 0 .AND. JHBE /= 16) THEN
C             PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT					   
                  N = I + NFT					   
                  IF (EL2FA(NN2+N) /= 0) THEN			   
                    IF (KCVT == 2) THEN				   
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)			
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)			
                      GAMA(3) = GBUF%GAMA(JJ(3) + I)			
                      GAMA(4) = GBUF%GAMA(JJ(4) + I)			
                      GAMA(5) = GBUF%GAMA(JJ(5) + I)			
                      GAMA(6) = GBUF%GAMA(JJ(6) + I)			
                    ELSE					   
                      GAMA(1) = ONE				   
                      GAMA(2) = ZERO				   
                      GAMA(3) = ZERO				   
                      GAMA(4) = ZERO				   
                      GAMA(5) = ONE				   
                      GAMA(6) = ZERO				   
                    ENDIF !IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,		   
     .          	        EVAR(1,I),GAMA, JHBE,IGTYP)	   
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)					   
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (KCVT /= 0 .AND. ICSIG == 0 .AND. JHBE /= 16)
c-----------
            ELSEIF (ISOLNOD == 10 .OR. (ISOLNOD == 4 .AND. ISROT == 1)) THEN
c-----------
              IF (MLW == 24 .AND. ISTRAIN > 0) THEN
                DO I=LFT,LLT
                  N = I + NFT  
                  DO IPT=1,NPT
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)        
!
                    EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)/NPT
                    EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)/NPT
                    EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)/NPT
                    EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF/NPT
                    EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF/NPT
                    EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF/NPT
!
                  ENDDO
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF ((MLW == 24 .AND. ISTRAIN > 0)
!
              IF (KCVT /= 0) THEN
!             PALSTIC STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT                              
                  N = I + NFT                             
                  IF (EL2FA(NN2+N) /= 0) THEN               
                    IF (KCVT == 2) THEN                       
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)   
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)   
                      GAMA(3) = GBUF%GAMA(JJ(3) + I)   
                      GAMA(4) = GBUF%GAMA(JJ(4) + I)   
                      GAMA(5) = GBUF%GAMA(JJ(5) + I)   
                      GAMA(6) = GBUF%GAMA(JJ(6) + I)   
                    ELSE                                  
                      GAMA(1) = ONE                          
                      GAMA(2) = ZERO                        
                      GAMA(3) = ZERO                        
                      GAMA(4) = ZERO                        
                      GAMA(5) = ONE                          
                      GAMA(6) = ZERO                        
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,            
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)             
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)                                   
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (KCVT /= 0)
c-----------
            ELSEIF ((ISOLNOD == 6 .OR. ISOLNOD == 8) .AND. JHBE == 15) THEN
c-----------
              IF (MLW == 24 .AND. ISTRAIN > 0) THEN
                DO I=LFT,LLT                                               
                  N = I + NFT                                              
                  DO IL= 1,NLAY
                    DO IPT=1,NPTG                                            
                      LBUF =>  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IPT,1,1)          
                      EVAR(1,I) = EVAR(1,I)+LBUF%PLA(JJ(1) + I + NEL)/(NPTG*NLAY)	       
                      EVAR(2,I) = EVAR(2,I)+LBUF%PLA(JJ(2) + I + NEL)/(NPTG*NLAY)	       
                      EVAR(3,I) = EVAR(3,I)+LBUF%PLA(JJ(3) + I + NEL)/(NPTG*NLAY)   
                      EVAR(4,I) = EVAR(4,I)+LBUF%PLA(JJ(4) + I + NEL)*
     .                            HALF/(NPTG*NLAY)      
                      EVAR(5,I) = EVAR(5,I)+LBUF%PLA(JJ(5) + I + NEL)*
     .                            HALF/(NPTG*NLAY) 
                      EVAR(6,I) = EVAR(6,I)+LBUF%PLA(JJ(6) + I + NEL)*
     .                            HALF/(NPTG*NLAY)   
                    ENDDO                                                    
                  ENDDO                                                    
                ENDDO                                                      
              ENDIF ! IF (MLW == 24 .AND. ISTRAIN > 0)
!
              IF (KCVT /= 0) THEN                     
C             PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM         
                DO I=LFT,LLT                          
                  N = I + NFT                         
                  IF (EL2FA(NN2+N) /= 0) THEN           
                    IF (KCVT == 2) THEN                   
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)  
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)  
                      GAMA(3) = ZERO                    
                      GAMA(4) =-GAMA(2)                 
                      GAMA(5) = GAMA(1)                 
                      GAMA(6) = ZERO                    
                    ELSE                               
                      GAMA(1) = ONE                       
                      GAMA(2) = ZERO                     
                      GAMA(3) = ZERO                     
                      GAMA(4) = ZERO                     
                      GAMA(5) = ONE                       
                      GAMA(6) = ZERO                     
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,       
     .                   EVAR(1,I),GAMA, JHBE,IGTYP)        
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (KCVT /= 0)
c-----------
            ENDIF  ! ISOLNOD & ......
C-----------------------------------------------
C    - END OF -     FULL PLASTIC STRAIN TENSOR (MEAN)
C-----------------------------------------------
C          STRESS / integration point
C-----------------------------------------------
          ELSEIF (ITENS>=10.AND.ITENS<=1009)THEN
            PTI = ITENS - 10
c-----------
            IF (ISOLNOD == 8 .AND. IGTYP == 43) THEN
c-----------
             IF(IVISC == 0) THEN
              DO I=LFT,LLT                                        
                DO IPT= 1,NPTR
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)
                  EVAR(3,I) = EVAR(3,I) + LBUF%SIG(JJ(3) + I)/NPTR
                  EVAR(2,I) = EVAR(2,I) + LBUF%SIG(JJ(5) + I)/NPTR
                  EVAR(1,I) = EVAR(1,I) + LBUF%SIG(JJ(6) + I)/NPTR
                ENDDO                                              
              ENDDO  
             ELSE
              DO I=LFT,LLT                                      
                DO IPT= 1,NPTR
                 LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1) 
                 EVAR(3,I)= EVAR(3,I)+ LBUF%SIG(JJ(3) + I)/NPTR+ LBUF%VISC(JJ(3) + I)/NPTR
                 EVAR(2,I)= EVAR(2,I)+ LBUF%SIG(JJ(5) + I)/NPTR+ LBUF%VISC(JJ(5) + I)/NPTR
                 EVAR(1,I)= EVAR(1,I)+ LBUF%SIG(JJ(6) + I)/NPTR+ LBUF%VISC(JJ(6) + I)/NPTR
                ENDDO                                          
              ENDDO 
             ENDIF                                                  
             DO I=LFT,LLT                        
               N = I + NFT                         
               IF(EL2FA(NN2+N) /= 0)THEN           
                 CALL SROTA6(X,IXS(1,N),KCVT,EVAR(1,I),GAMA,JHBE,IGTYP)     
               ENDIF                               
             ENDDO                                 
c-----------
            ELSEIF (ISOLNOD == 8.AND.NPT == 8.AND.
     .          JHBE /= 14.AND.JHBE /= 24.AND.JHBE /= 15) THEN
c-----------
c              NPTR=TWO
c              NPTS=TWO
c              NPTT=TWO
              IR = ABS(PTI)/100
              IS = MOD(ABS(PTI)/10,10)
              IT = MOD(ABS(PTI),10)
              IF (IR == 0 .AND. IT == 0)THEN

              ELSEIF(IR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT)THEN  
                IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR   
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)          
                IF (IPT <= 8 )THEN         
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)	 
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)	 
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)	 
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)	 
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)	 
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)	 
                  ENDDO
                  IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I) 
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I) 
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I) 
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I) 
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I) 
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I) 
                    ENDDO
                  ENDIF 
                ENDIF
                IF (KCVT /= 0) THEN
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN2+N) /= 0)THEN
                      IF(KCVT==2)THEN
                        GAMA(1)= GBUF%GAMA(JJ(1) + I)
                        GAMA(2)= GBUF%GAMA(JJ(2) + I)
                        GAMA(3)= GBUF%GAMA(JJ(3) + I)
                        GAMA(4)= GBUF%GAMA(JJ(4) + I)
                        GAMA(5)= GBUF%GAMA(JJ(5) + I)
                        GAMA(6)= GBUF%GAMA(JJ(6) + I)
                      ELSE
                        GAMA(1)=ONE
                        GAMA(2)=ZERO
                        GAMA(3)=ZERO
                        GAMA(4)=ZERO
                        GAMA(5)=ONE
                        GAMA(6)=ZERO
                      END IF
                      CALL SROTA6(X,IXS(1,N),KCVT,
     .                  EVAR(1,I),GAMA, JHBE,IGTYP)
                    ENDIF
                  ENDDO
                ENDIF
              ELSE
                DO I=LFT,LLT
                  EVAR(1,I) = ZERO
                  EVAR(2,I) = ZERO
                  EVAR(3,I) = ZERO
                  EVAR(4,I) = ZERO
                  EVAR(5,I) = ZERO
                  EVAR(6,I) = ZERO
                ENDDO
              ENDIF
c-----------
            ELSEIF((ISOLNOD == 8.OR.NPT == 1) .AND.
     .              JHBE /= 14.AND.JHBE /= 15.AND.JHBE /= 17)THEN
c-----------
              NPTR= ONE
              NPTS= ONE
              NPTT= ONE
              IR  = ABS(PTI)/100
              IS  = MOD(ABS(PTI)/10,10)
              IT  = MOD(ABS(PTI),10)
              IF (IR == 0 .AND. IT == 0)THEN
              ELSE
                IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                IF (IPT == 1 )THEN         
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                  ENDDO
                  IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                ENDIF
                IF (KCVT /= 0) THEN
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN2+N) /= 0)THEN
                      IF(KCVT==2)THEN
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
                      CALL SROTA6(X,IXS(1,N),KCVT,
     .                  EVAR(1,I),GAMA, JHBE,IGTYP)
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
c-----------
            ELSEIF (ISOLNOD == 20 .OR. ISOLNOD == 16 ) THEN 
c-----------
              IR=ABS(PTI)/100
              IS=MOD(ABS(PTI)/10,10)
              IT=MOD(ABS(PTI),10)
                IF (IR == 0 .OR. IS == 0.OR. IT == 0) CYCLE
                  IF (TSHELL == 1 .AND. IS <= NLAY ) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(IS)%LBUF(IR,1,IT)
                    IOK = 1
                  ELSEIF(IR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    IOK = 1
                  ENDIF         
                IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                IF (IOK==1) THEN
                  DO I=LFT,LLT 
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                  ENDDO
                   IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                   ENDIF
                ENDIF
                IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN2+N) /= 0)THEN
                      IF(KCVT==2)THEN
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
                      CALL SROTA6(X,IXS(1,N),KCVT,
     .                            EVAR(1,I),GAMA, JHBE,IGTYP)
                    ENDIF
                  ENDDO
                ENDIF
c-----------
            ELSEIF (ISOLNOD == 8 .AND. JHBE == 14 )THEN
c-----------
                ICSIG = IPARG(17,NG)
                NPTG = NPTR * NPTS * NPTT * NLAY
                IR0=ABS(PTI)/100
                IS0=MOD(ABS(PTI)/10,10)
                IT0=MOD(ABS(PTI),10)
                IPID = IXS(10,1 + NFT)
                IF (IR0==0.OR.IS0==0.OR.IT0==0) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
                IF (TSHELL == 1) THEN
                  IF (ICSIG==100) THEN
                    IR = IS0
                    IS = IT0
                    IT = IR0
                  ELSEIF (ICSIG==10) THEN
                    IR = IT0
                    IS = IR0
                    IT = IS0
                  ELSE
                    IR = IR0
                    IS = IS0
                    IT = IT0
                  END IF
                ENDIF
C---------------             
                  IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                  IOK = 0
                  IF (TSHELL == 1 .AND. IT <= NLAY ) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)
                    IOK = 1
                  ELSEIF(IR0 <= NPTR .AND. IS0 <= NPTS .AND. IT0 <= NPTT) THEN
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                    IOK = 1
                  ENDIF         
                  IF ( IOK == 1) THEN
                    DO I=LFT,LLT 
                      EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                      EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                      EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                      EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                      EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                      EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                    ENDDO
                    IF(IVISC > 0) THEN
                     DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                     ENDDO
                    ENDIF
                  ENDIF
                  IF (KCVT /= 0) THEN
C               STRESS TENSOR IN GLOBAL SYSTEM
C--------------thick shells----only pid21,irep=0--works--------
                  IF (ICSIG >0) THEN
                    IF (IGTYP == 21) THEN
                      SELECT CASE (ICSIG)                                             
                      CASE (1)                                                        
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=ZERO
                            GAMA(2)=GBUF%GAMA(JJ(1) + I)
                            GAMA(3)=GBUF%GAMA(JJ(2) + I)
                            GAMA(4)=ZERO
                            GAMA(5)=-GAMA(2)
                            GAMA(6)=GAMA(1)
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      CASE (10)                
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=GBUF%GAMA(JJ(1) + I)
                            GAMA(2)=GBUF%GAMA(JJ(2) + I)
                            GAMA(3)=ZERO
                            GAMA(4)=-GAMA(2)
                            GAMA(5)=GAMA(1)
                            GAMA(6)=ZERO
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      CASE (100)                                                        
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=GBUF%GAMA(JJ(2) + I)
                            GAMA(2)=ZERO
                            GAMA(3)=GBUF%GAMA(JJ(1) + I)
                            GAMA(4)=GAMA(3)
                            GAMA(5)=ZERO
                            GAMA(6)=-GAMA(1)
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      END SELECT 
                    ELSE
                      SELECT CASE (ICSIG)                                             
                      CASE (1)                                                        
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=ZERO
                            GAMA(2)=LBUF%GAMA(JJ(1) + I)
                            GAMA(3)=LBUF%GAMA(JJ(2) + I)
                            GAMA(4)=ZERO
                            GAMA(5)=-GAMA(2)
                            GAMA(6)=GAMA(1)
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      CASE (10)                
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=LBUF%GAMA(JJ(1) + I)
                            GAMA(2)=LBUF%GAMA(JJ(2) + I)
                            GAMA(3)=ZERO
                            GAMA(4)=-GAMA(2)
                            GAMA(5)=GAMA(1)
                            GAMA(6)=ZERO
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      CASE (100)                                                        
                      DO I=LFT,LLT
                        N = I + NFT
                        IF(EL2FA(NN2+N) /= 0)THEN
                          IF(KCVT==2)THEN
                            GAMA(1)=LBUF%GAMA(JJ(2) + I)
                            GAMA(2)=ZERO
                            GAMA(3)=LBUF%GAMA(JJ(1) + I)
                            GAMA(4)=GAMA(3)
                            GAMA(5)=ZERO
                            GAMA(6)=-GAMA(1)
                          ELSE
                            GAMA(1)=ONE
                            GAMA(2)=ZERO
                            GAMA(3)=ZERO
                            GAMA(4)=ZERO
                            GAMA(5)=ONE
                            GAMA(6)=ZERO
                          END IF
                          CALL SROTA6(X,IXS(1,N),KCVT,
     .                      EVAR(1,I),GAMA, JHBE,IGTYP)
                        ENDIF
                      ENDDO
                      END SELECT 
                    ENDIF
                  ELSE                                                            
                    DO I=LFT,LLT
                      N = I + NFT
                      IF(EL2FA(NN2+N) /= 0)THEN
                        IF(KCVT==2)THEN
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
                        CALL SROTA6(X,IXS(1,N),KCVT,
     .                    EVAR(1,I),GAMA, JHBE,IGTYP)
                      ENDIF
                    ENDDO
                  ENDIF !(ICSIG >0)
              ENDIF
c-----------
            ELSEIF(ISOLNOD == 10.OR.(ISOLNOD == 4 .AND. ISROT == 1))THEN
c-----------
              IR=ABS(PTI)/100
              IS=MOD(ABS(PTI)/10,10)
              IT=MOD(ABS(PTI),10)
              IF (IR == 0 .AND. IT == 0)THEN
              ELSE
                IPT = 0
                IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
                IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
                IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
                IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
                IF (IPT > 0) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                    EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                    EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                    EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                    EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                    EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                  ENDDO 
                   IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                ENDIF 
                IF (KCVT /= 0) THEN
C                 STRESS TENSOR IN GLOBAL SYSTEM
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN2+N) /= 0)THEN
                      IF(KCVT==2)THEN
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
                      CALL SROTA6(X,IXS(1,N),KCVT,
     .                  EVAR(1,I),GAMA, JHBE,IGTYP)
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
c-----------
            ELSEIF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
              IPT = MOD(ABS(PTI)/10,10)
              IF ( IPT > 0 .AND. IPT<=NLAY) THEN
               
               LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1)          
                DO I=LFT,LLT
                  EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                  EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                  EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                  EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                  EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                  EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                ENDDO 
                 IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                IF (KCVT==2) THEN
C               STRESS TENSOR IN GLOBAL SYSTEM
                 DO I=LFT,LLT
                  N = I + NFT
                   IF(EL2FA(NN2+N) /= 0)THEN
                      GAMA(1)= GBUF%GAMA(JJ(1) + I)
                      GAMA(2)= GBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)
                   ENDIF
                 ENDDO
                ENDIF
              ENDIF   
            ENDIF
c-----------
c
           IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
             DO I=LFT,LLT
               EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
               EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
               EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
               EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
               EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
               EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
             ENDDO
           ENDIF
C-----------------------------------------------
C         STRESS TENSOR / integration point more than 9 point in direction s
          ELSEIF(ITENS>=2010.AND.ITENS<=22109) THEN
C-------------- case NLAY>9
            PTI = ITENS - 2010
            IF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
              IPT = MOD(ABS(PTI)/10,201)
              IF ( IPT > 0 .AND. IPT<=NLAY .AND.NLAY>9) THEN
               
               LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1)          
                DO I=LFT,LLT
                  EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                  EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                  EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                  EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                  EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                  EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                ENDDO 
                 IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF
                IF (KCVT==2) THEN
C               STRESS TENSOR IN GLOBAL SYSTEM
                 DO I=LFT,LLT
                  N = I + NFT
                   IF(EL2FA(NN2+N) /= 0)THEN
                      GAMA(1)= LBUF%GAMA(JJ(1) + I)
                      GAMA(2)= LBUF%GAMA(JJ(2) + I)
                      GAMA(3)= ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)= GAMA(1)
                      GAMA(6)= ZERO
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)
                   ENDIF
                 ENDDO
                ENDIF
              ENDIF   
c-----------
            ELSEIF (ISOLNOD == 16.OR.(ISOLNOD == 8 .AND.JHBE == 14)) THEN
c----------- ISOLNOD=16 is not available w/ TYPE22 but keep here however
              ICSIG = IPARG(17,NG) 
              IR0=ABS(PTI)/2010
              IS0=MOD(ABS(PTI)/10,201)
              IT0=MOD(ABS(PTI),10)
     	      IF (IR0==0.OR.IS0==0.OR.IT0==0.OR.NLAY<10) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
     	      IF (TSHELL == 1) THEN
                IF (ICSIG==100) THEN
                  IR = IS0
                  IS = IT0
                  IT = IR0
                ELSEIF (ICSIG==10) THEN
                  IR = IT0
                  IS = IR0
                  IT = IS0
                ELSE
                  IR = IR0
                  IS = IS0
                  IT = IT0
                END IF
              ENDIF
     	      IF (IR>NPTR.OR.IS>NPTS) CYCLE
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF ( IPT <= NPT ) THEN                 
                IF (TSHELL == 1) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)
                ELSE
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                ENDIF         
                DO I=LFT,LLT                          
                  EVAR(1,I) = LBUF%SIG(JJ(1) + I)
                  EVAR(2,I) = LBUF%SIG(JJ(2) + I)
                  EVAR(3,I) = LBUF%SIG(JJ(3) + I)
                  EVAR(4,I) = LBUF%SIG(JJ(4) + I)
                  EVAR(5,I) = LBUF%SIG(JJ(5) + I)
                  EVAR(6,I) = LBUF%SIG(JJ(6) + I)
                ENDDO 
                 IF(IVISC > 0) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) =EVAR(1,I)+LBUF%VISC(JJ(1) + I)
                      EVAR(2,I) =EVAR(2,I)+LBUF%VISC(JJ(2) + I)
                      EVAR(3,I) =EVAR(3,I)+LBUF%VISC(JJ(3) + I)
                      EVAR(4,I) =EVAR(4,I)+LBUF%VISC(JJ(4) + I)
                      EVAR(5,I) =EVAR(5,I)+LBUF%VISC(JJ(5) + I)
                      EVAR(6,I) =EVAR(6,I)+LBUF%VISC(JJ(6) + I)
                    ENDDO
                  ENDIF                                
              ENDIF                                   
              IF (KCVT /= 0 .AND. JHBE /= 16) THEN                     
C               STRESS TENSOR IN GLOBAL SYSTEM        
C-------------- thick shells----only pid21,irep=0--works--------
                SELECT CASE (ICSIG)                                             
                  CASE (1)                                                        
                    DO I=LFT,LLT                                          
                      N = I + NFT                                         
                      IF(EL2FA(NN2+N) /= 0)THEN                           
                        IF(KCVT==2)THEN                                   
                          GAMA(1)=ZERO                                    
                          GAMA(2)=LBUF%GAMA(JJ(1) + I)                
                          GAMA(3)=LBUF%GAMA(JJ(2) + I)              
                          GAMA(4)=ZERO                                    
                          GAMA(5)=-GAMA(2)                                
                          GAMA(6)=GAMA(1)                                 
                        ELSE                                              
                          GAMA(1)=ONE                                      
                          GAMA(2)=ZERO                                    
                          GAMA(3)=ZERO                                    
                          GAMA(4)=ZERO                                    
                          GAMA(5)=ONE                                      
                          GAMA(6)=ZERO                                    
                        END IF                                            
                        CALL SROTA6(X,IXS(1,N),KCVT,                      
     .                    EVAR(1,I),GAMA, JHBE,IGTYP)                     
                      ENDIF                                               
                    ENDDO                                                 
                  CASE (10)                                                       
                    DO I=LFT,LLT                                          
                      N = I + NFT                                         
                      IF(EL2FA(NN2+N) /= 0)THEN                           
                        IF(KCVT==2)THEN                                   
                          GAMA(1)=LBUF%GAMA(JJ(1) + I)                
                          GAMA(2)=LBUF%GAMA(JJ(2) + I)
                          GAMA(3)=ZERO                                    
                          GAMA(4)=-GAMA(2)                                
                          GAMA(5)=GAMA(1)                                 
                          GAMA(6)=ZERO                                    
                        ELSE                                              
                          GAMA(1)=ONE                                      
                          GAMA(2)=ZERO                                    
                          GAMA(3)=ZERO                                    
                          GAMA(4)=ZERO                                    
                          GAMA(5)=ONE                                      
                          GAMA(6)=ZERO                                    
                        END IF                                            
                        CALL SROTA6(X,IXS(1,N),KCVT,                      
     .                    EVAR(1,I),GAMA, JHBE,IGTYP)                     
                      ENDIF                                               
                    ENDDO                                                 
                  CASE (100)                                                      
                    DO I=LFT,LLT                                          
                      N = I + NFT                                         
                      IF(EL2FA(NN2+N) /= 0)THEN                           
                        IF(KCVT==2)THEN                                   
                          GAMA(1)=LBUF%GAMA(JJ(2) + I)                
                          GAMA(2)=ZERO                                    
                          GAMA(3)=LBUF%GAMA(JJ(1) + I)              
                          GAMA(4)=GAMA(3)                                 
                          GAMA(5)=ZERO                                    
                          GAMA(6)=-GAMA(1)                                
                        ELSE                                              
                          GAMA(1)=ONE                                      
                          GAMA(2)=ZERO                                    
                          GAMA(3)=ZERO                                    
                          GAMA(4)=ZERO                                    
                          GAMA(5)=ONE                                      
                          GAMA(6)=ZERO                                    
                        END IF                                            
                        CALL SROTA6(X,IXS(1,N),KCVT,                      
     .                    EVAR(1,I),GAMA, JHBE,IGTYP)                     
                      ENDIF                                               
                    ENDDO                                                
                  END SELECT                                               
              END IF !(KCVT /= 0 .AND. JHBE /= 16) THEN                     
c-----------
            ENDIF     ! ISOLNOD
c
           IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) THEN
             DO I=LFT,LLT
               EVAR(1,I) = EVAR(1,I) * GBUF%FILL(I)
               EVAR(2,I) = EVAR(2,I) * GBUF%FILL(I)
               EVAR(3,I) = EVAR(3,I) * GBUF%FILL(I)
               EVAR(4,I) = EVAR(4,I) * GBUF%FILL(I)
               EVAR(5,I) = EVAR(5,I) * GBUF%FILL(I)
               EVAR(6,I) = EVAR(6,I) * GBUF%FILL(I)
             ENDDO
           ENDIF
C-----------------------------------------------
C          STRAIN / integration point
          ELSEIF (ITENS>=1010.AND.ITENS<=2009) THEN
C-----------------------------------------------
            PTI = ITENS - 1010
c-----------
            IF (ISOLNOD == 8.AND.NPT == 8 .AND.
     .     JHBE /= 14.AND.JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17) THEN
c-----------
c              NPTR=2
c              NPTS=2
c              NPTT=2
              IR=ABS(PTI)/100
              IS=MOD(ABS(PTI)/10,10)
              IT=MOD(ABS(PTI),10)
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR 
              IF (IPT <= 8) THEN   
                IF(IR <= NPTR .AND. IS <= NPTS .AND. IT <= NPTT)THEN        
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)          
                  IF (MLW >= 28) THEN
                    DO I=LFT,LLT
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)   
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)   
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)   
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)   
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)   
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)   
                    ENDDO
                  ENDIF
                ELSE
                  DO I=LFT,LLT
                    EVAR(1,I) = ZERO  
                    EVAR(2,I) = ZERO
                    EVAR(3,I) = ZERO
                    EVAR(4,I) = ZERO
                    EVAR(5,I) = ZERO
                    EVAR(6,I) = ZERO
                  ENDDO
                ENDIF
              ENDIF
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
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
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                 EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
              ENDIF
c-----------
            ELSEIF ((ISOLNOD == 8 .OR.NPT == 1 .OR.
     .              (ISOLNOD == 4 .AND. ISROT == 0)) .AND.
     .               JHBE /= 14.AND.JHBE /= 15.AND.JHBE /= 17) THEN 
c-----------
c              NPTR=ONE
c              NPTS=ONE
c              NPTT=ONE
              IR=ABS(PTI)/100
              IS=MOD(ABS(PTI)/10,10)
              IT=MOD(ABS(PTI),10)
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF (IPT == 1 ) THEN          
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
                IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN 
                  DO I=LFT,LLT
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                  END DO
                ELSEIF(MLW == 12 .OR. MLW == 14) THEN 
                  DO I=LFT,LLT
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)        
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)        
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)        
                  ENDDO  
                ELSEIF (ISTRAIN > 0)THEN 
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28.OR.
     .               MLW == 49) THEN                          
                    DO I=LFT,LLT 
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF  
C
              IF (KCVT /= 0) THEN
C               STRAIN TENSOR IN GLOBAL SYSTEM
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
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
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                   EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
              ENDIF     
c              DO I=LFT,LLT
c                EVAR(4,I) = EVAR(4,I) * TWO
c                EVAR(5,I) = EVAR(5,I) * TWO
c                EVAR(6,I) = EVAR(6,I) * TWO
c              ENDDO
c-----------
           ELSEIF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .            (JHBE == 14 .OR. JHBE == 17))) THEN
c-----------
               ICSIG = IPARG(17,NG) 
               IR0=ABS(PTI)/100
               IS0=MOD(ABS(PTI)/10,10)
               IT0=MOD(ABS(PTI),10)
                IF (IR0==0.OR.IS0==0.OR.IT0==0) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
                IF (TSHELL == 1) THEN
                  IF (ICSIG==100) THEN
                    IR = IS0
                    IS = IT0
                    IT = IR0
                  ELSEIF (ICSIG==10) THEN
                    IR = IT0
                    IS = IR0
                    IT = IS0
                  ELSE
                    IR = IR0
                    IS = IS0
                    IT = IT0
                  END IF
                ENDIF
                IF (IR>NPTR.OR.IS>NPTS) CYCLE
           IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
             IOK = 0
             IF (TSHELL == 1) THEN                              
               IF (ISOLNOD == 16.AND. IS0 <= NLAY) THEN                              
                 LBUF =>  ELBUF_TAB(NG)%BUFLY(IS0)%LBUF(IR,1,IT)   
                 IOK = 1
               ELSEIF (IT <= NLAY) THEN                              
                 LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)   
                 IOK = 1
               END IF
             ELSE                                               
               LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)   
               IOK = 1
             ENDIF                                          
           IF (IOK == 1 ) THEN
             IF(MLW>=28.AND.MLW /= 49)THEN        
               DO I=LFT,LLT
C                3*9*3 points d'integration (r*s*t)    
                  EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                  EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                  EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                  EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                  EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                  EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
               ENDDO
             ELSEIF(MLW == 12 .OR. MLW == 14)THEN
               DO I=LFT,LLT
C              3*9*3 points d'integration (r*s*t)       
                 EVAR(1,I) = LBUF%EPE(JJ(1) + I)   
                 EVAR(2,I) = LBUF%EPE(JJ(2) + I)   
                 EVAR(3,I) = LBUF%EPE(JJ(3) + I)   
               ENDDO       
             ELSEIF(MLW == 24 .OR. MLW == 25)THEN
               DO I=LFT,LLT
C                   3*9*3 points d'integration (r*s*t)         
                 EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                 EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                 EVAR(3,I) = LBUF%STRA(JJ(3) + I)		    
                 EVAR(4,I) = LBUF%STRA(JJ(4) + I) * HALF
                 EVAR(5,I) = LBUF%STRA(JJ(5) + I) * HALF
                 EVAR(6,I) = LBUF%STRA(JJ(6) + I) * HALF 
               ENDDO   
             ELSEIF (MLW == 25) THEN
                 DO I=LFT,LLT 
                 EVAR(1,I) =  LBUF%STRA(JJ(1) + I)		  
                 EVAR(2,I) =  LBUF%STRA(JJ(2) + I)		  
                 EVAR(3,I) =  LBUF%STRA(JJ(3) + I)		  
                 EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF	  
                 EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF		   
                 EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF	  
		               ENDDO  
               ELSEIF(ISTRAIN > 0)THEN
                 IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN
                   DO I=LFT,LLT 
C  3*9*3 points d'integration (r*s*t)    
                    EVAR(1,I) =  LBUF%STRA(JJ(1) + I)			    
                    EVAR(2,I) =  LBUF%STRA(JJ(2) + I)			    
                    EVAR(3,I) =  LBUF%STRA(JJ(3) + I)			    
                    EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF  	    
                    EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF  	      
                    EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF  	    
                   ENDDO 
                 ENDIF
               ENDIF
C
               IF (KCVT >1 .AND. JHBE /= 16) THEN
C              STRAIN TENSOR IN GLOBAL SYSTEM
            	  ICSIG=IPARG(17,NG)  
            	  IF (JHBE == 14.AND.ICSIG > 0) THEN
                    IF (IGTYP == 21) THEN
            	      SELECT CASE (ICSIG)                                             
            	      CASE (1)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
           	            GAMA(1)=ZERO
       	                    GAMA(2)=GBUF%GAMA(JJ(1) + I)
                            GAMA(3)=GBUF%GAMA(JJ(2) + I)
            	            GAMA(4)=ZERO
            	            GAMA(5)=-GAMA(2)
            	            GAMA(6)=GAMA(1)
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      CASE (10)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
                            GAMA(1)=GBUF%GAMA(JJ(1) + I)
                            GAMA(2)=GBUF%GAMA(JJ(2) + I)
            	            GAMA(3)=ZERO
            	            GAMA(4)=-GAMA(2)
            	            GAMA(5)=GAMA(1)
            	            GAMA(6)=ZERO
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      CASE (100)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
                            GAMA(1)=GBUF%GAMA(JJ(2) + I)
            	            GAMA(2)=ZERO
                            GAMA(3)=GBUF%GAMA(JJ(1) + I)
            	            GAMA(4)=GAMA(3)
            	            GAMA(5)=ZERO
            	            GAMA(6)=-GAMA(1)
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      END SELECT 
                    ELSE
            	      SELECT CASE (ICSIG)                                             
            	      CASE (1)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
           	            GAMA(1)=ZERO
       	                    GAMA(2)=LBUF%GAMA(JJ(1) + I)
                            GAMA(3)=LBUF%GAMA(JJ(2) + I)
            	            GAMA(4)=ZERO
            	            GAMA(5)=-GAMA(2)
            	            GAMA(6)=GAMA(1)
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      CASE (10)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
                            GAMA(1)=LBUF%GAMA(JJ(1) + I)
                            GAMA(2)=LBUF%GAMA(JJ(2) + I)
            	            GAMA(3)=ZERO
            	            GAMA(4)=-GAMA(2)
            	            GAMA(5)=GAMA(1)
            	            GAMA(6)=ZERO
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      CASE (100)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF(EL2FA(NN2+N) /= 0)THEN
            	          IF(KCVT==2)THEN
                            GAMA(1)=LBUF%GAMA(JJ(2) + I)
            	            GAMA(2)=ZERO
                            GAMA(3)=LBUF%GAMA(JJ(1) + I)
            	            GAMA(4)=GAMA(3)
            	            GAMA(5)=ZERO
            	            GAMA(6)=-GAMA(1)
            	          ELSE
            	            GAMA(1)=ONE
            	            GAMA(2)=ZERO
            	            GAMA(3)=ZERO
            	            GAMA(4)=ZERO
            	            GAMA(5)=ONE
            	            GAMA(6)=ZERO
            	          END IF
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	            EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF
            	      ENDDO
            	      END SELECT 
            	    ENDIF
	    	  ELSE  							
            	    DO I=LFT,LLT
            	      N = I + NFT
            	      IF(EL2FA(NN2+N) /= 0)THEN
            		IF(KCVT==2)THEN
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
            		CALL SROTA6(X,IXS(1,N),KCVT,
     .      		EVAR(1,I),GAMA, JHBE,IGTYP)
            	      ENDIF
            	    ENDDO
            	   ENDIF  !(JHBE == 14.AND.ICSIG > 0)	 
               ENDIF
             ENDIF

c             DO I=LFT,LLT
c               EVAR(4,I) = EVAR(4,I) * TWO
c               EVAR(5,I) = EVAR(5,I) * TWO
c               EVAR(6,I) = EVAR(6,I) * TWO
c             ENDDO 
c-----------
           ELSEIF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
              IPT = MOD(ABS(PTI)/10,10)
              IF ( IPT > 0 .AND. IPT<=NLAY ) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1) 
                IF(MLW>=28.AND.MLW /= 49)THEN        
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                  ENDDO
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%EPE(JJ(1) + I)
                    EVAR(2,I) = LBUF%EPE(JJ(2) + I)
                    EVAR(3,I) = LBUF%EPE(JJ(3) + I)  
                 ENDDO       
                ELSE
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)		    
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF   
                  ENDDO 
                END IF
                IF (KCVT /= 0 ) THEN
C           STRAIN TENSOR IN GLOBAL SYSTEM
                 DO I=LFT,LLT
                   N = I + NFT
                   IF(EL2FA(NN2+N) /= 0)THEN
                     IF(KCVT==2)THEN
                       GAMA(1)=GBUF%GAMA(JJ(1) + I)
                       GAMA(2)=GBUF%GAMA(JJ(2) + I)
                       GAMA(3)=ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)=GAMA(1)
                       GAMA(6)=ZERO
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(X,IXS(1,N),KCVT,
     .               EVAR(1,I),GAMA, JHBE,IGTYP)
                   ENDIF
                 ENDDO
                ENDIF 
              END IF
c-----------
            ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
              IR=ABS(PTI)/100
              IS=MOD(ABS(PTI)/10,10)
              IT=MOD(ABS(PTI),10)
              IPT = 0
              IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
              IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
              IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
              IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
              IF ( IPT > 0) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                IF (MLW>=28.AND.MLW /= 49 .OR. MLW == 24) THEN 
                  DO I=LFT,LLT
                    EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                  ENDDO
                ELSEIF (MLW == 12 .OR. MLW == 14) THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = EVAR(1,I) + LBUF%EPE(JJ(1) + I)       
                    EVAR(2,I) = EVAR(2,I) + LBUF%EPE(JJ(2) + I)       
                    EVAR(3,I) = EVAR(3,I) + LBUF%EPE(JJ(3) + I)       
                  ENDDO 
                ELSEIF (ISTRAIN > 0) THEN
                  IF (MLW /= 14.AND.MLW /= 24.AND.MLW<28) THEN               
                    DO I=LFT,LLT
                      EVAR(1,I) = EVAR(1,I) + LBUF%STRA(JJ(1) + I)
                      EVAR(2,I) = EVAR(2,I) + LBUF%STRA(JJ(2) + I)
                      EVAR(3,I) = EVAR(3,I) + LBUF%STRA(JJ(3) + I)
                      EVAR(4,I) = EVAR(4,I) + LBUF%STRA(JJ(4) + I)*HALF
                      EVAR(5,I) = EVAR(5,I) + LBUF%STRA(JJ(5) + I)*HALF
                      EVAR(6,I) = EVAR(6,I) + LBUF%STRA(JJ(6) + I)*HALF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF  
c
              IF (KCVT /= 0) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
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
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                 EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
              ENDIF  
c              DO I=LFT,LLT                    
c                EVAR(4,I) = EVAR(4,I) * TWO  
c                EVAR(5,I) = EVAR(5,I) * TWO  
c                EVAR(6,I) = EVAR(6,I) * TWO  
c              ENDDO                           
            END IF 
c-----------
C-----------------------------------------------
C        STRAIN TENSOR / integration point more than 9 point in direction s
          ELSEIF (ITENS>=22110.AND.ITENS<=42209) THEN
C-----------------------------------------------
            PTI = ITENS - 22110
            IF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
              IPT = MOD(ABS(PTI)/10,201)
              IF ( IPT > 0 .AND. IPT<=NLAY.AND.NLAY>9) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1) 
                IF(MLW>=28.AND.MLW /= 49)THEN        
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                  ENDDO
                ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%EPE(JJ(1) + I)
                    EVAR(2,I) = LBUF%EPE(JJ(2) + I)
                    EVAR(3,I) = LBUF%EPE(JJ(3) + I)  
                 ENDDO       
                ELSE
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                    EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                    EVAR(3,I) = LBUF%STRA(JJ(3) + I)		    
                    EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                    EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                    EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF   
                  ENDDO 
                END IF
                IF (KCVT /= 0 ) THEN
C           STRAIN TENSOR IN GLOBAL SYSTEM
                 DO I=LFT,LLT
                   N = I + NFT
                   IF(EL2FA(NN2+N) /= 0)THEN
                     IF(KCVT==2)THEN
                       GAMA(1)=LBUF%GAMA(JJ(1) + I)
                       GAMA(2)=LBUF%GAMA(JJ(2) + I)
                       GAMA(3)=ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)=GAMA(1)
                       GAMA(6)=ZERO
                     ELSE
                       GAMA(1)=ONE
                       GAMA(2)=ZERO
                       GAMA(3)=ZERO
                       GAMA(4)=ZERO
                       GAMA(5)=ONE
                       GAMA(6)=ZERO
                     END IF
                     CALL SROTA6(X,IXS(1,N),KCVT,
     .               EVAR(1,I),GAMA, JHBE,IGTYP)
                   ENDIF
                 ENDDO
                ENDIF 
              END IF
c-----------
	    ELSEIF (ISOLNOD==16.OR.(ISOLNOD==8.AND.JHBE==14)) THEN
c-----------
                 ICSIG = IPARG(17,NG) 
                 IR0=ABS(PTI)/2010
                 IS0=MOD(ABS(PTI)/10,201)
                 IT0=MOD(ABS(PTI),10)
                IF (IR0==0.OR.IS0==0.OR.IT0==0.OR.NLAY<10) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
                IF (TSHELL == 1) THEN
                  IF (ICSIG==100) THEN
                    IR = IS0
                    IS = IT0
                    IT = IR0
                  ELSEIF (ICSIG==10) THEN
                    IR = IT0
                    IS = IR0
                    IT = IS0
                  ELSE
                    IR = IR0
                    IS = IS0
                    IT = IT0
                  END IF
                ENDIF
     	        IF (IR>NPTR.OR.IS>NPTS) CYCLE
                 IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF (IPT  <= NPT ) THEN
                IF (TSHELL == 1) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)
                ELSE
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
                ENDIF         
c               LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1) ! TSHELL 
               IF(MLW>=28.AND.MLW /= 49)THEN        
                 DO I=LFT,LLT
                   EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                   EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                   EVAR(3,I) = LBUF%STRA(JJ(3) + I)
                   EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                   EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                   EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF
                 ENDDO
               ELSEIF(MLW == 12 .OR. MLW == 14)THEN
                 DO I=LFT,LLT
                   EVAR(1,I) = LBUF%EPE(JJ(1) + I)
                   EVAR(2,I) = LBUF%EPE(JJ(2) + I)
                   EVAR(3,I) = LBUF%EPE(JJ(3) + I)  
                 ENDDO       
               ELSEIF(MLW == 24 .OR. MLW == 25)THEN
                 DO I=LFT,LLT
                   EVAR(1,I) = LBUF%STRA(JJ(1) + I)
                   EVAR(2,I) = LBUF%STRA(JJ(2) + I)
                   EVAR(3,I) = LBUF%STRA(JJ(3) + I)		    
                   EVAR(4,I) = LBUF%STRA(JJ(4) + I)*HALF
                   EVAR(5,I) = LBUF%STRA(JJ(5) + I)*HALF
                   EVAR(6,I) = LBUF%STRA(JJ(6) + I)*HALF   
                 ENDDO 
               ELSEIF (MLW == 25) THEN
                 DO I=LFT,LLT 
                   EVAR(1,I) = LBUF%STRA(JJ(1) + I)		  
                   EVAR(2,I) = LBUF%STRA(JJ(2) + I)		  
                   EVAR(3,I) = LBUF%STRA(JJ(3) + I)		     
                   EVAR(4,I) = LBUF%STRA(JJ(4) + I) * HALF	  
                   EVAR(5,I) = LBUF%STRA(JJ(5) + I) * HALF		   
                   EVAR(6,I) = LBUF%STRA(JJ(6) + I) * HALF	  
                 ENDDO  
               ELSEIF(ISTRAIN > 0)THEN
                 IF(MLW /= 14.AND.MLW /= 24.AND.MLW<28)THEN
                   DO I=LFT,LLT 
                     EVAR(1,I) =  LBUF%STRA(JJ(1) + I)			      
                     EVAR(2,I) =  LBUF%STRA(JJ(2) + I)			      
                     EVAR(3,I) =  LBUF%STRA(JJ(3) + I)			      
                     EVAR(4,I) =  LBUF%STRA(JJ(4) + I) * HALF		      
                     EVAR(5,I) =  LBUF%STRA(JJ(5) + I) * HALF		      
                     EVAR(6,I) =  LBUF%STRA(JJ(6) + I) * HALF		      
                   ENDDO 
                 ENDIF
               END IF
              END IF
C
              IF (KCVT /= 0 .AND. JHBE /= 16) THEN
C           STRAIN TENSOR IN GLOBAL SYSTEM
               ICSIG=IPARG(17,NG)  
               IF (JHBE == 14.AND.ICSIG > 0) THEN
                SELECT CASE (ICSIG)                                             
                CASE (1)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
                      GAMA(1)=ZERO
                      GAMA(2)=LBUF%GAMA(JJ(1) + I)
                      GAMA(3)=LBUF%GAMA(JJ(2) + I)
                      GAMA(4)=ZERO
                      GAMA(5)=-GAMA(2)
                      GAMA(6)=GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
                CASE (10)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
                      GAMA(1)=LBUF%GAMA(JJ(1) + I)
                      GAMA(2)=LBUF%GAMA(JJ(2) + I)
                      GAMA(3)=ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)=GAMA(1)
                      GAMA(6)=ZERO
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
                CASE (100)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN2+N) /= 0)THEN
                    IF(KCVT==2)THEN
                      GAMA(1)=LBUF%GAMA(JJ(2) + I)
                      GAMA(2)=ZERO
                      GAMA(3)=LBUF%GAMA(JJ(1) + I)
                      GAMA(4)=GAMA(3)
                      GAMA(5)=ZERO
                      GAMA(6)=-GAMA(1)
                    ELSE
                      GAMA(1)=ONE
                      GAMA(2)=ZERO
                      GAMA(3)=ZERO
                      GAMA(4)=ZERO
                      GAMA(5)=ONE
                      GAMA(6)=ZERO
                    END IF
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF
                ENDDO
                END SELECT 
               END IF  !(JHBE == 14.AND.ICSIG > 0)    
              END IF 
	    END IF 
C-----------------------------------------------
!        PLASTIC STRAIN TENSOR / integration point
          ELSEIF (ITENS >= 42210 .AND. ITENS <= 43209) THEN
C--------------------------NLAY<10
            PTI = ITENS - 42210
c-----------
            IF (ISOLNOD == 16.OR.ISOLNOD == 20.OR.(ISOLNOD == 8.AND.
     .             (JHBE == 14   .OR. JHBE == 17))) THEN
c-----------
                ICSIG = IPARG(17,NG)
                IR0=ABS(PTI)/100
                IS0=MOD(ABS(PTI)/10,10)
                IT0=MOD(ABS(PTI),10)
                IPID = IXS(10,1 + NFT)
                IF (IR0==0.OR.IS0==0.OR.IT0==0) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
                IF (TSHELL == 1) THEN
                  IF (ICSIG==100) THEN
                    IR = IS0
                    IS = IT0
                    IT = IR0
                  ELSEIF (ICSIG==10) THEN
                    IR = IT0
                    IS = IR0
                    IT = IS0
                  ELSE
                    IR = IR0
                    IS = IS0
                    IT = IT0
                  END IF
                ENDIF
                IF (IR>NPTR.OR.IS>NPTS) CYCLE
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
               IOK = 0
                IF (TSHELL == 1) THEN                              
                  IF (ISOLNOD == 16.AND. IS0 <= NLAY) THEN                              
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(IS0)%LBUF(IR,1,IT)   
                    IOK = 1
                  ELSEIF (IT <= NLAY) THEN                              
                    LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1)   
                    IOK = 1
                  ENDIF                                          
                ELSE                                               
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)   
                  IOK = 1
                ENDIF                                          
              IF (IOK == 1 ) THEN
!
                IF (MLW == 24) THEN
                  DO I=LFT,LLT
C                  3*9*3 points d'integration (r*s*t)         
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)		    
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL) * HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL) * HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL) * HALF 
                  ENDDO   
                ENDIF ! IF (MLW == 24)
!
                IF (KCVT /= 0 .AND. JHBE /= 16) THEN
!               PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
                  ICSIG=IPARG(17,NG)
                  IF (JHBE == 14 .AND. ICSIG > 0) THEN
                    IF (IGTYP == 21) THEN
                     SELECT CASE (ICSIG)                                             
            	    CASE (1)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
           	            GAMA(1) = ZERO
       	                    GAMA(2) = GBUF%GAMA(JJ(1) + I)
                            GAMA(3) = GBUF%GAMA(JJ(2) + I)
            	            GAMA(4) = ZERO
            	            GAMA(5) =-GAMA(2)
            	            GAMA(6) = GAMA(1)
            	          ELSE
                            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	    CASE (10)
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
                            GAMA(1) = GBUF%GAMA(JJ(1) + I)
                            GAMA(2) = GBUF%GAMA(JJ(2) + I)
            	            GAMA(3) = ZERO
            	            GAMA(4) =-GAMA(2)
            	            GAMA(5) = GAMA(1)
            	            GAMA(6) = ZERO
            	          ELSE
            	            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	    CASE (100)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
                            GAMA(1) = GBUF%GAMA(JJ(2) + I)
            	            GAMA(2) = ZERO
                            GAMA(3) = GBUF%GAMA(JJ(1) + I)
            	            GAMA(4) = GAMA(3)
            	            GAMA(5) = ZERO
            	            GAMA(6) =-GAMA(1)
            	          ELSE
            	            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	      END SELECT 
                    ELSE  ! (IGTYP /= 21)
            	      SELECT CASE (ICSIG)                                             
            	    CASE (1)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
           	            GAMA(1) = ZERO
       	                    GAMA(2) = LBUF%GAMA(JJ(1) + I)
                            GAMA(3) = LBUF%GAMA(JJ(2) + I)
            	            GAMA(4) = ZERO
            	            GAMA(5) =-GAMA(2)
            	            GAMA(6) = GAMA(1)
            	          ELSE
            	            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	    CASE (10)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
                            GAMA(1) = LBUF%GAMA(JJ(1) + I)
                            GAMA(2) = LBUF%GAMA(JJ(2) + I)
            	            GAMA(3) = ZERO
            	            GAMA(4) =-GAMA(2)
            	            GAMA(5) = GAMA(1)
            	            GAMA(6) = ZERO
            	          ELSE
            	            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	    CASE (100)                                                        
            	      DO I=LFT,LLT
            	        N = I + NFT
            	        IF (EL2FA(NN2+N) /= 0) THEN
            	          IF (KCVT == 2) THEN
                            GAMA(1) = LBUF%GAMA(JJ(2) + I)
            	            GAMA(2) = ZERO
                            GAMA(3) = LBUF%GAMA(JJ(1) + I)
            	            GAMA(4) = GAMA(3)
            	            GAMA(5) = ZERO
            	            GAMA(6) =-GAMA(1)
            	          ELSE
            	            GAMA(1) = ONE
            	            GAMA(2) = ZERO
            	            GAMA(3) = ZERO
            	            GAMA(4) = ZERO
            	            GAMA(5) = ONE
            	            GAMA(6) = ZERO
            	          ENDIF ! IF (KCVT == 2)
            	          CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                      EVAR(1,I),GAMA, JHBE,IGTYP)
            	        ENDIF ! IF (EL2FA(NN2+N) /= 0)
            	      ENDDO ! DO I=LFT,LLT
            	      END SELECT 
            	    ENDIF ! IF (IGTYP == 21)
	    	  ELSE  							
            	    DO I=LFT,LLT
            	      N = I + NFT
            	      IF (EL2FA(NN2+N) /= 0) THEN
                        IF (KCVT == 2) THEN
                          GAMA(1) = LBUF%GAMA(JJ(1) + I)
                          GAMA(2) = LBUF%GAMA(JJ(2) + I)
                          GAMA(3) = LBUF%GAMA(JJ(3) + I)
                          GAMA(4) = LBUF%GAMA(JJ(4) + I)
                          GAMA(5) = LBUF%GAMA(JJ(5) + I)
                          GAMA(6) = LBUF%GAMA(JJ(6) + I)
            	        ELSE
                          GAMA(1) = ONE
                          GAMA(2) = ZERO
                          GAMA(3) = ZERO
                          GAMA(4) = ZERO
                          GAMA(5) = ONE
                          GAMA(6) = ZERO
            	        ENDIF ! IF (KCVT == 2)
            	        CALL SROTA6(X,IXS(1,N),KCVT,
     .      	                    EVAR(1,I),GAMA, JHBE,IGTYP)
                      ENDIF ! IF (EL2FA(NN2+N) /= 0)
                    ENDDO ! DO I=LFT,LLT
                  ENDIF  ! (JHBE == 14.AND.ICSIG > 0)	 
                ENDIF ! IF (KCVT /= 0 .AND. JHBE /= 16)
              ENDIF ! IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS .AND.
!            .            IT <= NPTT   .AND. IR*IS*IT >= 1)

c-----------
            ELSEIF (ISOLNOD == 10 .OR. (ISOLNOD==4 .AND. ISROT==1)) THEN
c-----------
              IR = ABS(PTI)/100
              IS = MOD(ABS(PTI)/10,10)
              IT = MOD(ABS(PTI),10)
              IPT = 0
              IF (IR == 1 .AND. IS == 1 .AND. IT == 1) IPT = 1 
              IF (IR == 2 .AND. IS == 1 .AND. IT == 1) IPT = 2 
              IF (IR == 1 .AND. IS == 2 .AND. IT == 1) IPT = 3 
              IF (IR == 1 .AND. IS == 1 .AND. IT == 2) IPT = 4 
              IF ( IPT > 0) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(1)%LBUF(IPT,1,1)          
                IF (MLW == 24) THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = EVAR(1,I) + LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = EVAR(2,I) + LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = EVAR(3,I) + LBUF%PLA(JJ(3) + I + NEL)
                    EVAR(4,I) = EVAR(4,I) + LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = EVAR(5,I) + LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = EVAR(6,I) + LBUF%PLA(JJ(6) + I + NEL)*HALF
                  ENDDO
                ENDIF ! IF (MLW == 24)
              ENDIF ! IF ( IPT > 0)
c
              IF (KCVT /= 0) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = GBUF%GAMA(JJ(1) + I)
                      GAMA(2) = GBUF%GAMA(JJ(2) + I)
                      GAMA(3) = GBUF%GAMA(JJ(3) + I)
                      GAMA(4) = GBUF%GAMA(JJ(4) + I)
                      GAMA(5) = GBUF%GAMA(JJ(5) + I)
                      GAMA(6) = GBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (KCVT /= 0)
c-----------
            ELSEIF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
            
              IPT = MOD(ABS(PTI)/10,10)
              IF ( IPT > 0 .AND. IPT<=NLAY) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1) ! TSHELL 
                IF (MLW == 24) THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)		    
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL)*HALF   
                  ENDDO 
                ENDIF ! IF (MLW == 24) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1)=GBUF%GAMA(JJ(1) + I)
                      GAMA(2)=GBUF%GAMA(JJ(2) + I)
                      GAMA(3)=ZERO
                      GAMA(4)=-GAMA(2)
                      GAMA(5)=GAMA(1)
                      GAMA(6)=ZERO
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! 
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF 
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS .AND.
!            .          JHBE /= 14.AND.JHBE /= 24.AND.JHBE /= 15.AND.JHBE /= 17)
            END IF 
C-----------------------------------------------
!         PLASTIC STRAIN TENSOR / integration points more than 9 points in direction s
          ELSEIF (ITENS >= 43210 .AND. ITENS <= 63309) THEN
C----------------------------NLAY>9
            PTI = ITENS - 43210
c-----------
            IF ((ISOLNOD == 6 .OR. ISOLNOD == 8).AND.JHBE == 15) THEN
            
              IPT = MOD(ABS(PTI)/10,201)
              IF ( IPT > 0 .AND. IPT<=NLAY .AND. NLAY>9) THEN
                LBUF =>  ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1) ! TSHELL 
                IF (MLW == 24) THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)		    
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL)*HALF   
                  ENDDO 
                ENDIF ! IF (MLW == 24) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                       GAMA(1)=GBUF%GAMA(JJ(1) + I)
                       GAMA(2)=GBUF%GAMA(JJ(2) + I)
                       GAMA(3)=ZERO
                       GAMA(4)=-GAMA(2)
                       GAMA(5)=GAMA(1)
                       GAMA(6)=ZERO
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! 
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF 
                ENDDO ! DO I=LFT,LLT
              ENDIF ! IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS .AND.
            ELSEIF ((ISOLNOD == 16.OR.(ISOLNOD == 8 .AND.JHBE == 14))) THEN
c-----------
                 ICSIG = IPARG(17,NG) 
                 IR0=ABS(PTI)/2010
                 IS0=MOD(ABS(PTI)/10,201)
                 IT0=MOD(ABS(PTI),10)
                IF (IR0==0.OR.IS0==0.OR.IT0==0.OR.NLAY<10) CYCLE
                 IR = IR0
                 IS = IS0
                 IT = IT0
                IF (TSHELL == 1) THEN
                  IF (ICSIG==100) THEN
                    IR = IS0
                    IS = IT0
                    IT = IR0
                  ELSEIF (ICSIG==10) THEN
                    IR = IT0
                    IS = IR0
                    IT = IS0
                  ELSE
                    IR = IR0
                    IS = IS0
                    IT = IT0
                  END IF
                ENDIF
     	        IF (IR>NPTR.OR.IS>NPTS) CYCLE
              IPT = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              IF (IPT  <= NPT ) THEN
                IF (ISOLNOD == 16) THEN
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IS0)%LBUF(IR,1,IT) ! TSHELL 
                ELSE
                  LBUF =>  ELBUF_TAB(NG)%BUFLY(IT)%LBUF(IR,IS,1) ! TSHELL 
                END IF
                IF (MLW == 24) THEN
                  DO I=LFT,LLT
                    EVAR(1,I) = LBUF%PLA(JJ(1) + I + NEL)
                    EVAR(2,I) = LBUF%PLA(JJ(2) + I + NEL)
                    EVAR(3,I) = LBUF%PLA(JJ(3) + I + NEL)		    
                    EVAR(4,I) = LBUF%PLA(JJ(4) + I + NEL)*HALF
                    EVAR(5,I) = LBUF%PLA(JJ(5) + I + NEL)*HALF
                    EVAR(6,I) = LBUF%PLA(JJ(6) + I + NEL)*HALF   
                  ENDDO 
                ENDIF ! IF (MLW == 24) THEN
              ENDIF ! IF (IPT  <= NPTG .AND. IR <= NPTR .AND. IS <= NPTS .AND.
!     .                   IT <= NPTT)
            IF (KCVT /= 0 .AND. JHBE /= 16) THEN
!           PLASTIC STRAIN TENSOR IN GLOBAL SYSTEM
              ICSIG=IPARG(17,NG)  
              IF (JHBE == 14 .AND. ICSIG > 0) THEN
                SELECT CASE (ICSIG)                                             
              CASE (1)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = ZERO
                      GAMA(2) = LBUF%GAMA(JJ(1) + I)
                      GAMA(3) = LBUF%GAMA(JJ(2) + I)
                      GAMA(4) = ZERO
                      GAMA(5) =-GAMA(2)
                      GAMA(6) = GAMA(1)
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
              CASE (10)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = LBUF%GAMA(JJ(1) + I)
                      GAMA(2) = LBUF%GAMA(JJ(2) + I)
                      GAMA(3) = ZERO
                      GAMA(4) =-GAMA(2)
                      GAMA(5) = GAMA(1)
                      GAMA(6) = ZERO
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
              CASE (100)                                                        
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = LBUF%GAMA(JJ(2) + I)
                      GAMA(2) = ZERO
                      GAMA(3) = LBUF%GAMA(JJ(1) + I)
                      GAMA(4) = GAMA(3)
                      GAMA(5) = ZERO
                      GAMA(6) =-GAMA(1)
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! IF (KCVT == 2)
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF ! IF (EL2FA(NN2+N) /= 0)
                ENDDO ! DO I=LFT,LLT
                END SELECT 
	      ELSE ! (JHBE == 14 .AND. ICSIG > 0)
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN2+N) /= 0) THEN
                    IF (KCVT == 2) THEN
                      GAMA(1) = LBUF%GAMA(JJ(1) + I)
                      GAMA(2) = LBUF%GAMA(JJ(2) + I)
                      GAMA(3) = LBUF%GAMA(JJ(3) + I)
                      GAMA(4) = LBUF%GAMA(JJ(4) + I)
                      GAMA(5) = LBUF%GAMA(JJ(5) + I)
                      GAMA(6) = LBUF%GAMA(JJ(6) + I)
                    ELSE
                      GAMA(1) = ONE
                      GAMA(2) = ZERO
                      GAMA(3) = ZERO
                      GAMA(4) = ZERO
                      GAMA(5) = ONE
                      GAMA(6) = ZERO
                    ENDIF ! 
                    CALL SROTA6(X,IXS(1,N),KCVT,
     .                          EVAR(1,I),GAMA, JHBE,IGTYP)
                  ENDIF ! IF (KCVT == 2)
                ENDDO ! DO I=LFT,LLT
              ENDIF  !(JHBE == 14.AND.ICSIG > 0)    
            ENDIF ! IF (KCVT /= 0 .AND. JHBE /= 16)
!
            ENDIF ! IF ((ISOLNOD == 16.OR.(ISOLNOD ==8 .AND.JHBE == 14).OR.
!     .                ((ISOLNOD == 6 .OR. ISOLNOD ==8).AND.JHBE == 15)).AND. 
!     .                  IGTYP == 22)
!
          ELSE ! (ITENS ...)
C-----------------------------------------------
C          
C-----------------------------------------------
          ENDIF ! (ITENS)
C
c-----------
          IF (ISOLNOD == 16) THEN
c-----------
             DO I=LFT,LLT
              N = I + NFT
              IF(EL2FA(NN2+N) /= 0)THEN
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
              IF(EL2FA(NN2+N) /= 0)THEN
               TENS(1,EL2FA(NN2+N)) = EVAR(1,I)
               TENS(2,EL2FA(NN2+N)) = EVAR(2,I)
               TENS(3,EL2FA(NN2+N)) = EVAR(3,I)
               TENS(4,EL2FA(NN2+N)) = EVAR(4,I)
               TENS(5,EL2FA(NN2+N)) = EVAR(5,I)
               TENS(6,EL2FA(NN2+N)) = EVAR(6,I)
              ENDIF
             ENDDO
          ENDIF
         ISORTHG = ISORTH   ! pour precaution	
C-----------------------------------------------
        ELSEIF (ISPH3D == 1.AND.ITY == 51) THEN
C-----------------------------------------------
C         TETRAS SPH.
C-----------------------------------------------
          IPRT=IPARTSP(1 + NFT)
          MT1  =IPART(1,IPRT)
          GBUF => ELBUF_TAB(NG)%GBUF    
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)          
C-----------------------------------------------
C         STRESS
          IF (ITENS == 1) THEN
C-----------------------------------------------
           IF(IVISC == 0) THEN
             DO I=LFT,LLT
               N = I + NFT
               IF (EL2FA(NN3+N) /= 0) THEN
                 TENS(1,EL2FA(NN3+N)) = LBUF%SIG(JJ(1) + I)
                 TENS(2,EL2FA(NN3+N)) = LBUF%SIG(JJ(2) + I)
                 TENS(3,EL2FA(NN3+N)) = LBUF%SIG(JJ(3) + I)
                 TENS(4,EL2FA(NN3+N)) = LBUF%SIG(JJ(4) + I)
                 TENS(5,EL2FA(NN3+N)) = LBUF%SIG(JJ(5) + I)
                 TENS(6,EL2FA(NN3+N)) = LBUF%SIG(JJ(6) + I)
               ENDIF
             ENDDO
           ELSE
             DO I=LFT,LLT
               N = I + NFT
               IF (EL2FA(NN3+N) /= 0) THEN
                 TENS(1,EL2FA(NN3+N)) = LBUF%SIG(JJ(1)+I) + LBUF%VISC(JJ(1)+I)
                 TENS(2,EL2FA(NN3+N)) = LBUF%SIG(JJ(2)+I) + LBUF%VISC(JJ(2)+I)
                 TENS(3,EL2FA(NN3+N)) = LBUF%SIG(JJ(3)+I) + LBUF%VISC(JJ(3)+I)
                 TENS(4,EL2FA(NN3+N)) = LBUF%SIG(JJ(4)+I) + LBUF%VISC(JJ(4)+I)
                 TENS(5,EL2FA(NN3+N)) = LBUF%SIG(JJ(5)+I) + LBUF%VISC(JJ(5)+I)
                 TENS(6,EL2FA(NN3+N)) = LBUF%SIG(JJ(6)+I) + LBUF%VISC(JJ(6)+I)
               ENDIF
             ENDDO           
           
           
           
           ENDIF  
C-----------------------------------------------
C         CRACKS
          ELSEIF(ITENS == 4.AND.MLW == 24.
     .                      AND.NINT(PM(56,MT1)) == 1)THEN
C-----------------------------------------------
           DO I=LFT,LLT
             N = I + NFT
             IF(EL2FA(NN3+N) /= 0)THEN
               TENS(1,EL2FA(NN3+N)) = LBUF%DGLO(JJ(1) + I)
               TENS(2,EL2FA(NN3+N)) = LBUF%DGLO(JJ(2) + I)
               TENS(3,EL2FA(NN3+N)) = LBUF%DGLO(JJ(3) + I)
               TENS(4,EL2FA(NN3+N)) = LBUF%DGLO(JJ(4) + I)
               TENS(5,EL2FA(NN3+N)) = LBUF%DGLO(JJ(5) + I)
               TENS(6,EL2FA(NN3+N)) = LBUF%DGLO(JJ(6) + I)
             ENDIF
           ENDDO
C-----------------------------------------------
          ELSE
C-----------------------------------------------
           DO I=LFT,LLT
             N = I + NFT
             IF (EL2FA(NN3+N) /= 0) THEN
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
        ELSEIF (ITY==101) THEN
C         ISOGEOMETRIC ELEMENT
C-----------------------------------------------
           DO I=LFT,LLT
             N = I + NFT
             EVAR(1,I) = ZERO
             EVAR(2,I) = ZERO
             EVAR(3,I) = ZERO
             EVAR(4,I) = ZERO
             EVAR(5,I) = ZERO
             EVAR(6,I) = ZERO
           ENDDO
C-----------------------------------------------
          DO I=LFT,LLT
            N = I + NFT
            IF (EL2FA(NN4+N) /= 0) THEN
              DO J=1,27
               TENS(1,EL2FA(NN4+N)+J-1) = EVAR(1,I)
               TENS(2,EL2FA(NN4+N)+J-1) = EVAR(1,I)
               TENS(3,EL2FA(NN4+N)+J-1) = EVAR(1,I)
               TENS(4,EL2FA(NN4+N)+J-1) = EVAR(1,I)
               TENS(5,EL2FA(NN4+N)+J-1) = EVAR(1,I)
               TENS(6,EL2FA(NN4+N)+J-1) = EVAR(1,I)  
              ENDDO  
            ENDIF
          ENDDO
C-----------------------------------------------
        ELSE
c
        ENDIF
C           
        ENDIF ! mlw /= 13 
 490   CONTINUE
 500  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1)THEN
        DO N=1,NBF
          R4(1) = TENS(1,N)
          R4(2) = TENS(2,N)
          R4(3) = TENS(3,N)
          R4(4) = TENS(4,N)
          R4(5) = TENS(5,N)
          R4(6) = TENS(6,N)
          CALL WRITE_R_C(R4,6)
        ENDDO
      ELSE
        DO N = 1, NBF
          WA(6*N-5) = TENS(1,N)
          WA(6*N-4) = TENS(2,N)
          WA(6*N-3) = TENS(3,N)
          WA(6*N-2) = TENS(4,N)
          WA(6*N-1) = TENS(5,N)
          WA(6*N  ) = TENS(6,N)
        ENDDO
        IF(ISPMD == 0) THEN
           BUF = NUMELSG*6 + NUMELS16G*18+NUMSPHG*6
        ELSE
           BUF = 1
        ENDIF
        CALL SPMD_R4GET_PARTN(6,6*NBF,NBPART,IADG,WA,BUF)
      ENDIF
C-----------------------------------------------
 600  CONTINUE
C-----------
      RETURN
      END SUBROUTINE TENSORS
Chd|====================================================================
Chd|  TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SHLROTG                       source/output/anim/generate/tensor6.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE TENSGPS1(FUNC1   ,FUNC2   ,IPARG   ,GEO     ,
     .                    IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC   ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X     ,ITAGPS  ,ELBUF_TAB)
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
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC1(3,*),FUNC2(3,*),GEO(NPROPG,*),X(3,*)
      INTEGER IPARG(NPARG,*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,ITAGPS(*) 
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,MVSIZ),GAMA(6),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA(MVSIZ)
      INTEGER I,II, NG, NEL, ISS, ISC,NBGAMA,KCVT,
     .        IADD, N, J, MLW,  
     .        ISTRAIN,NN, JTURB,MT, IMID, IALEL,IPID,
     .        NN1,NF,OFFSET,K,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IEXPAN,IHBE,MPT,
     .        IVISC,JJ(6)
      INTEGER MLW2                
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
C=======================================================================      
      DO 900 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        MLW2 = MLW
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN
C
           NNOD = ISOLNOD
           DO I=LFT,LLT
                N = I + NFT
                IF(ISOLNOD == 8)THEN
	                DO J = 1,ISOLNOD
	                  NC(J,I) = IXS(J+1,N)
                 ENDDO
                ELSEIF(ISOLNOD == 4)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
                ELSEIF(ISOLNOD == 6)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(3,N)
                  NC(3,I)=IXS(4,N)
                  NC(4,I)=IXS(6,N)
                  NC(5,I)=IXS(7,N)
                  NC(6,I)=IXS(8,N)
                ELSEIF(ISOLNOD == 10)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
		                NN1 = N - NUMELS8
                  DO J=1,6
c                   IF (IXS10(J,NN1)>0) THEN
                    NC(J+4,I) = IXS10(J,NN1)
c                   ENDIF
                  ENDDO
                ELSEIF(ISOLNOD == 16)THEN
	                 DO J = 1,8
	                   NC(J,I) = IXS(J+1,N)
                  ENDDO
		                NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
                  DO J=1,8
                   NC(J+8,I) = IXS16(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD == 20)THEN
	                 DO J = 1,8
	                   NC(J,I) = IXS(J+1,N)
                  ENDDO
		                NN1 = N - (NUMELS8+NUMELS10)
                  DO J=1,12
                    NC(J+8,I) = IXS20(J,NN1)
                  ENDDO
                ENDIF
           ENDDO 
C----------
           IF (KCVT==1.AND.ISORTH/=0.AND.JHBE/=14
     .        .AND.JHBE/=17.AND.JHBE/=15) KCVT=2
           DO I=LFT,LLT
             N = I + NFT
             EVAR(1,I) = GBUF%SIG(JJ(1) + I)
             EVAR(2,I) = GBUF%SIG(JJ(2) + I)
             EVAR(3,I) = GBUF%SIG(JJ(3) + I)
             EVAR(4,I) = GBUF%SIG(JJ(4) + I)
             EVAR(5,I) = GBUF%SIG(JJ(5) + I)
             EVAR(6,I) = GBUF%SIG(JJ(6) + I)
           ENDDO
           IF(IVISC > 0) THEN
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
              DO I=LFT,LLT
                EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                EVAR(3,I) =EVAR(3,I)+ LBUF%VISC(JJ(3) + I)
                EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                EVAR(5,I) =EVAR(5,I)+ LBUF%VISC(JJ(5) + I)
                EVAR(6,I) =EVAR(6,I)+ LBUF%VISC(JJ(6) + I)
              ENDDO
           ENDIF
           IF (KCVT /= 0) THEN
C            STRESS TENSOR -> GLOBAL SYSTEM
             DO I=LFT,LLT
               N = I + NFT
               IF(KCVT==2)THEN
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
               CALL SROTA6(X,IXS(1,N),KCVT,
     .                     EVAR(1,I),GAMA, JHBE,IGTYP) 
             ENDDO
           ENDIF
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        ELSEIF (ITY == 2)THEN
C-----------------------------------------------
C       COQUES 3 N 4 N
C-----------------------------------------------
        ELSEIF(ITY == 3.OR.ITY == 7)THEN
           IF(ITY == 7)THEN
            NNOD=3
            DO I=LFT,LLT
              N = I + NFT
              DO J = 1,NNOD
                NC(J,I) = IXTG(J+1,N)
              ENDDO
            ENDDO 
           ELSEIF(ITY == 3)THEN
            NNOD=4
            DO I=LFT,LLT
              N = I + NFT
              DO J = 1,NNOD
                NC(J,I) = IXC(J+1,N)
              ENDDO
            ENDDO 
           ENDIF
C-----------membrane terms only	 ------  
           DO I=LFT,LLT
             EVAR(1,I) = GBUF%FOR(JJ(1)+I)
             EVAR(2,I) = GBUF%FOR(JJ(2)+I)
             EVAR(3,I) = ZERO
             EVAR(4,I) = GBUF%FOR(JJ(3)+I)
             EVAR(5,I) = GBUF%FOR(JJ(4)+I)
             EVAR(6,I) = GBUF%FOR(JJ(5)+I)
           ENDDO
           CALL SHLROTG(LFT     ,LLT     ,NFT     ,X       ,EVAR    ,
     1                  ITY     ,IXC     ,IXTG    ,IHBE    ,AREA    )
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        ELSEIF(ITY == 4)THEN
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
        ELSEIF(ITY == 5)THEN
        ENDIF
C	
        DO I=LFT,LLT
	         DO J = 1,NNOD
	            N = NC(J,I)
	            IF (N>0)THEN
	             DO K = 1,3
	              FUNC1(K,N) = FUNC1(K,N)+EVAR(K,I)
	              FUNC2(K,N) = FUNC2(K,N)+EVAR(K+3,I)
	             ENDDO 
	             ITAGPS(N) = ITAGPS(N)+1
	            ENDIF
           ENDDO
        ENDDO
 900  CONTINUE
C-----------------------------------------------
      RETURN
      END SUBROUTINE TENSGPS1
Chd|====================================================================
Chd|  TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SHLROTG                       source/output/anim/generate/tensor6.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE TENSGPS2(FUNC1   ,FUNC2   ,IPARG   ,GEO     ,
     .                    IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC   ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X     ,VGPS    ,ELBUF_TAB)
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
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC1(3,*),FUNC2(3,*),GEO(NPROPG,*),X(3,*),VGPS(*)
      INTEGER IPARG(NPARG,*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*)  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,MVSIZ),GAMA(6),VOL(MVSIZ),THK0,
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA(MVSIZ)
      INTEGER I,II, NG, NEL, ISS, ISC,KCVT,
     .        IADD, N, J, MLW,  
     .        ISTRAIN,NN, K1, K2,JTURB,MT, IMID, IALEL,IPID,
     .        NN1,NF,OFFSET,K,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IEXPAN,IHBE,MPT,
     .        IVISC,JJ(6)
      INTEGER MLW2                
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
C=======================================================================      
      DO 900 NG=1,NGROUP
          CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        MLW2 = MLW
        ISOLNOD = IPARG(28,NG)
        IVISC = IPARG(61,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN
           GBUF => ELBUF_TAB(NG)%GBUF
           NNOD = ISOLNOD
           DO I=LFT,LLT
                N = I + NFT
                IF(ISOLNOD == 8)THEN
	                DO J = 1,ISOLNOD
	                 NC(J,I) = IXS(J+1,N)
                 ENDDO
                ELSEIF(ISOLNOD == 4)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
                ELSEIF(ISOLNOD == 6)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(3,N)
                  NC(3,I)=IXS(4,N)
                  NC(4,I)=IXS(6,N)
                  NC(5,I)=IXS(7,N)
                  NC(6,I)=IXS(8,N)
                ELSEIF(ISOLNOD == 10)THEN
                  NC(1,I)=IXS(2,N)
                  NC(2,I)=IXS(4,N)
                  NC(3,I)=IXS(7,N)
                  NC(4,I)=IXS(6,N)
		                NN1 = N - NUMELS8
                  DO J=1,6
c                   IF (IXS10(J,NN1)>0) THEN
                    NC(J+4,I) = IXS10(J,NN1)
c                   ENDIF
                  ENDDO
                ELSEIF(ISOLNOD == 16)THEN
	                DO J = 1,8
	                 NC(J,I) = IXS(J+1,N)
                 ENDDO
		               NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
                  DO J=1,8
                   NC(J+8,I) = IXS16(J,NN1)
                  ENDDO
                ELSEIF(ISOLNOD == 20)THEN
	                DO J = 1,8
	                 NC(J,I) = IXS(J+1,N)
                 ENDDO
		               NN1 = N - (NUMELS8+NUMELS10)
                  DO J=1,12
                    NC(J+8,I) = IXS20(J,NN1)
                  ENDDO
                ENDIF
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = GBUF%VOL(I)*OFF
           ENDDO 
C
           IF (KCVT==1.AND.ISORTH/=0.AND.JHBE/=14
     .        .AND.JHBE/=17.AND.JHBE/=15) KCVT=2
           DO I=LFT,LLT
             N = I + NFT
             EVAR(1,I) = GBUF%SIG(JJ(1) + I)
             EVAR(2,I) = GBUF%SIG(JJ(2) + I)
             EVAR(3,I) = GBUF%SIG(JJ(3) + I)
             EVAR(4,I) = GBUF%SIG(JJ(4) + I)
             EVAR(5,I) = GBUF%SIG(JJ(5) + I)
             EVAR(6,I) = GBUF%SIG(JJ(6) + I)
           ENDDO
           IF(IVISC > 0) THEN
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1) 
              DO I=LFT,LLT
                EVAR(1,I) =EVAR(1,I)+ LBUF%VISC(JJ(1) + I)
                EVAR(2,I) =EVAR(2,I)+ LBUF%VISC(JJ(2) + I)
                EVAR(3,I) =EVAR(3,I)+ LBUF%VISC(JJ(3) + I)
                EVAR(4,I) =EVAR(4,I)+ LBUF%VISC(JJ(4) + I)
                EVAR(5,I) =EVAR(5,I)+ LBUF%VISC(JJ(5) + I)
                EVAR(6,I) =EVAR(6,I)+ LBUF%VISC(JJ(6) + I)
              ENDDO
           ENDIF
           IF (KCVT /= 0) THEN
C           STRESS TENSOR -> GLOBAL SYSTEM
            DO I=LFT,LLT
              N = I + NFT
              IF(KCVT==2)THEN
                GAMA(1) = GBUF%GAMA(JJ(1) + I)
                GAMA(2) = GBUF%GAMA(JJ(2) + I)
                GAMA(3) = GBUF%GAMA(JJ(3) + I)
                GAMA(4) = GBUF%GAMA(JJ(4) + I)
                GAMA(5) = GBUF%GAMA(JJ(5) + I)
                GAMA(6) = GBUF%GAMA(JJ(6) + I)
              ELSE
                GAMA(1)=ONE
                GAMA(2)=ZERO
                GAMA(3)=ZERO
                GAMA(4)=ZERO
                GAMA(5)=ONE
                GAMA(6)=ZERO
              END IF
              CALL SROTA6(X,IXS(1,N),KCVT,
     .                    EVAR(1,I),GAMA, JHBE,IGTYP)
            ENDDO
           ENDIF
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        ELSEIF(ITY == 2)THEN
C-----------------------------------------------
C       COQUES 3 N 4 N
C-----------------------------------------------
        ELSEIF(ITY == 3.OR.ITY == 7)THEN
          GBUF => ELBUF_TAB(NG)%GBUF
C-----------membrane terms only	 ------  
           DO I=LFT,LLT
             EVAR(1,I) = GBUF%FOR(JJ(1)+I)
             EVAR(2,I) = GBUF%FOR(JJ(2)+I)
             EVAR(3,I) = ZERO
             EVAR(4,I) = GBUF%FOR(JJ(3)+I)
             EVAR(5,I) = GBUF%FOR(JJ(4)+I)
             EVAR(6,I) = GBUF%FOR(JJ(5)+I)
           ENDDO
           CALL SHLROTG(LFT     ,LLT     ,NFT     ,X       ,EVAR    ,
     1                  ITY     ,IXC     ,IXTG    ,IHBE    ,AREA    )
           IF(ITY == 7)THEN
	           NNOD=3
            DO I=LFT,LLT
                N = I + NFT
	               DO J = 1,NNOD
	                 NC(J,I) = IXTG(J+1,N)
                ENDDO
                THK0 = GEO(1,IXTG(5,N))
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA(I)*OFF
            ENDDO 
           ELSEIF(ITY == 3)THEN
	           NNOD=4
            DO I=LFT,LLT
                N = I + NFT
	               DO J = 1,NNOD
	                 NC(J,I) = IXC(J+1,N)
                ENDDO
                THK0 = GEO(1,IXC(6,N))
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA(I)*OFF
            ENDDO 
           ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        ELSEIF(ITY == 4)THEN
C-----------------------
C       5. ELEMENTS POUTRES
C-----------------------
        ELSEIF(ITY == 5)THEN
        ENDIF
C-----------------------------------------------
        DO I=LFT,LLT
	         DO J = 1,NNOD
	            N = NC(J,I)
	            IF (N>0)THEN
	             DO K = 1,3
	              FUNC1(K,N) = FUNC1(K,N)+EVAR(K,I)*VOL(I)
	              FUNC2(K,N) = FUNC2(K,N)+EVAR(K+3,I)*VOL(I)
	             ENDDO 
	             VGPS(N) = VGPS(N)+VOL(I)
	            ENDIF
           ENDDO
        ENDDO
 900  CONTINUE
C-----------
      RETURN
      END SUBROUTINE TENSGPS2
Chd|====================================================================
Chd|  TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GPS_SOLID                     source/output/outmaxsubr.F    
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        PRE_HEPH                      source/output/anim/generate/tensor6.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        SZSIGPARA                     source/elements/solid/solidez/szhour3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|====================================================================
      SUBROUTINE TENSGPS3(ELBUF_TAB,FUNC1   ,FUNC2   ,IPARG   ,GEO     ,
     .                    IXS      ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC      ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X        ,ITAGPS  ,PM        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE OUTMAX_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC1(3,*),FUNC2(3,*),GEO(NPROPG,*),X(3,*),
     .   PM(NPROPM,*)
      INTEGER IPARG(NPARG,*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,ITAGPS(*) 
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,NUMNOD),GAMA(6),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA(MVSIZ),
     .   A_GAUSS_R,A_GAUSS_S,A_GAUSS_T,N1,
     .   A_GAUSS_R1,A_GAUSS_S1,A_GAUSS_T1,
     .   A_GAUSS_P_R,A_GAUSS_P_S,A_GAUSS_P_T,
     .   KSI,ETA,ZETA
      INTEGER I,II, NG, NEL, ISS, ISC,NBGAMA,KCVT,
     .        IADD, N, J, MLW,  
     .        ISTRAIN,NN, JTURB,MT, IMID, IALEL,IPID,
     .        NN1,NF,OFFSET,K,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IEXPAN,IHBE,MPT,ILAY,
     .        ICSIG,DIR,IVISC,JJ(6),MAT(MVSIZ)
      INTEGER MLW2,NLAY
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF   
      my_real
     .  A_GAUSS(9,9),EVAR_TMP(6),ALPHA,BETA,ALPHA_1,BETA_1,
     .  JR0(MVSIZ),JS0(MVSIZ),JT0(MVSIZ),NU(MVSIZ),SIG_HOUR(MVSIZ,6),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), XD5(MVSIZ),
     .   XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), YD5(MVSIZ), 
     .   YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), ZD5(MVSIZ),
     .   ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   XDL(MVSIZ), YDL(MVSIZ), ZDL(MVSIZ),EVAR_T10(6,10),A_HEPH(3,8)
      INTEGER 
     .  SOL_NODE(3,8), IPERM1(10),IPERM2(10),NN2
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C======================================================================= 
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/  
      DATA SOL_NODE / 
     1 -1               ,-1               ,-1               ,
     2 -1               ,-1               , 1               ,
     3  1               ,-1               , 1               ,
     4  1               ,-1               ,-1               ,
     5 -1               , 1               ,-1               ,
     6 -1               , 1               , 1               ,
     7  1               , 1               , 1               ,
     8  1               , 1               ,-1               /
C-----Nj : KSI,ETA,ZETA     
      DATA A_HEPH / 
     1 -1               ,-1               ,-1               ,
     4  1               ,-1               ,-1               ,
     5 -1               , 1               ,-1               ,
     8  1               , 1               ,-1               ,
     2 -1               ,-1               , 1               ,
     3  1               ,-1               , 1               ,
     7  1               , 1               , 1               ,
     6 -1               , 1               , 1               /
C======================================================================= 
      ALPHA = ZEP1381966
      BETA  = ZEP5854102
      DO I=1,NUMNOD
        EVAR(1,I) = ZERO
        EVAR(2,I) = ZERO
        EVAR(3,I) = ZERO
        EVAR(4,I) = ZERO
        EVAR(5,I) = ZERO
        EVAR(6,I) = ZERO
      ENDDO  
      DO 900 NG=1,NGROUP
        IF (LMAX_NSIG >0 .AND. IPART_OK(NG,1)==0) CYCLE
        IVISC = IPARG(61,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        MLW2 = MLW
        ICSIG=IPARG(17,NG)  
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         IF (KCVT==1.AND.ISORTH/=0) KCVT=2
         NNOD = ISOLNOD
         DO I=LFT,LLT
           N = I + NFT
           IF(ISOLNOD == 8)THEN
             DO J = 1,ISOLNOD
               NC(J,I) = IXS(J+1,N)
             ENDDO
           ELSEIF(ISOLNOD == 4)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
           ELSEIF(ISOLNOD == 6)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(3,N)
             NC(3,I)=IXS(4,N)
             NC(4,I)=IXS(6,N)
             NC(5,I)=IXS(7,N)
             NC(6,I)=IXS(8,N)
           ELSEIF(ISOLNOD == 10)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
             NN1 = N - NUMELS8
             DO J=1,6
               NC(J+4,I) = IXS10(J,NN1)
             ENDDO
           ELSEIF(ISOLNOD == 16)THEN
             DO J = 1,8
               NC(J,I) = IXS(J+1,N)
             ENDDO
             NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
             DO J=1,8
               NC(J+8,I) = IXS16(J,NN1)
             ENDDO
           ELSEIF(ISOLNOD == 20)THEN
             DO J = 1,8
               NC(J,I) = IXS(J+1,N)
             ENDDO
             NN1 = N - (NUMELS8+NUMELS10)
             DO J=1,12
               NC(J+8,I) = IXS20(J,NN1)
             ENDDO
           ENDIF
         ENDDO 
C
         NPTR   = ELBUF_TAB(NG)%NPTR
         NPTS   = ELBUF_TAB(NG)%NPTS
         NPTT   = ELBUF_TAB(NG)%NPTT
         NLAY   = ELBUF_TAB(NG)%NLAY
         NPT = NPTR*NPTS*NPTT
         NNOD = ISOLNOD
         SIG_HOUR = ZERO
         IF (JHBE == 24) THEN
           CALL PRE_HEPH(X,IXS,JR0,JS0,JT0,PM,MAT,NU,NFT,NEL)
         ENDIF
C----------
         IF(ISOLNOD == 6 .OR. ISOLNOD == 8 .OR. 
     .              ISOLNOD == 16 .OR. ISOLNOD == 20)THEN
c
c T_SHELL ( JHBE = 15/16 )
          IF(NLAY > 1 .AND. JHBE /= 14) THEN
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             NPTS = NLAY
C 
             DO J=1,MIN(8,ISOLNOD)
               DO K=1,MIN(8,ISOLNOD)
                IF(SOL_NODE(2,K) == SOL_NODE(2,J)) THEN
c
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IR = MIN(NPTR,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IT = MIN(NPTT,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 IF (JHBE == 15 .OR. JHBE == 16) THEN
                  ILAY = IS
                  IS = 1 
                  N1 = FOURTH*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
                 ENDIF
c
                   LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                   EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                   EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                   EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                   EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                   EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                   EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
                   IF(IVISC > 0) THEN
                     EVAR_TMP(1) = EVAR_TMP(1) + LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) = EVAR_TMP(2) + LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) = EVAR_TMP(3) + LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) = EVAR_TMP(4) + LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) = EVAR_TMP(5) + LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) = EVAR_TMP(6) + LBUF%VISC(JJ(6) + I)
                   ENDIF
                   IF (KCVT /= 0)
     .             CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA,JHBE,IGTYP)
                   EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                   EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                   EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                   EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                   EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                   EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
                 ENDIF
               ENDDO
             ENDDO
           ENDDO
          ELSEIF (JHBE == 24) THEN 
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1:6) = GBUF%GAMA(JJ(1:6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             DO J=1,8
	         KSI = A_HEPH(1,J)
	         ETA = A_HEPH(2,J)
	         ZETA = A_HEPH(3,J)
c
                 ILAY = 1

                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
C------       orthotropic laws will be treated later         
                     CALL SZSIGPARA(JR0      ,JS0  ,JT0  ,GBUF%HOURG ,GBUF%SIG ,
     .                             SIG_HOUR   ,KSI    ,ETA  ,ZETA    ,NU   ,NEL , I)
                     EVAR_TMP(1:6) = SIG_HOUR(I,1:6)
                 IF(IVISC > 0) THEN
                   EVAR_TMP(1:6) =EVAR_TMP(1:6)+ LBUF%VISC(JJ(1:6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
                 EVAR(1:6,NC(J,I)) = EVAR(1:6,NC(J,I)) + EVAR_TMP(1:6)
             ENDDO
           ENDDO
          ELSE 
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             IF(IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
               NPTT = NLAY
             ENDIF
             DO J=1,MIN(8,ISOLNOD)
               DO K=1,MIN(8,ISOLNOD)
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IT = MIN(NPTT,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IR = MIN(NPTR,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 N1 = ONE_OVER_8*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(2,K) * A_GAUSS_P_S)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
c
                 IF (IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
                   ILAY = IT
                   IT = 1
                 ELSE
                   ILAY = 1
                 ENDIF
c
	         KSI = A_GAUSS(IR,2)
	         ETA = A_GAUSS(IS,2)
	         ZETA = A_GAUSS(IT,2)

                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)

                 EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                 EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                 EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                 EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                 EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                 EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
C            
                 IF(IVISC > 0) THEN
                     EVAR_TMP(1) =EVAR_TMP(1)+ LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) =EVAR_TMP(2)+ LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) =EVAR_TMP(3)+ LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) =EVAR_TMP(4)+ LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) =EVAR_TMP(5)+ LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) =EVAR_TMP(6)+ LBUF%VISC(JJ(6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
                 EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                 EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                 EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                 EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                 EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                 EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
               ENDDO
             ENDDO
           ENDDO
          ENDIF
c    
c----attention, ISROT=ITETRA4    
         ELSEIF(ISOLNOD == 4 .AND. ISROT/=1)THEN
c    
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
                 N1 = ONE
                 ILAY = 1
                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                 EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                 EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                 EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                 EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                 EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                 EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
                 IF(IVISC > 0) THEN
                     EVAR_TMP(1) =EVAR_TMP(1)+ LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) =EVAR_TMP(2)+ LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) =EVAR_TMP(3)+ LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) =EVAR_TMP(4)+ LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) =EVAR_TMP(5)+ LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) =EVAR_TMP(6)+ LBUF%VISC(JJ(6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
               DO J=1,4
                 EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                 EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                 EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                 EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                 EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                 EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
               ENDDO
           ENDDO
         ELSEIF(ISOLNOD == 10 .OR. (ISOLNOD == 4 .AND. ISROT==1))THEN
c    
           ALPHA_1 = -ALPHA/(BETA-ALPHA)
           BETA_1  = (ONE-ALPHA)/(BETA-ALPHA)
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             DO J=1,4
               EVAR_T10(1:6,J)=ZERO
               DO K=1,4
                   IR = K
                   IS = 1
                   IT = 1
C
                   IF (J==K) THEN
                     N1 = BETA_1
                   ELSE
                     N1 = ALPHA_1
                   ENDIF
                   ILAY = 1
                   LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                 EVAR_T10(1,J) = EVAR_T10(1,J)+ N1 *LBUF%SIG(JJ(1) + I)
                 EVAR_T10(2,J) = EVAR_T10(2,J)+ N1 *LBUF%SIG(JJ(2) + I)
                 EVAR_T10(3,J) = EVAR_T10(3,J)+ N1 *LBUF%SIG(JJ(3) + I)
                 EVAR_T10(4,J) = EVAR_T10(4,J)+ N1 *LBUF%SIG(JJ(4) + I)
                 EVAR_T10(5,J) = EVAR_T10(5,J)+ N1 *LBUF%SIG(JJ(5) + I)
                 EVAR_T10(6,J) = EVAR_T10(6,J)+ N1 *LBUF%SIG(JJ(6) + I)
                 IF(IVISC > 0) THEN
                   EVAR_T10(1,J) =EVAR_T10(1,J)+ N1 *LBUF%VISC(JJ(1) + I)
                   EVAR_T10(2,J) =EVAR_T10(2,J)+ N1 *LBUF%VISC(JJ(2) + I)
                   EVAR_T10(3,J) =EVAR_T10(3,J)+ N1 *LBUF%VISC(JJ(3) + I)
                   EVAR_T10(4,J) =EVAR_T10(4,J)+ N1 *LBUF%VISC(JJ(4) + I)
                   EVAR_T10(5,J) =EVAR_T10(5,J)+ N1 *LBUF%VISC(JJ(5) + I)
                   EVAR_T10(6,J) =EVAR_T10(6,J)+ N1 *LBUF%VISC(JJ(6) + I)
                 ENDIF
               ENDDO
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_T10(1,J),GAMA, JHBE,IGTYP)
             END DO !J=1,4
             DO J=1,4
               EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + EVAR_T10(1,J)
               EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + EVAR_T10(2,J)
               EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + EVAR_T10(3,J)
               EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + EVAR_T10(4,J)
               EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + EVAR_T10(5,J)
               EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + EVAR_T10(6,J)
             ENDDO
			IF(ISOLNOD == 10 ) THEN
              DO J=5,10
                NN1=IPERM1(J)
                NN2=IPERM2(J)
                EVAR_T10(1:6,J) = HALF*(EVAR_T10(1:6,NN1)+EVAR_T10(1:6,NN2))
              END DO
              DO J=5,10
               EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + EVAR_T10(1,J)
               EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + EVAR_T10(2,J)
               EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + EVAR_T10(3,J)
               EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + EVAR_T10(4,J)
               EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + EVAR_T10(5,J)
               EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + EVAR_T10(6,J)
              ENDDO
			END IF !(ISOLNOD == 10 ) THEN
           ENDDO
         ENDIF
         DO I=LFT,LLT
           DO J = 1,NNOD
             N = NC(J,I)
             IF (N>0)THEN
               DO K = 1,3
                FUNC1(K,N) = EVAR(K,N)
                FUNC2(K,N) = EVAR(K+3,N)
               ENDDO 
               ITAGPS(N) = ITAGPS(N)+1
             ENDIF
           ENDDO
         ENDDO
        ENDIF
c
 900  CONTINUE
C-----------------------------------------------
      RETURN
      END SUBROUTINE TENSGPS3
Chd|====================================================================
Chd|  SHLROTG                       source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        TENCGPS1                      source/output/anim/generate/tensorc.F
Chd|        TENCGPS2                      source/output/anim/generate/tensorc.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|====================================================================
      SUBROUTINE SHLROTG(JFT     ,JLT     ,NFT     ,X       ,TENS    ,
     1                   ITY     ,IXC     ,IXTG    ,IHBE    ,AREA    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NFT, IXC(NIXC,*),ITY, IXTG(NIXTG,*),IHBE
      my_real X(3,*), TENS(6,*),AREA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IREP
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),CDET(MVSIZ),
     .   OFF(MVSIZ),RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   L11,L12,L13,L22,L23,L33,
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33
C-----------------------------------------------
      IF(ITY == 3)THEN
C---------------------
C      shells 4 nodes
C---------------------
       DO N=JFT,JLT
        I=NFT+N
        RX(N)=X(1,IXC(3,I))+X(1,IXC(4,I))-X(1,IXC(2,I))-X(1,IXC(5,I))
        SX(N)=X(1,IXC(4,I))+X(1,IXC(5,I))-X(1,IXC(2,I))-X(1,IXC(3,I))
        RY(N)=X(2,IXC(3,I))+X(2,IXC(4,I))-X(2,IXC(2,I))-X(2,IXC(5,I))
        SY(N)=X(2,IXC(4,I))+X(2,IXC(5,I))-X(2,IXC(2,I))-X(2,IXC(3,I))
        RZ(N)=X(3,IXC(3,I))+X(3,IXC(4,I))-X(3,IXC(2,I))-X(3,IXC(5,I))
        SZ(N)=X(3,IXC(4,I))+X(3,IXC(5,I))-X(3,IXC(2,I))-X(3,IXC(3,I))
       ENDDO 
       IREP = 0
       IF (IHBE<11) THEN
        IF (ISHFRAM == 1) THEN
	        IREP = 2  
	       ELSE       
	        IREP = 1  
        ENDIF
       ENDIF
      ELSE
C---------------------
C       shells 3 nodes
C---------------------
       DO N=JFT,JLT
        I=NFT+N
        RX(N)=X(1,IXTG(3,I))-X(1,IXTG(2,I))
        RY(N)=X(2,IXTG(3,I))-X(2,IXTG(2,I))
        RZ(N)=X(3,IXTG(3,I))-X(3,IXTG(2,I))
        SX(N)=X(1,IXTG(4,I))-X(1,IXTG(2,I))
        SY(N)=X(2,IXTG(4,I))-X(2,IXTG(2,I))
        SZ(N)=X(3,IXTG(4,I))-X(3,IXTG(2,I))
       ENDDO
       IREP = 0
       IF (IHBE<11) IREP = 1
      ENDIF
      CALL CLSKEW3(JFT,JLT,IREP,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,CDET,OFF )
C--------------------------------------------------      
      DO I=JFT,JLT
        L11    =TENS(1,I)
        L22    =TENS(2,I)
        L33    =TENS(3,I)
        L12    =TENS(4,I)
        L23    =TENS(5,I)
        L13    =TENS(6,I)
        S11    =L11*R11(I)+L12*R12(I)+L13*R13(I)
        S12    =L11*R21(I)+L12*R22(I)+L13*R23(I) 
        S13    =L11*R31(I)+L12*R32(I)+L13*R33(I)
        S21    =L12*R11(I)+L22*R12(I)+L23*R13(I)
        S22    =L12*R21(I)+L22*R22(I)+L23*R23(I)
        S23    =L12*R31(I)+L22*R32(I)+L23*R33(I) 
        S31    =L13*R11(I)+L23*R12(I)+L33*R13(I)
        S32    =L13*R21(I)+L23*R22(I)+L33*R23(I)
        S33    =L13*R31(I)+L23*R32(I)+L33*R33(I)
        TENS(1,I)=R11(I)*S11+R12(I)*S21+R13(I)*S31
        TENS(2,I)=R21(I)*S12+R22(I)*S22+R23(I)*S32
        TENS(3,I)=R31(I)*S13+R32(I)*S23+R33(I)*S33
        TENS(4,I)=R11(I)*S12+R12(I)*S22+R13(I)*S32
        TENS(5,I)=R21(I)*S13+R22(I)*S23+R23(I)*S33
        TENS(6,I)=R11(I)*S13+R12(I)*S23+R13(I)*S33
        AREA(I) = HALF*CDET(I)
      ENDDO 
C-----------------------------------------------
      RETURN
      END SUBROUTINE SHLROTG
Chd|====================================================================
Chd|  TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        PRE_HEPH                      source/output/anim/generate/tensor6.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        SZSIGPARA                     source/elements/solid/solidez/szhour3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE TENSGPS_SKIN(ELBUF_TAB,FUNC1   ,FUNC2   ,IPARG   ,
     .                        IXS      ,IXS10   ,IXS16   ,IXS20   ,X        ,
     .                        ITAGPS  ,PM       ,TAG_SKIN_ND )
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
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FUNC1(3,*),FUNC2(3,*),X(3,*),
     .   PM(NPROPM,*)
      INTEGER IPARG(NPARG,*),TAG_SKIN_ND(*),
     .        IXS(NIXS,*),IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,ITAGPS(*) 
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,NUMNOD),GAMA(6),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA(MVSIZ),
     .   A_GAUSS_R,A_GAUSS_S,A_GAUSS_T,N1,
     .   A_GAUSS_R1,A_GAUSS_S1,A_GAUSS_T1,
     .   A_GAUSS_P_R,A_GAUSS_P_S,A_GAUSS_P_T,
     .   KSI,ETA,ZETA
      INTEGER I,II, NG, NEL, ISS, ISC,NBGAMA,KCVT,
     .        IADD, N, J, MLW,  
     .        ISTRAIN,NN, JTURB,MT, IMID, IALEL,IPID,
     .        NN1,NF,OFFSET,K,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(20,MVSIZ),NNOD,IEXPAN,IHBE,MPT,ILAY,
     .        ICSIG,DIR,IVISC,JJ(6),MAT(MVSIZ),ISKIN(MVSIZ)
      INTEGER MLW2,NLAY
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF   
      my_real
     .  A_GAUSS(9,9),EVAR_TMP(6),ALPHA,BETA,ALPHA_1,BETA_1,
     .  JR0(MVSIZ),JS0(MVSIZ),JT0(MVSIZ),NU(MVSIZ),SIG_HOUR(MVSIZ,6),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), XD5(MVSIZ),
     .   XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), YD5(MVSIZ), 
     .   YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), ZD5(MVSIZ),
     .   ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   XDL(MVSIZ), YDL(MVSIZ), ZDL(MVSIZ),EVAR_T10(6,10),A_HEPH(3,8)
      INTEGER 
     .  SOL_NODE(3,8), IPERM1(10),IPERM2(10),NN2
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C======================================================================= 
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/  
      DATA SOL_NODE / 
     1 -1               ,-1               ,-1               ,
     2 -1               ,-1               , 1               ,
     3  1               ,-1               , 1               ,
     4  1               ,-1               ,-1               ,
     5 -1               , 1               ,-1               ,
     6 -1               , 1               , 1               ,
     7  1               , 1               , 1               ,
     8  1               , 1               ,-1               /
C-----Nj : KSI,ETA,ZETA     
      DATA A_HEPH / 
     1 -1               ,-1               ,-1               ,
     4  1               ,-1               ,-1               ,
     5 -1               , 1               ,-1               ,
     8  1               , 1               ,-1               ,
     2 -1               ,-1               , 1               ,
     3  1               ,-1               , 1               ,
     7  1               , 1               , 1               ,
     6 -1               , 1               , 1               /
C======================================================================= 
      ALPHA = ZEP1381966
      BETA  = ZEP5854102
      DO I=1,NUMNOD
        EVAR(1,I) = ZERO
        EVAR(2,I) = ZERO
        EVAR(3,I) = ZERO
        EVAR(4,I) = ZERO
        EVAR(5,I) = ZERO
        EVAR(6,I) = ZERO
      ENDDO  
      DO 900 NG=1,NGROUP
        IVISC = IPARG(61,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        MLW2 = MLW
        ICSIG=IPARG(17,NG)  
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF (ITY == 1.AND.(IGTYP==14.OR.IGTYP==6)) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         IF (KCVT==1.AND.ISORTH/=0) KCVT=2
         NNOD = ISOLNOD
         ISKIN(1:NEL) = 0
         DO I=LFT,LLT
           N = I + NFT
           IF(ISOLNOD == 8)THEN
             DO J = 1,ISOLNOD
               NC(J,I) = IXS(J+1,N)
             ENDDO
             DO J=1,8
              ISKIN(I) = ISKIN(I) + TAG_SKIN_ND(NC(J,I))
             END DO              
           ELSEIF(ISOLNOD == 4)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
             DO J=1,4
              ISKIN(I) = ISKIN(I) + TAG_SKIN_ND(NC(J,I))
             END DO              
           ELSEIF(ISOLNOD == 6)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(3,N)
             NC(3,I)=IXS(4,N)
             NC(4,I)=IXS(6,N)
             NC(5,I)=IXS(7,N)
             NC(6,I)=IXS(8,N)
           ELSEIF(ISOLNOD == 10)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
             NN1 = N - NUMELS8
             DO J=1,6
               NC(J+4,I) = IXS10(J,NN1)
             ENDDO
             DO J=1,4
              ISKIN(I) = ISKIN(I) + TAG_SKIN_ND(NC(J,I))
             END DO              
           ELSEIF(ISOLNOD == 16)THEN
             DO J = 1,8
               NC(J,I) = IXS(J+1,N)
             ENDDO
             NN1 = N - (NUMELS8+NUMELS10+NUMELS20)
             DO J=1,8
               NC(J+8,I) = IXS16(J,NN1)
             ENDDO
           ELSEIF(ISOLNOD == 20)THEN
             DO J = 1,8
               NC(J,I) = IXS(J+1,N)
             ENDDO
             NN1 = N - (NUMELS8+NUMELS10)
             DO J=1,12
               NC(J+8,I) = IXS20(J,NN1)
             ENDDO
             DO J=1,8
              ISKIN(I) = ISKIN(I) + TAG_SKIN_ND(NC(J,I))
             END DO              
           ENDIF
         ENDDO 
C
         NPTR   = ELBUF_TAB(NG)%NPTR
         NPTS   = ELBUF_TAB(NG)%NPTS
         NPTT   = ELBUF_TAB(NG)%NPTT
         NLAY   = ELBUF_TAB(NG)%NLAY
         NPT = NPTR*NPTS*NPTT
         NNOD = ISOLNOD
         SIG_HOUR = ZERO
         IF (JHBE == 24) THEN
           CALL PRE_HEPH(X,IXS,JR0,JS0,JT0,PM,MAT,NU,NFT,NEL)
         ENDIF
C----------
         IF(ISOLNOD == 6 .OR. ISOLNOD == 8 .OR. 
     .              ISOLNOD == 16 .OR. ISOLNOD == 20)THEN
c
c T_SHELL ( JHBE = 15/16 )
          IF(NLAY > 1 .AND. JHBE /= 14) THEN
           DO I=LFT,LLT
             IF (ISKIN(I)==0) CYCLE
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             NPTS = NLAY
C 
             DO J=1,MIN(8,ISOLNOD)
               DO K=1,MIN(8,ISOLNOD)
                IF(SOL_NODE(2,K) == SOL_NODE(2,J)) THEN
c
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IR = MIN(NPTR,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IT = MIN(NPTT,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 IF (JHBE == 15 .OR. JHBE == 16) THEN
                  ILAY = IS
                  IS = 1 
                  N1 = FOURTH*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
                 ENDIF
c
                   LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                   EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                   EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                   EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                   EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                   EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                   EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
                   IF(IVISC > 0) THEN
                     EVAR_TMP(1) = EVAR_TMP(1) + LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) = EVAR_TMP(2) + LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) = EVAR_TMP(3) + LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) = EVAR_TMP(4) + LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) = EVAR_TMP(5) + LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) = EVAR_TMP(6) + LBUF%VISC(JJ(6) + I)
                   ENDIF
                   IF (KCVT /= 0)
     .             CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA,JHBE,IGTYP)
                   EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                   EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                   EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                   EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                   EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                   EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
                 ENDIF
               ENDDO
             ENDDO
           ENDDO
          ELSEIF (JHBE == 24) THEN 
           DO I=LFT,LLT
             IF (ISKIN(I)==0) CYCLE
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1:6) = GBUF%GAMA(JJ(1:6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             DO J=1,8
	         KSI = A_HEPH(1,J)
	         ETA = A_HEPH(2,J)
	         ZETA = A_HEPH(3,J)
c
                 ILAY = 1

                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
C------       orthotropic laws will be treated later         
                     CALL SZSIGPARA(JR0      ,JS0  ,JT0  ,GBUF%HOURG ,GBUF%SIG ,
     .                             SIG_HOUR   ,KSI    ,ETA  ,ZETA    ,NU   ,NEL , I)
                     EVAR_TMP(1:6) = SIG_HOUR(I,1:6)
                 IF(IVISC > 0) THEN
                   EVAR_TMP(1:6) =EVAR_TMP(1:6)+ LBUF%VISC(JJ(1:6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
                 EVAR(1:6,NC(J,I)) = EVAR(1:6,NC(J,I)) + EVAR_TMP(1:6)
             ENDDO
           ENDDO
          ELSE 
           DO I=LFT,LLT
             IF (ISKIN(I)==0) CYCLE
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             IF(IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
               NPTT = NLAY
             ENDIF
             DO J=1,MIN(8,ISOLNOD)
               DO K=1,MIN(8,ISOLNOD)
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IT = MIN(NPTT,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IR = MIN(NPTR,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 N1 = ONE_OVER_8*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(2,K) * A_GAUSS_P_S)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
c
                 IF (IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
                   ILAY = IT
                   IT = 1
                 ELSE
                   ILAY = 1
                 ENDIF
c
	         KSI = A_GAUSS(IR,2)
	         ETA = A_GAUSS(IS,2)
	         ZETA = A_GAUSS(IT,2)

                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
c
                 EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                 EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                 EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                 EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                 EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                 EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
                 IF(IVISC > 0) THEN
                     EVAR_TMP(1) =EVAR_TMP(1)+ LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) =EVAR_TMP(2)+ LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) =EVAR_TMP(3)+ LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) =EVAR_TMP(4)+ LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) =EVAR_TMP(5)+ LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) =EVAR_TMP(6)+ LBUF%VISC(JJ(6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
                 EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                 EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                 EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                 EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                 EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                 EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
               ENDDO
             ENDDO
           ENDDO
          ENDIF
c    
         ELSEIF(ISOLNOD == 4 )THEN
c    
           DO I=LFT,LLT
             IF (ISKIN(I)==0) CYCLE
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
                 N1 = FOURTH
                 ILAY = 1
                 LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                 EVAR_TMP(1) = LBUF%SIG(JJ(1) + I)
                 EVAR_TMP(2) = LBUF%SIG(JJ(2) + I)
                 EVAR_TMP(3) = LBUF%SIG(JJ(3) + I)
                 EVAR_TMP(4) = LBUF%SIG(JJ(4) + I)
                 EVAR_TMP(5) = LBUF%SIG(JJ(5) + I)
                 EVAR_TMP(6) = LBUF%SIG(JJ(6) + I)
                 IF(IVISC > 0) THEN
                     EVAR_TMP(1) =EVAR_TMP(1)+ LBUF%VISC(JJ(1) + I)
                     EVAR_TMP(2) =EVAR_TMP(2)+ LBUF%VISC(JJ(2) + I)
                     EVAR_TMP(3) =EVAR_TMP(3)+ LBUF%VISC(JJ(3) + I)
                     EVAR_TMP(4) =EVAR_TMP(4)+ LBUF%VISC(JJ(4) + I)
                     EVAR_TMP(5) =EVAR_TMP(5)+ LBUF%VISC(JJ(5) + I)
                     EVAR_TMP(6) =EVAR_TMP(6)+ LBUF%VISC(JJ(6) + I)
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_TMP,GAMA, JHBE,IGTYP)
               DO J=1,4
                 EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + N1 * EVAR_TMP(1)
                 EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + N1 * EVAR_TMP(2)
                 EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + N1 * EVAR_TMP(3)
                 EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + N1 * EVAR_TMP(4)
                 EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + N1 * EVAR_TMP(5)
                 EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + N1 * EVAR_TMP(6)
               ENDDO
           ENDDO
         ELSEIF(ISOLNOD == 10)THEN
c    
           ALPHA_1 = -ALPHA/(BETA-ALPHA)
           BETA_1  = (ONE-ALPHA)/(BETA-ALPHA)
           DO I=LFT,LLT
             IF (ISKIN(I)==0) CYCLE
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             DO J=1,4
               EVAR_T10(1:6,J)=ZERO
               DO K=1,4
                   IR = K
                   IS = 1
                   IT = 1
C
                   IF (J==K) THEN
                     N1 = BETA_1
                   ELSE
                     N1 = ALPHA_1
                   ENDIF
                   ILAY = 1
                   LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                 EVAR_T10(1,J) = EVAR_T10(1,J)+ N1 *LBUF%SIG(JJ(1) + I)
                 EVAR_T10(2,J) = EVAR_T10(2,J)+ N1 *LBUF%SIG(JJ(2) + I)
                 EVAR_T10(3,J) = EVAR_T10(3,J)+ N1 *LBUF%SIG(JJ(3) + I)
                 EVAR_T10(4,J) = EVAR_T10(4,J)+ N1 *LBUF%SIG(JJ(4) + I)
                 EVAR_T10(5,J) = EVAR_T10(5,J)+ N1 *LBUF%SIG(JJ(5) + I)
                 EVAR_T10(6,J) = EVAR_T10(6,J)+ N1 *LBUF%SIG(JJ(6) + I)
                 IF(IVISC > 0) THEN
                   EVAR_T10(1,J) =EVAR_T10(1,J)+ N1 *LBUF%VISC(JJ(1) + I)
                   EVAR_T10(2,J) =EVAR_T10(2,J)+ N1 *LBUF%VISC(JJ(2) + I)
                   EVAR_T10(3,J) =EVAR_T10(3,J)+ N1 *LBUF%VISC(JJ(3) + I)
                   EVAR_T10(4,J) =EVAR_T10(4,J)+ N1 *LBUF%VISC(JJ(4) + I)
                   EVAR_T10(5,J) =EVAR_T10(5,J)+ N1 *LBUF%VISC(JJ(5) + I)
                   EVAR_T10(6,J) =EVAR_T10(6,J)+ N1 *LBUF%VISC(JJ(6) + I)
                 ENDIF
               ENDDO
                 IF (KCVT /= 0)
     .           CALL SROTA6(X,IXS(1,N),KCVT,EVAR_T10(1,J),GAMA, JHBE,IGTYP)
             END DO !J=1,4
             DO J=5,10
                NN1=IPERM1(J)
                NN2=IPERM2(J)
                EVAR_T10(1:6,J) = HALF*(EVAR_T10(1:6,NN1)+EVAR_T10(1:6,NN2))
             END DO
             DO J=1,10
               EVAR(1,NC(J,I)) = EVAR(1,NC(J,I)) + EVAR_T10(1,J)
               EVAR(2,NC(J,I)) = EVAR(2,NC(J,I)) + EVAR_T10(2,J)
               EVAR(3,NC(J,I)) = EVAR(3,NC(J,I)) + EVAR_T10(3,J)
               EVAR(4,NC(J,I)) = EVAR(4,NC(J,I)) + EVAR_T10(4,J)
               EVAR(5,NC(J,I)) = EVAR(5,NC(J,I)) + EVAR_T10(5,J)
               EVAR(6,NC(J,I)) = EVAR(6,NC(J,I)) + EVAR_T10(6,J)
             ENDDO
           ENDDO
         ENDIF
         DO I=LFT,LLT
           IF (ISKIN(I)==0) CYCLE
           DO J = 1,NNOD
             N = NC(J,I)
             IF (N>0)THEN
               DO K = 1,3
                FUNC1(K,N) = EVAR(K,N)
                FUNC2(K,N) = EVAR(K+3,N)
               ENDDO 
               ITAGPS(N) = ITAGPS(N)+1
             ENDIF
           ENDDO
         ENDDO
        ENDIF
c
 900  CONTINUE
C-----------------------------------------------
      RETURN
      END SUBROUTINE TENSGPS_SKIN
Chd|====================================================================
Chd|  PRE_HEPH                      source/output/anim/generate/tensor6.F
Chd|-- called by -----------
Chd|        STRS_TENSCOR3                 source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|-- calls ---------------
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepiso3.F
Chd|====================================================================
        SUBROUTINE PRE_HEPH(X,IXS,JR0,JS0,JT0,PM,MAT,NU,NFT,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      my_real
     .   X(3,*),PM(NPROPM,*),NU(*),JR0(*),JS0(*),JT0(*)
      INTEGER IXS(NIXS,*),MAT(*),NEL ,NFT
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      my_real
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), XD5(MVSIZ),
     .   XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), YD5(MVSIZ), 
     .   YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), ZD5(MVSIZ),
     .   ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   XDL(MVSIZ), YDL(MVSIZ), ZDL(MVSIZ)
      INTEGER I,J,N,NC(8,MVSIZ)
C-----------------------------------------------
C- small strain case should use GBUF%SMSTR but we use current x() for all
           DO I=1,NEL
             N = I + NFT 
             DO J = 1,8
               NC(J,I) = IXS(J+1,N)
             ENDDO
           ENDDO
           DO I=1,NEL
             N = I + NFT 
             XD1(I)=X(1,NC(1,I))  
             YD1(I)=X(2,NC(1,I))  
             ZD1(I)=X(3,NC(1,I))
             XD2(I)=X(1,NC(2,I))
             YD2(I)=X(2,NC(2,I))
             ZD2(I)=X(3,NC(2,I))
             XD3(I)=X(1,NC(3,I))
             YD3(I)=X(2,NC(3,I))
             ZD3(I)=X(3,NC(3,I))
             XD4(I)=X(1,NC(4,I))
             YD4(I)=X(2,NC(4,I))
             ZD4(I)=X(3,NC(4,I))
             XD5(I)=X(1,NC(5,I))
             YD5(I)=X(2,NC(5,I))
             ZD5(I)=X(3,NC(5,I))
             XD6(I)=X(1,NC(6,I))
             YD6(I)=X(2,NC(6,I))
             ZD6(I)=X(3,NC(6,I))
             XD7(I)=X(1,NC(7,I))
             YD7(I)=X(2,NC(7,I))
             ZD7(I)=X(3,NC(7,I))
             XD8(I)=X(1,NC(8,I))
             YD8(I)=X(2,NC(8,I))
             ZD8(I)=X(3,NC(8,I))
           ENDDO
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
           CALL SREPISOT3(
     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
C--- 
           CALL SORTHO3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R12, R13, R11, R22, R23, R21, R32, R33, R31)
C--- 
              DO I=1,NEL      
                XDL(I)=R11(I)*XD1(I)+R21(I)*YD1(I)+R31(I)*ZD1(I)
                YDL(I)=R12(I)*XD1(I)+R22(I)*YD1(I)+R32(I)*ZD1(I)
                ZDL(I)=R13(I)*XD1(I)+R23(I)*YD1(I)+R33(I)*ZD1(I)
                XD1(I)=XDL(I)
                YD1(I)=YDL(I)
                ZD1(I)=ZDL(I)
                XDL(I)=R11(I)*XD2(I)+R21(I)*YD2(I)+R31(I)*ZD2(I)
                YDL(I)=R12(I)*XD2(I)+R22(I)*YD2(I)+R32(I)*ZD2(I)
                ZDL(I)=R13(I)*XD2(I)+R23(I)*YD2(I)+R33(I)*ZD2(I)
                XD2(I)=XDL(I)
                YD2(I)=YDL(I)
                ZD2(I)=ZDL(I)
                XDL(I)=R11(I)*XD3(I)+R21(I)*YD3(I)+R31(I)*ZD3(I)
                YDL(I)=R12(I)*XD3(I)+R22(I)*YD3(I)+R32(I)*ZD3(I)
                ZDL(I)=R13(I)*XD3(I)+R23(I)*YD3(I)+R33(I)*ZD3(I)
                XD3(I)=XDL(I)
                YD3(I)=YDL(I)
                ZD3(I)=ZDL(I)
                XDL(I)=R11(I)*XD4(I)+R21(I)*YD4(I)+R31(I)*ZD4(I)
                YDL(I)=R12(I)*XD4(I)+R22(I)*YD4(I)+R32(I)*ZD4(I)
                ZDL(I)=R13(I)*XD4(I)+R23(I)*YD4(I)+R33(I)*ZD4(I)
                XD4(I)=XDL(I)
                YD4(I)=YDL(I)
                ZD4(I)=ZDL(I)
                XDL(I)=R11(I)*XD5(I)+R21(I)*YD5(I)+R31(I)*ZD5(I)
                YDL(I)=R12(I)*XD5(I)+R22(I)*YD5(I)+R32(I)*ZD5(I)
                ZDL(I)=R13(I)*XD5(I)+R23(I)*YD5(I)+R33(I)*ZD5(I)
                XD5(I)=XDL(I)
                YD5(I)=YDL(I)
                ZD5(I)=ZDL(I)
                XDL(I)=R11(I)*XD6(I)+R21(I)*YD6(I)+R31(I)*ZD6(I)
                YDL(I)=R12(I)*XD6(I)+R22(I)*YD6(I)+R32(I)*ZD6(I)
                ZDL(I)=R13(I)*XD6(I)+R23(I)*YD6(I)+R33(I)*ZD6(I)
                XD6(I)=XDL(I)
                YD6(I)=YDL(I)
                ZD6(I)=ZDL(I)
                XDL(I)=R11(I)*XD7(I)+R21(I)*YD7(I)+R31(I)*ZD7(I)
                YDL(I)=R12(I)*XD7(I)+R22(I)*YD7(I)+R32(I)*ZD7(I)
                ZDL(I)=R13(I)*XD7(I)+R23(I)*YD7(I)+R33(I)*ZD7(I)
                XD7(I)=XDL(I)
                YD7(I)=YDL(I)
                ZD7(I)=ZDL(I)
                XDL(I)=R11(I)*XD8(I)+R21(I)*YD8(I)+R31(I)*ZD8(I)
                YDL(I)=R12(I)*XD8(I)+R22(I)*YD8(I)+R32(I)*ZD8(I)
                ZDL(I)=R13(I)*XD8(I)+R23(I)*YD8(I)+R33(I)*ZD8(I)
                XD8(I)=XDL(I)
                YD8(I)=YDL(I)
                ZD8(I)=ZDL(I) 
              ENDDO
c
           DO I=1,NEL
             JR0(I) = -XD1(I)+XD2(I)+XD3(I)-
     .                 XD4(I)-XD5(I)+XD6(I)+
     .                 XD7(I)-XD8(I)
             JS0(I) = -YD1(I)-YD2(I)+YD3(I)+
     .                 YD4(I)-YD5(I)-YD6(I)+
     .                 YD7(I)+YD8(I)
             JT0(I) = -ZD1(I)-ZD2(I)-ZD3(I)-
     .                 ZD4(I)+ZD5(I)+ZD6(I)+
     .                 ZD7(I)+ZD8(I)
             MAT(I)=IXS(1,I)
             NU(I)=PM(21,MAT(I))
           ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE PRE_HEPH
