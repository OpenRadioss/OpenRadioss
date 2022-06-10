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
Chd|  SIGIN3B                       source/elements/solid/solid8p/sigin3b.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        M38INIT                       source/materials/mat/mat038/m38init.F
Chd|        M70INIT                       source/materials/mat/mat070/m70init.F
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGIN3B(MAT     ,PM      ,IPM     ,SIG     ,VOL     ,
     2                   SIGSP   ,SIGI    ,EINT    ,RHO     ,
     3                   IX      ,NIX     ,NSIGI   ,NSIGS   ,
     4                   NEL     ,IDEF    ,BUFMAT  ,NPF     ,
     5                   TF      ,STRSGLOB,STRAGLOB,JHBE    ,
     6                   IGTYP   ,X       ,BUFGAMA ,BUFLY   ,L_PLA   ,
     7                   PT      )
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
#include       "com01_c.inc" 
#include       "com04_c.inc"
#include       "param_c.inc"
#include       "vect01_c.inc"
#include       "scr19_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX, N, JPS, NSIGI, NEL,IDEF,JHBE,IGTYP, NSIGS
      INTEGER IX(NIX,*), IPM(NPROPMI,*), NPF(*) ,
     .   STRSGLOB(*),STRAGLOB(*),MAT(NEL),L_PLA,PT(*)
C     REAL
      my_real
     .   SIG(NEL,6),EINT(NEL),RHO(NEL),VOL(*),BUFGAMA(6*NEL), 
     .   SIGSP(NSIGI,*),PM(NPROPM,*),SIGI(NSIGS,*),
     .   BUFMAT(*), TF(*),X(3,*)
      TYPE(BUF_LAY_), TARGET  :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,IPT,JPT,JPTP,JPS1,NUVAR,MA,IADBUF,NPAR,
     .   NFUNC,IFLAGINI,KK(6)
      INTEGER IFUNC(MAXFUNC) 
C     REAL
      my_real
     .  RHO0(NEL),GAMA(6),TENS(6)
      my_real,
     .  DIMENSION(:) ,POINTER :: UVAR
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
      NUVAR = BUFLY%NVAR_MAT
      DO I=LFT,LLT         
        MA=MAT(I)        
        EINT(I)=PM(23,MA)
        RHO(I) =PM(89,MA)
      ENDDO

!
      DO J=1,6
        KK(J) = (J-1)*NEL
      ENDDO
!
C      
      IF (MTN >= 28) THEN
        NPAR   = IPM(9,MAT(1))
        IADBUF = IPM(7,MAT(1))
        DO I=LFT,LLT
          RHO0(I)= PM( 1,MAT(I))
        END DO
C       attention loi 36
        NFUNC  = IPM(10,MAT(1))
        DO I=1,NFUNC
          IFUNC(I) = IPM(10+I,MAT(1))
        ENDDO
C
        DO IPT = 1,8
          UVAR => BUFLY%MAT(1,1,IPT)%VAR
          IF (MTN == 38) THEN
            CALL M38INIT(
     1         NEL   , NPAR   , NUVAR   ,NFUNC  ,IFUNC  ,
     2         NPF   ,TF      , BUFMAT(IADBUF),RHO0    ,VOL ,  
     3         EINT  ,UVAR    ) 
          ELSEIF (MTN == 70) THEN
            CALL M70INIT(
     1         NEL   ,NPAR    ,NUVAR   ,NFUNC  ,IFUNC  ,
     2         NPF   ,TF      ,BUFMAT(IADBUF),RHO0    ,VOL ,  
     3         EINT  ,UVAR    ) 
          ENDIF
        END DO
      ENDIF
C-----------------------      
      IF (ISIGI /= 0)THEN
C         
        DO IPT = 1,8
          LBUF => BUFLY%LBUF(1,1,IPT)                       
          JPT =(IPT-1)*NEL                                   
          JPTP= (IPT-1)*NEL*NUVAR                           
          JPS = 4 + (IPT-1)*9                                
          JPS1 = NVSOLID1 +  (IPT-1)*6                      
c
          DO I = LFT,LLT  
            IFLAGINI = 0                                  
            IF (STRAGLOB(I) == 1 .OR. STRSGLOB(I) == 1)THEN 
              IF (JCVT==2 .AND. JHBE/=14) THEN              
                GAMA(1)=BUFGAMA(I	 )		    
                GAMA(2)=BUFGAMA(I +   NEL)		    
                GAMA(3)=BUFGAMA(I + 2*NEL)		    
                GAMA(4)=BUFGAMA(I + 3*NEL)		    
                GAMA(5)=BUFGAMA(I + 4*NEL)		    
                GAMA(6)=BUFGAMA(I + 5*NEL)		    
              ELSE                                          
                GAMA(1)=ONE                                  
                GAMA(2)=ZERO                                
                GAMA(3)=ZERO                                
                GAMA(4)=ZERO                                
                GAMA(5)=ONE                                  
                GAMA(6)=ZERO                                
              END IF                                        
            ENDIF                                           
C           CONTRAINTES INITIALES 
            II=NFT+I
            JJ=PT(II)
            IFLAGINI = 1
            IF(JJ.EQ.0)IFLAGINI = 0                     
c---
            IF (IFLAGINI == 1) THEN
              IF (SIGSP(1,JJ) == 1) THEN                                         
                IF (STRSGLOB(I) == 1) THEN
                  TENS(1) = SIGSP(JPS+1,JJ)		    
                  TENS(2) = SIGSP(JPS+2,JJ)		    
                  TENS(3) = SIGSP(JPS+3,JJ)		    
                  TENS(4) = SIGSP(JPS+4,JJ)		    
                  TENS(5) = SIGSP(JPS+5,JJ)		    
                  TENS(6) = SIGSP(JPS+6,JJ)  
                  CALL SROTA6_M1(X,IX(1,II),JCVT,                
     .                           TENS,GAMA,JHBE,IGTYP)
                 SIGSP(JPS+1,JJ) = TENS(1)				
                 SIGSP(JPS+2,JJ) = TENS(2)				
                 SIGSP(JPS+3,JJ) = TENS(3)				
                 SIGSP(JPS+4,JJ) = TENS(4)				
                 SIGSP(JPS+5,JJ) = TENS(5)				
                 SIGSP(JPS+6,JJ) = TENS(6)
                ENDIF 
                 LBUF%SIG(KK(1)+I) = SIGSP(JPS+1,JJ)				  
                 LBUF%SIG(KK(2)+I) = SIGSP(JPS+2,JJ)				  
                 LBUF%SIG(KK(3)+I) = SIGSP(JPS+3,JJ)				  
                 LBUF%SIG(KK(4)+I) = SIGSP(JPS+4,JJ)				  
                 LBUF%SIG(KK(5)+I) = SIGSP(JPS+5,JJ)				  
                 LBUF%SIG(KK(6)+I) = SIGSP(JPS+6,JJ)				  
                 IF(L_PLA /= 0 .AND. SIGSP(JPS+7,JJ) /= ZERO) 
     .                 LBUF%PLA(I)     = SIGSP(JPS+7,JJ)                              
                 IF (SIGSP(3,JJ) /= 0.0) EINT(I)=SIGSP(3,JJ)                    
                 IF (SIGSP(4,JJ) /= 0.0) THEN                                   
                   VOL(I) = SIGSP(4,JJ)*VOL(I) / RHO(I)                         
                   RHO(I) = SIGSP(4,JJ)                                         
                 ENDIF                                                          
                 SIG(I,1) = SIG(I,1) + ONE_OVER_8*LBUF%SIG(KK(1)+I) 		  
                 SIG(I,2) = SIG(I,2) + ONE_OVER_8*LBUF%SIG(KK(2)+I) 		  
                 SIG(I,3) = SIG(I,3) + ONE_OVER_8*LBUF%SIG(KK(3)+I) 		  
                 SIG(I,4) = SIG(I,4) + ONE_OVER_8*LBUF%SIG(KK(4)+I) 		  
                 SIG(I,5) = SIG(I,5) + ONE_OVER_8*LBUF%SIG(KK(5)+I) 		  
                 SIG(I,6) = SIG(I,6) + ONE_OVER_8*LBUF%SIG(KK(6)+I) 		  
              ELSE                                                             
                LBUF%SIG(KK(1)+I)= SIG(I,1)					 
                LBUF%SIG(KK(2)+I)= SIG(I,2)					 
                LBUF%SIG(KK(3)+I)= SIG(I,3)					 
                LBUF%SIG(KK(4)+I)= SIG(I,4)					 
                LBUF%SIG(KK(5)+I)= SIG(I,5)					 
                LBUF%SIG(KK(6)+I)= SIG(I,6)    
                EINT(I)        = SIGI(9,JJ)                                    
                IF (BUFLY%L_PLA > 0) LBUF%PLA(I) = SIGI(10,JJ)                                   
                IF (STRSGLOB(I) == 1) THEN                                          
                  TENS(1) = LBUF%SIG(KK(1)+I)		      
                  TENS(2) = LBUF%SIG(KK(2)+I)		      
                  TENS(3) = LBUF%SIG(KK(3)+I)		      
                  TENS(4) = LBUF%SIG(KK(4)+I)		      
                  TENS(5) = LBUF%SIG(KK(5)+I)		      
                  TENS(6) = LBUF%SIG(KK(6)+I)
                  CALL SROTA6_M1(X    ,IX(1,II) ,JCVT ,          
     .                           TENS ,GAMA,JHBE ,IGTYP    )
                  LBUF%SIG(KK(1)+I) = TENS(1)				  
                  LBUF%SIG(KK(2)+I) = TENS(2)				  
                  LBUF%SIG(KK(3)+I) = TENS(3)				  
                  LBUF%SIG(KK(4)+I) = TENS(4)				  
                  LBUF%SIG(KK(5)+I) = TENS(5)				  
                  LBUF%SIG(KK(6)+I) = TENS(6)
                ENDIF
              ENDIF  ! STRSGLOB(I) == 1                                        
c
              IF (NVSOLID2 /= 0 .AND. IDEF /= 0) THEN                             
                LBUF%STRA(KK(1)+I) = SIGSP(JPS1 + 1,JJ) 			  
                LBUF%STRA(KK(2)+I) = SIGSP(JPS1 + 2,JJ) 			  
                LBUF%STRA(KK(3)+I) = SIGSP(JPS1 + 3,JJ) 			  
                LBUF%STRA(KK(4)+I) = SIGSP(JPS1 + 4,JJ) 			  
                LBUF%STRA(KK(5)+I) = SIGSP(JPS1 + 5,JJ) 			  
                LBUF%STRA(KK(6)+I) = SIGSP(JPS1 + 6,JJ) 			  
                IF (STRAGLOB(I) == 1) THEN
                  TENS(1) = LBUF%STRA(KK(1)+I)  	       
                  TENS(2) = LBUF%STRA(KK(2)+I)  	       
                  TENS(3) = LBUF%STRA(KK(3)+I)  	       
                  TENS(4) = LBUF%STRA(KK(4)+I)  	       
                  TENS(5) = LBUF%STRA(KK(5)+I)  	       
                  TENS(6) = LBUF%STRA(KK(6)+I)
                  CALL SROTA6_M1(X    ,IX(1,II),JCVT  ,       
     .                           TENS ,GAMA,JHBE ,IGTYP   )
                  LBUF%STRA(KK(1)+I) = TENS(1)  		       
                  LBUF%STRA(KK(2)+I) = TENS(2)  		       
                  LBUF%STRA(KK(3)+I) = TENS(3)  		       
                  LBUF%STRA(KK(4)+I) = TENS(4)  		       
                  LBUF%STRA(KK(5)+I) = TENS(5)  		       
                  LBUF%STRA(KK(6)+I) = TENS(6)        
                ENDIF
              ENDIF                                                             
            ENDIF  ! IFLAGINI == 1
c---   
          ENDDO ! I = LFT,LLT         
        ENDDO   ! IPT
      ENDIF     ! ISIGI /= 0
C-----------
      RETURN
      END
