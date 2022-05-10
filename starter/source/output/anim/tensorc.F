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
Chd|  TENSORC                       source/output/anim/tensorc.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENSORC(ELBUF_TAB,IPARG,ITENS,                            
     2                  INVERT,NELCUT,EL2FA,NBF  ,TENS  ,               
     3                  IADP ,NBF_L,NBPART,                             
     4                  X     ,IXC  ,IGEO ,IXTG  )                      
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
#include      "com01_c.inc"                                             
#include      "com04_c.inc"                                             
#include      "param_c.inc"                                             
#include      "scr01_c.inc"                                             
#include      "scr05_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      INTEGER IPARG(NPARG,*),ITENS, INVERT(*),                          
     .   EL2FA(*),IXC(NIXC,*), IGEO(NPROPGI,*),                         
     .   NELCUT,NBF,IADP(*),NBF_L,NBPART,                               
     .   IXTG(NIXTG,*)                                                  
      REAL WA(3*NBF_L)                                                  
      my_real                                                           
     .   TENS(3,*), X(3,*)                                     
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL ou REAL*8                                                    
      my_real                                                           
     .   OFF, FAC, A1, A2, A3, THK                   
      REAL R4(18)                                                       
      INTEGER I,J,I1,I2,N, NG, NEL, NFT, ITY, LFT, NPT, IL,IPT,NLAY,                 
     .  LLT, MLW, ISTRAIN, ISTRE,
     .  N0,NNI,NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,II(8)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C=======================================================================                       
      DO J=1,18                                                      
        R4(J) = ZERO                                                 
      ENDDO                                                         
C                                                                       
      NN1 = 1                                                           
      NN2 = NN1                                                         
      NN3 = NN2                                                         
      NN4 = NN3 + NUMELQ                                                
      NN5 = NN4 + NUMELC                                                
      NN6 = NN5 + NUMELTG                                               
      NN7 = NN6                                                         
      NN8 = NN7                                                         
      NN9 = NN8                                                         
C                                                                       
      DO NG=1,NGROUP                                               
        MLW = IPARG(1,NG)                                             
        NEL = IPARG(2,NG)                                             
        NFT = IPARG(3,NG)                                             
        ITY = IPARG(5,NG)                                             
        LFT = 1                                                           
        LLT = NEL                                                         
!
        DO J=1,8  ! lenght max of GBUF%G_STRA = 8
          II(J) = NEL*(J-1)
        ENDDO
!
C-----------------------------------------------                        
        IF(ITY == 2)THEN                                                
C         QUAD                                                            
C-----------------------------------------------                        
          DO I=LFT,LLT                                                 
            N = I + NFT                                            
            TENS(1,EL2FA(NN3+N)) = ZERO                            
            TENS(2,EL2FA(NN3+N)) = ZERO                            
            TENS(3,EL2FA(NN3+N)) = ZERO                            
          ENDDO                                                        
C-----------------------------------------------                        
        ELSEIF(ITY == 3.OR.ITY == 7)THEN                                
C         COQUES                                                          
C-----------------------------------------------                        
          GBUF => ELBUF_TAB(NG)%GBUF   
          NLAY = ELBUF_TAB(NG)%NLAY    
          NPT     = IABS(IPARG(6,NG))                                    
          ISTRAIN = IPARG(44,NG)                                         
C                                                                        
          FAC   = ZERO                                                   
          A1    = ZERO                                                   
          A2    = ZERO                                                   
          A3    = ZERO                                                   
          ISTRE = 1 
c
c         STRAIN                                                             
c
          IF (ITENS == 5) THEN                                         
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN                       
              A1 = UN
            ENDIF                                        
          ELSEIF (ITENS == 6) THEN                       
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN                       
              A2 = UN
            ENDIF                                        
          ELSEIF (ITENS == 7) THEN                       
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN                       
              A1 = UN
              A2 = UNDEMI
            ENDIF                                        
          ELSEIF (ITENS == 8) THEN                       
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN                       
              A1 = UN
              A2 = -UNDEMI
            ENDIF                                        
          ELSEIF (ITENS >=  201 .AND. ITENS <= 300) THEN 
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1 .AND. NPT /= 0) THEN        
              IPT = MIN(NPT,ITENS - 200)                 
              A1 = UN
              A2 = UNDEMI*(((2*IPT-UN)/NPT)-UN)
            ENDIF                                        
          ENDIF ! IF (ITENS == 5)
        ENDIF ! IF(ITY == 2)
C------------------------                                               
C         STRAIN RATE (ne fonctionne pas : pas d'EPSDOT)                 
C------------------------                                               
        IF (ITY == 3) THEN                                              
          N0 = 0                                                    
          NNI = NN4                                                 
        ELSE                                                          
          N0 = NUMELC                                               
          NNI = NN5                                                 
        ENDIF                                                         
c-----------------------------------------------------------
        IF (ISTRE == 1) THEN                                             
C------------------------                                               
C          STRESS                                                       
C------------------------                                               
          IF (ITENS == 1) THEN                                           
            DO I=LFT,LLT                                               
              N = I + NFT                                              
              DO J = 1,3                                             
                R4(J) = GBUF%FOR(II(J)+I)
              ENDDO                                                    
              R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                     
              TENS(1,EL2FA(NNI+N)) = R4(1)                             
              TENS(2,EL2FA(NNI+N)) = R4(2)                             
              TENS(3,EL2FA(NNI+N)) = R4(3)                             
            ENDDO                                                      
          ELSEIF (ITENS == 2) THEN                                       
            DO I=LFT,LLT                                               
              N = I + NFT                                              
              DO J = 1,3
                R4(J) = GBUF%MOM(II(J)+I)                  
              ENDDO                                                    
              R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                     
              TENS(1,EL2FA(NNI+N)) = R4(1)                             
              TENS(2,EL2FA(NNI+N)) = R4(2)                             
              TENS(3,EL2FA(NNI+N)) = R4(3)                             
            ENDDO                                                      
          ELSEIF (ITENS == 3) THEN                                       
            IF (MLW == 1) THEN                                           
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                DO J = 1,3                                           
                  R4(J) = GBUF%FOR(II(J)+I) + SIX*GBUF%MOM(II(J)+I)                
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ELSEIF (MLW == 3.OR.MLW == 23) THEN                          
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                DO J = 1,3                                           
                  R4(J) = GBUF%FOR(II(J)+I)        
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ELSEIF (MLW == 2  .OR. MLW == 19 .OR.                        
     .              MLW == 22 .OR. MLW == 25 .OR.                       
     .              MLW == 27 .OR. MLW == 32 .OR.                       
     .              MLW == 36 .OR. MLW == 15) THEN                       
              IF (NLAY > 1) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(NPT)                      
                DO I=LFT,LLT                                           
                  N = I + NFT                                          
                  I1 = (I-1) * 5                                
                  DO J = 1,3                                         
                    R4(J) = BUFLY%SIGPT(I1+J)                
                  ENDDO                                                
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                 
                  TENS(1,EL2FA(NNI+N)) = R4(1)                         
                  TENS(2,EL2FA(NNI+N)) = R4(2)                         
                  TENS(3,EL2FA(NNI+N)) = R4(3)                         
                ENDDO                                                  
              ELSE
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)                      
                DO I=LFT,LLT                                           
                  N = I + NFT                                          
                  I1 = (I-1) * 5                                
                  DO J = 1,3                                         
                   R4(J) = BUFLY%SIGPT((NPT-1)*NEL*5 + I1 + J)                
                  ENDDO                                                
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                 
                  TENS(1,EL2FA(NNI+N)) = R4(1)                         
                  TENS(2,EL2FA(NNI+N)) = R4(2)                         
                  TENS(3,EL2FA(NNI+N)) = R4(3)                         
                ENDDO                                                  
              ENDIF ! IF (NLAY > 1)
            ENDIF ! IF (MLW == 1)
c
          ELSEIF (ITENS == 4) THEN                                       
            IF (MLW == 1) THEN                                           
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                DO J = 1,3                                           
                  R4(J) = GBUF%FOR(II(J)+I) - SIX*GBUF%MOM(II(J)+I)                
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ELSEIF (MLW == 3.OR.MLW == 23) THEN                          
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                DO J = 1,3                                           
                  R4(J) = GBUF%FOR(II(J)+I)        
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ELSEIF (MLW == 2.OR.MLW == 19.OR.                           
     .              MLW == 22.OR.MLW == 25.OR.                          
     .              MLW == 27.OR.MLW == 32.OR.                          
     .              MLW == 36.OR.MLW == 15)THEN                         
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)                        
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                I1 = (I-1) * 5                                  
                DO J = 1,3                                           
                  R4(J) = BUFLY%SIGPT(I1+J)                  
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ENDIF ! IF (MLW == 1)
          ELSEIF (ITENS.GE.101.AND.ITENS.LE.200) THEN                  
            IF (MLW == 1.OR.MLW == 3.OR.MLW == 23) THEN                  
              DO I=LFT,LLT                                             
                N = I + NFT                                            
                DO J = 1,3                                           
                  R4(J) = GBUF%FOR(II(J)+I)          
                ENDDO                                                  
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                   
                TENS(1,EL2FA(NNI+N)) = R4(1)                           
                TENS(2,EL2FA(NNI+N)) = R4(2)                           
                TENS(3,EL2FA(NNI+N)) = R4(3)                           
              ENDDO                                                    
            ELSEIF (MLW == 2.OR.MLW == 19.OR.                           
     .              MLW == 22.OR.MLW == 25.OR.                          
     .              MLW == 27.OR.MLW == 32.OR.                          
     .              MLW == 36.OR.MLW == 15) THEN                         
              IPT  = MIN(NPT,ITENS-100)                                 
              IF (NLAY > 1) THEN
                BUFLY => ELBUF_TAB(NG)%BUFLY(IPT)                      
                DO I=LFT,LLT                                           
                  N = I + NFT                                          
                  I1 = (I-1) * 5                                
                  DO J = 1,3                                         
                    R4(J) = BUFLY%SIGPT(I1+J)                
                  ENDDO                                                
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                 
                  TENS(1,EL2FA(NNI+N)) = R4(1)                         
                  TENS(2,EL2FA(NNI+N)) = R4(2)                         
                  TENS(3,EL2FA(NNI+N)) = R4(3)                         
                ENDDO                                                  
              ELSE
                BUFLY => ELBUF_TAB(NG)%BUFLY(1)                      
                DO I=LFT,LLT                                           
                  N = I + NFT                                          
                  I1 = (I-1) * 5                                
                  DO J = 1,3                                         
                    R4(J) = BUFLY%SIGPT((IPT-1)*NEL*5 + I1 + J)                
                  ENDDO                                                
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))                 
                  TENS(1,EL2FA(NNI+N)) = R4(1)                         
                  TENS(2,EL2FA(NNI+N)) = R4(2)                         
                  TENS(3,EL2FA(NNI+N)) = R4(3)                         
                ENDDO                                                  
              ENDIF ! IF (NLAY > 1)
            ENDIF ! IF (MLW == 1.OR.MLW == 3.OR.MLW == 23)
          ENDIF ! IF (ITENS == 1)
C------------------------                                               
        ELSEIF (ISTRE == 0 .AND. GBUF%G_STRA > 0) THEN                                        
C------------------------                                               
C          STRAIN                                                       
C------------------------                                               
          DO I=LFT,LLT                                                 
            N = I + NFT                                                 
            THK = GBUF%THK(I)                                           
            IF (ITENS /= 6) THEN                                        
              DO J=1,3                                                  
                R4(J) = A1*GBUF%STRA(II(J)+I) + A2*GBUF%STRA(II(J)+I) * THK   
              ENDDO                                                     
            ELSE                                                        
              DO J=1,3                                                  
                R4(J) = GBUF%STRA(II(J)+I)                                 
              ENDDO                                                     
           ENDIF                                                       
            R4(3) = R4(3) * INVERT(EL2FA(NNI+N)) * UNDEMI               
            TENS(1,EL2FA(NNI+N)) = R4(1)                                
            TENS(2,EL2FA(NNI+N)) = R4(2)                                
            TENS(3,EL2FA(NNI+N)) = R4(3)                                
          ENDDO                                                        
        ENDIF ! IF (ISTRE == 1)
      ENDDO ! DO NG=1,NGROUP
C-----------------------------------------------                        
      DO N=1,NBF                                                        
        R4(1) = TENS(1,N)                                               
        R4(2) = TENS(2,N)                                               
        R4(3) = TENS(3,N)                                               
        CALL WRITE_R_C(R4,3)                                            
      ENDDO                                                             
C-----------------------------------------------                        
      IF (NELCUT .GT. 0) THEN                                              
        DO I=1,NELCUT                                                   
          CALL WRITE_R_C(R4,3)                                          
        ENDDO                                                           
      ENDIF                                                             
C-----------                                                                       
      RETURN                                                            
      END                                                               
