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
Chd|  DFUNCF                        source/output/anim/dfuncf.F   
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DFUNCF(ELBUF_TAB,FUNC  ,IFUNC ,IPARG  ,GEO   ,                  
     .                  IXT      ,IXP   ,IXR   ,MASS   ,PM    ,                  
     .                  EL2FA    ,NBF   ,IADP  ,NBPART ,XFUNC1)
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
#include      "mvsiz_p.inc"                                             
#include      "com01_c.inc"                                             
#include      "com04_c.inc"                                             
#include      "scr05_c.inc"                                             
#include      "scr14_c.inc"                                             
#include      "scr23_c.inc"                                             
#include      "param_c.inc"                                             
#include      "rnur_c.inc"                                              
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL ou REAL*8                                                    
      my_real                                                           
     .   FUNC(*), MASS(*), PM(NPROPM,*), GEO(NPROPG,*),        
     .   XFUNC1(10,*)                                                   
      INTEGER IPARG(NPARG,*),EL2FA(*),                                  
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IFUNC,NBF,            
     .        IADP(*),NBPART,NBF2                             
      INTEGER BUF                                                       
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL ou REAL*8                                                    
      my_real                                                           
     .   EVAR(MVSIZ),                                                   
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,                   
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, XM,                               
     .   FOR, AREA, FEQ                                                 
      INTEGER I, NG, NEL, NFT, ITY, LFT, NPT, ISS, ISC,            
     .        IADD, N, J, LLT, MLW,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,JALE, IMID, IALEL,IPID,       
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NF,              
     .        OFFSET,K,II,II_L,INC,KK,IHBE,JJ(3)
      REAL R4                                                           
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C La routine ne fonctionne que pour les IFUNC 3,7,14-19 (stress)        
C-----------------------------------------------                        
C                                                                       
      NN1 = 1                                                           
      NN3 = 1                                                           
      NN4 = NN3                                                         
      NN5 = NN4                                                         
      NN6 = NN5                                                         
      NN7 = NN6 + NUMELT                                                
      NN8 = NN7 + NUMELP                                                
      NN9 = NN8 + NUMELR                                                
      NN10= NN9                                                         
C                                                                       
      DO NG=1,NGROUP                                                
        MLW   =IPARG(1,NG)                                              
        NEL   =IPARG(2,NG)                                              
        ITY   =IPARG(5,NG)                                              
        GBUF => ELBUF_TAB(NG)%GBUF
        DO OFFSET = 0,NEL-1,NVSIZ                                       
          NFT   =IPARG(3,NG) + OFFSET                                   
          LFT=1                                                         
          LLT=MIN(NVSIZ,NEL-OFFSET)
!
          DO J=1,3
            JJ(J) = NEL*(J-1)
          ENDDO
!
C-----------------------------------------------                        
C       TRUSS                                                           
C-----------------------------------------------                        
          IF (ITY == 4) THEN                                                
            IF (IFUNC == 3) THEN                                            
              DO I=LFT,LLT                                                
               N = I + NFT                                                
               FUNC(EL2FA(NN6+N))=GBUF%EINT(I)/                           
     .              MAX(EM30,MASS(EL2FA(NN6+N)))                          
              ENDDO                                                       
            ELSEIF (IFUNC == 7) THEN                                        
              DO I=LFT,LLT                                                
                N = I + NFT                                                
                FOR = GBUF%FOR(I)                                        
                AREA = GBUF%AREA(I)                                      
                FEQ = FOR*FOR                                              
                FUNC(EL2FA(NN6+N)) = SQRT(FEQ)/AREA                        
              ENDDO                                                       
            ELSEIF (IFUNC == 14) THEN                                       
              DO I=LFT,LLT                                                
                N = I + NFT                                                
                FUNC(EL2FA(NN6+N)) = GBUF%FOR(I) / GBUF%AREA(I)
              ENDDO                                                       
            ELSE                                                          
              DO I=LFT,LLT                                                
                N = I + NFT                                                
                FUNC(EL2FA(NN6+N)) = ZERO                                  
              ENDDO                                                       
            ENDIF                                                         
C-----------------------------------------------                        
C       POUTRES                                                         
C-----------------------------------------------                        
        ELSEIF (ITY == 5) THEN                                            
          IF (IFUNC == 3) THEN                                            
            DO I=LFT,LLT                                                
              N = I + NFT
              FUNC(EL2FA(NN7+N)) = (GBUF%EINT(I) + GBUF%EINT(I+LLT))     
     .                           / MAX(EM30,MASS(EL2FA(NN7+N)))
            ENDDO                                                       
          ELSEIF (IFUNC == 7) THEN                                        
            DO I=LFT,LLT                                                
              N = I + NFT
              A1 = GEO(1,IXP(5,N))                                       
              B1 = GEO(2,IXP(5,N))                                       
              B2 = GEO(18,IXP(5,N))                                      
              B3 = GEO(4,IXP(5,N))                                       
              F1 = GBUF%FOR(JJ(1)+I)
              M1 = GBUF%MOM(JJ(1)+I)
              M2 = GBUF%MOM(JJ(2)+I)
              M3 = GBUF%MOM(JJ(3)+I)
              YEQ= F1*F1 + THREE* A1 *                                      
     +                   ( M1*M1 / MAX(B3,EM30)                          
     +                   + M2*M2 / MAX(B1,EM30)                          
     +                   + M3*M3 / MAX(B2,EM30) )                        
              FUNC(EL2FA(NN7+N)) = SQRT(YEQ)/A1                          
            ENDDO                                                       
          ELSEIF (IFUNC == 14) THEN                                       
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(1)+I)
     .                           / GEO(1,IXP(5,N))                       
            ENDDO                                                       
          ELSEIF (IFUNC == 17) THEN                                       
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(2)+I)
     .                           / GEO(1,IXP(5,N))                       
            ENDDO                                                       
          ELSEIF (IFUNC == 19) THEN                                       
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN7+N)) = GBUF%FOR(JJ(3)+I)
     .                           / GEO(1,IXP(5,N))                       
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN7+N)) = ZERO                                  
            ENDDO                                                       
          ENDIF                                                         
C-----------------------------------------------                        
C       RESSORTS                                                        
C-----------------------------------------------                        
        ELSEIF (ITY == 6) THEN                                            
          IF (IFUNC == 3) THEN                                            
            IF (MLW == 1) THEN                                           
              XM = ONE/GEO(1,IXR(1,1+NFT))                                 
              DO  I=LFT,LLT                                               
                N = I + NFT                                               
                FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM                      
              ENDDO                                                       
            ELSEIF (MLW == 2) THEN                                       
              XM = ONE/GEO(1,IXR(1,1+NFT))                                 
              DO  I=LFT,LLT                                               
                N = I + NFT                                               
                FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM                      
              ENDDO                                                       
            ELSEIF (MLW == 3) THEN                                       
              XM = ONE/GEO(1,IXR(1,1+NFT))                                 
              DO  I=LFT,LLT                                               
                N = I + NFT                                               
                FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM                      
              ENDDO                                                       
            ELSEIF (MLW == 4) THEN                                       
              XM = ONE/GEO(1,IXR(1,1+NFT))                                 
              DO  I=LFT,LLT                                               
                N = I + NFT                                               
                FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)*XM                      
              ENDDO                                                       
            ELSEIF (MLW == 5) THEN                                       
              DO  I=LFT,LLT                                               
                N = I + NFT                                               
                FUNC(EL2FA(NN8+N)) = GBUF%EINT(I)/MAX(EM30,GBUF%MASS(I)) 
              ENDDO                                                       
            ENDIF ! IF (MLW)
          ELSEIF (IFUNC == 11) THEN                                       
            DO  I=LFT,LLT                                               
              N = I + NFT                                               
*              FUNC(EL2FA(NN8+N)) = ANIM(N)                             
            ENDDO                                                       
          ELSEIF (IFUNC == 12) THEN                                       
            KK = NUMELR * ANIM_FE(11)                                   
            DO  I=LFT,LLT                                               
              N = I + NFT                                               
*              FUNC(EL2FA(NN8+N)) = ANIM(N+KK)                          
            ENDDO                                                       
          ELSEIF (IFUNC == 13) THEN                                       
            KK = NUMELR * (ANIM_FE(11)+ANIM_FE(12))                     
            DO  I=LFT,LLT                                               
              N = I + NFT                                               
*              FUNC(EL2FA(NN8+N)) = ANIM(N+KK)                          
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN8+N)) = ZERO                                  
            ENDDO                                                       
          ENDIF ! IF (IFUNC)
C
          IF (MLW == 3) THEN                                              
            DO I=LFT,LLT                                                
             N = I + NFT                                                
             FUNC(EL2FA(NN8+N)+1) = FUNC(EL2FA(NN8+N))                  
            ENDDO                                                       
          ENDIF
        ENDIF ! IF (ITY)
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES OFFSET                                    
C-----------------------------------------------                        
       ENDDO ! DO OFFSET = 0,NEL-1,NVSIZ
      ENDDO !       DO NG=1,NGROUP
C-----------------------------------------------                        
      DO N=1,NBF                                                        
        R4 = FUNC(N)                                                   
        CALL WRITE_R_C(R4,1)                                           
      ENDDO                                                             
      IF (IFUNC == 3) THEN                                              
        DO N=1,NANIM1D                                                   
          VALUE = XFUNC1(1,N)                                           
          R4 = VALUE                                                     
          CALL WRITE_R_C(R4,1)                                           
        ENDDO                                                            
      ELSE                                                              
        DO N=1,NANIM1D                                                   
          R4 = ZERO
          CALL WRITE_R_C(R4,1)                                           
        ENDDO                                                            
      ENDIF                                                             
C---
      RETURN                                                            
      END                                                               
