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
Chd|  DFUNCC                        source/output/anim/dfuncc.F   
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DFUNCC(ELBUF_TAB,BUFEL,FUNC ,IFUNC,IPARG,                
     .                  IXQ      ,IXC  ,IXTG ,PM   ,EL2FA,                    
     .                  NBF      )                                          
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
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   BUFEL(*),FUNC(*),PM(NPROPM,*)                                  
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),EL2FA(*),        
     .        IXQ(NIXQ,*),IFUNC,NBF        
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   EVAR(MVSIZ),                                                   
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE         
      INTEGER I,II(6), NG, NEL, N, MLW, IUS,MT,IALEL,             
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,              
     .        OFFSET,IGTYP,JJ                    
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      REAL R4
C-----------------------------------------------                        
C La routine ne fonctionne que pour les IFUNC 3,6,7,14-19 (stress)      
      NN1 = 1                                                           
      NN3 = 1                                                           
      NN4 = NN3 + NUMELQ                                                
      NN5 = NN4 + NUMELC                                                
      NN6 = NN5 + NUMELTG                                               
      NN7 = NN6                                                         
      NN8 = NN7                                                         
      NN9 = NN8                                                         
C                                                                       
      DO 900 NG=1,NGROUP
              MLW = IPARG(1,NG)
        NEL = IPARG(2,NG)                                            
        NFT = IPARG(3,NG)
        ITY = IPARG(5,NG)
        DO OFFSET = 0,NEL-1,NVSIZ                                       
          LFT=1                                                         
          LLT=MIN(NVSIZ,NEL-OFFSET)                                     
!
          DO I=1,6
            II(I) = (I-1)*NEL
          ENDDO
!
C-----------------------------------------------                        
C       QUAD                                                            
C-----------------------------------------------                        
        IF (ITY == 2) THEN                                                
          GBUF => ELBUF_TAB(NG)%GBUF
C-----                                                                       
          IF(IFUNC == 3)THEN                                             
            DO I=LFT,LLT                                                 
               N = I + NFT                                              
               IALEL=IPARG(7,NG)+IPARG(11,NG)                           
               IF(IALEL == 0)THEN                                       
                 MT=IXQ(1,N)                                            
                  VALUE = GBUF%EINT(I)/MAX(EM30,PM(1,MT))
               ELSE                                                     
                  VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
               ENDIF                                                    
               FUNC(EL2FA(NN3+N)) = VALUE                               
            ENDDO                                                        
C-----                                                                       
          ELSEIF (IFUNC == 6 .or. IFUNC == 7) THEN                           
           DO I=LFT,LLT                                             
               N = I + NFT                                              
               P = -(GBUF%SIG(II(1) + I)         
     .             + GBUF%SIG(II(2) + I)         
     .             + GBUF%SIG(II(3) + I))*THIRD 
               FUNC(EL2FA(NN3+NFT+I)) = P
               VALUE = P                                                
               IF(IFUNC == 7) THEN                                      
                  S1 = GBUF%SIG(II(1) + I) + P
                  S2 = GBUF%SIG(II(2) + I) + P
                  S3 = GBUF%SIG(II(3) + I) + P
                  VONM2 = THREE*(GBUF%SIG(II(4) + I)**2
     .                  + HALF*(S1**2+S2**2+S3**2))
                  VALUE = SQRT(VONM2)
               ENDIF                                                    
               FUNC(EL2FA(NN3+N)) = VALUE                               
           ENDDO                                                        
C-----                                                                       
          ELSEIF(IFUNC == 14)THEN                                        
            DO I=LFT,LLT                                                 
               N = I + NFT                                              
               FUNC(EL2FA(NN3+N)) = GBUF%SIG(II(3) + I)                        
            ENDDO                                                        
C-----                                                                       
          ELSEIF(IFUNC == 15)THEN                                        
            DO I=LFT,LLT                                                 
               N = I + NFT                                              
               FUNC(EL2FA(NN3+N)) = GBUF%SIG(II(1) + I)                          
            ENDDO                                                        
C-----                                                                       
          ELSEIF(IFUNC == 16)THEN                                        
            DO I=LFT,LLT                                                 
               N = I + NFT                                              
               FUNC(EL2FA(NN3+N)) = GBUF%SIG(II(2) + I)                            
            ENDDO                                                        
C-----                                                                       
          ELSEIF(IFUNC == 17.OR.IFUNC == 18)THEN                         
            DO I=LFT,LLT                                                 
               N = I + NFT                                              
               FUNC(EL2FA(NN3+N)) = GBUF%SIG(II(4) + I)                               
            ENDDO                                                        
C-----                                                                       
          ELSE                                                            
            DO I=LFT,LLT                                              
               N = I + NFT                                              
               FUNC(EL2FA(NN3+N)) = ZERO                               
            ENDDO                                                      
          ENDIF                                                          
C-----------------------------------------------                        
        ELSEIF (ITY == 3 .OR. ITY == 7)THEN                                
C         COQUES 3 N 4 N                                                  
C-----------------------------------------------                        
          GBUF => ELBUF_TAB(NG)%GBUF
          DO I=LFT,LLT                                                  
            EVAR(I) = ZERO                                               
          ENDDO                                                         
c-----
          IF (MLW == 0) THEN                                        
            CONTINUE
          ELSEIF (IFUNC == 3)THEN                                        
            DO I=LFT,LLT                                                
              EVAR(I) = GBUF%EINT(I) + GBUF%EINT(I+LLT)
            ENDDO                                                       
c-----
          ELSEIF(IFUNC == 7)THEN                                        
            DO I=LFT,LLT                                                
              S1 = GBUF%FOR(II(1)+I)  
              S2 = GBUF%FOR(II(2)+I)  
              S12= GBUF%FOR(II(3)+I)  
              VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12  
              EVAR(I) = SQRT(VONM2)                         
            ENDDO                                                       
c-----
          ELSEIF(IFUNC>=14 .and. IFUNC<=15)THEN                       
            IUS = IFUNC-13
            DO I=LFT,LLT                                                
              EVAR(I) = GBUF%FOR(II(IUS)+I)
            ENDDO                                                       
c-----
          ELSEIF(IFUNC>=17 .and. IFUNC<=19)THEN                       
            IUS = IFUNC-14
            DO I=LFT,LLT                                                
              EVAR(I) = GBUF%FOR(II(IUS)+I)
            ENDDO                                                       
          ENDIF                                                         
C-------------------                                                    
          IF(ITY == 3)THEN                                             
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN4+N)) = EVAR(I)                               
            ENDDO                                                       
          ELSE                                                         
            DO I=LFT,LLT                                                
             N = I + NFT                                                
             FUNC(EL2FA(NN5+N)) = EVAR(I)                               
            ENDDO                                                       
          ENDIF                                                        
C                                                                       
        ELSE    
          CONTINUE                                                        
        ENDIF                                                           
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES OFFSET                                    
C-----------------------------------------------                        
       END DO                                                           
 900  CONTINUE                                                          
C-----------------------------------------------                        
      DO N=1,NBF                                                        
         R4 = FUNC(N)                                                   
         CALL WRITE_R_C(R4,1)                                           
      ENDDO                                                             
C                                                                       
      RETURN                                                            
      END                                                               
