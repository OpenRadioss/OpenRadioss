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
Chd|  ANIOFFS                       source/output/anim/anioffs.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFFS(ELBUF_TAB ,IPARG   ,IOFF             ,EL2FA   ,    
     .                   NBF     ,NBPART  ,ISPH3D  )             
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
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),                      
     .        NBPART,ISPH3D                                                    
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL                                                    
      INTEGER I, NG, NEL, NFT, ITY, LFT, NPT,                      
     .        N, LLT, MLW,ISOLNOD,NN1,NN2,NN3 
      INTEGER IOFFBUF(NBF),RBUF                                         
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = NN2 + NUMELS                                                
C-----------------------------------------------                        
C                                                                       
      DO 490 NG=1,NGROUP                                                
        MLW   =IPARG(1,NG)                                              
        NEL   =IPARG(2,NG)                                              
        ITY   =IPARG(5,NG)                                              
        NFT   =IPARG(3,NG)                                              
        ISOLNOD = IPARG(28,NG)                                          
        LFT=1                                                           
        LLT=NEL                                                         
C-----------------------------------------------                        
C       SOLIDES 16N                                                     
C-----------------------------------------------                        
        IF (ITY==1.AND.ISOLNOD==16) THEN  
          GBUF =>  ELBUF_TAB(NG)%GBUF                           
          IF (MLW == 0 .OR. MLW == 13)THEN                                              
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN2+N)) = 0                                    
              IOFF(EL2FA(NN2+N)+1) = 0                                  
              IOFF(EL2FA(NN2+N)+2) = 0                                  
              IOFF(EL2FA(NN2+N)+3) = 0                                  
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN2+N))   = NINT(MIN(GBUF%OFF(I),ONE))
              IOFF(EL2FA(NN2+N)+1) = NINT(MIN(GBUF%OFF(I),ONE))
              IOFF(EL2FA(NN2+N)+2) = NINT(MIN(GBUF%OFF(I),ONE))
              IOFF(EL2FA(NN2+N)+3) = NINT(MIN(GBUF%OFF(I),ONE))
            ENDDO                                                       
          ENDIF                                                         
C-----------------------------------------------                        
C       AUTRES SOLIDES                                                  
C-----------------------------------------------                        
        ELSEIF(ITY==1)THEN                                            
          GBUF =>  ELBUF_TAB(NG)%GBUF                           
          IF(MLW==0)THEN                                              
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN2+N)) = 0                                    
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN2+N)) = NINT(MIN(GBUF%OFF(I),ONE))        
            ENDDO                                                       
          ENDIF                                                         
        ELSEIF(ISPH3D==1.AND.ITY==51)THEN                           
          GBUF =>  ELBUF_TAB(NG)%GBUF                           
C-----------------------------------------------                        
C         TETRAS SPH.                                                  
C-----------------------------------------------                        
          IF(MLW==0)THEN                                              
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN3+N)) = 0                                    
            ENDDO                                                       
          ELSE                                                          
            DO I=LFT,LLT                                                
              N = NFT + I                                               
              IOFF(EL2FA(NN3+N)) = NINT(MIN(GBUF%OFF(I),ONE))      
            ENDDO                                                       
          ENDIF                                                         
        ELSE                                                            
        ENDIF                                                           
C-----------------------------------------------                        
 490  CONTINUE                                                          
C-----------------------------------------------                        
      CALL WRITE_C_C(IOFF,NBF)                                          
C-----------                                                                       
      RETURN                                                            
      END                                                               
