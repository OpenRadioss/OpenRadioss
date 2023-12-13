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
Chd|  ANIOFFC                       source/output/anim/anioffc.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFFC(ELBUF_TAB,IPARG,IOFF,EL2FA,NBF   )                      
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
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB                                  
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL                                                    
      INTEGER I, NG, NEL, NFT, ITY, LFT,LLT, MLW,                      
     .   N1,N2,N3,N4,NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,N_FA                          
      REAL R4                                                           
      my_real
     .   OFF
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = 1                                                           
      NN4 = NN3 + NUMELQ                                                
      NN5 = NN4 + NUMELC                                                
      NN6 = NN5 + NUMELTG                                               
      NN7 = NN6                                                         
      NN8 = NN7                                                         
      NN9 = NN8                                                                                                    
C-----------------------------------------------                        
C                                                                       
      DO NG=1,NGROUP                                                    
        MLW = IPARG(1,NG)                                              
        NEL = IPARG(2,NG)                                            
        NFT = IPARG(3,NG)
        ITY = IPARG(5,NG)
        LFT=1                                                           
        LLT=NEL                                                         
C-----------------------------------------------                        
C       QUAD                                                            
C-----------------------------------------------                        
        IF(ITY==2)THEN                                                
          N_FA = NN3 + NFT                                              
C-----------------------------------------------                        
C       COQUES 4 N                                                      
C-----------------------------------------------                        
        ELSEIF(ITY==3)THEN                                            
          N_FA = NN4 + NFT                                              
C-----------------------------------------------                        
C       COQUES 3 N                                                      
C-----------------------------------------------                        
        ELSEIF(ITY==7)THEN                                            
          N_FA = NN5 + NFT                                              
C-----------------------------------------------                        
C       RNUR                                                            
C-----------------------------------------------                        
        ELSEIF(ITY==50)THEN                                           
          N_FA = NN9 + NFT                                              
        ELSE                                                            
          ITY=0                                                         
        ENDIF                                                           
C-----------------------------------------------                        
        IF(ITY/=0)THEN                                                
          IF(MLW==0)THEN                                               
C-----------------------------------------------                        
C           DUMMY ELEMENTS                                                
C-----------------------------------------------                        
            DO I=LFT,LLT                                                
              IOFF(EL2FA(N_FA+I)) = 0                                   
            ENDDO                                                       
          ELSE                                                           
C-----------------------------------------------                        
C           OFF                                                           
C-----------------------------------------------                        
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT                                                  
              IF (MLW/=0 .AND. MLW/=13) OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(OFF,ONE))
            ENDDO                                                         
          ENDIF                                                          
        ENDIF                                                           
C-----------------------------------------------                        
      ENDDO                                                             
C-----------------------------------------------                        
      CALL WRITE_C_C(IOFF,NBF)                                          
C                                                                       
      RETURN                                                            
      END                                                               
