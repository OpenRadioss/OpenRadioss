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
Chd|  ANIOFFF                       source/output/anim/aniofff.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_C_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFFF(ELBUF_TAB,IPARG,IOFF  ,EL2FA,                      
     .                   NBF  ,IOFFX1)           
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
c-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),                      
     .        NBPART,IOFFX1(*)                                                 
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      INTEGER I, NG, NEL, NFT, ITY, LFT, NPT,                      
     .        N, J, LLT, N1,N2,N3,N4,N_FA,MLW,                                         
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NBF2,RBUF                                   
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = 1                                                           
      NN4 = NN3                                                         
      NN5 = NN4                                                         
      NN6 = NN5                                                         
      NN7 = NN6 + NUMELT                                                
      NN8 = NN7 + NUMELP                                                
C-----------------------------------------------                        
      DO NG=1,NGROUP                                                    
        MLW   =IPARG(1,NG)                                              
        NEL   =IPARG(2,NG)                                              
        ITY   =IPARG(5,NG)                                              
        NFT   =IPARG(3,NG)                                              
        LFT=1                                                           
        LLT=NEL                                                         
C
        GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------------------------------                        
C       TRUSS                                                           
C-----------------------------------------------                        
        IF (ITY ==4) THEN                                                
          N_FA = NN6 + NFT                                            
C-----------------------------------------------                        
C       POUTRES                                                         
C-----------------------------------------------                        
        ELSEIF (ITY == 5) THEN                                            
          N_FA = NN7 + NFT                                            
C-----------------------------------------------                        
C       RESSORTS                                                        
C-----------------------------------------------                        
        ELSEIF (ITY == 6) THEN                                            
          N_FA = NN8 + NFT                                             
        ELSE                                                            
          ITY=0                                                         
        ENDIF                                                           
C-----------------------------------------------                        
        IF (ITY /= 0) THEN                                                
          IF (MLW == 0) THEN                                               
C-----------------------------------------------                        
C         DUMMY ELEMENTS                                                
C-----------------------------------------------                        
            DO I=LFT,LLT                                                  
              IOFF(EL2FA(N_FA+I)) = 0                                     
            ENDDO                                                         
          ELSEIF (ITY == 6 .AND. MLW == 3) THEN                              
C-----------------------------------------------                        
C         PULLEY SPRING                                                 
C-----------------------------------------------                        
            DO I=LFT,LLT                                                  
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),ONE))            
              IOFF(EL2FA(N_FA+I)+1) = NINT(MIN(GBUF%OFF(I),ONE))          
            ENDDO                                                         
C-----------------------------------------------                        
          ELSEIF (ITY == 1 .or. ITY == 2 .or. ITY == 51 .or.
     .            ITY == 3 .or. ITY == 7 .or. ITY == 4  .or.
     .            ITY == 5 .or. ITY == 6) THEN
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),ONE))
            ENDDO
C-----------------------------------------------                        
          ENDIF ! IF (MLW == 0)
        ENDIF ! IF (ITY /= 0)
C-----------------------------------------------                        
      ENDDO ! DO NG=1,NGROUP
C-----------------------------------------------                        
      CALL WRITE_C_C(IOFF,NBF)                                          
C + X-ELEMENTS                                                     
      CALL WRITE_C_C(IOFFX1,NANIM1D)                                    
C                                                                       
      RETURN                                                            
      END                                                               
