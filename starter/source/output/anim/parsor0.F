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
Chd|  PARSOR0                       source/output/anim/parsor0.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE PARSOR0(IADD ,IPARG   ,MATER   ,EL2FA   ,              
     3                KXSP    ,IPARTSP )                                
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"                                          
C-----------------------------------------------                        
C   C o m m o n   B l o c k s                                           
C-----------------------------------------------                        
#include      "com01_c.inc"                                             
#include      "com04_c.inc"                                             
#include      "sphcom.inc"                                              
#include      "param_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      INTEGER IADD(*),IPARG(NPARG,*),                                   
     .        MATER(*),EL2FA(*),                                        
     .        KXSP(NISP,*),IPARTSP(*)                                   
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   OFF                                                            
      INTEGER II, IE, NG, ITY, LFT, LLT, KPT, N, I,                     
     .        NEL, IAD, NPAR, NFT, IPRT,                                
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,                 
     .        JJ, INOD,                                                 
     .        NP(NUMSPH),BUF,BUFP                                       
C-----------------------------------------------                        
C                                                                       
      NN1 = 1                                                           
      NN2 = NN1 + NUMSPH+MAXPJET                                        
      IE = 0                                                            
C-----------------------------------------------                        
C     MID                                                               
C-----------------------------------------------                        
      NPAR = 0                                                          
      JJ = 0                                                            
C                                                                       
      DO 100 IPRT = 1 , NPART                                           
C                                                                       
       IF(MATER(IPRT)/=4) GOTO 100                                    
       NPAR = NPAR + 1                                                  
       DO 90 NG=1,NGROUP                                                
        NEL   =IPARG(2,NG)                                              
        NFT   =IPARG(3,NG)                                              
        IAD   =IPARG(4,NG)                                              
        ITY   =IPARG(5,NG)                                              
        LFT=1                                                           
        LLT=NEL                                                         
        IF(ITY==51)THEN                                               
C-----------------------------------------------                        
C         PARTICULES SPH.                                               
C-----------------------------------------------                        
          DO 20 I=LFT,LLT                                               
           N = I + NFT                                                  
           IF(IPARTSP(N)/=IPRT) GOTO 20                               
             INOD=KXSP(3,N)                                             
             II = INOD-1                                                
             CALL WRITE_I_C(II,1)                                       
             JJ = JJ+1                                                  
             IE = IE + 1                                                
             EL2FA(NN1+N) = IE                                          
 20      CONTINUE                                                       
        ELSE                                                            
        ENDIF                                                           
 90    CONTINUE                                                         
C-----------------------------------------------                        
C       PART ADRESS                                                     
C-----------------------------------------------                        
       IADD(NPAR) = IE                                                  
 100  CONTINUE                                                          
C                                                                       
C-----------------------------------------------                        
      RETURN                                                            
      END                                                               
