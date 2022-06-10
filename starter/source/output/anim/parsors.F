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
Chd|  PARSORS                       source/output/anim/parsors.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE PARSORS(IADD ,IPARG     ,IXS    ,MATER ,IPARTS ,       
     2                  EL2FA ,                                         
     3                                    INSPH  ,KXSP  ,IPARTSP,       
     4                  IXS10 ,IXS20     ,IXS16  ,NNSPH ,ISPH3D,        
     5                  SHFT16   ,SHFTSPH ,NNSPHG)                      
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
#include      "scr05_c.inc"                                             
#include      "scr14_c.inc"                                             
#include      "rnur_c.inc"                                              
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      INTEGER IADD(*),IPARG(NPARG,*),IXS(NIXS,*),                       
     .        MATER(*),EL2FA(*),IPARTS(*),                              
     .        INSPH,KXSP(NISP,*),IPARTSP(*),                            
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,NNSPH,              
     .        ISPH3D,SHFT16,SHFTSPH ,NNSPHG                             
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   OFF                                                            
      INTEGER II(8),IE,NG, ITY, LFT, LLT, KPT, N, I, J,                 
     .        IPID, NEL, IAD, NPAR, NFT, IPRT,IALEL,MTN,                
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,                 
     .        JJ, K, INOD, ISOLNOD ,NNN,                                
     .        N9,N10,N11,N12,N13,N14,N15,N16                            
      INTEGER NP(NUMELS*8+24*NUMELS16+NUMSPH*8),BUF,BUFP                
C-----------------------------------------------                        
C                                                                       
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = NN2 + NUMELS                                                
      NN4 = NN3 + ISPH3D*(NUMSPH+MAXPJET)                               
      IE = 0                                                            
C-----------------------------------------------                        
C     MID                                                               
C-----------------------------------------------                        
      NPAR = 0                                                          
      JJ = 0                                                            
C                                                                       
      DO 100 IPRT = 1 , NPART                                           
C                                                                       
       IF(MATER(IPRT).NE.2) GOTO 100                                    
       NPAR = NPAR + 1                                                  
       DO 90 NG=1,NGROUP                                                
        NEL   =IPARG(2,NG)                                              
        NFT   =IPARG(3,NG)                                              
        IAD   =IPARG(4,NG)                                              
        ITY   =IPARG(5,NG)                                              
        ISOLNOD = IPARG(28,NG)                                          
        LFT=1                                                           
        LLT=NEL                                                         
C-----------------------------------------------                        
C       SOLID 16N                                                       
C-----------------------------------------------                        
        NNN = INSPH + ISPH3D*NNSPH                                      
        IF(ITY.EQ.1.AND.ISOLNOD.EQ.16)THEN                              
          DO I=LFT,LLT                                                  
           N = I + NFT                                                  
           J = N - NUMELS8 - NUMELS10 - NUMELS20                        
           N9 =IXS16(1,J)                                               
           IF( N9.EQ.0) N9=IXS(2,N)                                     
           N10=IXS16(2,J)                                               
           IF(N10.EQ.0)N10=IXS(3,N)                                     
           N11=IXS16(3,J)                                               
           IF(N11.EQ.0)N11=IXS(4,N)                                     
           N12=IXS16(4,J)                                               
           IF(N12.EQ.0)N12=IXS(5,N)                                     
           N13=IXS16(5,J)                                               
           IF(N13.EQ.0)N13=IXS(6,N)                                     
           N14=IXS16(6,J)                                               
           IF(N14.EQ.0)N14=IXS(7,N)                                     
           N15=IXS16(7,J)                                               
           IF(N15.EQ.0)N15=IXS(8,N)                                     
           N16=IXS16(8,J)                                               
           IF(N16.EQ.0)N16=IXS(9,N)                                     
           IF(IPARTS(N).EQ.IPRT) THEN                                   
             II(1) = IXS(2,N)    -1                                     
             II(2) = N9   -1                                            
             II(3) = NNN + 2*J-1 -1                                     
             II(4) = N12  -1                                            
             II(5) = IXS(6,N)    -1                                     
             II(6) = N13  -1                                            
             II(7) = NNN + 2*J   -1                                     
             II(8) = N16  -1                                            
             CALL WRITE_I_C(II,8)                                       
             II(1) = N9   -1                                            
             II(2) = IXS(3,N)    -1                                     
             II(3) = N10  -1                                            
             II(4) = NNN + 2*J-1 -1                                     
             II(5) = N13  -1                                            
             II(6) = IXS(7,N)    -1                                     
             II(7) = N14  -1                                            
             II(8) = NNN + 2*J   -1                                     
             CALL WRITE_I_C(II,8)                                       
             II(1) = N12  -1                                            
             II(2) = NNN + 2*J-1 -1                                     
             II(3) = N11  -1                                            
             II(4) = IXS(5,N)-1                                         
             II(5) = N16  -1                                            
             II(6) = NNN + 2*J   -1                                     
             II(7) = N15  -1                                            
             II(8) = IXS(9,N)-1                                         
             CALL WRITE_I_C(II,8)                                       
             II(1) = NNN + 2*J-1 -1                                     
             II(2) = N10  -1                                            
             II(3) = IXS(4,N)-1                                         
             II(4) = N11  -1                                            
             II(5) = NNN + 2*J   -1                                     
             II(6) = N14  -1                                            
             II(7) = IXS(8,N)-1                                         
             II(8) = N15  -1                                            
             CALL WRITE_I_C(II,8)                                       
             IE = IE + 1                                                
             EL2FA(NN2+N) = IE                                          
             IE = IE + 3                                                
             JJ = JJ + 32                                               
           END IF                                                       
          ENDDO                                                         
C-----------------------------------------------                        
C       SOLID 8N 4N 10N 20N                                             
C-----------------------------------------------                        
        ELSEIF(ITY.EQ.1)THEN                                            
          DO 10 I=LFT,LLT                                               
           N = I + NFT                                                  
           IF(IPARTS(N).NE.IPRT) GOTO 10                                
             II(1) = IXS(2,N)-1                                         
             II(2) = IXS(3,N)-1                                         
             II(3) = IXS(4,N)-1                                         
             II(4) = IXS(5,N)-1                                         
             II(5) = IXS(6,N)-1                                         
             II(6) = IXS(7,N)-1                                         
             II(7) = IXS(8,N)-1                                         
             II(8) = IXS(9,N)-1                                         
             CALL WRITE_I_C(II,8)                                       
             IE = IE + 1                                                
             EL2FA(NN2+N) = IE                                          
             JJ = JJ + 8                                                
 10      CONTINUE                                                       
        ELSEIF(ISPH3D.EQ.1.AND.ITY.EQ.51)THEN                           
C-----------------------------------------------                        
C         TETRAS SPH.                                                  
C-----------------------------------------------                        
          DO 20 I=LFT,LLT                                               
           N = I + NFT                                                  
           IF(IPARTSP(N).NE.IPRT) GOTO 20                               
             INOD=KXSP(3,N)                                             
             II(1) = INSPH+4*(N-1)+1                                    
             II(2) = INSPH+4*(N-1)+2                                    
             II(3) = INSPH+4*(N-1)                                      
             II(4) = INSPH+4*(N-1)+1                                    
             II(5) = INSPH+4*(N-1)+3                                    
             II(6) = INSPH+4*(N-1)+2                                    
             II(7) = INSPH+4*(N-1)+3                                    
             II(8) = INOD-1                                             
             CALL WRITE_I_C(II,8)                                       
             IE = IE + 1                                                
             EL2FA(NN3+N) = IE                                          
             JJ = JJ + 8                                                
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
