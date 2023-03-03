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
Chd|  PARSORF                       source/output/anim/parsorf.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE PARSORF(IADD ,IPARG,IXT  ,IXP  ,IXR  ,                  
     .                  MATER,EL2FA,                                    
     .                  IPARTT,IPARTP,IPARTR,NFACPTX,IXEDGE)                                
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
      INTEGER IADD(*),IPARG(NPARG,*),                                   
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),                      
     .        MATER(*),EL2FA(*),                                        
     .        IPARTT(*),IPARTP(*),IPARTR(*),                            
     .        NFACPTX(3,*),IXEDGE(2,*)                                  
C-----------------------------------------------                        
      INTEGER II(4),IE,NG, ITY, LFT, LLT, KPT, N, I, J,                 
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,                
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,JJ, K,NAX1D                                                     
C-----------------------------------------------                        
      IE = 0                                                            
C                                                                       
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = 1                                                           
      NN4 = NN3                                                         
      NN5 = NN4                                                         
      NN6 = NN5                                                         
      NN7 = NN6 + NUMELT                                                
      NN8 = NN7 + NUMELP                                                
c      NN9 = NN8 + NUMELR                                                
c      NN10= NN9                                                         
C-----------------------------------------------                        
      NPAR = 0                                                          
C-----------------------------------------------                        
C     PART                                                              
C-----------------------------------------------                        
      IF(NUMELP + NUMELT + NUMELR/=0)THEN                             
      JJ = 0                                                            
      DO 500 IPRT=1,NPART                                               
       IF(MATER(IPRT)/=3)GOTO 500                                     
       IF(NUMELX>0)THEN                                              
         IF(NFACPTX(1,IPRT)>0)GOTO 500                               
       ENDIF                                                            
       NPAR = NPAR + 1                                                  
       DO 490 NG=1,NGROUP                                               
        MTN   =IPARG(1,NG)                                              
        NEL   =IPARG(2,NG)                                              
        NFT   =IPARG(3,NG)                                              
        IAD   =IPARG(4,NG)                                              
        ITY   =IPARG(5,NG)                                              
        LFT=1                                                           
        LLT=NEL                                                         
C-----------------------------------------------                        
C       TRUSS                                                           
C-----------------------------------------------                        
        IF(ITY==4)THEN                                                
         DO 140 I=LFT,LLT                                               
          N = I + NFT                                                   
          IF(IPARTT(N)/=IPRT)GOTO 140                                 
          II(1) = IXT(2,N)-1                                            
          II(2) = IXT(3,N)-1                                            
          CALL WRITE_I_C(II,2)                                          
          IE = IE + 1                                                   
          EL2FA(NN6+N) = IE                                             
          JJ = JJ + 2                                                   
 140     CONTINUE                                                       
C-----------------------------------------------                        
C       POUTRES                                                         
C-----------------------------------------------                        
        ELSEIF(ITY==5)THEN                                            
         DO 150 I=LFT,LLT                                               
          N = I + NFT                                                   
          IF(IPARTP(N)/=IPRT)GOTO 150                                 
          II(1) = IXP(2,N)-1                                            
          II(2) = IXP(3,N)-1                                            
          CALL WRITE_I_C(II,2)                                          
          IE = IE + 1                                                   
          EL2FA(NN7+N) = IE                                             
          JJ = JJ + 2                                                   
 150     CONTINUE                                                       
C-----------------------------------------------                        
C       RESSORTS                                                        
C-----------------------------------------------                        
        ELSEIF(ITY==6)THEN                                            
         DO 160 I=LFT,LLT                                               
          N = I + NFT                                                   
          IF(IPARTR(N)/=IPRT)GOTO 160                                 
          II(1) = IXR(2,N)-1                                            
          II(2) = IXR(3,N)-1                                            
          CALL WRITE_I_C(II,2)                                          
          IE = IE + 1                                                   
          EL2FA(NN8+N) = IE                                             
          JJ = JJ + 2                                                   
          IF(MTN==3)THEN                                              
            II(1) = IXR(3,N)-1                                          
            II(2) = IXR(4,N)-1                                          
            CALL WRITE_I_C(II,2)                                        
            IE = IE + 1                                                 
            JJ = JJ + 2                                                 
          ENDIF                                                         
 160     CONTINUE                                                       
        ELSE                                                            
        ENDIF                                                           
 490   CONTINUE                                                         
C-----------------------------------------------                        
C       PART ADRESS                                                     
C-----------------------------------------------                        
       IADD(NPAR) = IE                                                  
 500  CONTINUE                                                          
      ENDIF                                                             
C-----------------------------------------------                        
C     X-ELEMENTS PARTS ARE WRITTEN AFTER ALL (BUT RBODIES) 1D PARTS.   
C-----------------------------------------------                        
      IF (NANIM1D>0) THEN                                            
      NAX1D=0                                                           
      DO 600 IPRT=1,NPART                                               
       IF(MATER(IPRT)/=3)GOTO 600                                     
       IF(NFACPTX(1,IPRT)/=0) THEN                                    
         NPAR = NPAR + 1                                                
         DO J=1,NFACPTX(1,IPRT)                                         
          II(1)=IXEDGE(1,NAX1D+J)-1                                     
          II(2)=IXEDGE(2,NAX1D+J)-1                                     
          CALL WRITE_I_C(II,2)                                          
          JJ = JJ+2                                                     
         ENDDO                                                          
        NAX1D=NAX1D+NFACPTX(1,IPRT)                                     
        IE=IE+NFACPTX(1,IPRT)                                           
        IADD(NPAR)=IE                                                   
       ENDIF                                                            
 600  CONTINUE                                                          
      ENDIF                                                             
C-----------------------------------------------                        
      RETURN                                                            
      END                                                               
