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
Chd|  TENSOR0                       source/output/anim/tensor0.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENSOR0(ELBUF_TAB ,IPARG   ,ITENS   ,PM      ,EL2FA   ,    
     2                   NBF     ,TENS    ,IPART   ,IPARTSP )             
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
#include      "com01_c.inc"                                             
#include      "param_c.inc"                                             
#include      "scr17_c.inc"
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   TENS(6,*),PM(NPROPM,*)                      
      INTEGER IPARG(NPARG,*),ITENS, EL2FA(*),                           
     .   NBF,IPART(LIPART1,*),IPARTSP(*)                      
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
      REAL R4(18),WA(6*NBF)                                                      
      INTEGER I,II(6),J,N,  NG, NEL, IPT, MT1, MLW,NN1,IPRT,JJ
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
      DO J=1,18                                                      
        R4(J) = ZERO                                                 
      ENDDO                                                         
C                                                                       
      NN1 = 1                                                           
c      NN2 = NN1 + (NUMSPH+MAXPJET)                                      
C                                                                       
      DO 490 NG=1,NGROUP                                               
              MLW = IPARG(1,NG)
        NEL = IPARG(2,NG)                                            
        NFT = IPARG(3,NG)
        ITY = IPARG(5,NG)
        LFT=1                                                           
        LLT=NEL                                                         
!
        DO I=1,6
          II(I) = (I-1)*NEL
        ENDDO
!
C-----------------------------------------------                        
        IF (ITY == 51) THEN                                               
C         PARTICULES SPH.                                               
C-----------------------------------------------                        
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          IPRT=IPARTSP(1 + NFT)                                         
          MT1 =IPART(1,IPRT)                                            
C-----------------------------------------------                        
          IF(ITENS == 1)THEN                                            
C          STRESS                                                       
C-----------------------------------------------                        
           DO I=LFT,LLT                                                 
             N = I + NFT                                                
             IF(EL2FA(NN1+N)/=0)THEN                                  
               TENS(1,EL2FA(NN1+N)) = GBUF%SIG(II(1) + I)
               TENS(2,EL2FA(NN1+N)) = GBUF%SIG(II(2) + I)
               TENS(3,EL2FA(NN1+N)) = GBUF%SIG(II(3) + I)
               TENS(4,EL2FA(NN1+N)) = GBUF%SIG(II(4) + I)
               TENS(5,EL2FA(NN1+N)) = GBUF%SIG(II(5) + I)
               TENS(6,EL2FA(NN1+N)) = GBUF%SIG(II(6) + I)
             ENDIF                                                      
           ENDDO                                                        
C-----------------------------------------------                        
          ELSEIF (ITENS == 4 .AND. MLW == 24 .AND.                              
     .            NINT(PM(56,MT1)) == 1) THEN              
C          CRACKS                                                       
C-----------------------------------------------                        
           DO I=LFT,LLT                                                 
             N = I + NFT                                                
             IF(EL2FA(NN1+N)/=0)THEN                                  
               TENS(1,EL2FA(NN1+N)) =  LBUF%DGLO(II(1) + I)
               TENS(2,EL2FA(NN1+N)) =  LBUF%DGLO(II(2) + I)
               TENS(3,EL2FA(NN1+N)) =  LBUF%DGLO(II(3) + I)        
               TENS(4,EL2FA(NN1+N)) =  LBUF%DGLO(II(4) + I)
               TENS(5,EL2FA(NN1+N)) =  LBUF%DGLO(II(5) + I)
               TENS(6,EL2FA(NN1+N)) =  LBUF%DGLO(II(6) + I)
             ENDIF                                                      
           ENDDO                                                        
          ELSE                                                          
C-----------------------------------------------                        
           DO I=LFT,LLT                                                 
             N = I + NFT                                                
             IF(EL2FA(NN1+N)/=0)THEN                                  
               TENS(1,EL2FA(NN1+N)) = ZERO                              
               TENS(2,EL2FA(NN1+N)) = ZERO                              
               TENS(3,EL2FA(NN1+N)) = ZERO                              
               TENS(4,EL2FA(NN1+N)) = ZERO                              
               TENS(5,EL2FA(NN1+N)) = ZERO                              
               TENS(6,EL2FA(NN1+N)) = ZERO                              
             ENDIF                                                      
           ENDDO                                                        
          ENDIF                                                         
C-----------------------------------------------                        
        ELSE                                                            
        ENDIF                                                           
 490   CONTINUE                                                         
 500  CONTINUE                                                          
C-----------------------------------------------                        
      DO N=1,NBF                                                        
        R4(1) = TENS(1,N)                                               
        R4(2) = TENS(2,N)                                               
        R4(3) = TENS(3,N)                                               
        R4(4) = TENS(4,N)                                               
        R4(5) = TENS(5,N)                                               
        R4(6) = TENS(6,N)                                               
        CALL WRITE_R_C(R4,6)                                            
      ENDDO                                                             
C-----------------------------------------------                        
      RETURN                                                            
      END                                                               
