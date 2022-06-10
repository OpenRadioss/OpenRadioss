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
Chd|  DFUNC0                        source/output/anim/dfunc0.F   
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DFUNC0(ELBUF_TAB ,FUNC    ,IFUNC   ,IPARG   ,PM      ,
     2                  EL2FA   ,NBF   ,SPBUF   ,IPART   ,IPARTSP )                                         
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
#include      "sphcom.inc"
#include      "com01_c.inc"                                             
#include      "com04_c.inc"                                             
#include      "scr01_c.inc"                                             
#include      "scr17_c.inc"
#include      "param_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   FUNC(*), PM(NPROPM,*),SPBUF(NSPBUF,*)                                                
      INTEGER IPARG(NPARG,*),EL2FA(*),IFUNC,NBF,                        
     .        IPART(LIPART1,*),IPARTSP(*)                                    
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB                                  
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      my_real                                                           
     .   EVAR(MVSIZ),                                                   
     .   P, VONM2, VONM, S1, S2, S12, S3, VALUE
      INTEGER I,II(6),NG,NEL,N,MLW,NN,NN1,JTURB,MT,IALEL,IPRT           
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      REAL R4,WA(NBF)                                                   
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C La routine ne fonctionne que pour les IFUNC 3,6,7,14-19 (stress)      
C=======================================================================                                                                       
      NN1 = 1                                                           
c      NN2 = NN1 + (NUMSPH+MAXPJET)                                      
C                                                                       
C-----------------------------------------------                        
      DO 900 NG=1,NGROUP                                                
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
          IF (IFUNC == 0) THEN                                            
            DO I=LFT,LLT                                                
              N = I + NFT                                                
              FUNC(EL2FA(NN1+N)) = SPBUF(1,N)                            
            ENDDO                                                       
          ELSE                                                          
            GBUF => ELBUF_TAB(NG)%GBUF
            JTURB= IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))               
            IALEL= IPARG(7,NG)+IPARG(11,NG)                           
c
            IF (IFUNC == 3) THEN                                           
              DO I=LFT,LLT                                               
                N = I + NFT                                              
                IF(IALEL == 0)THEN                                       
                  IPRT=IPARTSP(N)                                        
                  MT  =IPART(1,IPRT)                                     
                  VALUE = GBUF%EINT(I)/MAX(EM30,PM(89,MT))
                ELSE                                                     
                 VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                ENDIF                                                    
                FUNC(EL2FA(NN1+N)) = VALUE                               
              ENDDO                                                      
c
            ELSEIF (IFUNC == 6 .OR. IFUNC == 7) THEN                         
              DO I=LFT,LLT                                               
!!                II = (I-1)*6
                N = I + NFT                                              
                IF(EL2FA(NN1+N).NE.0)THEN                                
                  P = - (GBUF%SIG(II(1) + I)                
     .                 + GBUF%SIG(II(2) + I)                
     .                 + GBUF%SIG(II(3) + I) ) * THIRD
                  VALUE = P
                  IF(IFUNC == 7) THEN                                   
                    S1=GBUF%SIG(II(1) + I) + P  	      
                    S2=GBUF%SIG(II(2) + I) + P  	      
                    S3=GBUF%SIG(II(3) + I) + P  	      
                    VONM2= THREE*(GBUF%SIG(II(4) + I)**2 +    
     .                            GBUF%SIG(II(5) + I)**2 +    
     .                            GBUF%SIG(II(6) + I)**2 +    
     .                     HALF*(S1*S1+S2*S2+S3*S3) )         
                    VONM= SQRT(VONM2) 
                    VALUE = VONM
                  ENDIF                                                 
                  FUNC(EL2FA(NN1+N)) = VALUE                            
                ENDIF                                                    
              ENDDO                                                      
c
            ELSEIF (IFUNC >= 14 .AND. IFUNC <= 19) THEN                      
              DO I=LFT,LLT                                               
!!                II = (I-1)*6
                N = I + NFT                                               
                IF(EL2FA(NN1+N).NE.0)THEN                                 
                   FUNC(EL2FA(NN1+N)) = GBUF%SIG(II(IFUNC - 13) + I)
               ENDIF                                                     
              ENDDO                                                      
            ELSE                                                         
              DO I=LFT,LLT                                               
                N = I + NFT                                               
                IF(EL2FA(NN1+N).NE.0)THEN                                 
                   FUNC(EL2FA(NN1+N)) = ZERO                             
                ENDIF                                                     
              ENDDO                                                      
            ENDIF                                                        
          ENDIF                                                         
        ENDIF                                                           
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES GROUPES                                   
C-----------------------------------------------                        
 900  CONTINUE                                                          
C-----------------------------------------------                        
      DO N=1,NBF                                                        
         R4 = FUNC(N)                                                   
         CALL WRITE_R_C(R4,1)                                           
      ENDDO                                                             
C-----------                                                                      
      RETURN                                                            
      END                                                               
