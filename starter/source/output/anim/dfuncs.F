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
Chd|  DFUNCS                        source/output/anim/dfuncs.F   
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        INITBUF                       source/output/anim/initbuf.F  
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DFUNCS(ELBUF_TAB,FUNC    ,IFUNC   ,IPARG   ,    
     2                IXS     ,PM      ,EL2FA   ,NBF     ,ISPH3D  )                                                                                                                 
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
#include      "scr01_c.inc"                                             
#include      "scr17_c.inc"
#include      "param_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL ou REAL*8                                                    
      my_real                                                           
     .   FUNC(*), PM(NPROPM,*)                                                    
      INTEGER IPARG(NPARG,*),EL2FA(*),                                  
     .        IXS(NIXS,*),IFUNC,NBF,ISPH3D                         
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      REAL  WAL(NBF)                                                    
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
C     REAL ou REAL*8                                                    
      my_real                                                           
     .   EVAR(MVSIZ),                                                   
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE                    
      INTEGER I, NG, NEL,N, J, MLW,NN, JTURB,MT, IALEL,                    
     .        NN1,NN2,NN3,OFFSET,II(6),INOD, ISOLNOD,                                
     .        JHBE, JIVF, NVAUX, JCLOSE, JPLASOL, IREP, IGTYP,          
     .        ICSEN, ISORTHG, IFAILURE, IINT                                                      
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      REAL R4                                                           
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C La routine ne fonctionne que pour les IFUNC 3,6,7,14-19 (stress)      
C-----------------------------------------------                        
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = NN2 + NUMELS                                                
C                                                                       
      DO 900 NG=1,NGROUP                                                
          CALL INITBUF (IPARG    ,NG      ,                             
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,           
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,           
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,           
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,           
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,           
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE)                    
        DO OFFSET = 0,NEL-1,NVSIZ                                       
          NFT   =IPARG(3,NG) + OFFSET                                   
          ISOLNOD = IPARG(28,NG)                                        
          LFT=1                                                         
          LLT=MIN(NVSIZ,NEL-OFFSET)                                     
!
          DO I=1,6
            II(I) = (I-1)*LLT
          ENDDO
!
C-----------------------------------------------                        
C       SOLID 8N                                                        
C-----------------------------------------------                        
          IF (ITY == 1) THEN                                              
C-----------
            GBUF => ELBUF_TAB(NG)%GBUF
c
            IF (MLW == 0 .OR. MLW == 13 . OR. IGTYP == 0) THEN
              DO I=LFT,LLT
                EVAR(I) = ZERO
              ENDDO
            ELSE
              JTURB=IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))                
C                                                                       
              IF (IFUNC == 1) THEN                                           
                DO I=LFT,LLT     
                  IF (MLW == 10 .OR. MLW == 21) THEN
                    EVAR(I) = ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%EPSQ(I)
                  ELSEIF (GBUF%G_PLA > 0) THEN  
                    EVAR(I) = GBUF%PLA(I)
                  ENDIF
                ENDDO                         
              ELSEIF(IFUNC == 2)THEN                                       
                DO I=LFT,LLT                                            
                  EVAR(I) = GBUF%RHO(I)                        
                ENDDO                                                   
              ELSEIF(IFUNC == 3)THEN                                       
                DO I=LFT,LLT                                            
                  N = I + NFT                                           
                  IALEL=IPARG(7,NG)+IPARG(11,NG)                        
                  IF (IALEL == 0) THEN                                    
                    MT=IXS(1,N)                                         
                    EVAR(I) = GBUF%EINT(I)/MAX(EM30,PM(1,MT))          
                  ELSE                                                  
                    EVAR(I) = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                  ENDIF                                                 
                ENDDO                                                   
              ELSEIF (IFUNC == 4) THEN                                       
                DO I=LFT,LLT                                            
                  IF (IPARG(1,NG) .EQ. 2 .AND. JTHE .EQ. 0) THEN       
                    EVAR(I) = PM(54,IXS(1,NFT+I)) + GBUF%EINT(I)*     
     .                        GBUF%VOL(I) * PM(53,IXS(1,NFT+I))      
                  ELSEIF (GBUF%G_TEMP > 0) THEN                                                  
                    EVAR(I) = GBUF%TEMP(I)
                  ENDIF                                                 
                ENDDO                                                   
              ELSEIF(IFUNC == 6 .OR. IFUNC == 7)THEN
                DO I=LFT,LLT  
                    N = I + NFT                                 
                    P = - (GBUF%SIG(II(1) + I)                
     .                   + GBUF%SIG(II(2) + I)                
     .                   + GBUF%SIG(II(3) + I)) * THIRD      
                    VALUE = P                                   
                    IF (IFUNC == 7) THEN                        
                      S1=GBUF%SIG(II(1) + I)+P                
                      S2=GBUF%SIG(II(2) + I)+P                
                      S3=GBUF%SIG(II(3) + I)+P                
                      VONM2= TROIS*(GBUF%SIG(II(4) + I)**2 +  
     .                              GBUF%SIG(II(5) + I)**2 +  
     .                              GBUF%SIG(II(6) + I)**2 +  
     .                       UNDEMI*(S1*S1+S2*S2+S3*S3) )       
                      VONM= SQRT(VONM2)                         
                      VALUE = VONM                              
                    ENDIF                                       
                    EVAR(I) = VALUE                             
                ENDDO                                           
c-----------
              ELSEIF(IFUNC >= 14 .AND. IFUNC <= 19)THEN
                 DO I=LFT,LLT
                   EVAR(I) = GBUF%SIG(II(IFUNC-13) + I)
                 ENDDO
              ENDIF                                                        
c-----------
              IF (ISOLNOD == 16) THEN                                        
                DO I=LFT,LLT                                               
                  N = NN2 + I + NFT                                        
                  IF(EL2FA(N).NE.0)THEN                                    
                     FUNC(EL2FA(N))   = EVAR(I)                             
                     FUNC(EL2FA(N)+1) = EVAR(I)                             
                     FUNC(EL2FA(N)+2) = EVAR(I)                             
                     FUNC(EL2FA(N)+3) = EVAR(I)                             
                  ENDIF                                                    
                ENDDO                                                      
              ELSE                                                         
                DO I=LFT,LLT                                               
                  N = NN2 + I + NFT                                         
                  IF(EL2FA(N).NE.0)THEN                                     
                    FUNC(EL2FA(N)) = EVAR(I)                                
                  ENDIF                                                     
                ENDDO                                                      
              ENDIF                                                        
            ENDIF                                                        
C                                                                       
C-----------------------------------------------                        
          ELSEIF (ISPH3D == 1.AND.ITY == 51) THEN                           
C           TETRAS SPH.                                                  
C-----------------------------------------------                        
            GBUF => ELBUF_TAB(NG)%GBUF
            IF (IFUNC >= 14 .AND. IFUNC <= 19) THEN                         
              DO I=LFT,LLT                                               
                N = I + NFT                                              
                IF (EL2FA(NN3+N).NE.0) THEN                                
                  FUNC(EL2FA(NN3+N)) = GBUF%SIG(II(IFUNC-13) + I)
                ENDIF                                                    
              ENDDO                                                      
            ELSE                                                         
             DO I=LFT,LLT                                                
              N = I + NFT                                                
              IF(EL2FA(NN3+N).NE.0)THEN                                  
                  FUNC(EL2FA(NN3+N)) = ZERO                              
              ENDIF                                                      
             ENDDO                                                       
            ENDIF                                                        
C                                                                       
          ENDIF                                                           
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES OFFSET                                    
C-----------------------------------------------                        
       ENDDO                                                            
 900  CONTINUE                                                          
C-----------------------------------------------                        
      DO N=1,NBF                                                        
         R4 = FUNC(N)                                                   
         CALL WRITE_R_C(R4,1)                                           
      ENDDO                                                             
C-----------                                                                       
      RETURN                                                            
      END                                                               
