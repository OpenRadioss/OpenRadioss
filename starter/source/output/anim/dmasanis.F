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
Chd|  DMASANIS                      source/output/anim/dmasanis.F 
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        INITBUF                       source/output/anim/initbuf.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DMASANIS(ELBUF_TAB,IPARG   ,       
     2              IXS     ,MAS     ,PM      ,EL2FA   ,NBF     ,       
     3              IPART   ,IPARTSP ,ISPH3D  )                         
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
     .   MAS(*) ,PM(NPROPM,*)                                              
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),EL2FA(*),NBF,IPART(LIPART1,*),     
     .        IPARTSP(*),ISPH3D                                         
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      my_real                                                           
     .   EVAR(MVSIZ),                                                   
     .   OFF,VALUE
      INTEGER I, NG, NEL, ISS, ISC,N, J, MLW,                                          
     .        NN, K1, K2,JTURB,MT, IALEL,IPID,                     
     .        N1,N2,N3,N4,NN1,NN2,NN3,       
     .        OFFSET,NFT_FA,N_FA,                       
     .        INOD, ISOLNOD, IPRT,                                      
     .        JHBE, JIVF, NVAUX, JCLOSE, JPLASOL, IREP, IGTYP,          
     .        ICSEN, ISORTHG, IFAILURE, IINT                            
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      REAL R4                                                           
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1                                                           
      NN2 = 1                                                           
      NN3 = NN2 + NUMELS                                                
C-----------------------------------------------                        
C       face ext                                                        
C-----------------------------------------------                        
      DO 490 NG=1,NGROUP                                                
        CALL INITBUF (IPARG    ,NG      ,                               
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,           
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,           
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,           
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,           
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,           
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE)                    
        ISOLNOD = IPARG(28,NG)                                          
        DO OFFSET = 0,NEL-1,NVSIZ                                       
          NFT   =IPARG(3,NG) + OFFSET                                   
          IAD   =IPARG(4,NG)                                            
          LFT=1                                                         
          LLT=MIN(NVSIZ,NEL-OFFSET)                                     
          NFT_FA = NFT                                                  
C-----------------------------------------------                        
C       SOLIDE                                                          
C-----------------------------------------------                        
          IF (ITY.EQ.1 .and. MLW > 0) THEN                                              
           IALEL=IPARG(7,NG)+IPARG(11,NG)                               
           GBUF => ELBUF_TAB(NG)%GBUF
           DO 130 I=LFT,LLT                                             
             N = I + NFT                                                
             N_FA = I + NFT_FA                                          
             IF(EL2FA(NN2+N_FA).NE.0)THEN                               
               IF(IALEL.EQ.0)THEN                                       
                 MT=IXS(1,N)                                            
                 VALUE=PM(89,MT)*GBUF%VOL(I)                          
               ELSE                                                     
                 OFF = MIN(GBUF%OFF(I),ONE)                             
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF                    
               ENDIF                                                    
               IF(ISOLNOD.EQ.16)THEN                                    
                 VALUE = FOURTH*VALUE                                    
                 MAS(EL2FA(NN2+N_FA)) = VALUE                           
                 MAS(EL2FA(NN2+N_FA)+1) = VALUE                         
                 MAS(EL2FA(NN2+N_FA)+2) = VALUE                         
                 MAS(EL2FA(NN2+N_FA)+3) = VALUE                         
               ELSE                                                     
                 MAS(EL2FA(NN2+N_FA)) = VALUE                           
               ENDIF                                                    
             ENDIF                                                      
 130       CONTINUE                                                     
C                                                                       
        ELSEIF(ISPH3D.EQ.1.AND.ITY.EQ.51.and.MLW > 0)THEN                           
C-----------------------------------------------                        
C        TETRAS SPH.                                                    
C-----------------------------------------------                        
           GBUF => ELBUF_TAB(NG)%GBUF
           IALEL=IPARG(7,NG)+IPARG(11,NG)                               
           DO 140 I=LFT,LLT                                             
             N = I + NFT                                                
             N_FA = I + NFT_FA                                          
             IF(EL2FA(NN3+N_FA).NE.0)THEN                               
               IF(IALEL.EQ.0)THEN                                       
                 IPRT=IPARTSP(N)                                        
                 MT  =IPART(1,IPRT)                                     
                 VALUE=PM(89,MT)*GBUF%VOL(I)                       
               ELSE                                                     
                 OFF = MIN(GBUF%OFF(I),ONE)                             
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF                    
               ENDIF                                                    
               MAS(EL2FA(NN3+N_FA)) = VALUE                             
             ENDIF                                                      
 140       CONTINUE                                                     
C                                                                       
        ELSE                                                            
        ENDIF                                                           
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES OFFSET                                    
C-----------------------------------------------                        
       END DO                                                           
 490  CONTINUE                                                          
C-----------------------------------------------                        
C                                                                       
      RETURN                                                            
      END                                                               
