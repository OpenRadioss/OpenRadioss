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
Chd|  DMASANI0                      source/output/anim/dmasani0.F 
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DMASANI0(ELBUF_TAB   ,IPARG   ,       
     2              MAS     ,PM      ,EL2FA   ,IPART   ,IPARTSP )       
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
#include      "com04_c.inc"                                             
#include      "scr01_c.inc"                                             
#include      "scr17_c.inc"                                             
#include      "param_c.inc"                                             
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
C     REAL                                                    
      my_real                                                           
     .   MAS(*) ,PM(NPROPM,*)                        
      INTEGER IPARG(NPARG,*),EL2FA(*),IPART(LIPART1,*),                      
     .        IPARTSP(*)                                                
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------                        
C   L o c a l   V a r i a b l e s                                       
C-----------------------------------------------                        
      my_real                                                           
     .   OFF, VALUE                                                     
      INTEGER I, NG, NEL, N, MT,IALEL, IPRT,NN1                           
      REAL R4                                                           
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1                                                           
C-----------------------------------------------                        
C       face ext                                                        
C-----------------------------------------------                        
      DO 490 NG=1,NGROUP                                                
        NEL = IPARG(2,NG)                                            
        NFT = IPARG(3,NG)
        ITY = IPARG(5,NG)
        LFT=1                                                           
        LLT=NEL                                                         
C-----------------------------------------------                        
        IF(ITY==51)THEN                                               
C          PARTICULES SPH.                                              
C-----------------------------------------------                        
           GBUF => ELBUF_TAB(NG)%GBUF                                                      
           IALEL=IPARG(7,NG)+IPARG(11,NG)                               
           DO I=LFT,LLT                                                 
             N = I + NFT                                                
             IF(EL2FA(NN1+N)/=0)THEN                                  
               IF(IALEL==0)THEN                                       
                 IPRT=IPARTSP(N)                                        
                 MT  =IPART(1,IPRT)                                     
                 VALUE=PM(89,MT)*GBUF%VOL(I)                        
               ELSE                                                     
                 OFF = MIN(GBUF%OFF(I),ONE)                             
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF
              ENDIF                                                    
               MAS(EL2FA(NN1+N)) = VALUE                                
             ENDIF                                                      
           ENDDO                                                        
        ENDIF                                                           
C-----------------------------------------------                        
C       FIN DE BOUCLE SUR LES GROUPES                                   
C-----------------------------------------------                        
 490  CONTINUE                                                          
C-----------------------------------------------                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
