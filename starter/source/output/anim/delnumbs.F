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
Chd|  DELNUMBS                      source/output/anim/delnumbs.F 
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE DELNUMBS(IPARG,IXS  ,EL2FA,NBF  ,INUM ,
     .                    KXSP ,ISPH3D)
C===============================================                        
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
      INTEGER IPARG(NPARG,*),
     .        IXS(NIXS,*),EL2FA(*),INUM(*),
     .        NBF,KXSP(NISP,*)
      INTEGER ISPH3D ,NG, ITY, LFT, LLT, N, I, J, NEL, IAD, NFT,
     .        NN1,NN2,NN3,ISOLNOD
C=======================================================================                       
      NN1 = 1
      NN2 = 1
      NN3 = NN2 + NUMELS
C                                                                       
      DO 490 NG=1,NGROUP
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------                        
C       SOLIDES 16N                                                     
C-----------------------------------------------                        
        IF(ITY==1.AND.ISOLNOD==16)THEN
          DO I=LFT,LLT
           N = I + NFT
           IF(EL2FA(NN2+N)/=0)THEN
               INUM(EL2FA(NN2+N)) = IXS(NIXS,N)
               INUM(EL2FA(NN2+N)+1) = IXS(NIXS,N)
               INUM(EL2FA(NN2+N)+2) = IXS(NIXS,N)
               INUM(EL2FA(NN2+N)+3) = IXS(NIXS,N)
           ENDIF
          ENDDO
C-----------------------------------------------                        
C       AUTRES SOLIDES                                                  
C-----------------------------------------------                        
        ELSEIF(ITY==1)THEN
          DO I=LFT,LLT
           N = I + NFT
           IF(EL2FA(NN2+N)/=0)THEN
               INUM(EL2FA(NN2+N)) = IXS(NIXS,N)
           ENDIF
          ENDDO
        ELSEIF(ISPH3D==1.AND.ITY==51)THEN
C-----------------------------------------------                        
C         TETRAS SPH.                                                  
C-----------------------------------------------                        
          DO I=LFT,LLT
           N = I + NFT
           IF(EL2FA(NN3+N)/=0)THEN
               INUM(EL2FA(NN3+N)) = KXSP(NISP,N)
           ENDIF
          ENDDO
        ELSE
        ENDIF
 490  CONTINUE
C-----------------------------------------------                        
      CALL WRITE_I_C(INUM,NBF)
C-----------------------------------------------                        
      RETURN
      END
