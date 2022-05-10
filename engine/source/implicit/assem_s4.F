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
Chd|  ASSEM_S4                      source/implicit/assem_s4.F    
Chd|-- called by -----------
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_S4(
     1                   IXS   ,NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2                   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,     
     3                   K13   ,K14   ,K22   ,K23   ,K24   ,
     4                   K33   ,K34   ,K44   ,OFF   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),NEL   ,IDDL(*)  ,NDOF(*)  ,
     .          IADK(*)  ,JDIK(*)        
C     REAL ou REAL*8
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,
     .   K11(9,*) ,K12(9,*)  ,K13(9,*)  ,K14(9,*)  ,K22(9,*)  ,
     .   K23(9,*) ,K24(9,*)  ,K33(9,*)  ,K34(9,*)  ,K44(9,*), 
     .   OFF(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,NN(MVSIZ,4)
C----6---------------------------------------------------------------7---------8
        DO EP = 1,NEL
         NN(EP,1)=IXS(2,EP)
         NN(EP,2)=IXS(4,EP)
         NN(EP,3)=IXS(7,EP)
         NN(EP,4)=IXS(6,EP)
        ENDDO 
C-------block diagonal k11,k22...k44-----
       CALL ASSEM_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,K11,3,OFF) 
       CALL ASSEM_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,K22,3,OFF) 
       CALL ASSEM_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,K33,3,OFF) 
       CALL ASSEM_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT ,K44,3,OFF) 
C-------non diag kij-----
       CALL ASSEM_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K12,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K13,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K14,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K23,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K24,3,OFF) 
       CALL ASSEM_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K34,3,OFF) 
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
