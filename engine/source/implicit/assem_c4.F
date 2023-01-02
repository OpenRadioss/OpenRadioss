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
Chd|  ASSEM_C4                      source/implicit/assem_c4.F    
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|-- calls ---------------
Chd|        ASSEMC_KII                    source/implicit/imp_glob_k.F  
Chd|        ASSEMC_KIJ                    source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_C4(
     1         IXC      ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT     ,IADK      ,JDIK      ,KC11      ,KC12      ,
     3         KC13     ,KC14      ,KC22      , KC23     ,KC24      ,    
     5         KC33     ,KC34      ,KC44      ,OFF       )
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
      INTEGER 
     .       IXC(NIXC,*), NEL   ,IDDL(*)   ,
     .       IADK(*)  ,JDIK(*)  ,NDOF(*)
C     REAL
      my_real
     .   OFF(*),K_DIAG(*) ,K_LT(*)   ,
     .   KC11(6,6,*),KC12(6,6,*),KC13(6,6,*),
     .   KC22(6,6,*),KC23(6,6,*),KC24(6,6,*),
     .   KC33(6,6,*),KC34(6,6,*),KC44(6,6,*),KC14(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,EP,NN(MVSIZ,4)
C
       DO K = 1,4
        DO EP = 1,NEL
         NN(EP,K)=IXC(K+1,EP)
        ENDDO 
       ENDDO 
C-------block diagonal k11,k22...k44-----
       CALL ASSEMC_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,KC11,6,OFF, 
     1                NDOF) 
       CALL ASSEMC_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,KC22,6,OFF, 
     1                NDOF) 
       CALL ASSEMC_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,KC33,6,OFF, 
     1                NDOF) 
       CALL ASSEMC_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT ,KC44,6,OFF, 
     1                NDOF) 
C-------non diag kij-----
       CALL ASSEMC_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC12,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC13,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC14,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC23,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC24,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC34,6,OFF,NDOF) 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
