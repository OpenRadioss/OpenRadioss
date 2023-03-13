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
Chd|  ASSEM_C3                      source/implicit/assem_c3.F    
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|        ASSEMC_KII                    source/implicit/imp_glob_k.F  
Chd|        ASSEMC_KIJ                    source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_C3(
     1         IXTG     ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT     ,IADK      ,JDIK      ,KC11      ,KC12      ,
     3         KC13     ,KC22      , KC23     ,KC33      ,OFF       )
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .       IXTG(NIXTG,*), NEL   ,IDDL(*)   ,NDOF(*)   ,
     .       IADK(*)  ,JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
     .   KC11(6,6,*),KC12(6,6,*),KC13(6,6,*),
     .   KC22(6,6,*),KC23(6,6,*),KC33(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,EP,NN(MVSIZ,3)
C
       DO K = 1,3
        DO EP = 1,NEL
         NN(EP,K)=IXTG(K+1,EP)
        ENDDO 
       ENDDO 
C-------block diagonal k11,k22...k33-----
       CALL ASSEMC_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,KC11,6,OFF,
     1                NDOF) 
       CALL ASSEMC_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,KC22,6,OFF, 
     1                NDOF) 
       CALL ASSEMC_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,KC33,6,OFF, 
     1                NDOF) 
C-------non diag kij-----
       CALL ASSEMC_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC12,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC13,6,OFF,NDOF) 
       CALL ASSEMC_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,KC23,6,OFF,NDOF) 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
