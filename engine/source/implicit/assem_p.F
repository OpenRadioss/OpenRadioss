Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    assem_p      ../engine/source/implicit/assem_p.F
!||--- called by ------------------------------------------------------
!||    pke3         ../engine/source/elements/beam/pke3.F
!||    r13ke3       ../engine/source/elements/spring/r13ke3.F
!||    r4ke3        ../engine/source/elements/spring/r4ke3.F
!||    r8ke3        ../engine/source/elements/spring/r8ke3.F
!||    ruser32ke3   ../engine/source/elements/spring/ruser32ke3.F
!||    tke3         ../engine/source/elements/truss/tke3.F
!||--- calls      -----------------------------------------------------
!||    assem_kii    ../engine/source/implicit/imp_glob_k.F
!||    assem_kij    ../engine/source/implicit/imp_glob_k.F
!||====================================================================
      SUBROUTINE ASSEM_P(NIXPL,ND  ,
     1         IXP      ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT     ,IADK      ,JDIK      ,KC11      ,KC12      ,
     3         KC22     ,OFF       )
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
      INTEGER NIXPL,ND
      INTEGER 
     .       IXP(NIXPL,*), NEL   ,IDDL(*)   ,NDOF(*)   ,
     .       IADK(*)  ,JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,
     .   KC11(ND,ND,*),KC12(ND,ND,*),KC22(ND,ND,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,EP,N1(MVSIZ),N2(MVSIZ)
C
       DO EP = 1,NEL
        N1(EP)=IXP(2,EP)
        N2(EP)=IXP(3,EP)
       ENDDO 
C-------block diagonal k11,k22...k33-----
       CALL ASSEM_KII(N1,NEL,IDDL,IADK,K_DIAG,K_LT ,KC11,ND,OFF) 
       CALL ASSEM_KII(N2,NEL,IDDL,IADK,K_DIAG,K_LT ,KC22,ND,OFF) 
C-------non diag kij-----
       CALL ASSEM_KIJ(N1,N2,NEL,IDDL,IADK,JDIK,K_DIAG,K_LT,KC12,ND,OFF) 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
