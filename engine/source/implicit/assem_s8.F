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
Chd|  ASSEM_S8                      source/implicit/assem_s8.F    
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_S8(
     1                   IXS   ,NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2                   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,
     3                   K13   ,K14   ,K15   ,K16   ,K17   ,
     4                   K18   ,K22   ,K23   ,K24   ,K25   ,
     5                   K26   ,K27   ,K28   ,K33   ,K34   ,
     6                   K35   ,K36   ,K37   ,K38   ,K44   ,
     7                   K45   ,K46   ,K47   ,K48   ,K55   ,
     8                   K56   ,K57   ,K58   ,K66   ,K67   ,
     9                   K68   ,K77   ,K78   ,K88   ,OFF   )
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
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .   K15(3,3,*),K16(3,3,*),K17(3,3,*),K18(3,3,*),
     .   K22(3,3,*),K23(3,3,*),K24(3,3,*),K25(3,3,*),
     .   K26(3,3,*),K27(3,3,*),K28(3,3,*),
     .   K33(3,3,*),K34(3,3,*),K35(3,3,*),K36(3,3,*),
     .   K37(3,3,*),K38(3,3,*),K44(3,3,*),K45(3,3,*),
     .   K46(3,3,*),K47(3,3,*),K48(3,3,*),
     .   K55(3,3,*),K56(3,3,*),K57(3,3,*),K58(3,3,*),
     .   K66(3,3,*),K67(3,3,*),K68(3,3,*),
     .   K77(3,3,*),K78(3,3,*),K88(3,3,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,EP,NN(MVSIZ,8)
C----6---------------------------------------------------------------7---------8
       DO K = 1,8
        DO EP = 1,NEL
         NN(EP,K)=IXS(K+1,EP)
        ENDDO
       ENDDO
C-------block diagonal k11,k22...k88-----
       CALL ASSEM_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,K11,3,OFF)
       CALL ASSEM_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,K22,3,OFF)
       CALL ASSEM_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,K33,3,OFF)
       CALL ASSEM_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT ,K44,3,OFF)
       CALL ASSEM_KII(NN(1,5),NEL,IDDL,IADK,K_DIAG,K_LT ,K55,3,OFF)
       CALL ASSEM_KII(NN(1,6),NEL,IDDL,IADK,K_DIAG,K_LT ,K66,3,OFF)
       CALL ASSEM_KII(NN(1,7),NEL,IDDL,IADK,K_DIAG,K_LT ,K77,3,OFF)
       CALL ASSEM_KII(NN(1,8),NEL,IDDL,IADK,K_DIAG,K_LT ,K88,3,OFF)
C-------non diag kij-----
       CALL ASSEM_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K12,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K13,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K14,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K15,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K16,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K17,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K18,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K23,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K24,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K25,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K26,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K27,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K28,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K34,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K35,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K36,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K37,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K38,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K45,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K46,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K47,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K48,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K56,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K57,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K58,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K67,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K68,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                K_DIAG, K_LT,K78,3,OFF)
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
