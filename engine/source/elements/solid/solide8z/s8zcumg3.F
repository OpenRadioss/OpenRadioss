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
Chd|  S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        SYSTRAN3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|====================================================================
      SUBROUTINE S8ZCUMG3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     K11,     K12,     K13,
     4   K14,     K15,     K16,     K17,
     5   K18,     K22,     K23,     K24,
     6   K25,     K26,     K27,     K28,
     7   K33,     K34,     K35,     K36,
     8   K37,     K38,     K44,     K45,
     9   K46,     K47,     K48,     K55,
     A   K56,     K57,     K58,     K66,
     B   K67,     K68,     K77,     K78,
     C   K88,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K22(9,*)    ,K23(9,*)   ,
     .   K24(9,*)   ,K25(9,*)   ,K26(9,*)    ,K27(9,*)    ,K28(9,*)   ,
     .   K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,K37(9,*)   ,
     .   K38(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K47(9,*)   ,
     .   K48(9,*)   ,K55(9,*)   ,K56(9,*)    ,K57(9,*)    ,K58(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)    ,K77(9,*)    ,K78(9,*)   ,
     .   K88(9,*)  
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISYM
C     REAL
      my_real
     .   Q(3,3,MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
       Q(1,1,I)=R11(I)
       Q(2,1,I)=R21(I)
       Q(3,1,I)=R31(I)
       Q(1,2,I)=R12(I)
       Q(2,2,I)=R22(I)
       Q(3,2,I)=R32(I)
       Q(1,3,I)=R13(I)
       Q(2,3,I)=R23(I)
       Q(3,3,I)=R33(I)
      ENDDO
      ISYM=1
      CALL SYSTRAN3(1,NEL,Q,K11,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K22,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K33,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K44,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K55,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K66,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K77,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K88,ISYM) 
      ISYM=0
      CALL SYSTRAN3(1,NEL,Q,K12,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K13,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K14,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K15,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K16,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K17,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K18,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K23,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K24,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K25,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K26,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K27,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K28,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K34,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K35,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K36,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K37,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K38,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K45,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K46,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K47,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K48,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K56,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K57,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K58,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K67,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K68,ISYM) 
      CALL SYSTRAN3(1,NEL,Q,K78,ISYM) 
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
