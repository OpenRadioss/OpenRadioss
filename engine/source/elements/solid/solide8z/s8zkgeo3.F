Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S8ZKGEO3                      source/elements/solid/solide8z/s8zkgeo3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKEH3                       source/elements/solid/solide8z/s8zkeh3.F
Chd|====================================================================
      SUBROUTINE S8ZKGEO3(
     1   SIG,     VOL,     PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     K11,     K12,
     5   K13,     K14,     K15,     K16,
     6   K17,     K18,     K22,     K23,
     7   K24,     K25,     K26,     K27,
     8   K28,     K33,     K34,     K35,
     9   K36,     K37,     K38,     K44,
     A   K45,     K46,     K47,     K48,
     B   K55,     K56,     K57,     K58,
     C   K66,     K67,     K68,     K77,
     D   K78,     K88,     NEL)
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*) ,K15(3,3,*)   ,
     .   K16(3,3,*),K17(3,3,*),K18(3,3,*),K22(3,3,*) ,K23(3,3,*)   ,
     .   K24(3,3,*),K25(3,3,*),K26(3,3,*),K27(3,3,*) ,K28(3,3,*)   ,
     .   K33(3,3,*),K34(3,3,*),K35(3,3,*),K36(3,3,*) ,K37(3,3,*)   ,
     .   K38(3,3,*),K44(3,3,*),K45(3,3,*),K46(3,3,*) ,K47(3,3,*)   ,
     .   K48(3,3,*),K55(3,3,*),K56(3,3,*),K57(3,3,*) ,K58(3,3,*)   ,
     .   K66(3,3,*),K67(3,3,*),K68(3,3,*),K77(3,3,*) ,K78(3,3,*)   ,
     .   K88(3,3,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ),HIJ(MVSIZ)
C-----------------------------------------------
       DO I=1,NEL
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C------------K11,K17,K77
       CALL S8ZKEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K11(J,J,I)=K11(J,J,I)+HIJ(I)
         K77(J,J,I)=K77(J,J,I)+HIJ(I)
         K17(J,J,I)=K17(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K12,K18,K27,K78
       CALL S8ZKEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K12(J,J,I)=K12(J,J,I)+HIJ(I)
         K18(J,J,I)=K18(J,J,I)-HIJ(I)
         K27(J,J,I)=K27(J,J,I)-HIJ(I)
         K78(J,J,I)=K78(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K13,K15,K37,K57
       CALL S8ZKEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K13(J,J,I)=K13(J,J,I)+HIJ(I)
         K15(J,J,I)=K15(J,J,I)-HIJ(I)
         K37(J,J,I)=K37(J,J,I)-HIJ(I)
         K57(J,J,I)=K57(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K14,K16,K47,K67
       CALL S8ZKEH3(1,NEL,
     .   PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K14(J,J,I)=K14(J,J,I)+HIJ(I)
         K16(J,J,I)=K16(J,J,I)-HIJ(I)
         K47(J,J,I)=K47(J,J,I)-HIJ(I)
         K67(J,J,I)=K67(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K22,K28,K88
       CALL S8ZKEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K22(J,J,I)=K22(J,J,I)+HIJ(I)
         K88(J,J,I)=K88(J,J,I)+HIJ(I)
         K28(J,J,I)=K28(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K23,K25,K38,K58
       CALL S8ZKEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K23(J,J,I)=K23(J,J,I)+HIJ(I)
         K25(J,J,I)=K25(J,J,I)-HIJ(I)
         K38(J,J,I)=K38(J,J,I)-HIJ(I)
         K58(J,J,I)=K58(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K24,K26,K48,K68
       CALL S8ZKEH3(1,NEL,
     .   PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K24(J,J,I)=K24(J,J,I)+HIJ(I)
         K26(J,J,I)=K26(J,J,I)-HIJ(I)
         K48(J,J,I)=K48(J,J,I)-HIJ(I)
         K68(J,J,I)=K68(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K33,K35,K55
       CALL S8ZKEH3(1,NEL,
     .   PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K33(J,J,I)=K33(J,J,I)+HIJ(I)
         K55(J,J,I)=K55(J,J,I)+HIJ(I)
         K35(J,J,I)=K35(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K34,K36,K56,K45
       CALL S8ZKEH3(1,NEL,
     .   PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K34(J,J,I)=K34(J,J,I)+HIJ(I)
         K36(J,J,I)=K36(J,J,I)-HIJ(I)
         K56(J,J,I)=K56(J,J,I)+HIJ(I)
         K45(J,J,I)=K45(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K44,K46,K66
       CALL S8ZKEH3(1,NEL,
     .   PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=1,NEL
         K44(J,J,I)=K44(J,J,I)+HIJ(I)
         K66(J,J,I)=K66(J,J,I)+HIJ(I)
         K46(J,J,I)=K46(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
