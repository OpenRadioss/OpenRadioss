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
Chd|  S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        SYSTRAN3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|====================================================================
      SUBROUTINE S8ZCUMG3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISYM
C     REAL
      my_real
     .   Q(3,3,MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
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
      CALL SYSTRAN3(LFT,LLT,Q,K11,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K22,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K33,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K44,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K55,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K66,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K77,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K88,ISYM)
      ISYM=0
      CALL SYSTRAN3(LFT,LLT,Q,K12,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K13,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K14,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K15,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K16,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K17,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K18,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K23,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K24,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K25,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K26,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K27,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K28,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K34,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K35,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K36,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K37,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K38,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K45,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K46,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K47,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K48,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K56,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K57,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K58,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K67,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K68,ISYM)
      CALL SYSTRAN3(LFT,LLT,Q,K78,ISYM)
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  SYSTRAN3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|-- called by -----------
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SYSTRAN3(JFT   ,JLT    ,VQ   ,KK,ISYM)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real
     .   VQ(3,3,*), KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          K(I,J,EP)=VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+
     1                  KK(1,2,EP)*VQ(J,2,EP)+KK(1,3,EP)*VQ(J,3,EP))+
     2              VQ(I,2,EP)*(KK(1,2,EP)*VQ(J,1,EP)+
     3                  KK(2,2,EP)*VQ(J,2,EP)+KK(2,3,EP)*VQ(J,3,EP))+
     4              VQ(I,3,EP)*(KK(1,3,EP)*VQ(J,1,EP)+
     5                  KK(2,3,EP)*VQ(J,2,EP)+KK(3,3,EP)*VQ(J,3,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO EP=JFT,JLT
         KK(2,1,EP)= K(1,2,EP)
         KK(3,1,EP)= K(1,3,EP)
         KK(3,2,EP)= K(2,3,EP)
        ENDDO
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J,EP)=VQ(I,1,EP)*(KK(1,1,EP)*VQ(J,1,EP)+
     1                  KK(1,2,EP)*VQ(J,2,EP)+KK(1,3,EP)*VQ(J,3,EP))+
     2              VQ(I,2,EP)*(KK(2,1,EP)*VQ(J,1,EP)+
     3                  KK(2,2,EP)*VQ(J,2,EP)+KK(2,3,EP)*VQ(J,3,EP))+
     4              VQ(I,3,EP)*(KK(3,1,EP)*VQ(J,1,EP)+
     5                  KK(3,2,EP)*VQ(J,2,EP)+KK(3,3,EP)*VQ(J,3,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
