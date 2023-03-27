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
Chd|  S10COORK                      source/elements/solid/solide10/s10coork.F
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10COORK(X,IXS,IXS10,
     .   XX, YY, ZZ,  
     .   OFFG,OFF,SAV,
     .   NC,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .   K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .   K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .   K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .   K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .   K89,K80,K99,K90,K00,NEL)
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
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .   OFFG(*), OFF(*)
      DOUBLE PRECISION 
     .   SAV(NEL,30)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K19(9,*)    ,K10(9,*)   ,
     .   K22(9,*)   ,K23(9,*)   ,K24(9,*)    ,K25(9,*)    ,K26(9,*)   ,
     .   K27(9,*)   ,K28(9,*)   ,K29(9,*)    ,K20(9,*)    ,K33(9,*)   ,
     .   K34(9,*)   ,K35(9,*)   ,K36(9,*)    ,K37(9,*)    ,K38(9,*)   ,
     .   K39(9,*)   ,K30(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,
     .   K47(9,*)   ,K48(9,*)   ,K49(9,*)   ,K40(9,*)    ,K55(9,*)   ,
     .   K56(9,*)   ,K57(9,*)   ,K58(9,*)   ,K59(9,*)    ,K50(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)   ,K69(9,*)    ,K60(9,*)   ,
     .   K77(9,*)   ,K78(9,*)   ,K79(9,*)   ,K70(9,*)    ,K88(9,*)   ,
     .   K89(9,*)   ,K80(9,*)   ,K99(9,*)   ,K90(9,*)   ,K00(9,*)   
      INTEGER NC(MVSIZ,10),MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*), IXS10(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN,J,MXT_1
C     REAL
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
      IUN=1
C
      MXT_1 = IXS(1,1)
      DO I=1,NEL
        NGEO(I)=IXS(10,I)
        NGL(I)=IXS(11,I)
        MXT(I)=MXT_1
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
        NC(I,5) =IXS10(1,I)
        NC(I,6) =IXS10(2,I)
        NC(I,7) =IXS10(3,I)
        NC(I,8) =IXS10(4,I)
        NC(I,9) =IXS10(5,I)
        NC(I,10)=IXS10(6,I)
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,10
       DO I=1,NEL
        NN = MAX(IUN,NC(I,N))
        XX(I,N)=X(1,NN)
        YY(I,N)=X(2,NN)
        ZZ(I,N)=X(3,NN)
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
       ENDDO
      ENDDO
C-----------
      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
      DO N=1,10
       DO I=1,NEL
       IF(ABS(OFFG(I))>ONE)THEN
        XX(I,N)=SAV(I,N)
        YY(I,N)=SAV(I,N+10)
        ZZ(I,N)=SAV(I,N+20)
        OFF(I) = ABS(OFFG(I))-ONE
       ELSE
        OFF(I) = OFFG(I)
       ENDIF
       ENDDO
      ENDDO
C
      ENDIF
C
      DO N=5,10
        N1=IPERM1(N)
        N2=IPERM2(N)
        DO I=1,NEL
          IF(NC(I,N)==0)THEN
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
          ENDIF
        ENDDO
      ENDDO
C-----------
      DO J=1,9      
      DO I=1,NEL
        K11(J,I)=ZERO
        K12(J,I)=ZERO
        K13(J,I)=ZERO
        K14(J,I)=ZERO
        K15(J,I)=ZERO
        K16(J,I)=ZERO
        K17(J,I)=ZERO
        K18(J,I)=ZERO
        K19(J,I)=ZERO
        K10(J,I)=ZERO
        K22(J,I)=ZERO
        K23(J,I)=ZERO
        K24(J,I)=ZERO
        K25(J,I)=ZERO
        K26(J,I)=ZERO
        K27(J,I)=ZERO
        K28(J,I)=ZERO
        K29(J,I)=ZERO
        K20(J,I)=ZERO
        K33(J,I)=ZERO
        K34(J,I)=ZERO
        K35(J,I)=ZERO
        K36(J,I)=ZERO
        K37(J,I)=ZERO
        K38(J,I)=ZERO
        K39(J,I)=ZERO
        K30(J,I)=ZERO
        K44(J,I)=ZERO
        K45(J,I)=ZERO
        K46(J,I)=ZERO
        K47(J,I)=ZERO
        K48(J,I)=ZERO
        K49(J,I)=ZERO
        K40(J,I)=ZERO
        K55(J,I)=ZERO
        K56(J,I)=ZERO
        K57(J,I)=ZERO
        K58(J,I)=ZERO
        K59(J,I)=ZERO
        K50(J,I)=ZERO
        K66(J,I)=ZERO
        K67(J,I)=ZERO
        K68(J,I)=ZERO
        K69(J,I)=ZERO
        K60(J,I)=ZERO
        K77(J,I)=ZERO
        K78(J,I)=ZERO
        K79(J,I)=ZERO
        K70(J,I)=ZERO
        K88(J,I)=ZERO
        K89(J,I)=ZERO
        K80(J,I)=ZERO
        K99(J,I)=ZERO
        K90(J,I)=ZERO
        K00(J,I)=ZERO
      ENDDO
      ENDDO
C-----------
      RETURN
      END
