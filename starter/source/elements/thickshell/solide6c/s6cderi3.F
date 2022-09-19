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
C
Chd|====================================================================
Chd|  S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|-- called by -----------
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SLEN                          source/elements/solid/solide/slen.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S6CDERI3(NEL ,VOL  ,GEO   ,VZL   ,NGL, DELTAX, DET,
     .     X1, X2, X3, X4, X5, X6,
     .     Y1, Y2, Y3, Y4, Y5, Y6,
     .     Z1, Z2, Z3, Z4, Z5, Z6)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NGL(*)
      my_real
     .   VOL(*), GEO(NPROPG,*),VZL(*), DELTAX(*), DET(*)
      my_real
     .     X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .     Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),
     .     Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NFAC
      my_real
     .   X21(MVSIZ) , X31(MVSIZ) , X41(MVSIZ) , X54(MVSIZ),X64(MVSIZ),
     .   Y21(MVSIZ) , Y31(MVSIZ) , Y41(MVSIZ) , Y54(MVSIZ),Y64(MVSIZ),
     .   Z21(MVSIZ) , Z31(MVSIZ) , Z41(MVSIZ) , Z54(MVSIZ),Z64(MVSIZ),
     .   JAC1(MVSIZ), JAC2(MVSIZ) ,JAC3(MVSIZ),
     .   JAC4(MVSIZ), JAC5(MVSIZ) ,JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ) ,JAC9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ)

      my_real
     .   XIOFF(MVSIZ), AREAM(MVSIZ),
     .   ATEST(MVSIZ), AREA(6,MVSIZ)

C=======================================================================

      DO I=1,NEL
        X21(I)=X2(I)-X1(I)
        X31(I)=X3(I)-X1(I)
        X41(I)=X4(I)-X1(I)
        X54(I)=X5(I)-X4(I)
        X64(I)=X6(I)-X4(I)
C
        Y21(I)=Y2(I)-Y1(I)
        Y31(I)=Y3(I)-Y1(I)
        Y41(I)=Y4(I)-Y1(I)
        Y54(I)=Y5(I)-Y4(I)
        Y64(I)=Y6(I)-Y4(I)
C
        Z21(I)=Z2(I)-Z1(I)
        Z31(I)=Z3(I)-Z1(I)
        Z41(I)=Z4(I)-Z1(I)
        Z54(I)=Z5(I)-Z4(I)
        Z64(I)=Z6(I)-Z4(I)
      ENDDO

      DO I=1,NEL
C-------ri.xi---->ksi--------
        JAC1(I)=X21(I)+X54(I)
        JAC2(I)=Y21(I)+Y54(I)
        JAC3(I)=Z21(I)+Z54(I)
      ENDDO

      DO I=1,NEL
C-------si.xi--->eta--------
        JAC4(I)=X31(I)+X64(I)
        JAC5(I)=Y31(I)+Y64(I)
        JAC6(I)=Z31(I)+Z64(I)
C-------ti.xi----zeta-------
        JAC7(I)=THIRD*(X41(I)+X5(I)-X2(I)+X6(I)-X3(I))
        JAC8(I)=THIRD*(Y41(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
        JAC9(I)=THIRD*(Z41(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
      ENDDO

      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
        DET(I)=ONE_OVER_8*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
        VOL(I)=DET(I)
      ENDDO
C
      DO I=1,NEL
        IF(DET(I)>ZERO) CYCLE
        CALL ANCMSG(MSGID=245,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=NGL(I))
      ENDDO

      DO I=1,NEL
         VZL(I) = FOURTH*(JAC9(I)*(X54(I)*Y64(I)-X21(I)*Y31(I)-X64(I)*Y54(I)+X31(I)*Y21(I))
     .                   -JAC8(I)*(X54(I)*Z64(I)+X31(I)*Z21(I)-X21(I)*Z31(I)-X64(I)*Z54(I))
     .                   +JAC7(I)*(Y54(I)*Z64(I)+Y31(I)*Z21(I)-Y21(I)*Z31(I)-Y64(I)*Z54(I)))
C
      ENDDO
      DO I=1,NEL
        XIOFF(I)=ONE
        AREAM(I)=ZERO
      ENDDO
C
      CALL SLEN(X1,X2,X5,X4,Y1,Y2,Y4,Y5,Z1,Z2,Z5,Z4,1, AREA, AREAM)
      CALL SLEN(X2,X5,X6,X3,Y2,Y5,Y6,Y3,Z2,Z5,Z6,Z3,2, AREA, AREAM)
      CALL SLEN(X1,X4,X6,X3,Y1,Y4,Y6,Y3,Z1,Z4,Z6,Z3,3, AREA, AREAM)
      CALL SLEN(X1,X2,X3,X3,Y1,Y2,Y3,Y3,Z1,Z2,Z3,Z3,4, AREA, AREAM)
      CALL SLEN(X4,X5,X6,X6,Y4,Y5,Y6,Y6,Z4,Z5,Z6,Z6,5, AREA, AREAM)
C
      DO I=1,NEL
        ATEST(I)=EM4*AREAM(I)
        NFAC=0
        IF(AREA(1,I)<ATEST(I)) NFAC=NFAC+1
        IF(AREA(2,I)<ATEST(I)) NFAC=NFAC+1
        IF(AREA(3,I)<ATEST(I)) NFAC=NFAC+1
        IF(AREA(4,I)<ATEST(I)) NFAC=NFAC+1
        IF(AREA(5,I)<ATEST(I)) NFAC=NFAC+1
        IF(NFAC>=2) XIOFF(I)=EP03
      ENDDO
      DO I=1,NEL
         DELTAX(I)=FOUR*DET(I)*XIOFF(I)/SQRT(AREAM(I))
      ENDDO
C---
      RETURN
      END SUBROUTINE S6CDERI3