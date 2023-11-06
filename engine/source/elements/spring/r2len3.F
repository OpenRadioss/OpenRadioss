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
Chd|  R2LEN3                        source/elements/spring/r2len3.F
Chd|-- called by -----------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R2LEN3(
     1   JFT,     JLT,     OFF,     DT2T,
     2   NELTST,  ITYPTST, STI,     STIR,
     3   MS,      IN,      MSRT,    DMELRT,
     4   G_DT,    DTEL,    NGL,     XCR,
     5   XIN,     XM,      XKM,     XCM,
     6   XKR,     NC1,     NC2,     JSMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: JSMS
        INTEGER JFT,JLT,NELTST ,ITYPTST,NGL(*),NC1(*),NC2(*)
        my_real DT2T,
     .     OFF(*), STI(3,*), STIR(3,*), MS(*), IN(*),
     .     MSRT(*), DMELRT(*),XCR(MVSIZ), XIN(MVSIZ),
     .     XM(MVSIZ),XKM(MVSIZ),XCM(MVSIZ),XKR(MVSIZ)
        my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
        INTEGER,INTENT(IN)    :: G_DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        my_real
     .     DT(MVSIZ),DTC(MVSIZ),
     .     A, MASS2, IN2, B, DTA, DTB, MX
C-----------------------------------------------
        IF(NODADT/=0.OR.IDTMINS==2)THEN
C
          DO I=JFT,JLT
            STI(1,I) = ZERO
            STI(2,I) = ZERO
            STIR(1,I) = ZERO
            STIR(2,I) = ZERO
            IF(OFF(I)>ZERO)THEN
              IF(MS(NC1(I))/=ZERO)THEN
                MASS2 = TWO * MS(NC1(I))
              ELSE
                MASS2 = XM(I)
              ENDIF
              IF(XKM(I)/=ZERO.AND.XCM(I)/=ZERO.AND.
     .          MASS2/=ZERO)THEN
                STI(1,I) = (XCM(I) + SQRT(XCM(I)**2+XKM(I)*MASS2))**2/MASS2
              ELSEIF(XKM(I)/=ZERO)THEN
                STI(1,I) = XKM(I)
              ELSEIF(XCM(I)/=ZERO.AND.MASS2/=ZERO)THEN
                A = FOUR * XCM(I)**2
                STI(1,I) = A / MASS2
              ENDIF
C
              IF(MS(NC2(I))/=ZERO)THEN
                MASS2 = TWO * MS(NC2(I))
              ELSE
                MASS2 = XM(I)
              ENDIF
              IF(XKM(I)/=ZERO.AND.XCM(I)/=ZERO.AND.
     .          MASS2/=ZERO)THEN
                STI(2,I) = (XCM(I) + SQRT(XCM(I)**2+XKM(I)*MASS2))**2/MASS2
              ELSEIF(XKM(I)/=ZERO)THEN
                STI(2,I) = XKM(I)
              ELSEIF(XCM(I)/=ZERO.AND.MASS2/=ZERO)THEN
                A = FOUR * XCM(I)**2
                STI(2,I) = A / MASS2
              ENDIF
C
              IF(IN(NC1(I))/=ZERO)THEN
                IN2 = TWO * IN(NC1(I))
              ELSE
                IN2 = XIN(I)
              ENDIF
              IF(XKR(I)/=ZERO.AND.XCR(I)/=ZERO.AND.
     .          IN2/=ZERO)THEN
                STIR(1,I) = (XCR(I) + SQRT(XCR(I)**2+XKR(I)*IN2))**2/IN2
              ELSEIF(XKR(I)/=ZERO)THEN
                STIR(1,I) = XKR(I)
              ELSEIF(XCR(I)/=ZERO.AND.IN2/=ZERO)THEN
                A = FOUR * XCR(I)**2
                STIR(1,I) = A / IN2
              ENDIF
C
              IF(IN(NC2(I))/=ZERO)THEN
                IN2 = TWO * IN(NC2(I))
              ELSE
                IN2 = XIN(I)
              ENDIF
              IF(XKR(I)/=ZERO.AND.XCR(I)/=ZERO.AND.
     .          IN2/=ZERO)THEN
                STIR(2,I) = (XCR(I) + SQRT(XCR(I)**2+XKR(I)*IN2))**2/IN2
              ELSEIF(XKR(I)/=ZERo)THEN
                STIR(2,I) = XKR(I)
              ELSEIF(XCR(I)/=ZERO.AND.IN2/=ZERO)THEN
                A = FOUR * XCR(I)**2
                STIR(2,I) = A / IN2
              ENDIF
            ENDIF
          ENDDO ! DO I=JFT,JLT
C
          IF(IDTMIN(6)==0.AND.(IDTMINS/=2.OR.JSMS==0))RETURN
C
          IF(IDTMINS==2.AND.JSMS/=0)THEN
C
C IDTMINS=2 & JSMS=1 <=> AMS & elementary time step
C
            DTA=DTMINS/DTFACS
            DTB=DTA*DTA
            DO I=JFT,JLT
              IF(OFF(I)<=ZERO) CYCLE
              DMELRT(I)=MAX(DMELRT(I),
     .          XCM(I)*DTA+HALF*XKM(I)*DTB-HALF*MSRT(I))
C
C         MX = 2*(Mn+2*DeltaM)
              MX =MSRT(I)+TWO*DMELRT(I)
C
              IF(XCM(I)+XKM(I)<EM15)MX =ONE
              XKM(I)= MAX(EM15,XKM(I))

              DT(I)=DTFACS*
     .          MX /MAX(EM15,SQRT(XCM(I)*XCM(I)+MX*XKM(I))+XCM(I))
            ENDDO
C
            DO I=JFT,JLT
              IF(OFF(I)<=ZERO) CYCLE
              IF(DT(I)<DT2T) THEN
                DT2T=DT(I)
                NELTST =NGL(I)
                ITYPTST=6
              ENDIF
            ENDDO
          ELSE
C
            DO I=JFT,JLT
              IF(XCM(I)+XKM(I)<EM15)XM(I) =ONE
              XKM(I)= MAX(EM15,XKM(I))
              DT(I)=XM(I)/MAX(EM15,SQRT(XCM(I)*XCM(I)+XM(I)*XKM(I))+XCM(I))
              IF(XCR(I)+XKR(I)<EM15)XIN(I)=ONE
              XKR(I)= MAX(EM15,XKR(I))
              DTC(I)=XIN(I)/MAX(EM15,SQRT(XCR(I)*XCR(I)+XIN(I)*XKR(I))+XCR(I))
              DT(I)= MIN(DT(I),DTC(I))
            ENDDO
C
            DO I=JFT,JLT
              IF(OFF(I)>ZERO)THEN
                DT(I)=DTFAC1(6)*DT(I)
                IF(IDTMIN(6)==1.AND.DT(I)<DTMIN1(6))THEN
                  TSTOP = TT
#include "lockon.inc"
                  WRITE(IOUT,*)
     .            ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
                  WRITE(ISTDO,*)
     .            ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
                ELSEIF(IDTMIN(6)==5.AND.DT(I)<DTMIN1(6))THEN
                  MSTOP = 2
#include "lockon.inc"
                  WRITE(IOUT,*)
     .            ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
                  WRITE(ISTDO,*)
     .            ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
                ELSEIF(IDTMIN(6)==2.AND.DT(I)<DTMIN1(6))THEN
                  OFF(I)=ZERO
#include "lockon.inc"
                  WRITE(IOUT,*) '-- DELETE OF SPRING ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
                  IDEL7NOK = 1
                ENDIF
              ENDIF
            ENDDO
          END IF
C
        ELSE
C
          DO I=JFT,JLT
            IF(XCM(I)+XKM(I)<EM15)XM(I) =ONE
            XKM(I)= MAX(EM15,XKM(I))
            DT(I)=XM(I)/MAX(EM15,SQRT(XCM(I)*XCM(I)+XM(I)*XKM(I))+XCM(I))
          ENDDO
C
          DO I=JFT,JLT
            STI(1,I) = ZERO
            STI(2,I) = ZERO
            STIR(1,I) = ZERO
            STIR(2,I) = ZERO
            IF(OFF(I)>ZERO) THEN
              STI(1,I) = XM(I) / DT(I)**2
              STI(2,I) = STI(1,I)
            ENDIF
          ENDDO
C
          DO  I=JFT,JLT
            IF(XCR(I)+XKR(I)<EM15)XIN(I)=ONE
            XKR(I)= MAX(EM15,XKR(I))
            DTC(I)=XIN(I)/MAX(EM15,SQRT(XCR(I)*XCR(I)+XIN(I)*XKR(I))+XCR(I))
            DT(I)= MIN(DT(I),DTC(I))
          ENDDO
C
          DO I=JFT,JLT
            IF(OFF(I)>ZERO)THEN
              DT(I)=DTFAC1(6)*DT(I)
              IF(IDTMIN(6)==1.AND.DT(I)<DTMIN1(6))THEN
                TSTOP = TT
#include "lockon.inc"
                WRITE(IOUT,*)
     .          ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
                WRITE(ISTDO,*)
     .          ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
              ELSEIF(IDTMIN(6)==5.AND.DT(I)<DTMIN1(6))THEN
                MSTOP = 2
#include "lockon.inc"
                WRITE(IOUT,*)
     .          ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
                WRITE(ISTDO,*)
     .          ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
              ELSEIF(IDTMIN(6)==2.AND.DT(I)<DTMIN1(6))THEN
                OFF(I)=ZERO
#include "lockon.inc"
                WRITE(IOUT,*) '-- DELETE OF SPRING ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
                IDEL7NOK = 1
              ENDIF
              IF(DT(I)<DT2T)THEN
                DT2T=DT(I)
                NELTST =NGL(I)
                ITYPTST=6
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C------------------------------
        IF(G_DT/=0)THEN
          DO I=JFT,JLT
            DTEL(I) = DT(I)
          ENDDO
        ENDIF
C------------------------------

        RETURN
C
      END
