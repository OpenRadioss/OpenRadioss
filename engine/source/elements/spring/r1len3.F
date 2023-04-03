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
Chd|  R1LEN3                        source/elements/spring/r1len3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R1LEN3(
     1   JFT,     JLT,     OFF,     DT2T,
     2   NELTST,  ITYPTST, STI,     MS,
     3   MSRT,    DMELRT,  G_DT,    DTEL,
     4   NGL,     XK,      XM,      XC,
     5   AK,      NC1,     NC2,     JSMS)
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
      INTEGER NGL(*),NC1(*),NC2(*)
      my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
      INTEGER,INTENT(IN)    :: G_DT
      INTEGER JFT,JLT,NELTST ,ITYPTST
      my_real DT2T,
     .   OFF(*), STI(3,*), MS(*),XK(MVSIZ),XM(MVSIZ),XC(MVSIZ),
     .   AK(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DT(MVSIZ),DTC(MVSIZ),
     .   A, MASS2, MSRT(*), DMELRT(*),
     .   DTA, DTB, MX
C-----------------------------------------------
      DO I=JFT,JLT
        XK(I)=XK(I)*AK(I)
      END DO

      IF(NODADT/=0.OR.IDTMINS==2)THEN
C
        DO 10 I=JFT,JLT
        STI(1,I) = ZERO
        IF(OFF(I)>ZERO)THEN
         IF(XK(I)/=ZERO.AND.XC(I)/=ZERO.AND.
     .     MS(NC1(I))/=ZERO)THEN
          MASS2 = TWO * MS(NC1(I))
          STI(1,I) = (SQRT(XC(I)**2+XK(I)*MASS2)+XC(I))**2/MASS2
         ELSEIF(XK(I)/=ZERO)THEN
          STI(1,I) = XK(I)
         ELSEIF(XC(I)/=ZERO.AND.MS(NC1(I))/=ZERO)THEN
          A = TWO * XC(I)**2
          STI(1,I) = A / MS(NC1(I))
         ENDIF
        ENDIF
        STI(2,I) = ZERO
        IF(OFF(I)>ZERO)THEN
         IF(XK(I)/=ZERO.AND.XC(I)/=ZERO.AND.
     .     MS(NC2(I))/=ZERO)THEN
          MASS2 = TWO * MS(NC2(I))
          STI(2,I) = (SQRT(XC(I)**2+XK(I)*MASS2)+XC(I))**2/MASS2
         ELSEIF(XK(I)/=ZERO)THEN
          STI(2,I) = XK(I)
         ELSEIF(XC(I)/=ZERO.AND.MS(NC2(I))/=ZERO)THEN
          A = TWO * XC(I)**2 
          STI(2,I) = A / MS(NC2(I))
         ENDIF
        ENDIF
 10     CONTINUE
C
        IF(IDTMIN(6)==0.AND.(IDTMINS/=2.OR.JSMS==0))RETURN
C
        IF(IDTMINS==2.AND.JSMS/=0)THEN
C-----
C IDTMINS=2 & JSMS=1 <=> AMS & elementary time step
C
         DTA=DTMINS/DTFACS
         DTB=DTA*DTA
         DO I=JFT,JLT
           IF(OFF(I)<=ZERO) CYCLE
           DMELRT(I)=MAX(DMELRT(I),
     .       XC(I)*DTA+HALF*XK(I)*DTB-HALF*MSRT(I))
C
C          MX = 2*(Mn+2*DeltaM)
           MX =MSRT(I)+TWO*DMELRT(I)
           DT(I)=DTFACS*
     .       MX /MAX(EM15,SQRT(XC(I)*XC(I)+MX*XK(I))+XC(I))
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
         DO I=JFT,JLT
           DT(I)= XM(I)/MAX(EM15,SQRT(XC(I)*XC(I)+XM(I)*XK(I))+XC(I))
         ENDDO

         DO I=JFT,JLT
          IF(DT(I)==ZERO) DT(I)=DTC(I)
          IF(OFF(I)>ZERO) THEN
           DT(I)=DTFAC1(6)*DT(I)
           IF(IDTMIN(6)==1.AND.DT(I)<DTMIN1(6))THEN
             TSTOP = TT
#include "lockon.inc"
             WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
             WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
           ELSEIF(IDTMIN(6)==5.AND.DT(I)<DTMIN1(6))THEN
             MSTOP = 2
#include "lockon.inc"
             WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
             WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
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
        DO I=JFT,JLT
         DT(I)= XM(I)/MAX(EM15,SQRT(XC(I)*XC(I)+XM(I)*XK(I))+XC(I))
         DTC(I)=HALF*XM(I) / MAX(EM15,XC(I))
         DT(I)= MIN(DT(I),DTC(I))
        ENDDO
C
        DO I=JFT,JLT
         STI(1,I) = ZERO
         STI(2,I) = ZERO
         IF(DT(I)==ZERO) DT(I)=DTC(I)
         IF(OFF(I)>ZERO) THEN
           STI(1,I) = XM(I) / DT(I)**2
           STI(2,I) = STI(1,I)
           DT(I)=DTFAC1(6)*DT(I)
           IF(IDTMIN(6)==1.AND.DT(I)<DTMIN1(6))THEN
             TSTOP = TT
#include "lockon.inc"
             WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
             WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
           ELSEIF(IDTMIN(6)==5.AND.DT(I)<DTMIN1(6))THEN
             MSTOP = 2
#include "lockon.inc"
             WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
             WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
           ELSEIF(IDTMIN(6)==2.AND.DT(I)<DTMIN1(6))THEN
             OFF(I)=ZERO
#include "lockon.inc"
             WRITE(IOUT,*) '-- DELETE OF SPRING ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
             IDEL7NOK = 1
           ENDIF
           IF(DT(I)<DT2T) THEN
             DT2T=DT(I)
             NELTST =NGL(I)
             ITYPTST=6
           ENDIF
         ENDIF
        ENDDO
      END IF
C------------------------------
        IF(G_DT/=ZERO)THEN
           DO I=JFT,JLT
             DTEL(I) = DT(I)
           ENDDO
        ENDIF
C------------------------------
      
      RETURN
      END
