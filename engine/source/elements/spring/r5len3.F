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
Chd|  R5LEN3                        source/elements/spring/r5len3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R5LEN3(
     1   JFT,     JLT,     OFF,     DT2T,
     2   NELTST,  ITYPTST, STI,     STIR,
     3   MS,      IN,      USTI,    USTIR,
     4   VISI,    VISIR,   UMAS,    UINER,
     5   FR_WAVE, FR_W_E,  EINT,    FX,
     6   XMOM,    YMOM,    ZMOM,    VX,
     7   RY1,     RZ1,     RX,      RY2,
     8   RZ2,     XL,      FY,      FZ,
     9   PARTSAV, IPARTR,  MSRT,    DMELRT,
     A   G_DT,    DTEL,    NGL,     NC1,
     B   NC2,     JSMS)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
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
      my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
      INTEGER,INTENT(IN)    :: G_DT
      INTEGER JFT,JLT,NELTST ,ITYPTST
      INTEGER IPARTR(*),NGL(*),NC1(*),NC2(*)
      my_real DT2T,
     .   OFF(*), STI(3,*), STIR(3,*), MS(*), IN(*),
     .   USTI(*) ,USTIR(*), VISI(*) ,VISIR(*)  ,UMAS(*) ,
     .   UINER(*),FR_WAVE(*) ,FR_W_E(*) ,EINT(*) ,  
     .   FX(*), FY(*), FZ(*), XMOM(*), YMOM(*),ZMOM(*),XL(*),
     .   VX(*), RY1(*), RZ1(*), RX(*), RY2(*), RZ2(*),PARTSAV(NPSAV,*),
     .   MSRT(*), DMELRT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real
     .   DT(MVSIZ), DTC(MVSIZ), 
     .     DTINV, A, MASS2, IN2, DTA, DTB, MX2
C--------------------------------------------
C     OFF
C--------------------------------------------
      DO I=JFT,JLT
        FX(I) = FX(I)*OFF(I)
        FY(I) = FY(I)*OFF(I)
        FZ(I) = FZ(I)*OFF(I)
        XMOM(I) = XMOM(I)*OFF(I)
        YMOM(I) = YMOM(I)*OFF(I)
        ZMOM(I) = ZMOM(I)*OFF(I)
      ENDDO
C--------------------------------------------
C     Energy
C--------------------------------------------
      DO I=JFT,JLT
        EINT(I) = EINT(I)
     .+ HALF*DT1 * (VX(I) * FX(I) + RX(I) * XMOM(I)
     .          + (RY2(I) - RY1(I)) * YMOM(I) 
     .          + (RZ2(I) - RZ1(I)) * ZMOM(I)
     .          + HALF * (RY2(I) + RY1(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2(I) + RZ1(I)) * FY(I) * XL(I) )
      ENDDO
      IF (NPSAV >= 21) THEN
        DO I=JFT,JLT
          MX = IPARTR(I)
          PARTSAV(23,MX)=PARTSAV(23,MX)
     .          + HALF*DT1 * (RX(I) * XMOM(I) 
     .          + (RY2(I) - RY1(I)) * YMOM(I) 
     .          + (RZ2(I) - RZ1(I)) * ZMOM(I)
     .          + HALF * (RY2(I) + RY1(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2(I) + RZ1(I)) * FY(I) * XL(I) )
        ENDDO
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF (IFRWV /= 0) THEN
#include "lockon.inc"
        DO I=JFT,JLT
          IF (FR_WAVE(NC1(I)) == ZERO)FR_WAVE(NC1(I))=-FR_W_E(I)
          IF (FR_WAVE(NC2(I)) == ZERO)FR_WAVE(NC2(I))=-FR_W_E(I)
        ENDDO
#include "lockoff.inc"
      ENDIF
C--------------------------------------------
C     time step
C--------------------------------------------
      IF (NODADT /= 0 .OR. IDTMINS == 2) THEN
        DO I=JFT,JLT
          USTI(I) =USTI(I) *MAX(ZERO,OFF(I))
          USTIR(I)=USTIR(I)*MAX(ZERO,OFF(I))
          VISI(I) =VISI(I) *MAX(ZERO,OFF(I))
          VISIR(I)=VISIR(I)*MAX(ZERO,OFF(I))
        ENDDO
        IF (DT1 == ZERO)THEN
          DO I=JFT,JLT
            IF (VISIR(I) < EM15) UINER(I) =ONE
            STI(1,I)  = USTI(I) 
            STIR(1,I) = USTIR(I) 
            IF (UMAS(I) > EM15)
     .         STI(1,I) = STI(1,I) + FOUR*VISI(I)**2 / UMAS(I)
            IF (UINER(I) > EM15)
     .         STIR(1,I) = STIR(1,I) + FOUR*VISIR(I)**2 / UINER(I)
            STI(2,I)  = STI(1,I)
            STIR(2,I) = STIR(1,I)
          ENDDO
        ELSE
          DO I=JFT,JLT
            STI(1,I)  = USTI(I) + TWO*VISI(I)/DT1
            STIR(1,I) = USTIR(I)+ TWO*VISIR(I)/DT1
            STI(2,I)  = STI(1,I)
            STIR(2,I) = STIR(1,I)
          ENDDO
        ENDIF
C
        IF (IDTMINS == 2 .AND. JSMS /= 0) THEN
          DTA=DTMINS/DTFACS
          DTB=DTA*DTA
          DO I=JFT,JLT
            IF (OFF(I) <= ZERO) CYCLE
             DT(I)=EP20
             IF (VISI(I)+USTI(I) >= EM15) THEN
               USTI(I)= MAX(EM15,USTI(I))
               DMELRT(I)=MAX(DMELRT(I),
     .           VISI(I)*DTA+HALF*USTI(I)*DTB-HALF*MSRT(I))
C                 MX2 = 2*(Mn+2*DeltaM)
                 MX2 =MSRT(I)+TWO*DMELRT(I)
                 DT(I)=DTFACS*
     .               MX2 /MAX(EM15,SQRT(VISI(I)*VISI(I)+MX2*USTI(I))+VISI(I))
             ENDIF
           ENDDO
C
           DO I=JFT,JLT
            IF (OFF(I) <= ZERO) CYCLE
            IF (DT(I) < DT2T) THEN
              DT2T=DT(I)
              NELTST =NGL(I)
              ITYPTST=6
            ENDIF
          ENDDO
        ENDIF ! IF (IDTMINS == 2 .AND. JSMS /= 0)
      ENDIF ! IF (NODADT /= 0 .OR. IDTMINS == 2)
C
      IF (NODADT /= 0 .OR. (IDTMINS == 2. AND. JSMS /= 0)) RETURN
C
      DO I=JFT,JLT
        IF (VISI(I)+USTI(I) < EM15) UMAS(I) =ONE
      ENDDO
C
      DO I=JFT,JLT
        USTI(I)= MAX(EM15,USTI(I))
        DT(I)=(SQRT(VISI(I)*VISI(I)+UMAS(I)*USTI(I))-VISI(I))/USTI(I)
        DTC(I)=HALF*UMAS(I)/ MAX(EM15,VISI(I))
        DT(I)= MIN(DT(I),DTC(I))
      ENDDO
C
      IF (IDTMINS /= 2) THEN
        DO I=JFT,JLT
          STI(1,I) = ZERO
          STI(2,I) = ZERO
          STIR(1,I) = ZERO
          STIR(2,I) = ZERO
          IF (DT(I) == ZERO) DT(I)=DTC(I)
          IF (OFF(I) <= ZERO) CYCLE     
          STI(1,I) = UMAS(I) / DT(I)**2
          STI(2,I) = STI(1,I)
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        IF (VISIR(I)+USTIR(I) < EM15) UINER(I)=ONE
      ENDDO
C
      DO I=JFT,JLT
        USTIR(I)= MAX(EM15,USTIR(I))
        DTC(I)=(SQRT(VISIR(I)*VISIR(I)+UINER(I)*USTIR(I))-VISIR(I))
     .         /USTIR(I)
        DT(I)= MIN(DT(I),DTC(I))
        DTC(I)=HALF*UINER(I)/ MAX(EM15,VISIR(I))
        DT(I)= MIN(DT(I),DTC(I))
      ENDDO
C
      DO I=JFT,JLT
        IF (OFF(I) <= ZERO) CYCLE
        IF (DT(I) == ZERO) DT(I)=DTC(I)
        DT(I)=DTFAC1(6)*DT(I)
        IF (IDTMIN(6) == 1 .AND. DT(I) < DTMIN1(6)) THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
        ELSEIF (IDTMIN(6) == 5 .AND. DT(I) < DTMIN1(6)) THEN
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
        ELSEIF (IDTMIN(6) == 2 .AND. DT(I) < DTMIN1(6)) THEN
          OFF(I)=ZERO
#include "lockon.inc"
          WRITE(IOUT,*) '-- DELETE OF SPRING ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
          IDEL7NOK = 1
        ENDIF
        IF (DT(I) >= DT2T) CYCLE
        DT2T=DT(I)
        NELTST =NGL(I)
        ITYPTST=6
      ENDDO
C------------------------------
        IF(G_DT/=ZERO)THEN
           DO I=JFT,JLT
             DTEL(I) = DT(I)
           ENDDO
        ENDIF
C------------------------------
      RETURN
      END
