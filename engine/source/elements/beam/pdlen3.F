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
Chd|  PDLEN3                        source/elements/beam/pdlen3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PDLEN3(JFT  ,JLT    ,PM     ,GEO  ,OFFG ,
     .                  DT2T ,NELTST ,ITYPTST,STI  ,STIR ,
     .                  MSP  ,DMELP  ,G_DT   ,DTEL ,AL   ,
     .                  MAT  ,PID    ,NGL    )
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
      INTEGER,INTENT(IN)    :: G_DT
      INTEGER JFT,JLT,NELTST ,ITYPTST,MAT(MVSIZ),PID(MVSIZ),
     .   NGL(MVSIZ)
      my_real DT2T ,
     .   PM(NPROPM,*), GEO(NPROPG,*), OFFG(*), STI(*), STIR(*),
     .   MSP(*), DMELP(*),AL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SSP(MVSIZ), DT(MVSIZ), DMP(MVSIZ), FAC(MVSIZ),
     .   A1, B1, B2, B3, YOUNG,G,AA,BB,   
     .   PHI,SHF,DSH(MVSIZ),SL2I(MVSIZ),F1,F2
C-----------------------------------------------
      DT(1:MVSIZ) = ZERO
!
      DO I=LFT,LLT
        DMP(I)=MAX(GEO(16,PID(I)),GEO(17,PID(I)))
      ENDDO
!
      IF (IDTMINS /= 2 .OR. JSMS == 0) THEN
        IF (NODADT /= 0 .OR. IDTMINS == 2) THEN
          DO I=JFT,JLT
            STI(I) = ZERO
            STIR(I) = ZERO
            SSP(I) =PM(27,MAT(I))
            FAC(I)=ZERO
            IF (OFFG(I) /= ZERO) THEN
              YOUNG  =PM(20,MAT(I))
              G      =PM(22,MAT(I))
              A1  =GEO(1,PID(I))
              B1  =GEO(2,PID(I))
              B2  =GEO(18,PID(I))
              B3  =GEO(4,PID(I))
              DMP(I)=DMP(I)*SQRT(TWO)
              AA    =(SQRT(ONE +DMP(I)*DMP(I))-DMP(I))
              AA  = AL(I) * AA * AA
              BB  = MAX(B1,B2)
              STIR(I) = MAX(G*B3,FOUR*YOUNG*BB) / AA
              STI(I) = A1 * YOUNG / AA
!             calcul du pourcentage d'amortissement en cisaillement
              SL2I(I)= A1*AL(I)**2 / MAX(B1,B2,EM30)
              SHF    = ONE-GEO(37,PID(I))
              PHI    = TWELVE*YOUNG/(FIVE/SIX*G)/MAX(EM30,SL2I(I))
! ---  lm(41u)  DSH > DMP : ne change pas ce qui marche, meme si trop severe
              DSH(I) = DMP(I)
     .                *MAX(ONE,
     .                 SQRT(TWELVE/MAX(EM30,SL2I(I)))*SQRT(ONE+PHI*SHF))
              AA     = SQRT(ONE+DSH(I)*DSH(I))-DSH(I)
              AA     = AL(I) * AA * AA
              STI(I) = MAX(STI(I),TWELVE*BB*YOUNG/AL(I)/AL(I) / AA)
            ENDIF ! IF (OFFG(I) /= ZERO)
          ENDDO
          IF (IDTMIN(5) == 0) RETURN
        ELSE
          DO I=JFT,JLT
            STI(I) = ZERO
            STIR(I) = ZERO
            SSP(I) =PM(27,MAT(I))
            YOUNG  =PM(20,MAT(I))
            A1  =GEO(1,PID(I))
            B1  =GEO(2,PID(I))
            B2  =GEO(18,PID(I))
            SL2I(I)=A1*AL(I)**2 / MAX(B1,B2,EM30)
            DMP(I)=DMP(I)*SQRT(TWO)
!           calcul du pourcentage d'amortissement en cisaillement
            G      = PM(22,MAT(I))
            SHF    = ONE-GEO(37,PID(I))
            PHI    = TWELVE*YOUNG/(FIVE/SIX*G)/MAX(EM30,SL2I(I))
            DSH(I) = DMP(I)
     .         * MAX(ONE,
     .           SQRT(TWELVE/MAX(EM30,SL2I(I)))*SQRT(ONE+PHI*SHF))
            IF (OFFG(I) > ZERO) STI(I) = A1 * YOUNG / AL(I)
          ENDDO
        ENDIF ! IF (NODADT /= 0 .OR. IDTMINS == 2)
      ELSE  ! IF (IDTMINS /= 2 .OR. JSMS == 0)
! IDTMINS=2 & JSMS=1 <=> AMS & elementary time step
        DO I=JFT,JLT
          STI(I) = ZERO
          STIR(I) = ZERO
          SSP(I) =PM(27,MAT(I))
          FAC(I)=ZERO
          IF (OFFG(I) /= ZERO) THEN
            YOUNG  =PM(20,MAT(I))
            G      =PM(22,MAT(I))
            A1  =GEO(1,PID(I))
            B1  =GEO(2,PID(I))
            B2  =GEO(18,PID(I))
            B3  =GEO(4,PID(I))
            DMP(I)=DMP(I)*SQRT(TWO)
            AA    =(SQRT(ONE +DMP(I)*DMP(I))-DMP(I))
            AA  = AL(I) * AA * AA
            BB  = MAX(B1,B2)
            STIR(I) = MAX(G*B3,FOUR*YOUNG*BB) / AA
            STI(I) = A1 * YOUNG / AA
!           calcul du pourcentage d'amortissement en cisaillement
            SL2I(I)= A1*AL(I)**2 / MAX(B1,B2,EM30)
            SHF    = ONE-GEO(37,PID(I))
            PHI    = TWELVE*YOUNG/(FIVE/SIX*G)/MAX(EM30,SL2I(I))
            DSH(I) = DMP(I)
     .              *MAX(ONE,
     .               SQRT(TWELVE/MAX(EM30,SL2I(I)))*SQRT(ONE+PHI*SHF))
            AA     = SQRT(ONE+DSH(I)*DSH(I))-DSH(I)
            AA     = AL(I) * AA * AA
            STI(I) = MAX(STI(I),TWELVE*BB*YOUNG/AL(I)/AL(I) / AA)
          ENDIF ! IF (OFFG(I) /= ZERO)
        ENDDO

        DO I=JFT,JLT
          IF (OFFG(I) /= ZERO) THEN
!         DT(I)  = DTFACS*
!      .           SQRT(TWO*(MSP(I)+DMELP(I))/MAX(EM20,STI(I)))
            DMELP(I)=MAX(DMELP(I),
     .             HALF*(DTMINS/DTFACS)**2 * STI(I) - MSP(I))
            DT(I)=DTMINS
            IF (DT(I) < DT2T) THEN
              DT2T    = DT(I)
              NELTST  = NGL(I)
              ITYPTST = 5
            ENDIF ! IF (DT(I) < DT2T)
          ENDIF ! IF (OFFG(I) /= ZERO)
        ENDDO
!---
       RETURN
      ENDIF
!
      DO I=JFT,JLT
        F1    =SQRT(ONE+DMP(I)*DMP(I))-DMP(I)
        F2    =MIN(F1,SQRT(ONE+DSH(I)*DSH(I))-DSH(I))
        FAC(I)=HALF*MIN(SQRT(MIN(FOUR,ONE+SL2I(I)/TWELVE))*F1,
     .                    SQRT(SL2I(I)/THREE)*F2)
        DT(I)=DTFAC1(5)*FAC(I)*AL(I)/SSP(I)
      ENDDO ! DO I=JFT,JLT
C
      DO I=JFT,JLT
        IF (DT(I) < DTMIN1(5) .AND. OFFG(I) == ONE) THEN
          IF (IDTMIN(5) == 1 ) THEN
            TSTOP = TT
#include "lockon.inc"
            WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR BEAM ELEMENT'
            WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR BEAM ELEMENT'
#include "lockoff.inc"
          ELSEIF (IDTMIN(5) == 2) THEN
            OFFG(I)=ZERO
#include "lockon.inc"
            WRITE(IOUT,*) '-- DELETE  OF BEAM ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
          ELSEIF (IDTMIN(5) == 5) THEN
            MSTOP = 2
#include "lockon.inc"
            WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR BEAM ELEMENT'
            WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR BEAM ELEMENT'
#include "lockoff.inc"
          ENDIF ! IF (IDTMIN(5) == 1 )
        ENDIF ! IF (DT(I) < DTMIN1(5) .AND. OFFG(I) == ONE)
      ENDDO ! DO I=JFT,JLT
!
      IF (NODADT /= 0) RETURN
!
      DO I=JFT,JLT
        IF (DT(I) < DT2T .and. OFFG(I) > ZERO) THEN
          DT2T=DT(I)
          NELTST =NGL(I)
          ITYPTST=5
        ENDIF ! IF (DT(I) < DT2T .OR. OFFG(I) > ZERO)
      ENDDO ! DO I=JFT,JLT
C------------------------------
      IF (G_DT /= ZERO) THEN
        DO I=JFT,JLT
          DTEL(I) = DT(I)
        ENDDO
      ENDIF
C------------------------------
      RETURN
      END
