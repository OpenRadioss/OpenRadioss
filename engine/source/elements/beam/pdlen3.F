Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    pdlen3   ../engine/source/elements/beam/pdlen3.F
!||--- called by ------------------------------------------------------
!||    pforc3   ../engine/source/elements/beam/pforc3.F
!||====================================================================
      SUBROUTINE PDLEN3(MAT_PARAM,
     1   NEL,     GEO,
     2   OFFG,    DT2T,    NELTST,  ITYPTST,
     3   STI,     STIR,    MSP,     DMELP,
     4   G_DT,    DTEL,    AL,      
     5   PID,     NGL,     IGTYP,   JSMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: IGTYP
      INTEGER, INTENT(IN) :: JSMS
      INTEGER, INTENT(IN) :: G_DT
      INTEGER, INTENT(INOUT) :: NELTST,ITYPTST
      INTEGER PID(MVSIZ),NGL(MVSIZ)
      my_real, INTENT(INOUT) :: DTEL(1:NEL)
      my_real :: DT2T
      my_real :: GEO(NPROPG,*), OFFG(*), STI(*), STIR(*),
     .   MSP(*), DMELP(*),AL(MVSIZ)
      TYPE (MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: ssp
      my_real :: DT(MVSIZ), DMP(MVSIZ), FAC(MVSIZ),
     .   A1, B1, B2, B3, YOUNG,G,AA,BB,   
     .   PHI,SHF,DSH(MVSIZ),SL2I(MVSIZ),
     .   FACDT(MVSIZ),PHII(MVSIZ),CST,PHMAX,
     .   KPHI(MVSIZ),PHMIN,FSH(MVSIZ)
!===============================================================================
      YOUNG = MAT_PARAM%YOUNG
      G     = MAT_PARAM%SHEAR
      SSP   = SQRT(YOUNG / MAT_PARAM%RHO)
!
      DO I=1,NEL
        DMP(I)=MAX(GEO(16,PID(I)),GEO(17,PID(I)))
      ENDDO
      DT(1:MVSIZ) = ZERO
C----------------------------------------------
C     for new dt
C----------------------------------------------
      DO I=1,NEL
        CST = SIX_OVER_5*YOUNG/G
        A1  =GEO(1,PID(I))
        B1  =GEO(2,PID(I))
        B2  =GEO(18,PID(I))
        BB  = MAX(B1,B2,EM30)
        SL2I(I) = A1*AL(I)**2 / BB
        FACDT(I) = ONE_OVER_12*SL2I(I)
        PHMAX = CST/FACDT(I)        
        PHMIN = MIN(B1,B2)*PHMAX/BB
        KPHI(I) = (FOUR+PHMIN)/(ONE+PHMIN)        
        PHII(I) = KPHI(I)/(ONE+FACDT(I))        
        PHII(I) = MAX(ONE,PHII(I))        
        FSH(I) = AL(I)/(FACDT(I)+CST)
        FSH(I) = MAX(ONE,FSH(I))
      ENDDO
      IF (IGTYP == 18) THEN
          FSH(1:NEL) = ONE
          KPHI(1:NEL) = MAX(ONE,SL2I(1:NEL))   
      END IF
      IF (IDTMINS /= 2 .OR. JSMS == 0) THEN
        IF (NODADT /= 0 .OR. IDTMINS == 2) THEN
          DO I=1,NEL
            STI(I) = ZERO
            STIR(I) = ZERO
            FAC(I)=ZERO
            IF (OFFG(I) /= ZERO) THEN
              A1  =GEO(1,PID(I))
              B1  =GEO(2,PID(I))
              B2  =GEO(18,PID(I))
              B3  =GEO(4,PID(I))
              DMP(I)=DMP(I)*SQRT(TWO)
              AA    =(SQRT(ONE +DMP(I)*DMP(I))-DMP(I))
              AA  = AL(I) * AA * AA
              BB  = MAX(B1,B2)
              STIR(I) = MAX(G*B3,KPHI(I)*YOUNG*BB) / AA
              STI(I) = FSH(I)*A1 * YOUNG / AA
            ENDIF ! IF (OFFG(I) /= ZERO)
          ENDDO
          IF (IDTMIN(5) == 0) RETURN
        ELSE
          DO I=1,NEL
            STI(I) = ZERO
            STIR(I) = ZERO
            A1  =GEO(1,PID(I))
            DMP(I)=DMP(I)*SQRT(TWO)
            IF (OFFG(I) > ZERO) STI(I) = FSH(I)*A1 * YOUNG / AL(I)
          ENDDO
        ENDIF ! IF (NODADT /= 0 .OR. IDTMINS == 2)
      ELSE  ! IF (IDTMINS /= 2 .OR. JSMS == 0)
! IDTMINS=2 & JSMS=1 <=> AMS & elementary time step
        DO I=1,NEL
          STI(I) = ZERO
          STIR(I) = ZERO
          FAC(I)=ZERO
          IF (OFFG(I) /= ZERO) THEN
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
!           Calculation of the shear depreciation percentage
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

        DO I=1,NEL
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
      DO I=1,NEL
        FAC(I) =SQRT(ONE+DMP(I)*DMP(I))-DMP(I)
        DT(I)=DTFAC1(5)*FAC(I)*AL(I)/SSP/SQRT(FSH(I))
      ENDDO ! DO I=1,NEL
C
      DO I=1,NEL
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
      ENDDO ! DO I=1,NEL
!
      IF (NODADT /= 0) RETURN
!
      DO I=1,NEL
        IF (DT(I) < DT2T .and. OFFG(I) > ZERO) THEN
          DT2T=DT(I)
          NELTST =NGL(I)
          ITYPTST=5
        ENDIF ! IF (DT(I) < DT2T .OR. OFFG(I) > ZERO)
      ENDDO ! DO I=1,NEL
C------------------------------
      IF (G_DT /= ZERO) THEN
        DO I=1,NEL
          DTEL(I) = DT(I)
        ENDDO
      ENDIF
C------------------------------
      RETURN
      END
