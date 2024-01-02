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
Chd|  MQVISC26                      source/materials/mat_share/mqvisc26.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MQVISC26(
     1   PM,      OFF,     RHO,     RK,
     2   T,       SSP,     RE,      STI,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VOL,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QVIS,    SSP_EQ,
     8   XK,      NEL,     ITY,     ISMSTR,
     9   JTUR,    JTHE)
C============================================================================
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "cong1_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER NELTST,ITYPTST,PID(*),MAT(*), NGL(*)
      my_real  DT2T
      my_real
     .   PM(NPROPM,*), OFF(*), RHO(*), RK(*), T(*), RE(*),STI(*),
     .   OFFG(*),GEO(NPROPG,*),
     .   VOL(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QVIS(*) ,SSP_EQ(*), 
     .   D1(*), D2(*), D3(*), XK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT, K, MX
C     REAL
      my_real
     .   DD(MVSIZ), AL(MVSIZ),
     .   DTX(MVSIZ), AD(MVSIZ), QX(MVSIZ), CX(MVSIZ),
     .   QA, QB, VISI, FACQ,QAA,
     .   CNS1, CNS2, SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU, RPR,
     .   ATU, QAD, QBD, QAAP
      my_real 
     .   TIDT,TVOL,TRHO,TAIRE
C=======================================================================
      IF(IMPL == 0)THEN
       DO I=1,NEL
         DD(I)=-D1(I)-D2(I)-D3(I)
         AD(I)=ZERO
         AL(I)=ZERO
         CX(I)=SSP(I)+SQRT(VD2(I))
       ENDDO
       IF(IMPL_S>0)THEN
        VISI=ZERO
        FACQ=ZERO
       ELSE
       VISI=ONE
       FACQ=ONE
       ENDIF
      ELSE
       DO I=1,NEL
         DD(I)=-D1(I)-D2(I)-D3(I)
         AD(I)=ZERO
         AL(I)=ZERO
         CX(I)=SQRT(VD2(I))
       ENDDO
       VISI=ZERO
       FACQ=ZERO
      ENDIF
C
      IF(N2D>0) THEN
        DO I=1,NEL
         IF(OFF(I)==1.)THEN
           AL(I)=SQRT(AIRE(I))
           AD(I)= MAX(ZERO,DD(I))
         ENDIF
        ENDDO
      ELSE
        DO I=1,NEL
         IF(OFF(I)==1.)THEN
           AL(I)=VOL(I)**THIRD
           AD(I)= MAX(ZERO,DD(I))
         ENDIF
        ENDDO
      ENDIF
C
      IF(INVSTR>=35)THEN
        MT  = MAT(1)
        MX = PID(1)
        QA =FACQ*GEO(14,MX)
        QB =FACQ*GEO(15,MX)
        CNS1=GEO(16,MX)
        DO I=1,NEL
          CNS2=GEO(17,MX)*SSP(I)*AL(I)*RHO(I)
          PSH(I)=PM(88,MT)
          PNEW(I)=0.
          QAA = QA*QA*AD(I)
          QX(I)=(QB+CNS1)*SSP(I)+DELTAX(I) * QAA
     .     + VISI*(TWO*VIS(I)+CNS2) / MAX(EM20,RHO(I)*DELTAX(I))
          QVIS(I)=RHO(I)*AD(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))
        ENDDO
      ELSE
        MT  = MAT(1)
        QA =FACQ*PM(2,MT)
        QB =FACQ*PM(3,MT)
        CNS1=PM(93,MT)
        DO I=1,NEL
          CNS2=PM(94,MT)*SSP(I)*AL(I)*RHO(I)
          PSH(I)=PM(88,MT)
          PNEW(I)=0.
          QAA = QA*QA*AD(I)
          QX(I)=(QB+CNS1)*SSP(I)+DELTAX(I) *QAA
     .     + VISI*(2.*VIS(I)+CNS2) / MAX(EM20,RHO(I)*DELTAX(I))
          QVIS(I)=RHO(I)*AD(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))
        ENDDO
      ENDIF
C
C
      DO I=1,NEL
        SSP_EQ(I) = MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
        DTX(I) = DELTAX(I) / SSP_EQ(I)
      ENDDO
C
      IF(JTHE==1)THEN
        MT  = MAT(1)
        SPH = PM(69,MT)
        AK1 = PM(75,MT)
        BK1 = PM(76,MT)
        AK2 = PM(77,MT)
        BK2 = PM(78,MT)
        TLI = PM(80,MT)
        DO I=1,NEL
         IF(T(I)<TLI)THEN
          AKK=AK1+BK1*T(I)
         ELSE
          AKK=AK2+BK2*T(I)
         ENDIF
         AKK = AKK+XK(I)
         IF(JTUR/=0)THEN
          XMU = RHO(I)*PM(24,MT)
          TMU = PM(81,MT)
          RPR = PM(95,MT)
          ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
          AKK=AKK*(ONE+ATU)
         ENDIF
         DTX(I) = MIN(DTX(I),HALF*DELTAX(I)*DELTAX(I)*SPH/MAX(AKK,EM20))
        ENDDO
      ENDIF
C
      DO 60 I=1,NEL
       STI(I) = ZERO
      IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 60
      IF(N2D==0) THEN
       TIDT = 1./DTX(I)
       TRHO = RHO(I) * TIDT
       TVOL = VOL(I) * TIDT
       STI(I) = FOURTH * TRHO * TVOL
      ELSE
       TIDT = 1./DTX(I)
       TRHO = RHO(I) * TIDT
       TAIRE = AIRE(I) * TIDT
       STI(I) = HALF * TRHO * TAIRE
      ENDIF
      DTX(I)= DTFAC1(ITY)*DTX(I)
      IF(NODADT==0)DT2T= MIN(DTX(I),DT2T)
  60  CONTINUE
C
      IF(IDTMIN(ITY)==1)THEN
        DO 70 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO. 
     .        OR.OFFG(I)<ZERO) GO TO 70
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
   70   CONTINUE
      ELSEIF(IDTMIN(ITY)==2)THEN
        DO 75 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.
     .    OR.OFFG(I)<ZERO) GO TO 75
          OFF(I) = 0.0
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
          WRITE(ISTDO,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
#include "lockoff.inc"
         IDEL7NOK = 1
   75   CONTINUE
      ELSEIF(IDTMIN(ITY)==3.AND.ISMSTR==2)THEN
        DO 76 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.
     .      OFF(I)<ONE.OR.OFFG(I)==TWO) GO TO 76
          OFFG(I) = TWO
#include "lockon.inc"
          WRITE(IOUT,*)
     . '-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
          WRITE(ISTDO,*)
     . '-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
#include "lockoff.inc"
   76   CONTINUE
      ELSEIF(IDTMIN(ITY)==5)THEN
        DO 570 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.
     .      OR.OFFG(I)<ZERO) GO TO 570
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
  570   CONTINUE
      ENDIF
C
      IF(NODADT==0)THEN
       DO 80 I=1,NEL
        IF(DTX(I)>DT2T.OR.OFF(I)<=ZERO.OR.OFFG(I)<=ZERO)GOTO 80 
        DT2T    = DTX(I)
        NELTST =NGL(I)
        ITYPTST=ITY
   80  CONTINUE
      ENDIF
C
      RETURN
      END
