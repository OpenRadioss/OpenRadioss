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
Chd|  MQVISC8                       source/materials/mat_share/mqvisc8.F
Chd|-- called by -----------
Chd|        M1LAW8                        source/materials/mat/mat001/m1law8.F
Chd|        M2LAW8                        source/materials/mat/mat002/m2law8.F
Chd|        MEOS8                         source/materials/mat_share/meos8.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MQVISC8(
     1   PM,      OFF,     RHO,     RK,
     2   T,       RE,      STI,     EINT,
     3   D1,      D2,      D3,      VOL,
     4   DVOL,    VD2,     DELTAX,  VIS,
     5   QOLD,    SSP,     MAT,     NC,
     6   NGL,     GEO,     PID,     DT2T,
     7   NELTST,  ITYPTST, OFFG,    MSSA,
     8   DMELS,   NEL,     ITY,     JTUR,
     9   JTHE,    JSMS)
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
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JSMS
      INTEGER :: NEL
C     REAL
      my_real
     .   PM(NPROPM,*), OFF(MVSIZ) , RHO(NEL) , RK(MVSIZ)  , T(MVSIZ),
     .   RE(MVSIZ)   , STI(*)   , EINT(NEL),
     .   D1(MVSIZ,8) , D2(MVSIZ,8), D3(MVSIZ,8),
     .   VOL(MVSIZ)  , DVOL(MVSIZ),
     .   VD2(MVSIZ) ,DELTAX(MVSIZ),VIS(MVSIZ),QOLD(NEL), SSP(MVSIZ),
     .   GEO(NPROPG,*) , DT2T , OFFG(*), MSSA(*), DMELS(*)
      INTEGER MAT(MVSIZ),NC(8,MVSIZ),NGL(MVSIZ), PID(*), NELTST,ITYPTST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J, MT,IPT
C     REAL
      my_real
     .   DD(MVSIZ), AL(MVSIZ), DTX(MVSIZ), DTY(MVSIZ),
     .   AD(MVSIZ), QX(MVSIZ), CX(MVSIZ),QVIS(MVSIZ),
     .   VISI, FACQ, QA, QB,
     .   CNS1, CNS2, SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU, RPR,
     .   ATU
C-----------------------------------------------
C
      DO I=1, NEL
       AL(I)=ZERO
      ENDDO
      DO 2 I=1,NEL
   2  DD(I)=ZERO
      DO 5 IPT=1,8
      DO 5 I=1,NEL
   5  DD(I)=DD(I)- ONE_OVER_8*(D1(I,IPT)+D2(I,IPT)+D3(I,IPT))
C
      IF(IMPL==ZERO)THEN
       DO 10 I=1,NEL
   10  CX(I)=SSP(I)+SQRT(VD2(I))
       VISI=1.
       FACQ=1.
       IF(IMPL_S>0.AND.IDYNA==0)THEN
        VISI=ZERO
        FACQ=ZERO
       ENDIF
       IF(IMCONV<0) THEN
        DO I=1,NEL
          VOL(I)= ABS(VOL(I))
        ENDDO
       ENDIF
      ELSE
       DO 15 I=1,NEL
   15  CX(I)=SQRT(VD2(I))
       VISI=ZERO
       FACQ=ZERO
      ENDIF
C
C
      DO 20 I=1,NEL
      AD(I)=ZERO
      IF(OFF(I)<ONE.OR.OFFG(I)<=ZERO) GO TO 20
      AL(I)=VOL(I)** THIRD
      IF(N2D>0) AL(I)=SQRT(VOL(I))
      AD(I)= MAX(ZERO,DD(I))*AL(I)
   20 CONTINUE
C
      IF(INVSTR>=35)THEN
        DO I=1,NEL
          QA =FACQ*GEO(14,PID(I))
          QB =FACQ*GEO(15,PID(I))
          CNS1=GEO(16,PID(I))
          CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*RHO(I)
          QX(I)=(QB+CNS1)*SSP(I)+QA*QA*DELTAX(I) * MAX(ZERO,DD(I))
     .     + VISI*(TWO*VIS(I)+CNS2)
     .     / MAX(EM20,RHO(I)*DELTAX(I))
          QVIS(I)=RHO(I)*AD(I)*(QA*QA*AD(I)+QB*SSP(I))
        ENDDO
      ELSE
        MT  = MAT(1)
        QA =FACQ*PM(2,MT)
        QB =FACQ*PM(3,MT)
        CNS1=PM(93,MT)
        DO I=1,NEL
          CNS2=PM(94,MT)*SSP(I)*AL(I)*RHO(I)
C         PSH(I)=PM(88,MAT(I))
C         PNEW(I)=0.
          QX(I)=(QB+CNS1)*SSP(I)+QA*QA*DELTAX(I) * MAX(ZERO,DD(I))
     .     + VISI*(TWO*VIS(I)+CNS2)
     .     / MAX(EM20,RHO(I)*DELTAX(I))
          QVIS(I)=RHO(I)*AD(I)*(QA*QA*AD(I)+QB*SSP(I))
        ENDDO
      ENDIF
C
      DO 30 I=1,NEL
   30 DTX(I)=DELTAX(I)/
     .  MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
C
      IF(JTHE > 0 )THEN
         MT  = MAT(1)
         SPH = PM(69,MT)
         AK1 = PM(75,MT)
         BK1 = PM(76,MT)
         AK2 = PM(77,MT)
         BK2 = PM(78,MT)
         TLI = PM(80,MT)
        DO 40 I=1,NEL
         IF(T(I)<TLI)THEN
          AKK=AK1+BK1*T(I)
         ELSE
          AKK=AK2+BK2*T(I)
         ENDIF
         IF(JTUR/=0)THEN
          XMU = RHO(I)*PM(24,MT)
          TMU = PM(81,MT)
          RPR = PM(95,MT)
          ATU=RPR*TMU*RK(I)**2/(MAX(EM15,RE(I)*VOL(I))*XMU)
          AKK=AKK*(1.+ATU)
         ENDIF
         DTX(I) = MIN(DTX(I),HALF*DELTAX(I)**2*SPH/AKK)
   40   CONTINUE
      ENDIF
C----
C IDTMINS/=2
C----
      IF(.NOT.(IDTMINS==2.AND.JSMS/=0))THEN
       DO I=1,NEL
        EINT(I)=EINT(I)+HALF*OFF(I)*DVOL(I)*(-QOLD(I)-QVIS(I))
        QOLD(I)=QVIS(I)
C       STI will be changed to 2*STI/NNE in SxCUMU  
C       [ STI(I) = FOURTH * OFF(I) * RHO(I) * VOL(I) / DTX(I)**2 for 8node bricks ]
        STI(I) = OFF(I) * RHO(I) * VOL(I) / DTX(I)**2
        DTX(I)= DTFAC1(ITY)*DTX(I)
       ENDDO
C
       IF(NODADT==0)THEN
         DO I = 1,NEL
           IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
           IF(DTX(I)<DT2T)THEN
             DT2T    = DTX(I)
             NELTST  = NGL(I)
             ITYPTST = ITY
           END  IF
         ENDDO
       END IF
C
      ELSE ! IDTMINS == 2 .AND. JSMS /= 0
       DO I=1,NEL
         DTY(I)= DTX(I)
         DTX(I)= DTFAC1(ITY)*DTX(I)
       END DO
      END IF
C----
      IF(IMCONV==1)THEN
       IF(IDTMIN(ITY)==1)THEN
        DO 70 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.OR.
     .   OFFG(I)<ZERO)GO TO 70
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
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.OR.
     .   OFFG(I)<ZERO)GO TO 75
          OFF(I) = ZERO
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
          WRITE(ISTDO,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
#include "lockoff.inc"
          IDEL7NOK = 1
   75   CONTINUE
       ELSEIF(IDTMIN(ITY)==5)THEN
        DO 570 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.OR.
     .   OFFG(I)<ZERO)GO TO 570
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
  570   CONTINUE
       ENDIF
      END IF ! IF(IMCONV==1)
C----
      IF(IDTMINS == 2 .AND. JSMS /= 0)THEN
       DO I=1,NEL
        EINT(I)=EINT(I)+HALF*OFF(I)*DVOL(I)*(-QOLD(I)-QVIS(I))
        QOLD(I)=QVIS(I)
C       STI will be changed to 2*STI/NNE in SxCUMU  
C       [ STI(I) = FOURTH * OFF(I) * RHO(I) * VOL(I) / DTX(I)**2 for 8node bricks ]
        STI(I) = OFF(I) * RHO(I) * VOL(I) / DTX(I)**2
c
c       DTX(I) = DTFACS*SQRT(ONE+DMELS(I)/MSSA(I))*DTX(I)
c       DMELS(I)=MAX(DMELS(I),
c     .          MSSA(I)*((DTMINS/DTX(I))**2 - ONE))
c       DTX(I)=MAX(DTX(I),DTMINS)
        DMELS(I)=MAX(DMELS(I),
     .           TWO*MSSA(I)*((DTMINS/(DTFACS*DTY(I)))**2 - ONE))
        DTX(I)=DTFACS*SQRT(ONE+DMELS(I)/(TWO*MSSA(I)))*DTY(I)
       ENDDO
C
       DO I = 1,NEL
         IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
         IF(DTX(I)<DT2T)THEN
           DT2T    = DTX(I)
           NELTST  = NGL(I)
           ITYPTST = ITY
         END  IF
       ENDDO
C 
      END IF
C
      RETURN
      END
