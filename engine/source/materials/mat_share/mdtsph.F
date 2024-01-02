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
Chd|  MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|-- called by -----------
Chd|        M1LAW                         source/materials/mat/mat001/m1law.F
Chd|        M1LAWI                        source/materials/mat/mat001/m1lawi.F
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1lawtot.F
Chd|        M22LAW                        source/materials/mat/mat022/m22law.F
Chd|        M24LAW                        source/materials/mat/mat024/m24law.F
Chd|        M2LAW                         source/materials/mat/mat002/m2law.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        SBOLTLAW                      source/elements/solid/solide/sboltlaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MDTSPH(
     1   PM,      OFF,     RHO,     RK,
     2   T,       RE,      STI,     DT2T,
     3   NELTST,  ITYPTST, OFFG,    GEO,
     4   PID,     MUMAX,   SSP,     VOL,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QVIS,    SSP_EQ,
     8   G_DT,    DTSPH,   NEL,     ITY,
     9   JTUR,    JTHE)
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
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "cong1_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER NELTST,ITYPTST,PID(*),MAT(*), NGL(*)
      my_real :: DT2T

      my_real
     .   PM(NPROPM,*), OFF(*), RHO(*), RK(*), T(*),
     .   RE(*),STI(*),OFFG(*),GEO(NPROPG,*),MUMAX(*),
     .   VOL(*), VD2(*), DELTAX(*), SSP(*), VIS(*),
     .   PSH(*), PNEW(*),QVIS(*) ,SSP_EQ(*), D1(*),
     .   D2(*), D3(*)
      my_real, INTENT(INOUT) :: DTSPH(1:NEL)
      INTEGER,INTENT(IN)     :: G_DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT
      my_real
     .   AL(MVSIZ),DTX(MVSIZ), QX(MVSIZ), CX(MVSIZ), QXMATER(MVSIZ),
     .   QA, QB, VISI, FACQ,
     .   CNS1, CNS2, SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU, RPR,
     .   ATU
C-----------------------------------------------
C   S o u  r c e   L  i n e s
C-----------------------------------------------
      IF(IMPL==ZERO)THEN
       DO I=1,NEL
         CX(I)=SSP(I)+SQRT(VD2(I))
       ENDDO
       VISI=ONE
       FACQ=ONE
      ELSE
       DO I=1,NEL
         CX(I)=SQRT(VD2(I))
       ENDDO
       VISI=ZERO
       FACQ=ZERO
      ENDIF
      
      !not a bug : only law 24 uses CNS1 & CNS2 
      !(they are not yet available with SPH).
      DO I=1,NEL
        AL(I)=ZERO
        IF(OFF(I)<1.) CYCLE
        AL(I)=VOL(I)**THIRD
      ENDDO

      MT = MAT(1)
      DO I=1,NEL
        QA =FACQ*GEO(14,PID(I))
        QB =FACQ*GEO(15,PID(I))
        CNS1=GEO(16,PID(I))
        CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*RHO(I)
        PSH(I)=PM(88,MT)
        PNEW(I)=ZERO
        QXMATER(I)=CNS1*SSP(I) + VISI*(TWO*VIS(I)+CNS2) / MAX(EM20,RHO(I)*DELTAX(I))
        QX(I)=QB*SSP(I) + QA*MUMAX(I) + QXMATER(I)
        QVIS(I)=ZERO
      ENDDO

      DO I=1,NEL
       DTX(I)=DELTAX(I)/MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
      !preparing material sound speed for nodal time step computation: 
       SSP_EQ(I) = MAX(EM20,QXMATER(I)+SQRT(QXMATER(I)*QXMATER(I)+CX(I)*CX(I)))
      ENDDO            

      IF(JTHE/=0)THEN
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
         IF(JTUR/=0)THEN
          XMU = RHO(I)*PM(24,MT)
          TMU = PM(81,MT)
          RPR = PM(95,MT)
          ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
          AKK=AKK*(ONE + ATU)
         ENDIF
         DTX(I) = MIN(DTX(I),HALF*DELTAX(I)*DELTAX(I)*SPH/AKK)
        ENDDO       
      ENDIF

      DO I=1,NEL
        STI(I) = ZERO
        IF(OFF(I)==ZERO) CYCLE
        STI(I) = TWO*RHO(I) * VOL(I) / (DTX(I)*DTX(I))
        DTX(I)= DTFAC1(ITY)*DTX(I)
        !dt2 remplace par dt2t
        IF(NODADT==0)DT2T= MIN(DTX(I),DT2T)
      ENDDO
      
      IF(G_DT/=ZERO)THEN
        DO I=1,NEL
          DTSPH(I) = DTX(I)
        ENDDO
      ENDIF
      

      IF(NODADT==0)THEN
       IF(IDTMIN(ITY)==1)THEN
        DO 170 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO) GO TO 170
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
          WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
#include "lockoff.inc"
 170     CONTINUE
       ELSEIF(IDTMIN(ITY)==2)THEN
        DO 270 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO) GO TO 270
          OFF(I) = ZERO
#include "lockon.inc"
          WRITE(IOUT,*) ' -- DELETE SPH PARTICLE',NGL(I)
          WRITE(ISTDO,*)' -- DELETE SPH PARTICLE',NGL(I)
#include "lockoff.inc"
 270    CONTINUE
       ELSEIF(IDTMIN(ITY)==5)THEN
        DO 570 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==0.) GO TO 570
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
#include "lockoff.inc"
 570    CONTINUE
       ENDIF
       
       DO I=1,NEL
         IF(DTX(I)>DT2T.OR.OFF(I)==ZERO) CYCLE
         !nelts et itypts remplaces par neltst et itypst
         NELTST =NGL(I)
         ITYPTST=ITY
       ENDDO

      ENDIF

      RETURN
      END
