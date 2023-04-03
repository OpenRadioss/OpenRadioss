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
Chd|  M6LAW                         source/materials/mat/mat006/m6law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M6LAW(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     RK,      RE,      VORTI,
     3   VNEW,    RHO0,    VIS,     PSH,
     4   D1,      D2,      D3,      D4,
     5   D5,      D6,      C1,      C2,
     6   C3,      C4,      C5,      C6,
     7   PC,      AMU,     AMU2,    ESPE,
     8   SSP,     DF,      TMU,     MAT,
     9   WXX,     WYY,     WZZ,     NEL,
     A   DPDM,    JTUR,    JPOR)
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JPOR
      INTEGER MAT(*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), RK(*), RE(*),
     .   VORTI(*)
      my_real
     .   VNEW(*), RHO0(*), D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   PC(*), VIS(*), C1(*), C2(*), C3(*), C4(*), C5(*),
     .   C6(*), AMU(*), AMU2(*), ESPE(*), SSP(*), DF(*),
     .   TMU(*), PSH(*), WXX(*), WYY(*), WZZ(*), RHO0_1,
     .   PC_1, C1_1, C2_1, C3_1,
     .   C4_1, C5_1, C6_1, TMU_1, PSH_1
      my_real
     .     DPDM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   DAV, RK2T, VIS2, FAC
C-----------------------------------------------

      !SAUVEGARDE VORTICITE SI ANIM
      IF((ANIM_E(10)==1 .OR. ANIM_SE(10)==1).AND. DT1/=ZERO)THEN
         FAC=FOUR/DT1
        IF(N2D==0)THEN
          DO 5 I=1,NEL
 5        VORTI(I)=FAC*SQRT(WXX(I)**2+WYY(I)**2+WZZ(I)**2)
        ELSE
          DO 6 I=1,NEL
 6        VORTI(I)=FAC*WZZ(I)
        ENDIF
      ENDIF

      !EOS COEFFICIENTS
      MX     =MAT(1)
      RHO0_1=PM( 1,MX)
      PC_1  =PM(37,MX)
      C1_1  =PM(104,MX)-PM(88,MX)
      C2_1  =PM(32,MX)
      C3_1  =PM(33,MX)
      C4_1  =PM(34,MX)
      C5_1  =PM(35,MX)
      C6_1  =PM(36,MX)
      TMU_1 =PM(81,MX)
      PSH_1=PM(88,MX)
      DO 10 I=1,NEL
      RHO0(I)=RHO0_1
      VIS(I) =PM(24,MX)*RHO(I)
      PC(I)  =PC_1
      C1(I)  =C1_1
      C2(I)  =C2_1
      C3(I)  =C3_1
      C4(I)  =C4_1
      C5(I)  =C5_1
      C6(I)  =C6_1
      TMU(I) =TMU_1
      PSH(I)=PSH_1
   10 CONTINUE

      !IF TURBULENT
      IF(JTUR/=0)THEN
        IF(JPOR/=2)THEN
          DO I=1,NEL
            TMU(I)=TMU(I)*RK(I)*RK(I) / MAX(EM15,(RE(I)*VNEW(I)))
            VIS(I)=VIS(I)+TMU(I)
          ENDDO
        ELSE
          DO I=1,NEL
            TMU(I)=ZERO
          ENDDO
        ENDIF
      ELSE
        DO I=1,NEL
          RK(I)=ZERO
        ENDDO
      ENDIF

      !OUTPUT
      DO I=1,NEL
       VIS2 = TWO*VIS(I)
       DAV  = -(D1(I)+D2(I)+D3(I))*THIRD
       !------------------------------!
       !   VISCOUS STRESS TENSOR      !
       !------------------------------!
       SIG(I,1)=VIS2*(D1(I)+DAV)
       SIG(I,2)=VIS2*(D2(I)+DAV)
       SIG(I,3)=VIS2*(D3(I)+DAV)
       SIG(I,4)=VIS(I) *D4(I)
       SIG(I,5)=VIS(I) *D5(I)
       SIG(I,6)=VIS(I) *D6(I)

C         DF(I)  =RHO0(I)/RHO(I)
C         AMU(I) =ONE/DF(I)-ONE
       AMU2(I)= MAX(ZERO,AMU(I))**2
       ESPE(I)=DF(I)*EINT(I) / MAX(EM15,VNEW(I))
       RK2T = TWO*RK(I) / MAX(EM15,THREE*VNEW(I))
       
       !------------------------------!
       !         SOUND SPEED          !
       !------------------------------!
       
       DPDM(I) = DPDM(I)
     .       +(C5(I)+C6(I)*AMU(I))*DF(I)*DF(I)*RK2T + RK2T*DF(I)
       SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO

      RETURN
      END
