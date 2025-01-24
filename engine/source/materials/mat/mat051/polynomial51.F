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
      !||    polyun51   ../engine/source/materials/mat/mat051/polynomial51.F
      !||--- called by ------------------------------------------------------
      !||    sigeps51   ../engine/source/materials/mat/mat051/sigeps51.F90
      !||====================================================================
      SUBROUTINE POLYUN51 (
     .    C0      ,C1     ,C2      ,C3       ,C4     , C5   ,GG,
     .    VOLUME  ,DVOL   ,VOLD    ,
     .    RHO     ,MASA   ,RHOA0   ,DD       ,MU     , MUP1 ,
     .    POLD    ,PEXT   ,P       ,PM       ,Q      ,
     .    RHO0E   ,EINTA  ,VISCMAX ,XL       ,SSP    ,
     .    QA      ,QB     ,UPARAM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN) :: C0,C1,C2,C3,C5,C4,GG,VOLUME,VOLD,MASA ,RHOA0,DD, POLD,PEXT,PM
      my_real,INTENT(IN) :: RHO0E,EINTA ,XL, QA,QB,UPARAM(*)
      my_real,INTENT(INOUT) :: P, DVOL, RHO, MU, VISCMAX, Q, SSP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: AA,BB,DPDV,DPDMU,V0,QAL,QBL,MUP1,MUMX,BULK, BULK2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      MUMX    = UPARAM(20)
      BULK    = UPARAM(21)
      BULK2   = UPARAM(21)
C------------------------
      V0      = MASA / RHOA0
      DVOL    = VOLUME - VOLD
      RHO     = MASA/VOLUME
      MUP1    = RHO/RHOA0
      MU      = MUP1 - ONE

      DPDV    = (-(C1 + (C2 + C2 + THREE*C3*MU)*MAX(ZERO,MU) + C5*RHO0E )*MUP1*MUP1 -(C4 + C5*MU)*(POLD+PEXT) )  /  V0
      DPDMU   = -DPDV*VOLUME/MUP1
      DPDMU   = ABS(DPDMU)
      SSP     = SQRT((DPDMU + TWO_THIRD*GG)/RHOA0)
      QAL     = QA*XL
      QAL     = QAL*QAL
      QBL     = QB*XL
      VISCMAX = RHO*(QAL*MAX(ZERO,DD) + QBL*SSP)
      Q       = VISCMAX*MAX(ZERO,DD)

      AA      = (C4 + C5*MU)/V0
      BB      = HALF*(VOLUME-VOLD)
      P       = ( C0 + C1*MU+ MAX(MU,ZERO)*(C2*MU + C3*MU*MU) + AA*EINTA )!  /  (ONE+AA*BB)
      P       = MAX(P,PM)

      DPDV    = (-(C1 + (C2 + C2 + THREE*C3*MU)*MAX(ZERO,MU) + C5*RHO0E )*MUP1*MUP1 -(C4 + C5*MU)*(P+PEXT) )  /  V0
      DPDMU   = -DPDV*VOLUME/MUP1
      DPDMU   = ABS(DPDMU)
      SSP     = SQRT((DPDMU + TWO_THIRD*GG)/RHOA0)

      RETURN
      END

      !||====================================================================
      !||    poly51     ../engine/source/materials/mat/mat051/polynomial51.F
      !||--- called by ------------------------------------------------------
      !||    sigeps51   ../engine/source/materials/mat/mat051/sigeps51.F90
      !||====================================================================
      SUBROUTINE POLY51 (C01,C11,C21,C31,C41,C51,GG1,
     .          V10,V1,V1OLD,MU1,MUP1,EINT1,
     .          PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E_INF,
     .          UPARAM ,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN) :: C01,C11,C21,C31,C41,C51,GG1,
     .                       V10,V1,V1OLD,
     .                       PEXT,PM1,P1I,
     .                       RHO10,MAS1,E_INF,
     .                       UPARAM(*)
      my_real,INTENT(INOUT) :: DVDP1,P1,SSP1,DPDV1,EINT1,MU1,MUP1,RHO1
      INTEGER FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  AA,BB,DVDP1I,BULK,BULK2,MUMX
C------------------------
      MUMX  = UPARAM(20)
      BULK  = UPARAM(21)
      BULK2 = UPARAM(21)
C------------------------
      DVDP1I = DVDP1
      IF (FLAG == 1) RHO1   = MAS1/V1
      MUP1 = RHO1/RHO10
      MU1 = MUP1 - ONE
      AA = (C41 + C51*MU1)/V10
      BB = HALF*(V1-V1OLD)
      IF (FLAG == 1)  EINT1  = EINT1 - (P1I+PEXT+PEXT)*BB
      IF (FLAG == 1) THEN
        P1 = ( C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + AA*EINT1 )  /  (ONE+AA*BB)
      ELSE
        P1 = ( C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + AA*EINT1 )
      ENDIF
      P1 = MAX(P1,PM1)
      IF (FLAG == 1)  EINT1  = EINT1 - P1*BB
      IF (FLAG == 1)  EINT1  = MAX(EINT1, E_INF*V10)

      DPDV1 = (-(C11 + (C21 + C21 + THREE*C31*MU1)*MAX(ZERO,MU1) + C51*EINT1/V10 )*MUP1*MUP1 -(C41 + C51*MU1)*(P1+PEXT) ) /  V10
      SSP1 = (-DPDV1*V1/MUP1 + TWO_THIRD*GG1)/RHO10
      SSP1 = SQRT(ABS(SSP1))

      IF(ABS(DPDV1)<EM20)THEN
        DVDP1 = ZERO
      ELSE
        DVDP1 = ONE/DPDV1
      ENDIF

      RETURN
      END
