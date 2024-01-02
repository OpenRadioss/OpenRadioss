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
Chd|  QHVIS2                        source/elements/solid_2d/quad/qhvis2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE QHVIS2(PM,OFF,RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24,
     .                  AREA,CXX,MAT,VD2,VIS,EANI,PID,GEO,
     .                  PARTSAV,IPARTQ,EHOU, IPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "cong1_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real PM(NPROPM,NUMMAT),GEO(NPROPG,NUMGEO), OFF(*), RHO(*),EANI(*),
     .        Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .        VY1(*), VY2(*), VY3(*), VY4(*), VZ1(*), VZ2(*), VZ3(*),
     .        PY1(*), PY2(*), PZ1(*), PZ2(*),EHOU(*),
     .        T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*),
     .        VZ4(*), AREA(*), CXX(*),VD2(*),VIS(*), PARTSAV(NPSAV,*)
      INTEGER MAT(*),PID(*),IPARTQ(NUMELQ), IPARG(63:63)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX, ISFLUID
      my_real CAQ(MVSIZ),
     .        FCL(MVSIZ) , FCQ(MVSIZ),
     .        G11(MVSIZ) , G21(MVSIZ) , G31(MVSIZ) , G41(MVSIZ),
     .        HGY(MVSIZ), HGZ(MVSIZ),
     .        HY,HZ,FAC,PX1H1,PX2H1,EHOURT, ARE
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
      IF(MTN == 11 .OR. ((MTN == 17 .OR. MTN == 47) .AND. ALE%UPWIND%UPWM == 0))THEN
       DO I=LFT,LLT
         T11(I) = ZERO
         T12(I) = ZERO
         T13(I) = ZERO
         T14(I) = ZERO
         T21(I) = ZERO
         T22(I) = ZERO
         T23(I) = ZERO
         T24(I) = ZERO 
       ENDDO
       RETURN
      ENDIF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(INVSTR >= 35)THEN
        DO I=LFT,LLT
          CAQ(I)=GEO(13,PID(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
          CAQ(I)=PM(4,MAT(I))
        ENDDO
      ENDIF

      DO I=LFT,LLT
        ARE=MAX(AREA(I),EM20)
        FCQ(I)=RHO(I)*SQRT(ARE)
        FCL(I)=CAQ(I)*FCQ(I)
      ENDDO

      ISFLUID=IPARG(63)
 
      IF(ISFLUID == 1 .AND. ALE%UPWIND%UPWM == 0)THEN
       DO I=LFT,LLT
         FCL(I)=FCL(I)*CXX(I)
         FCQ(I)=ZERO
       ENDDO
      ELSEIF(ISFLUID == 1 .AND. ALE%UPWIND%UPWM == 1)THEN
       DO I=LFT,LLT
         FCL(I)=MIN(FCL(I)*CXX(I),MAX(20.*CAQ(I)*VIS(I),FCL(I)*SQRT(VD2(I))))
         FCQ(I)=ZERO
       ENDDO
      ELSEIF(ISFLUID == 1 .AND. ALE%UPWIND%UPWM > 0)THEN
       DO I=LFT,LLT
        IF(VIS(I) > ZERO)THEN
          FCQ(I)=ZERO
          FCL(I)=TWENTY*CAQ(I)*VIS(I)
        ELSE
          FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
          FCL(I)=FCL(I)*CXX(I)
        ENDIF
       ENDDO
      ELSE
       DO I=LFT,LLT
         FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
         FCL(I)=FCL(I)*CXX(I)
       ENDDO
      ENDIF
      IF(IMPL /= ZERO)THEN
       DO I=LFT,LLT
         FCQ(I)=ZERO
       ENDDO
      ENDIF

      DO I=LFT,LLT
       IF(OFF(I) < ONE)THEN
        FCL(I)=ZERO
        FCQ(I)=ZERO
       ENDIF
      ENDDO

      IF(JHBE == 0)THEN
       !
       ! HOURGLASS HALLQUIST
       !
       DO I=LFT,LLT
         HGY(I)=HALF*(VY1(I)-VY2(I)+VY3(I)-VY4(I))
         HGZ(I)=HALF*(VZ1(I)-VZ2(I)+VZ3(I)-VZ4(I))
       ENDDO
       DO I=LFT,LLT
         T11(I)=HGY(I)*(FCL(I)+ABS(HGY(I))*FCQ(I))
         T12(I)=-T11(I)
         T13(I)= T11(I)
         T14(I)=-T11(I)
         T21(I)=HGZ(I)*(FCL(I)+ABS(HGZ(I))*FCQ(I))
         T22(I)=-T21(I)
         T23(I)= T21(I)
         T24(I)=-T21(I)
         EHOU(I)=  TWO*DT1*(T11(I)*HGY(I) + T21(I)*HGZ(I))
       ENDDO
      ELSE
        !
        ! HOURGLASS BELYTSCHKO
        !
        DO I=LFT,LLT
          HY=Y1(I)-Y2(I)+Y3(I)-Y4(I)
          HZ=Z1(I)-Z2(I)+Z3(I)-Z4(I)
          FAC=ONE/MAX(EM20,AREA(I))
          PX1H1=FAC*(PY1(I)*HY+PZ1(I)*HZ)
          PX2H1=FAC*(PY2(I)*HY+PZ2(I)*HZ)
          G11(I)= ONE -PX1H1
          G21(I)=-ONE -PX2H1
          G31(I)= ONE +PX1H1
          G41(I)=-ONE +PX2H1
        ENDDO
        DO I=LFT,LLT
          HGY(I)=HALF*(G11(I)*VY1(I)+G21(I)*VY2(I)+G31(I)*VY3(I)+G41(I)*VY4(I))
          HGZ(I)=HALF*(G11(I)*VZ1(I)+G21(I)*VZ2(I)+G31(I)*VZ3(I)+G41(I)*VZ4(I))
        ENDDO
        DO I=LFT,LLT
          HY=HGY(I)*(FCL(I)+ABS(HGY(I))*FCQ(I))
          HZ=HGZ(I)*(FCL(I)+ABS(HGZ(I))*FCQ(I))
          T11(I) =G11(I)*HY
          T12(I) =G21(I)*HY
          T13(I) =G31(I)*HY
          T14(I) =G41(I)*HY
          T21(I) =G11(I)*HZ
          T22(I) =G21(I)*HZ
          T23(I) =G31(I)*HZ
          T24(I) =G41(I)*HZ
          EHOU(I)=  TWO*DT1*(HY*HGY(I) + HZ*HGZ(I))
        ENDDO
      ENDIF

      IF(JLAG == 1)THEN
        EHOURT = ZERO
        DO I=LFT,LLT
          EHOURT= EHOURT+EHOU(I)
        ENDDO
        DO I=LFT,LLT
          MX = IPARTQ(I)
          PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
        ENDDO
#include "atomic.inc"
        EHOUR = EHOUR + EHOURT
#include "atomend.inc"
      ENDIF !JLAG

      ! OUTPUT (ANIM & H3D)
      DO I=LFT,LLT
        EANI(NFT+I) = EANI(NFT+I)+EHOU(I)/MAX(EM30,RHO(I)*AREA(I))
      ENDDO

C-----------------------------------------------
      RETURN
      END
