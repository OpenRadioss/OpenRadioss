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
Chd|  SHVIS3                        source/elements/solid/solide/shvis3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE SHVIS3(
     1   PM,       RHO,      OFF,      VX1,
     2   VX2,      VX3,      VX4,      VX5,
     3   VX6,      VX7,      VX8,      VY1,
     4   VY2,      VY3,      VY4,      VY5,
     5   VY6,      VY7,      VY8,      VZ1,
     6   VZ2,      VZ3,      VZ4,      VZ5,
     7   VZ6,      VZ7,      VZ8,      F11,
     8   F21,      F31,      F12,      F22,
     9   F32,      F13,      F23,      F33,
     A   F14,      F24,      F34,      F15,
     B   F25,      F35,      F16,      F26,
     C   F36,      F17,      F27,      F37,
     D   F18,      F28,      F38,      PX1H1,
     E   PX1H2,    PX1H3,    PX2H1,    PX2H2,
     F   PX2H3,    PX3H1,    PX3H2,    PX3H3,
     G   PX4H1,    PX4H2,    PX4H3,    VOL,
     H   MAT,      CXX,      VIS,      VD2,
     I   DELTAX,   EANI,     PID,      GEO,
     J   PARTSAV,  IPARTS,   OFFG,     VOL0,
     K   IPARG1,   IFVM_SKIP,NEL,      NFT,
     L   MTN,      ISMSTR,   JLAG,     JHBE)
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
C   C o m m o n   B l o c k sj
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: JHBE
      INTEGER MAT(*),PID(*),IPARTS(*), IPARG1(*)
      my_real
     .   PM(NPROPM,NUMMAT),GEO(NPROPG,NUMGEO), RHO(*),OFF(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),EANI(*),PARTSAV(NPSAV,*),
     .   VOL(*),CXX(*),VIS(*),VD2(*),DELTAX(*) ,OFFG(*),VOL0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, FLUID,MX, J, II, IC, JST(MVSIZ+1), MT, IFVM_SKIP
      my_real ::
     .   CAQ(MVSIZ), FCL(MVSIZ), FCQ(MVSIZ),EHOU(MVSIZ),
     .   HX1(MVSIZ), HX2(MVSIZ), HX3(MVSIZ), HX4(MVSIZ),
     .   HY1(MVSIZ), HY2(MVSIZ), HY3(MVSIZ), HY4(MVSIZ),
     .   HZ1(MVSIZ), HZ2(MVSIZ), HZ3(MVSIZ), HZ4(MVSIZ),
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
     .   G13(MVSIZ),G23(MVSIZ),G33(MVSIZ),G43(MVSIZ),
     .   G53(MVSIZ),G63(MVSIZ),G73(MVSIZ),G83(MVSIZ),
     .   LEN(MVSIZ),RHO0(MVSIZ),
     .   EHOURT,AA
!       SP issue, need double precision for these variables   
        REAL(kind=8) ::
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ)
        REAL(kind=8) ::
     .   VX3478, VX2358, VX1467, VX1256, 
     .   VY3478, VY2358, VY1467, VY1256,
     .   VZ3478, VZ2358, VZ1467, VZ1256
C-----------------------------------------------
      
      IF(IPARG1(64)==1 .OR. (MTN==17 .AND. ALE%UPWIND%UPWM<2) .OR. INT22>0 .OR. IFVM_SKIP==1)THEN
C
       DO I=1,NEL

        F11(I) =ZERO
        F12(I) =ZERO
        F13(I) =ZERO
        F14(I) =ZERO
        F15(I) =ZERO
        F16(I) =ZERO
        F17(I) =ZERO
        F18(I) =ZERO
C
        F21(I) =ZERO
        F22(I) =ZERO
        F23(I) =ZERO
        F24(I) =ZERO
        F25(I) =ZERO
        F26(I) =ZERO
        F27(I) =ZERO
        F28(I) =ZERO
C
        F31(I) =ZERO
        F32(I) =ZERO
        F33(I) =ZERO
        F34(I) =ZERO
        F35(I) =ZERO
        F36(I) =ZERO
        F37(I) =ZERO
        F38(I) =ZERO
       ENDDO
C
       RETURN
      ENDIF
C
      IF(INVSTR>=35)THEN
        MT = PID(1)
        DO I=1,NEL
          CAQ(I)=FOURTH*OFF(I)*GEO(13,MT)
        ENDDO
      ELSE
        MX = MAT(1)
        DO I=1,NEL
          CAQ(I)=FOURTH*OFF(I)*PM(4,MX)
        ENDDO
      ENDIF
C
      FLUID=IPARG1(63)
C MATERIAUX FLUIDES 
      IF(FLUID==1)THEN
       IF(ALE%UPWIND%UPWM>1 .OR. JLAG==1)THEN
        DO I=1,NEL
         IF(VIS(I)>ZERO)THEN
          FCQ(I)=ZERO
          FCL(I)=TEN*CAQ(I)*VIS(I)*DELTAX(I)
         ELSE
          FCQ(I)=ZERO
          FCL(I)=CAQ(I)*RHO(I)*CXX(I)*DELTAX(I)**2
         ENDIF
        ENDDO
       ELSEIF(ALE%UPWIND%UPWM==0)THEN
        DO I=1,NEL
          FCL(I)=CAQ(I)*RHO(I)*CXX(I)*VOL(I)**TWO_THIRD
          FCQ(I)=ZERO
        ENDDO
       ELSEIF(ALE%UPWIND%UPWM==1)THEN
        DO I=1,NEL
          FCL(I)=CAQ(I)*RHO(I)*DELTAX(I)**2
          FCL(I)=MIN(FCL(I)*CXX(I),MAX(TEN*CAQ(I)*VIS(I)*DELTAX(I),FCL(I)*SQRT(VD2(I))))
          FCQ(I)=ZERO
        ENDDO
       ENDIF
      ELSE
C MATERIAUX NON FLUIDES 
       IF(ISMSTR == 1)  THEN
         MX = MAT(1)
         DO I=1,NEL
           RHO0(I) = PM(1,MX)
         END DO
         DO I=1,NEL
           FCL(I)=CAQ(I)*RHO0(I)*VOL(I)**TWO_THIRD     
           FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
           FCL(I)=FCL(I)*CXX(I)
         ENDDO
       ELSEIF(ISMSTR == 2) THEN
         MX = MAT (1) 
         DO I=1,NEL
          IF(OFFG(I) > ONE) THEN
           RHO0(I) = PM(1,MX)
          END IF
         END DO
         DO I=1,NEL
          IF(OFFG(I) > ONE) THEN
           AA = RHO0(I)*VOL0(I)/MAX(EM20,VOL(I))
           FCL(I)=CAQ(I)*AA*VOL(I)**TWO_THIRD
           FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
           FCL(I)=FCL(I)*CXX(I)
          ELSE 
           FCL(I)=CAQ(I)*RHO(I)*VOL(I)**TWO_THIRD
           FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
           FCL(I)=FCL(I)*CXX(I)
          ENDIF 
         ENDDO
       ELSE
         DO I=1,NEL
           FCL(I)=CAQ(I)*RHO(I)*VOL(I)**TWO_THIRD
           FCQ(I)=FCL(I)*CAQ(I)*HUNDRED
           FCL(I)=FCL(I)*CXX(I)
         ENDDO
       ENDIF
      END IF
      IF(JHBE==0)THEN
       DO I=1,NEL
        VX3478=VX3(I)-VX4(I)-VX7(I)+VX8(I)
        VX2358=VX2(I)-VX3(I)-VX5(I)+VX8(I)
        VX1467=VX1(I)-VX4(I)-VX6(I)+VX7(I)
        VX1256=VX1(I)-VX2(I)-VX5(I)+VX6(I)
C
        VY3478=VY3(I)-VY4(I)-VY7(I)+VY8(I)
        VY2358=VY2(I)-VY3(I)-VY5(I)+VY8(I)
        VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
        VY1256=VY1(I)-VY2(I)-VY5(I)+VY6(I)
C
        VZ3478=VZ3(I)-VZ4(I)-VZ7(I)+VZ8(I)
        VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
        VZ1467=VZ1(I)-VZ4(I)-VZ6(I)+VZ7(I)
        VZ1256=VZ1(I)-VZ2(I)-VZ5(I)+VZ6(I)
C
        HGX1(I)=VX1467-VX2358
        HGX2(I)=VX1467+VX2358
        HGX3(I)=VX1256-VX3478
        HGX4(I)=VX1256+VX3478
C
        HGY1(I)=VY1467-VY2358
        HGY2(I)=VY1467+VY2358
        HGY3(I)=VY1256-VY3478
        HGY4(I)=VY1256+VY3478
C
        HGZ1(I)=VZ1467-VZ2358
        HGZ2(I)=VZ1467+VZ2358
        HGZ3(I)=VZ1256-VZ3478
        HGZ4(I)=VZ1256+VZ3478
       ENDDO
C
       DO I=1,NEL
        HX1(I)=HGX1(I)*(FCL(I)+ABS(HGX1(I))*FCQ(I))
        HX2(I)=HGX2(I)*(FCL(I)+ABS(HGX2(I))*FCQ(I))
        HX3(I)=HGX3(I)*(FCL(I)+ABS(HGX3(I))*FCQ(I))
        HX4(I)=HGX4(I)*(FCL(I)+ABS(HGX4(I))*FCQ(I))
C
        HY1(I)=HGY1(I)*(FCL(I)+ABS(HGY1(I))*FCQ(I))
        HY2(I)=HGY2(I)*(FCL(I)+ABS(HGY2(I))*FCQ(I))
        HY3(I)=HGY3(I)*(FCL(I)+ABS(HGY3(I))*FCQ(I))
        HY4(I)=HGY4(I)*(FCL(I)+ABS(HGY4(I))*FCQ(I))
C
        HZ1(I)=HGZ1(I)*(FCL(I)+ABS(HGZ1(I))*FCQ(I))
        HZ2(I)=HGZ2(I)*(FCL(I)+ABS(HGZ2(I))*FCQ(I))
        HZ3(I)=HGZ3(I)*(FCL(I)+ABS(HGZ3(I))*FCQ(I))
        HZ4(I)=HGZ4(I)*(FCL(I)+ABS(HGZ4(I))*FCQ(I))
       ENDDO
C
       DO  I=1,NEL
        F11(I) =-HX1(I)-HX2(I)-HX3(I)-HX4(I)
        F12(I) = HX1(I)-HX2(I)+HX3(I)+HX4(I)
        F13(I) =-HX1(I)+HX2(I)+HX3(I)-HX4(I)
        F14(I) = HX1(I)+HX2(I)-HX3(I)+HX4(I)
        F15(I) =-HX1(I)+HX2(I)+HX3(I)+HX4(I)
        F16(I) = HX1(I)+HX2(I)-HX3(I)-HX4(I)
        F17(I) =-HX1(I)-HX2(I)-HX3(I)+HX4(I)
        F18(I) = HX1(I)-HX2(I)+HX3(I)-HX4(I)
C
        F21(I) =-HY1(I)-HY2(I)-HY3(I)-HY4(I)
        F22(I) = HY1(I)-HY2(I)+HY3(I)+HY4(I)
        F23(I) =-HY1(I)+HY2(I)+HY3(I)-HY4(I)
        F24(I) = HY1(I)+HY2(I)-HY3(I)+HY4(I)
        F25(I) =-HY1(I)+HY2(I)+HY3(I)+HY4(I)
        F26(I) = HY1(I)+HY2(I)-HY3(I)-HY4(I)
        F27(I) =-HY1(I)-HY2(I)-HY3(I)+HY4(I)
        F28(I) = HY1(I)-HY2(I)+HY3(I)-HY4(I)
C
        F31(I) =-HZ1(I)-HZ2(I)-HZ3(I)-HZ4(I)
        F32(I) = HZ1(I)-HZ2(I)+HZ3(I)+HZ4(I)
        F33(I) =-HZ1(I)+HZ2(I)+HZ3(I)-HZ4(I)
        F34(I) = HZ1(I)+HZ2(I)-HZ3(I)+HZ4(I)
        F35(I) =-HZ1(I)+HZ2(I)+HZ3(I)+HZ4(I)
        F36(I) = HZ1(I)+HZ2(I)-HZ3(I)-HZ4(I)
        F37(I) =-HZ1(I)-HZ2(I)-HZ3(I)+HZ4(I)
        F38(I) = HZ1(I)-HZ2(I)+HZ3(I)-HZ4(I)
       ENDDO
      ELSEIF(JHBE>=1) THEN
       DO I=1,NEL
        G11(I)= ONE-PX1H1(I)
        G21(I)=-ONE-PX2H1(I)
        G31(I)= ONE-PX3H1(I)
        G41(I)=-ONE-PX4H1(I)
        G51(I)= ONE+PX3H1(I)
        G61(I)=-ONE+PX4H1(I)
        G71(I)= ONE+PX1H1(I)
        G81(I)=-ONE+PX2H1(I)
        HGX1(I) = G11(I)*VX1(I)+G21(I)*VX2(I)+G31(I)*VX3(I)+G41(I)*VX4(I)+G51(I)*VX5(I)+G61(I)*VX6(I)+G71(I)*VX7(I)+G81(I)*VX8(I)
        HGY1(I) = G11(I)*VY1(I)+G21(I)*VY2(I)+G31(I)*VY3(I)+G41(I)*VY4(I)+G51(I)*VY5(I)+G61(I)*VY6(I)+G71(I)*VY7(I)+G81(I)*VY8(I)
        HGZ1(I) = G11(I)*VZ1(I)+G21(I)*VZ2(I)+G31(I)*VZ3(I)+G41(I)*VZ4(I)+G51(I)*VZ5(I)+G61(I)*VZ6(I)+G71(I)*VZ7(I)+G81(I)*VZ8(I)
       ENDDO
C
       DO I=1,NEL
        G12(I)= ONE-PX1H2(I)
        G22(I)= ONE-PX2H2(I)
        G32(I)=-ONE-PX3H2(I)
        G42(I)=-ONE-PX4H2(I)
        G52(I)=-ONE+PX3H2(I)
        G62(I)=-ONE+PX4H2(I)
        G72(I)= ONE+PX1H2(I)
        G82(I)= ONE+PX2H2(I)
        HGX2(I)=G12(I)*VX1(I)+G22(I)*VX2(I)+G32(I)*VX3(I)+G42(I)*VX4(I)+G52(I)*VX5(I)+G62(I)*VX6(I)+G72(I)*VX7(I)+G82(I)*VX8(I)
        HGY2(I)=G12(I)*VY1(I)+G22(I)*VY2(I)+G32(I)*VY3(I)+G42(I)*VY4(I)+G52(I)*VY5(I)+G62(I)*VY6(I)+G72(I)*VY7(I)+G82(I)*VY8(I)
        HGZ2(I)=G12(I)*VZ1(I)+G22(I)*VZ2(I)+G32(I)*VZ3(I)+G42(I)*VZ4(I)+G52(I)*VZ5(I)+G62(I)*VZ6(I)+G72(I)*VZ7(I)+G82(I)*VZ8(I)
       ENDDO
C
       DO I=1,NEL
        G13(I)= ONE-PX1H3(I)
        G23(I)=-ONE-PX2H3(I)
        G33(I)=-ONE-PX3H3(I)
        G43(I)= ONE-PX4H3(I)
        G53(I)=-ONE+PX3H3(I)
        G63(I)= ONE+PX4H3(I)
        G73(I)= ONE+PX1H3(I)
        G83(I)=-ONE+PX2H3(I)
        HGX3(I)=G13(I)*VX1(I)+G23(I)*VX2(I)+G33(I)*VX3(I)+G43(I)*VX4(I)+G53(I)*VX5(I)+G63(I)*VX6(I)+G73(I)*VX7(I)+G83(I)*VX8(I)
        HGY3(I)=G13(I)*VY1(I)+G23(I)*VY2(I)+G33(I)*VY3(I)+G43(I)*VY4(I)+G53(I)*VY5(I)+G63(I)*VY6(I)+G73(I)*VY7(I)+G83(I)*VY8(I)
        HGZ3(I)=G13(I)*VZ1(I)+G23(I)*VZ2(I)+G33(I)*VZ3(I)+G43(I)*VZ4(I)+G53(I)*VZ5(I)+G63(I)*VZ6(I)+G73(I)*VZ7(I)+G83(I)*VZ8(I)
       ENDDO       
C
       DO I=1,NEL
C 1 -1 1 -1 -1 1 -1 1
         HGX4(I)=VX1(I)-VX2(I)+VX3(I)-VX4(I)-VX5(I)+VX6(I)-VX7(I)+VX8(I)
         HGY4(I)=VY1(I)-VY2(I)+VY3(I)-VY4(I)-VY5(I)+VY6(I)-VY7(I)+VY8(I)
         HGZ4(I)=VZ1(I)-VZ2(I)+VZ3(I)-VZ4(I)-VZ5(I)+VZ6(I)-VZ7(I)+VZ8(I)
       ENDDO
C 
       DO I=1,NEL
        HX1(I)=HGX1(I)*(FCL(I)+ABS(HGX1(I))*FCQ(I))
        HX2(I)=HGX2(I)*(FCL(I)+ABS(HGX2(I))*FCQ(I))
        HX3(I)=HGX3(I)*(FCL(I)+ABS(HGX3(I))*FCQ(I))
        HX4(I)=HGX4(I)*(FCL(I)+ABS(HGX4(I))*FCQ(I))
C
        HY1(I)=HGY1(I)*(FCL(I)+ABS(HGY1(I))*FCQ(I))
        HY2(I)=HGY2(I)*(FCL(I)+ABS(HGY2(I))*FCQ(I))
        HY3(I)=HGY3(I)*(FCL(I)+ABS(HGY3(I))*FCQ(I))
        HY4(I)=HGY4(I)*(FCL(I)+ABS(HGY4(I))*FCQ(I))
C
        HZ1(I)=HGZ1(I)*(FCL(I)+ABS(HGZ1(I))*FCQ(I))
        HZ2(I)=HGZ2(I)*(FCL(I)+ABS(HGZ2(I))*FCQ(I))
        HZ3(I)=HGZ3(I)*(FCL(I)+ABS(HGZ3(I))*FCQ(I))
        HZ4(I)=HGZ4(I)*(FCL(I)+ABS(HGZ4(I))*FCQ(I))
       ENDDO
C
       DO I=1,NEL
        F11(I) =-G11(I)*HX1(I)-G12(I)*HX2(I)-G13(I)*HX3(I)-HX4(I)
        F12(I) =-G21(I)*HX1(I)-G22(I)*HX2(I)-G23(I)*HX3(I)+HX4(I)
        F13(I) =-G31(I)*HX1(I)-G32(I)*HX2(I)-G33(I)*HX3(I)-HX4(I)
        F14(I) =-G41(I)*HX1(I)-G42(I)*HX2(I)-G43(I)*HX3(I)+HX4(I)
        F15(I) =-G51(I)*HX1(I)-G52(I)*HX2(I)-G53(I)*HX3(I)+HX4(I)
        F16(I) =-G61(I)*HX1(I)-G62(I)*HX2(I)-G63(I)*HX3(I)-HX4(I)
        F17(I) =-G71(I)*HX1(I)-G72(I)*HX2(I)-G73(I)*HX3(I)+HX4(I)
        F18(I) =-G81(I)*HX1(I)-G82(I)*HX2(I)-G83(I)*HX3(I)-HX4(I)
C
        F21(I) =-G11(I)*HY1(I)-G12(I)*HY2(I)-G13(I)*HY3(I)-HY4(I)
        F22(I) =-G21(I)*HY1(I)-G22(I)*HY2(I)-G23(I)*HY3(I)+HY4(I)
        F23(I) =-G31(I)*HY1(I)-G32(I)*HY2(I)-G33(I)*HY3(I)-HY4(I)
        F24(I) =-G41(I)*HY1(I)-G42(I)*HY2(I)-G43(I)*HY3(I)+HY4(I)
        F25(I) =-G51(I)*HY1(I)-G52(I)*HY2(I)-G53(I)*HY3(I)+HY4(I)
        F26(I) =-G61(I)*HY1(I)-G62(I)*HY2(I)-G63(I)*HY3(I)-HY4(I)
        F27(I) =-G71(I)*HY1(I)-G72(I)*HY2(I)-G73(I)*HY3(I)+HY4(I)
        F28(I) =-G81(I)*HY1(I)-G82(I)*HY2(I)-G83(I)*HY3(I)-HY4(I)
C
        F31(I) =-G11(I)*HZ1(I)-G12(I)*HZ2(I)-G13(I)*HZ3(I)-HZ4(I)
        F32(I) =-G21(I)*HZ1(I)-G22(I)*HZ2(I)-G23(I)*HZ3(I)+HZ4(I)
        F33(I) =-G31(I)*HZ1(I)-G32(I)*HZ2(I)-G33(I)*HZ3(I)-HZ4(I)
        F34(I) =-G41(I)*HZ1(I)-G42(I)*HZ2(I)-G43(I)*HZ3(I)+HZ4(I)
        F35(I) =-G51(I)*HZ1(I)-G52(I)*HZ2(I)-G53(I)*HZ3(I)+HZ4(I)
        F36(I) =-G61(I)*HZ1(I)-G62(I)*HZ2(I)-G63(I)*HZ3(I)-HZ4(I)
        F37(I) =-G71(I)*HZ1(I)-G72(I)*HZ2(I)-G73(I)*HZ3(I)+HZ4(I)
        F38(I) =-G81(I)*HZ1(I)-G82(I)*HZ2(I)-G83(I)*HZ3(I)-HZ4(I)
       ENDDO
      ENDIF

      DO I=1,NEL
        EHOU(I)= DT1*(
     &   HZ1(I)*HGZ1(I) + HZ2(I)*HGZ2(I) + 
     &   HZ3(I)*HGZ3(I) + HZ4(I)*HGZ4(I) + 
     &   HX1(I)*HGX1(I) + HX2(I)*HGX2(I) + 
     &   HX3(I)*HGX3(I) + HX4(I)*HGX4(I) + 
     &   HY1(I)*HGY1(I) + HY2(I)*HGY2(I) + 
     &   HY3(I)*HGY3(I) + HY4(I)*HGY4(I) )
      END DO
C
      EHOURT = ZERO
      DO I=1,NEL     
        EHOURT= EHOURT+EHOU(I)
      ENDDO   

C ALE/EULER : hourglassenergy is not calculated
C
      IF(JLAG==1)THEN
C
        IF (IVECTOR==0) THEN
          DO I=1,NEL
            MX = IPARTS(I)
            PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
          ENDDO
        ELSE
          IC=1
          JST(IC)=1
          DO J=1+1,NEL
            IF (IPARTS(J)/=IPARTS(J-1)) THEN
              IC=IC+1
              JST(IC)=J
            ENDIF
          ENDDO
          JST(IC+1)=NEL+1                 
          DO II=1,IC
            MX=IPARTS(JST(II))
            IF (JST(II+1)-JST(II)>15) THEN
#include "vectorize.inc"
              DO J=JST(II),JST(II+1)-1
                PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(J)
              ENDDO
            ELSE
              DO J=JST(II),JST(II+1)-1
                PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(J)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
#include "atomic.inc"
          EHOUR = EHOUR + EHOURT
#include "atomend.inc"
      ENDIF
C
       IF((ANIM_E(25)==1).OR.(OUTP_CS(25)==1))THEN
#include "vectorize.inc"
        DO I=1,NEL
          EANI(NFT+I) = EANI(NFT+I)+EHOU(I)/MAX(EM30,RHO(I)*VOL(I))
        ENDDO
      ENDIF
C
      RETURN
      END
