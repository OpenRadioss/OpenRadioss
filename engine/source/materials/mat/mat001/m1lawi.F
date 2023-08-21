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
Chd|  M1LAWI                        source/materials/mat/mat001/m1lawi.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M1LAWI(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     QOLD,    VOL,     STIFN,
     3   DT2T,    NELTST,  ITYPTST, OFFG,
     4   GEO,     PID,     MUMAX,   MAT,
     5   NGL,     SSP,     DVOL,    AIRE,
     6   VNEW,    VD2,     DELTAX,  VIS,
     7   D1,      D2,      D3,      D4,
     8   D5,      D6,      PNEW,    PSH,
     9   QNEW,    SSP_EQ,  SOLD1,   SOLD2,
     A   SOLD3,   SOLD4,   SOLD5,   SOLD6,
     B   MSSA,    DMELS,   CONDE,   AMU,
     C   VOL_AVG, DTEL,    G_DT,    NEL,
     D   IPM,     RHOREF,  RHOSP,   ITY,
     E   JTUR,    JTHE,    JSPH,    ISMSTR,
     F   JSMS,    NPG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JSMS
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JSPH,NPG
C
      INTEGER NELTST,ITYPTST,PID(*),G_DT,NEL
      INTEGER MAT(*),NGL(*), IPM(NPROPMI,*)
      my_real
     .   DT2T

      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   VOL(*),STIFN(*), OFFG(*),GEO(NPROPG,*),MUMAX(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   MSSA(*), DMELS(*),CONDE(*),AMU(*),VOL_AVG(*),DTEL(*), RHOREF(*), RHOSP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,IBID
      my_real
     .   RHO0(MVSIZ), 
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ),
     .   C1(MVSIZ),
     .   DF,DAV, DPDM, P,
     .   E1, E2, E3, E4, E5, E6, EINC, P2,
     .   BID1, BID2, BID3, DTA,  YM, DPDMP,FACQ0,
     .   RHO0_1,C1_1
      my_real
     .    lmb(mvsiz),epsm 
C-----------------------------------------------
      FACQ0 = ONE
      MX = MAT(1)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      DO 10 I=1,NEL
      RHO0(I) =RHO0_1
      G(I)    =PM(22,MX)*OFF(I)
      C1(I)   =C1_1
      lmb(i)  =(THREE*c1(i)-TWO*g(i))
   10 CONTINUE
C

      DO I=1,NEL
       G1(I)=DT1*G(I)
       G2(I)=TWO*G1(I)
       !-----------------------
       !     CALCUL DE DP/DRHO 
       !-----------------------
       !     P = C1 mu, mu = rho/rho0-1
       !     d(rho)/d(mu)  = rho0
       !-----------------------
       SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
       RHOSP(I)=RHO0(I)
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       DAV=-THIRD*(D1(I)+D2(I)+D3(I))
      ENDDO

      IF (JSPH==0)THEN
       CALL MQVISCB(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    SSP,     BID3,    STIFN,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   VOL,     MSSA,    DMELS,   IBID,
     9   FACQ0,   CONDE,   DTEL,    G_DT,
     A   IPM,     RHOREF,  RHOSP,   NEL,
     B   ITY,     ISMSTR,  JTUR,    JTHE,
     C   JSMS,    NPG )
      ELSE
       CALL MDTSPH(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    BID3,    STIFN,   DT2T,
     3   NELTST,  ITYPTST, OFFG,    GEO,
     4   PID,     MUMAX,   SSP,     VNEW,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QNEW,    SSP_EQ,
     8   G_DT,    DTEL,    NEL,     ITY,
     9   JTUR,    JTHE)
      ENDIF

      DTA =HALF*DT1

      DO I=1,NEL
       epsm = third*(D1(i)+d2(i)+d3(i))
       sig(i,1)=sig(i,1)+ (TWO*G(i)*d1(i)+lmb(i)*epsm) *dt1
       sig(i,2)=sig(i,2)+ (TWO*G(i)*d2(i)+lmb(i)*epsm) *dt1
       sig(i,3)=sig(i,3)+ (TWO*G(i)*d3(i)+lmb(i)*epsm) *dt1
       sig(i,4)=sig(i,4)+ G(i)*d4(i) *dt1
       sig(i,5)=sig(i,5)+ G(i)*d5(i) *dt1
       sig(i,6)=sig(i,6)+ G(i)*d6(i) *dt1
       pnew(i) =- (sig(i,1)+sig(i,2)+sig(i,3))
       P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+P2)
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+P2)
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+P2)
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - HALF*DVOL(I)*(QOLD(I)+QNEW(I)+P2)
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
       
      ENDDO

      RETURN
      END
