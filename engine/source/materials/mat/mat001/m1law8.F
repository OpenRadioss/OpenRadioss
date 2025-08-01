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
C LOI ELASTIQUE 8 PTS D'INTEGRATION
C***********************************************
!||====================================================================
!||    m1law8         ../engine/source/materials/mat/mat001/m1law8.F
!||--- called by ------------------------------------------------------
!||    mmain8         ../engine/source/materials/mat_share/mmain8.F
!||--- calls      -----------------------------------------------------
!||    mqvisc8        ../engine/source/materials/mat_share/mqvisc8.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE M1LAW8(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     QOLD,    VOL,     STIFN,
     3   NEL,     D1,      D2,      D3,
     4   D4,      D5,      D6,      VNEW,
     5   VOLGP,   DELTAX,  RHO0,    DVOL,
     6   VD2,     VIS,     MAT,     NC,
     7   NGL,     GEO,     PID,     DT2T,
     8   NELTST,  ITYPTST, OFFG,    MSSA,
     9   DMELS,   BUFLY,   SSP,     ITY,
     A   NPT,     JTUR,    JTHE,    JSMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
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
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JSMS
      INTEGER MAT(MVSIZ),NC(8,MVSIZ),NGL(MVSIZ),PID(MVSIZ)
      INTEGER NEL,NELTST,ITYPTST
C     REAL
      my_real
     .  PM(NPROPM,*), OFF(MVSIZ) ,SIG(NEL,6), EINT(NEL),
     .  RHO(NEL), QOLD(NEL), VOL(NEL) , STIFN(*)   ,
     .  D1(MVSIZ,*)          , D2(MVSIZ,*)           ,
     .  D3(MVSIZ,*)          , D4(MVSIZ,*)           , 
     .  D5(MVSIZ,*)          , D6(MVSIZ,*)           ,
     .  DELTAX(MVSIZ)          ,
     . VNEW(MVSIZ), RHO0(MVSIZ), DVOL(MVSIZ), VOLGP(MVSIZ,*),
     . VD2(MVSIZ) ,  VIS(MVSIZ),GEO(NPROPG,*),DT2T, OFFG(NEL),
     . MSSA(*)    ,DMELS(*)   ,SSP(MVSIZ) 
      TYPE (BUF_LAY_), TARGET :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,IPT,MX,JJ(6)
C     REAL
      my_real
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ),
     .   SOLD4(MVSIZ), SOLD5(MVSIZ), SOLD6(MVSIZ),
     .   G(MVSIZ)    , C1   , P(MVSIZ)    ,
     .   G1(MVSIZ)  , G2(MVSIZ),
     .   DF(MVSIZ)  , AMU(MVSIZ)   , EINC(MVSIZ) ,
     .   DPDM(MVSIZ), PNEW(MVSIZ) ,
     .   DTA,  DAV
      my_real,
     .  DIMENSION(:), POINTER :: SIGP
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C=======================================================================
      DTA = HALF*DT1
      MX=MAT(1)
      C1 = PM(32,MX)
      DO I=1,NEL
        G(I) = PM(22,MX)*OFF(I)
      ENDDO
C
      DO I=1,NEL
        DF(I)=RHO0(I)/RHO(I)
      ENDDO
C
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
C-----------------------
C     PRESSION ANCIENNE
C-----------------------
      DO I=1,NEL
        P(I) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        G1(I)=DT1*G(I)
        G2(I)=TWO*G1(I)
        AMU(I)=ONE/DF(I)-ONE
        SIG(I,1)=ZERO
        SIG(I,2)=ZERO
        SIG(I,3)=ZERO
        SIG(I,4)=ZERO
        SIG(I,5)=ZERO
        SIG(I,6)=ZERO
        EINC(I)=ZERO   
      ENDDO
C------------------------------
C     DP/DRHO ET VITESSE DU SON
C------------------------------
      DO I=1,NEL
        DPDM(I)=ONEP333*G(I)+C1
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C--------------------------------------------------
C     VISCOSITE VOLUMETRIQUE ET PAS DE TEMPS
C--------------------------------------------------
      CALL MQVISC8(
     1   PM,      OFF,     RHO,     VIS,
     2   VIS,     VIS,     STIFN,   EINT,
     3   D1,      D2,      D3,      VNEW,
     4   DVOL,    VD2,     DELTAX,  VIS,
     5   QOLD,    SSP,     MAT,     NC,
     6   NGL,     GEO,     PID,     DT2T,
     7   NELTST,  ITYPTST, OFFG,    MSSA,
     8   DMELS,   NEL,     ITY,     JTUR,
     9   JTHE,    JSMS)
C--------------------------------------------------
C     NOUVELLE PRESSION
C--------------------------------------------------
      DO I=1,NEL
        PNEW(I)=C1*AMU(I)
      ENDDO
C--------------------------------------------------
C     BOUCLE SUR LES POINTS DE GAUSS
C--------------------------------------------------
      DO IPT=1,NPT
        LBUF => BUFLY%LBUF(1,1,IPT)
        SIGP => BUFLY%LBUF(1,1,IPT)%SIG(1:NEL*6)
C
        DO I=1,NEL
          DAV=ONE - DVOL(I)/VNEW(I)
          SOLD1(I)=SIGP(JJ(1)+I)*DAV
          SOLD2(I)=SIGP(JJ(2)+I)*DAV
          SOLD3(I)=SIGP(JJ(3)+I)*DAV
          SOLD4(I)=SIGP(JJ(4)+I)*DAV
          SOLD5(I)=SIGP(JJ(5)+I)*DAV
          SOLD6(I)=SIGP(JJ(6)+I)*DAV
        ENDDO
C--------------------------------------------------
C     CONTRAINTES DEVIATORIQUES AUX POINTS DE GAUSS
C--------------------------------------------------
        DO I=1,NEL
          DAV=-THIRD*(D1(I,IPT)+D2(I,IPT)+D3(I,IPT))
          SIGP(JJ(1)+I)=SIGP(JJ(1)+I)+P(I)+G2(I)*(D1(I,IPT)+DAV)
          SIGP(JJ(2)+I)=SIGP(JJ(2)+I)+P(I)+G2(I)*(D2(I,IPT)+DAV)
          SIGP(JJ(3)+I)=SIGP(JJ(3)+I)+P(I)+G2(I)*(D3(I,IPT)+DAV)
          SIGP(JJ(4)+I)=SIGP(JJ(4)+I)    +G1(I)* D4(I,IPT)
          SIGP(JJ(5)+I)=SIGP(JJ(5)+I)    +G1(I)* D5(I,IPT)
          SIGP(JJ(6)+I)=SIGP(JJ(6)+I)    +G1(I)* D6(I,IPT)
        ENDDO
C--------------------------------------------------
C     CONTRAINTE AUX POINTS DE GAUSS
C--------------------------------------------------
        DO I=1,NEL
          SIGP(JJ(1)+I)=(SIGP(JJ(1)+I)-PNEW(I))*OFF(I)
          SIGP(JJ(2)+I)=(SIGP(JJ(2)+I)-PNEW(I))*OFF(I)
          SIGP(JJ(3)+I)=(SIGP(JJ(3)+I)-PNEW(I))*OFF(I)
          SIGP(JJ(4)+I)= SIGP(JJ(4)+I)         *OFF(I)
          SIGP(JJ(5)+I)= SIGP(JJ(5)+I)         *OFF(I)
          SIGP(JJ(6)+I)= SIGP(JJ(6)+I)         *OFF(I)
        ENDDO
C--------------------------------------------------
C     ENERGIE INTERNE
C--------------------------------------------------
        DO I=1,NEL
          DAV=VOLGP(I,IPT)*OFF(I)*DTA
          EINT(I)=EINT(I)+DAV*(D1(I,IPT)*(SOLD1(I)+SIGP(JJ(1)+I))+
     +                         D2(I,IPT)*(SOLD2(I)+SIGP(JJ(2)+I))+
     +                         D3(I,IPT)*(SOLD3(I)+SIGP(JJ(3)+I))+
     +                         D4(I,IPT)*(SOLD4(I)+SIGP(JJ(4)+I))+
     +                         D5(I,IPT)*(SOLD5(I)+SIGP(JJ(5)+I))+
     +                         D6(I,IPT)*(SOLD6(I)+SIGP(JJ(6)+I)))
        ENDDO
C--------------------------------------------------
C     CONTRAINTE MOYENNE (OUTPUT)
C--------------------------------------------------
        DO I=1,NEL
          SIG(I,1)=SIG(I,1)+ONE_OVER_8*SIGP(JJ(1)+I)
          SIG(I,2)=SIG(I,2)+ONE_OVER_8*SIGP(JJ(2)+I)
          SIG(I,3)=SIG(I,3)+ONE_OVER_8*SIGP(JJ(3)+I)
          SIG(I,4)=SIG(I,4)+ONE_OVER_8*SIGP(JJ(4)+I)
          SIG(I,5)=SIG(I,5)+ONE_OVER_8*SIGP(JJ(5)+I)
          SIG(I,6)=SIG(I,6)+ONE_OVER_8*SIGP(JJ(6)+I)     
        ENDDO
C
      ENDDO
C
      DO I=1,NEL
        EINT(I)=EINT(I)/MAX(EM15,VOL(I))
      ENDDO
C-----------
      RETURN
      END
