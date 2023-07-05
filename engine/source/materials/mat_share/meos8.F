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
Chd|  MEOS8                         source/materials/mat_share/meos8.F
Chd|-- called by -----------
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|-- calls ---------------
Chd|        MQVISC8                       source/materials/mat_share/mqvisc8.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MEOS8(
     1   PM,      OFF,     SIG,     EINT,
     2   RHO,     QOLD,    VOL,     RK,
     3   T,       RE,      STIFN,   NEL,
     4   D1,      D2,      D3,      VNEW,
     5   DELTAX,  RHO0,    DVOL,    VD2,
     6   VIS,     MAT,     NC,      NGL,
     7   GEO,     PID,     DT2T,    NELTST,
     8   ITYPTST, OFFG,    MSSA,    DMELS,
     9   BUFLY,   SSP,     ITY,     NPT,
     A   JTUR,    JTHE,    JSMS)
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
     . PM(NPROPM,*),
     . OFF(MVSIZ) ,SIG(NEL,6), EINT(NEL), RHO(NEL) , QOLD(NEL),
     . VOL(NEL) ,RK(*)       , T(*)       , RE(*)      , STIFN(*)   ,
     . D1(*)      , D2(*)      , D3(*)      ,
     . VNEW(MVSIZ), DELTAX(MVSIZ)           , RHO0(MVSIZ), DVOL(MVSIZ),
     . VD2(MVSIZ) , VIS(MVSIZ),GEO(NPROPG,*), DT2T, OFFG(*),
     . MSSA(*)    , DMELS(*)   , SSP(MVSIZ)
      TYPE (BUF_LAY_), TARGET :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JPT,IPT,MX,JJ(6)
C     REAL
      my_real
     .   AMU(MVSIZ), AMU2(MVSIZ), ESPE(MVSIZ),
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   C5(MVSIZ), C6(MVSIZ), G(MVSIZ) , POLD(MVSIZ), DPDM(MVSIZ),
     .   A(MVSIZ) , B(MVSIZ) , PC(MVSIZ), PNEW(MVSIZ), DF(MVSIZ),
     .   PSH(MVSIZ),
     .   DVV     
      my_real,
     .  DIMENSION(:), POINTER :: SIGP
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C=======================================================================
      MX     =MAT(1)
C
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
C
      DO I=1,NEL
        G(I)   =PM(22,MX)
        C1(I)  =PM(31,MX)
        C2(I)  =PM(32,MX)
        C3(I)  =PM(33,MX)
        C4(I)  =PM(34,MX)
        C5(I)  =PM(35,MX)
        C6(I)  =PM(36,MX)
        PC(I)  =PM(37,MX)
        PSH(I) =PM(88,MX)
      ENDDO
C
      DO I=1,NEL
        POLD(I)=(SIG(I,1)+SIG(I,2)+SIG(I,3)) * THIRD
        SIG(I,1)=ZERO
        SIG(I,2)=ZERO
        SIG(I,3)=ZERO
        SIG(I,4)=ZERO
        SIG(I,5)=ZERO
        SIG(I,6)=ZERO
      ENDDO
C
      DO I=1,NEL
        DF(I)  =RHO0(I)/RHO(I)
        AMU(I) =ONE/DF(I)-ONE
        AMU2(I)= MAX(ZERO,AMU(I))**2
        ESPE(I)=DF(I)*EINT(I)/ MAX(EM15,VNEW(I))
      ENDDO
C--------------------------------------------------
C     VITESSE DU SON APPROCHEE COURANTE
C--------------------------------------------------
      DO I=1,NEL
        DPDM(I)=ONEP333*G(I)
     .    +C2(I)+TWO*C3(I)*MAX(ZERO,AMU(I))+THREE*C4(I)*AMU(I)*AMU(I)
     .         +C6(I)*ESPE(I)
     .         +(C5(I)+C6(I)*AMU(I))*DF(I)*DF(I)*
     .  (PSH(I)+C1(I)+(C2(I)+C4(I)*AMU(I)*AMU(I))*AMU(I)+C3(I)*AMU2(I)+
     .         (C5(I)+C6(I)*AMU(I))*ESPE(I))
      ENDDO
C
      DO I=1,NEL
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C--------------------------------------------------
C     VISCOSITE VOLUMETRIQUE ET PAS DE TEMPS
C--------------------------------------------------
      CALL MQVISC8(
     1   PM,      OFF,     RHO,     RK,
     2   T,       RE,      STIFN,   EINT,
     3   D1,      D2,      D3,      VNEW,
     4   DVOL,    VD2,     DELTAX,  VIS,
     5   QOLD,    SSP,     MAT,     NC,
     6   NGL,     GEO,     PID,     DT2T,
     7   NELTST,  ITYPTST, OFFG,    MSSA,
     8   DMELS,   NEL,     ITY,     JTUR,
     9   JTHE,    JSMS)
C--------------------------------------------------
C     PRESSION
C--------------------------------------------------
      DO I=1,NEL
        EINT(I)=EINT(I)+HALF*DVOL(I)*(POLD(I)-PSH(I))*OFF(I)
        ESPE(I)=DF(I)*EINT(I)/ MAX(EM15,VNEW(I))
      ENDDO
C
      DO I=1,NEL
        A(I)=C1(I)+AMU(I)*(C2(I)+C4(I)*AMU(I)*AMU(I))+C3(I)*AMU2(I)
        B(I)=C5(I)+C6(I)*AMU(I)
      ENDDO
C
      DO I=1,NEL
        DVV=HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        PNEW(I)=(A(I)+(ESPE(I)-PSH(I)*DVV)*B(I))/(ONE + B(I)*DVV)
        PNEW(I)= MAX(PNEW(I),PC(I))*OFF(I)
      ENDDO
C
      DO I=1,NEL
        EINT(I)=EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I))
      ENDDO
C--------------------------------------------------
C     CONTRIBUTION VOLUMIQUE AUX POINTS DE GAUSS
C--------------------------------------------------
      DO IPT=1,NPT
        LBUF => BUFLY%LBUF(1,1,IPT)
        SIGP => BUFLY%LBUF(1,1,IPT)%SIG(1:NEL*6)
        JPT=(IPT-1)*NEL
        DO I=1,NEL
          SIGP(JJ(1)+I)= SIGP(JJ(1)+I)-PNEW(I)
          SIGP(JJ(2)+I)= SIGP(JJ(2)+I)-PNEW(I)
          SIGP(JJ(3)+I)= SIGP(JJ(3)+I)-PNEW(I)
          SIGP(JJ(4)+I)= SIGP(JJ(4)+I)        
          SIGP(JJ(5)+I)= SIGP(JJ(5)+I)        
          SIGP(JJ(6)+I)= SIGP(JJ(6)+I)       
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
      ENDDO ! DO IPT=1,NPT
C---------------------------------------------
C
C  DIVISION PAR LE VOLUME POUR L'ALE <(:o))=
C  VOL EST LE VOLUME INITIAL EN LAG.
C
C---------------------------------------------
      DO I=1,NEL
        EINT(I)=EINT(I) / MAX(EM15,VOL(I))
      ENDDO
C-----------
      RETURN
      END
