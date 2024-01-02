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
C calcul des forces sur un groupe d'elements multi-purpose.
Chd|====================================================================
Chd|  XFORC3                        source/elements/xelem/xforc3.F
Chd|-- called by -----------
Chd|        FORINTS                       source/elements/forints.F     
Chd|-- calls ---------------
Chd|        SAV_BUF_POINT                 source/user_interface/eng_callback_c.c
Chd|        XBILAN3                       source/elements/xelem/xbilan3.F
Chd|        XCOOR3                        source/elements/xelem/xcoor3.F
Chd|        XCUM3                         source/elements/xelem/xcum3.F 
Chd|        XCUM3P                        source/elements/xelem/xcum3.F 
Chd|        XDEFO3                        source/elements/xelem/xdefo3.F
Chd|        XFORC28                       source/elements/xelem/xforc28.F
Chd|        XFORC29                       source/elements/xelem/xforc29.F
Chd|        XFORC30                       source/elements/xelem/xforc30.F
Chd|        XFORC31                       source/elements/xelem/xforc31.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE XFORC3(
     1   NFT,      NEL,      GEO,      PM,
     2   ITAB,     KXX,      IXX,      X,
     3   V,        VR,       F,        AR,
     4   EV,       NPC,      PLD,      SKEW,
     5   DT2T,     NELTST,   ITYPTST,  STIFN,
     6   STIFR,    MS,       IN,       FSKYI,
     7   ISKY,     PARTSAV,  IPARTX,   BUFMAT,
     8   BUFGEO,   GRESAV,   GRTH,     IGRTH,
     9   ELBUF_STR,IGRE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE
      INTEGER NFT,NEL,KXX(NIXX,*),IXX(*), NPC(*),ISKY(*),
     .        IPARTX(*),NELTST ,ITYPTST,
     .        ITAB(*),GRTH(*),IGRTH(*)
C     REAL
      my_real DT2T ,
     .   GEO(NPROPG,*), PM(*), X(3,*),VR(3,*), V(3,*), F(3,*), AR(3,*),
     .   EV(*),PLD(*),SKEW(LSKEW,*),FSKYI(*),
     .   STIFN(*),STIFR(*),MS(*), IN(*),PARTSAV(*),
     .   BUFMAT(*),BUFGEO(*) ,GRESAV(*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .     DT1U,
     .     DTE, DTC, XKM, XCM, XKR, XCR, XM, XINE, EUSR
      INTEGER I, J, K, IPROP, IMAT, ISENS, NX,
     .   NB1, NB2, NB3, NB4, NB5, NBFI, UID,
     .   IGTYP,NUVAR,NUVARN,NUPARAM,
     .   NISKYL,KEUSR,
     .   UIX(MAXNX),KVAR,KVARN
       my_real
     .   XUSR(3,MAXNX),VUSR(3,MAXNX),VRUSR(3,MAXNX),UMASS(MAXNX),
     .   UFORC(3,MAXNX),USTIFM(MAXNX), USTIFR(MAXNX), UVISR(MAXNX),
     .   UMOMT(3,MAXNX),UVISM(MAXNX),UINER(MAXNX)
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
C
      DT1U=DT1
C------
      DO I=1,NEL
        J=I+NFT
C
        IMAT =KXX(1,J)
        IPROP=KXX(2,J)
        NX   =KXX(3,J)
C
        IF (IPARIT /= 0) THEN
#include "lockon.inc"
         NISKYL = NISKY
         NISKY  = NISKY + NX
#include "lockoff.inc"
        END IF
C
        IGTYP =  NINT(GEO(12,IPROP))
        NUVAR =  NINT(GEO(25,IPROP))
        NUVARN=  NINT(GEO(35,IPROP))
C        ISENS =  NINT(GEO(45,IPROP))
C
cc        NB1 =(I-1)*(3+NUVAR+NUVARN*NX)+1
cc        NB2 =NB1 +1
cc        NB3 =NB2 +1
cc        NB4 =NB3 +1
cc        NB5 =NB4 +NUVAR
cc        NBFI=NB5 +NUVARN*NX
C
        KVAR  = NUVAR*(I-1)+1
        KVARN = NUVARN*NX*(I-1)+1
C-------
C       FILL COORDINATES.
        CALL XCOOR3(X   ,KXX(1,J) ,IXX  ,ITAB ,NX ,
     2               UID ,UIX      ,XUSR )
C-------
C       FILL VELOCITIES.
        CALL XDEFO3(V    ,VR    ,KXX(1,J) ,IXX  ,NX   ,
     2               VUSR ,VRUSR )
C-------
        CALL SAV_BUF_POINT(PM,1)
        CALL SAV_BUF_POINT(BUFMAT,2)
        CALL SAV_BUF_POINT(GEO,3)
        CALL SAV_BUF_POINT(BUFGEO,4)
        CALL SAV_BUF_POINT(NPC,5)
        CALL SAV_BUF_POINT(PLD,6)
C-------
        IF (IGTYP == 28) THEN
C         NSTRAND ELEMENTS.
          KEUSR=0
          EUSR =ZERO
          CALL XFORC28(NX      ,
     2         XUSR    ,VUSR  ,VRUSR  ,UIX     ,UID      ,
     3         IOUT    ,IPROP ,IMAT   ,GBUF%OFF(I) ,KEUSR    ,
     4         EUSR  ,UMASS  ,UINER ,USTIFM   ,USTIFR ,
     5         UVISM ,UVISR  ,UFORC   ,UMOMT  ,
     6         NUVAR  ,GBUF%VAR(KVAR) ,NUVARN ,GBUF%VARN(KVARN) ,DT1U , DTE)
        ELSEIF (IGTYP == 29) THEN
          KEUSR=0
          EUSR =ZERO
          DTE  =EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
            UFORC(1,K)=ZERO
            UFORC(2,K)=ZERO
            UFORC(3,K)=ZERO
            UMOMT(1,K)=ZERO
            UMOMT(2,K)=ZERO
            UMOMT(3,K)=ZERO
          ENDDO
          CALL XFORC29(NX      ,
     2         XUSR    ,VUSR  ,VRUSR  ,UIX     ,UID      ,
     3         IOUT    ,IPROP ,IMAT   ,GBUF%OFF(I) ,KEUSR    ,
     4         EUSR  ,UMASS  ,UINER ,USTIFM   ,USTIFR ,
     5         UVISM ,UVISR  ,UFORC   ,UMOMT  ,
     6         NUVAR  ,GBUF%VAR(KVAR) ,NUVARN ,GBUF%VARN(KVARN) ,DT1U , DTE)
        ELSEIF (IGTYP == 30) THEN
          KEUSR=0
          EUSR =ZERO
          DTE=EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
            UFORC(1,K)=ZERO
            UFORC(2,K)=ZERO
            UFORC(3,K)=ZERO
            UMOMT(1,K)=ZERO
            UMOMT(2,K)=ZERO
            UMOMT(3,K)=ZERO
          ENDDO
          CALL XFORC30(NX      ,
     2         XUSR    ,VUSR  ,VRUSR  ,UIX     ,UID      ,
     3         IOUT    ,IPROP ,IMAT   ,GBUF%OFF(I) ,KEUSR    ,
     4         EUSR  ,UMASS  ,UINER ,USTIFM   ,USTIFR ,
     5         UVISM ,UVISR  ,UFORC   ,UMOMT  ,
     6         NUVAR  ,GBUF%VAR(KVAR) ,NUVARN ,GBUF%VARN(KVARN) ,DT1U , DTE)
        ELSEIF (IGTYP == 31) THEN
          KEUSR=0
          EUSR =ZERO
          DTE=EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
            UFORC(1,K)=ZERO
            UFORC(2,K)=ZERO
            UFORC(3,K)=ZERO
            UMOMT(1,K)=ZERO
            UMOMT(2,K)=ZERO
            UMOMT(3,K)=ZERO
          ENDDO
          CALL XFORC31(NX      ,
     2         XUSR    ,VUSR  ,VRUSR  ,UIX     ,UID      ,
     3         IOUT    ,IPROP ,IMAT   ,GBUF%OFF(I) ,KEUSR    ,
     4         EUSR  ,UMASS  ,UINER ,USTIFM   ,USTIFR ,
     5         UVISM ,UVISR  ,UFORC   ,UMOMT  ,
     6         NUVAR  ,GBUF%VAR(KVAR) ,NUVARN ,GBUF%VARN(KVARN) ,DT1U , DTE)
        ENDIF ! IF (IGTYP == 28)
C-------
C       CALCUL DE DT ELEMENTAIRE.
        IF (GBUF%OFF(I) /= ZERO) THEN
          IF (DTE < DT2T) THEN
           DT2T=DTE
           NELTST =KXX(5,J)
           ITYPTST=100
          ENDIF
        ELSE
          DO K=1,NX
            USTIFR(K)=ZERO
            USTIFM(K)=ZERO
            UVISR(K) =ZERO
            UVISM(K) =ZERO
            UFORC(1,K)=ZERO
            UFORC(2,K)=ZERO
            UFORC(3,K)=ZERO
            UMOMT(1,K)=ZERO
            UMOMT(2,K)=ZERO
            UMOMT(3,K)=ZERO
          ENDDO
        ENDIF
C-------
        CALL XBILAN3(
     1   NX,          KXX(1,J),    IXX,         X,
     2   V,           VR,          UMASS,       UINER,
     3   UFORC,       UMOMT,       KEUSR,       EUSR,
     4   GBUF%EINT(I),PARTSAV,     IPARTX(J),   GRESAV,
     5   GRTH,        IGRTH(J),    IGRE)
C-------
        IF (IPARIT == 0) THEN
          CALL XCUM3(NX,KXX(1,J),IXX,UFORC ,USTIFM,
     2               UVISM,MS, F, STIFN )
        ELSE
          CALL XCUM3P(NX,KXX(1,J),IXX,UFORC ,USTIFM,
     2                UVISM, MS, NISKYL, FSKYI ,ISKY)
        ENDIF
      ENDDO ! DO I=1,NEL
C-----------------------------------------------
      RETURN
      END
