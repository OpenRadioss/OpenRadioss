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
!||    intti12a        ../engine/source/interfaces/interf/intti12.F
!||--- called by ------------------------------------------------------
!||    intti0          ../engine/source/interfaces/interf/intti0.F
!||--- calls      -----------------------------------------------------
!||    i12cor3         ../engine/source/interfaces/inter3d/i12cor3.F
!||    i12dis3         ../engine/source/interfaces/inter3d/i12dis3.F
!||    i12gathcor      ../engine/source/interfaces/interf/i12gathcor.F
!||    i12loc3         ../engine/source/interfaces/inter3d/i12loc3.F
!||    i12m2s          ../engine/source/interfaces/interf/i12m2s.F
!||    i12msr3         ../engine/source/interfaces/inter3d/i12msr3.F
!||    i12nod3         ../engine/source/interfaces/interf/i12nod3.F
!||    i12rot3         ../engine/source/interfaces/inter3d/i12rot3.F
!||    i12s2m          ../engine/source/interfaces/interf/i12s2m.F
!||    i3cst3          ../engine/source/interfaces/inter3d/i3cst3.F
!||    varcondec       ../engine/source/interfaces/interf/varcondec.F
!||--- uses       -----------------------------------------------------
!||    ale_mod         ../common_source/modules/ale/ale_mod.F
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||    segvar_mod      ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE INTTI12A(IPARI, X, WA, ITAB, SEGVAR, TEST, SKEW, ISKWN ,INTBUF_TAB )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  FLUID/FLUID INTERFACE DERIVATED FROM TYPE 2 & 5 (SLIDING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEGVAR_MOD
      USE ALE_MOD
      USE INTBUFDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*),ITAB(*),TEST,ISKWN(*)
      my_real X(*),WA(*),SKEW(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
 
      INTEGER ITIED,NOINT,NGROUS,NGROUM,NG,
     .  L1 ,L2 ,L3 ,L4,L5,L6
      INTEGER JCODV(ALE%GLOBAL%LCONV),CODTOT,I,ITEST,ILEV,IREF,ICEN
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4
      my_real
     .  GAP
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ) :: XFACE,N1,N2,N3
      my_real, DIMENSION(MVSIZ) :: SSC,TTC,AREA,THK,ALP
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,ANS
      my_real, DIMENSION(MVSIZ) :: XX1,XX2,XX3,XX4
      my_real, DIMENSION(MVSIZ) :: YY1,YY2,YY3,YY4
      my_real, DIMENSION(MVSIZ) :: ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(MVSIZ) :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(MVSIZ) :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(MVSIZ) :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(MVSIZ) :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(MVSIZ) :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(MVSIZ) :: ZN1,ZN2,ZN3,ZN4
      my_real, DIMENSION(MVSIZ) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ) :: H1,H2,H3,H4
      INTEGER :: NRTS, NRTM, NSN, NMN, NST, MST
      INTEGER :: LFT, LLT, NFT
C-----------------------------------------------
      ITEST =0

      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NST   =IPARI(8)
      MST   =IPARI(9)
      NOINT =IPARI(15)
      ILEV=IPARI(20)
      IREF=IPARI(21)
      ICEN=IPARI(22)
      CALL VARCONDEC(JCODV,IPARI(30),CODTOT)

      NGROUS=1+(NSN-1)/NVSIZ
      NGROUM=1+(NMN-1)/NVSIZ

      ITIED=INT(INTBUF_TAB%VARIABLES(1))
      GAP  =INTBUF_TAB%VARIABLES(2)
      IF(ITIED==0 .OR. ITIED==3)THEN
        L1=1
        L2=L1+3*NSN
        L3=L2+3*NMN
        CALL I12GATHCOR(X,WA(L1),WA(L2),NSN,NMN,INTBUF_TAB%NSV,INTBUF_TAB%MSR)

        DO NG=1,NGROUS
          NFT=(NG-1)*NVSIZ
          LFT=1
          LLT=MIN0(NVSIZ,NSN-NFT)
          !----------------------------------------------!
          !   FIND NEAREST NODE                          !
          !----------------------------------------------!
          CALL I12LOC3(
     1   WA(L1),           WA(L2),           INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,
     2   INTBUF_TAB%MSR,   INTBUF_TAB%NSV,   INTBUF_TAB%ILOCS, INTBUF_TAB%NSEGM,
     3   LFT,              LLT,              NFT)

          !----------------------------------------------!
          !   FIND MAIN SEGMENT
          !----------------------------------------------!
          CALL I12MSR3(
     1   WA(L1),           WA(L2),           INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,
     2   INTBUF_TAB%MSR,   INTBUF_TAB%NSV,   INTBUF_TAB%ILOCS, INTBUF_TAB%IRTLM,
     3   INTBUF_TAB%NSEGM, LFT,              LLT,              NFT)
        ENDDO

        IF(ILEV>0)
     &  CALL I12ROT3(WA(L1),WA(L2),X,SKEW,ISKWN,ILEV,IREF,ICEN,NSN,NMN,
     &               INTBUF_TAB%STFNS,INTBUF_TAB%STFNM)
 
        DO NG=1,NGROUS
          NFT=(NG-1)*NVSIZ
          LFT=1
          LLT=MIN0(NVSIZ,NSN-NFT)
          CALL I12COR3(
     1   WA(L1),           WA(L2),           INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,
     2   INTBUF_TAB%IRTLM, ILEV,             X,                INTBUF_TAB%STFNS,
     3   IX1,              IX2,              IX3,              IX4,
     4   X1,               X2,               X3,               X4,
     5   XI,               Y1,               Y2,               Y3,
     6   Y4,               YI,               Z1,               Z2,
     7   Z3,               Z4,               ZI,               XFACE,
     8   LFT,              LLT,              NFT)
          !-----------------------------------------------------------!
          !   CALCUL COORDONNEES ISOPARAMETRIQUES S & T NOEUD SECONDARY   !
          !-----------------------------------------------------------!
          CALL I3CST3(
     1   X1,      X2,      X3,      X4,
     2   XI,      Y1,      Y2,      Y3,
     3   Y4,      YI,      Z1,      Z2,
     4   Z3,      Z4,      ZI,      XFACE,
     5   N1,      N2,      N3,      SSC,
     6   TTC,     X0,      Y0,      Z0,
     7   XX1,     XX2,     XX3,     XX4,
     8   YY1,     YY2,     YY3,     YY4,
     9   ZZ1,     ZZ2,     ZZ3,     ZZ4,
     A   XI1,     XI2,     XI3,     XI4,
     B   YI1,     YI2,     YI3,     YI4,
     C   ZI1,     ZI2,     ZI3,     ZI4,
     D   XN1,     XN2,     XN3,     XN4,
     E   YN1,     YN2,     YN3,     YN4,
     F   ZN1,     ZN2,     ZN3,     ZN4,
     G   AREA,    LFT,     LLT)
          !-----------------------------------------------------------!
          ! VERIFIE QUE LE NOEUD EST SUR LA FACETTE ET STOCKE S & T   !
          !-----------------------------------------------------------!
        CALL I12DIS3(
     1   INTBUF_TAB%CSTS,  INTBUF_TAB%NSV,   ITAB,             GAP,
     2   NOINT,            ITEST,            INTBUF_TAB%FCOUNT,SSC,
     3   TTC,              LFT,              LLT,              NFT)
        ENDDO
        IF(ITEST>0)THEN
          WRITE(IOUT,'(A,I8,A,I8,A)')
     .    ' *** WARNING INTERFACE',NOINT,' : ',
     .    ITEST,' SECONDARY NODES LIE OUTSIDE OF MAIN SEGMENT'
        TEST=1
        ENDIF
      ENDIF !(ITIED==0 .OR. ITIED==3)

      IF(CODTOT/=0 .AND. ALE%GLOBAL%INCOMP==0 .AND. ITIED/=3)THEN
        L1=1
        L2=L1+NSN*ALE%GLOBAL%NVCONV
        L3=L2+NSN*ALE%GLOBAL%NVCONV
        L4=L3+NRTM        
        L5=L4+NMN*ALE%GLOBAL%NVCONV
        L6=L5+NMN
        !-----------------------------------------------------------------!
        !   ECHANGE DES INFORMATIONS ENTRE LES TWO FACES DE l'INTERFACE  !
        !-----------------------------------------------------------------!
        ! 1- Transforme Valeurs elementaires en valeurs nodales cote second.     
        !-----------------------------------------------------------------!      
        CALL I12NOD3(NRTS,INTBUF_TAB%IELES,INTBUF_TAB%IRECTS,SEGVAR,JCODV,
     .               WA(L1),INTBUF_TAB%FCOUNT,NSN)
        ! 2- Valeurs MAIN -> SECONDARY
        !-----------------------------------------------------------------!
        CALL I12M2S(NSN ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRTLM,WA(L2),NRTM,NRTS,
     .              INTBUF_TAB%IELEM,INTBUF_TAB%IELES,SEGVAR,JCODV)
        ! 3- Valeurs Nodales SECONDARY -> MAIN
        !-----------------------------------------------------------------!
        CALL I12S2M(NSN,INTBUF_TAB%IRTLM,NRTM,JCODV,WA(L1),WA(L3),
     .              NMN,WA(L4),INTBUF_TAB%IRECTM,WA(L5),INTBUF_TAB%MSR,
     .              SEGVAR,INTBUF_TAB%IELEM,NOINT)
      ENDIF

      RETURN
      END

!||====================================================================
!||    intti12f        ../engine/source/interfaces/interf/intti12.F
!||--- called by ------------------------------------------------------
!||    intti1          ../engine/source/interfaces/interf/intti1.F
!||--- calls      -----------------------------------------------------
!||    i12for3         ../engine/source/interfaces/interf/i12for3.F
!||    in12r           ../engine/source/interfaces/interf/in12r.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE INTTI12F(IPARI,INTBUF_TAB ,X     ,V     ,
     .                    A    ,MS   ,ITAB ,WEIGHT,STIFN ,WA,SKEW  )
C INTERFACE FLUIDE/FLUIDE DERIVEE DES INTERFACE 2 ET 5 (SLIDING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ITAB(*),WEIGHT(*)
      my_real
     .   X(*), V(*), A(*), MS(*), STIFN(*),WA(*),SKEW(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITIED,NOINT,NGROUS,NGROUM,NG
      INTEGER ILEV,IREF
      my_real
     .  GAP
      INTEGER :: NRTS, NRTM, NSN, NMN, NST, MST

C-----------------------------------------------

      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NST   =IPARI(8)
      MST   =IPARI(9)
      NOINT =IPARI(15)
      ILEV  =IPARI(20)
      IREF  =IPARI(21)

      NGROUS=1+(NSN-1)/NVSIZ
      NGROUM=1+(NMN-1)/NVSIZ

      ITIED=INT(INTBUF_TAB%VARIABLES(1))
      !-----------------------------------------------------------------!
      ! CONDITIONS PERIODIQUES                                          !
      ! TRANSFORMATIONS DES ACCELERATIONS SECONDARYS                        !
      !-----------------------------------------------------------------!
      IF(ITIED==2)CALL IN12R(A,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,
     .                         NSN,1,INTBUF_TAB%FCOUNT)
      !-----------------------------------------------------------------!     
      ! CALCUL FORCES COTE MAIN (IDEM TYPE 2)                         !
      !-----------------------------------------------------------------!
      CALL I12FOR3(
     1 NSN         ,NMN            ,A         ,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,
     2 INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,MS          ,WEIGHT    ,
     3 STIFN         ,INTBUF_TAB%NMAS,INTBUF_TAB%FCOUNT,SKEW          ,WA             ,
     4 INTBUF_TAB%STFNS,INTBUF_TAB%STFNM,ILEV,IREF)
      RETURN
      END
      
      
!||====================================================================
!||    intti12v        ../engine/source/interfaces/interf/intti12.F
!||--- called by ------------------------------------------------------
!||    intti2          ../engine/source/interfaces/interf/intti2.F
!||--- calls      -----------------------------------------------------
!||    i12vit3         ../engine/source/interfaces/interf/i12vit3.F
!||    in12r           ../engine/source/interfaces/interf/in12r.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE INTTI12V(IPARI,INTBUF_TAB  ,X    ,V   ,
     .                    A    ,MS   ,WEIGHT,WA,SKEW )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD      
!-----------------------------------------------------------------!
! INTERFACE FLUIDE/FLUIDE DERIVEE DES INTERFACE 2 ET 5 (SLIDING)  !
!-----------------------------------------------------------------!
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), WEIGHT(*)
      my_real
     .   X(*), V(*), A(*),MS(*),WA(*),SKEW(*)
  
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .  ITIED,ILEV,IREF
      INTEGER :: NRTS, NRTM, NSN, NMN, NST, MST

C-----------------------------------------------
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NST   =IPARI(8)
      MST   =IPARI(9)
      ILEV  =IPARI(20)
      IREF  =IPARI(21)

      ITIED=INT(INTBUF_TAB%VARIABLES(1))
      !-----------------------------------------------------------------!
      ! BC MAIN ET IMPOSE VITESSE ET ACC COTE SECONDARY                   !
      ! VITESSE DE GRILLE (CAS TIED) FAIT DS INTERF 1                   !
      !-----------------------------------------------------------------!
      CALL I12VIT3(NSN       ,NMN        ,
     2 A         ,INTBUF_TAB%IRECTM,
     3 INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     4 V         ,MS            ,WEIGHT    ,INTBUF_TAB%NMAS,INTBUF_TAB%FCOUNT,
     5 SKEW         ,WA            ,INTBUF_TAB%STFNS,INTBUF_TAB%STFNM,ILEV,IREF)

      IF(ITIED==2)THEN
       CALL IN12R(A,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,NSN,2,INTBUF_TAB%FCOUNT)
       CALL IN12R(V,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,NSN,2,INTBUF_TAB%FCOUNT)
      ENDIF

      RETURN
      END
