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
!||    mmain8                ../engine/source/materials/mat_share/mmain8.F
!||--- called by ------------------------------------------------------
!||    s8forc3               ../engine/source/elements/solid/solide8/s8forc3.F
!||--- calls      -----------------------------------------------------
!||    fail_biquad_s         ../engine/source/materials/fail/biquad/fail_biquad_s.F
!||    fail_emc              ../engine/source/materials/fail/emc/fail_emc.F
!||    fail_johnson          ../engine/source/materials/fail/johnson_cook/fail_johnson.F
!||    fail_orthbiquad_s     ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_s.F
!||    fail_rtcl_s           ../engine/source/materials/fail/rtcl/fail_rtcl_s.F
!||    fail_spalling_s       ../engine/source/materials/fail/spalling/fail_spalling_s.F90
!||    fail_tab_old_s        ../engine/source/materials/fail/tabulated/fail_tab_old_s.F
!||    fail_tab_s            ../engine/source/materials/fail/tabulated/fail_tab_s.F
!||    fail_tbutcher_s       ../engine/source/materials/fail/tuler_butcher/fail_tbutcher_s.F
!||    fail_wierzbicki_s     ../engine/source/materials/fail/wierzbicki/fail_wierzbicki_s.F
!||    fail_wilkins_s        ../engine/source/materials/fail/wilkins/fail_wilkins_s.F
!||    m1law8                ../engine/source/materials/mat/mat001/m1law8.F
!||    m2law8                ../engine/source/materials/mat/mat002/m2law8.F
!||    m3law8                ../engine/source/materials/mat/mat003/m3law8.F
!||    meos8                 ../engine/source/materials/mat_share/meos8.F
!||    mulaw8                ../engine/source/materials/mat_share/mulaw8.F90
!||    put_etfac             ../engine/source/elements/solid/solide8z/put_etfac.F
!||    startime              ../engine/source/system/timer_mod.F90
!||    stoptime              ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    fail_spalling_s_mod   ../engine/source/materials/fail/spalling/fail_spalling_s.F90
!||    mat_elem_mod          ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mulaw8_mod            ../engine/source/materials/mat_share/mulaw8.F90
!||    table_mod             ../engine/share/modules/table_mod.F
!||    timer_mod             ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE MMAIN8(TIMERS, PM     ,GEO      ,ELBUF_STR,MAT_ELEM ,
     2                  IX     ,IPARG    ,V        ,TF       ,
     3                  NPF    ,BUFMAT   ,STIFN    ,X        ,D1      ,
     4                  D2     ,D3       ,D4       ,D5       ,D6      ,
     5                  VOLGP  ,DELTAX   ,VOLN     ,DVOL     ,VD2     ,
     6                  RHO0   ,MAT      ,NC       ,NGL      ,FV      ,
     7                  NEL    ,WXX      ,WYY      ,WZZ      ,PID     ,
     8                  DT2T   ,NELTST   ,ITYPTST  ,RX       ,RY      ,
     9                  RZ     ,SX       ,SY       ,SZ       ,TX      ,
     A                  TY     ,TZ       ,OFF      ,IPM      ,GAMA    ,
     B                  MSSA   ,DMELS    ,TABLE    ,SSP      ,ITASK   ,
     C                  SVIS   ,SNPC     ,NUMGEO   ,SBUFMAT  ,STF,
     D                  NTABLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD         
      USE MULAW8_MOD
      USE ELBUFDEF_MOD
      USE FAIL_SPALLING_S_MOD
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
#include      "vect01_c.inc"
#include      "com08_c.inc"
#include      "scr19_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER, INTENT(IN) :: SNPC
      INTEGER, INTENT(IN) :: NUMGEO
      INTEGER, INTENT(IN) :: SBUFMAT
      INTEGER, INTENT(IN) :: STF
      INTEGER, INTENT(IN) :: NTABLE
      INTEGER IX(*), IPARG(*), NPF(*),MAT(*),NC(*),NGL(*),
     .        NEL, NELTST, ITYPTST, PID(MVSIZ,*),IPM(NPROPMI,*),
     .        ITASK
      my_real
     .   PM(NPROPM,*), GEO(*), FV(*), V(*), TF(*), 
     .   BUFMAT(*), STIFN(*), X(3,*),WXX(*),WYY(*),WZZ(*),
     .   D1(MVSIZ,*),D2(MVSIZ,*),D3(MVSIZ,*),
     .   D4(MVSIZ,*),D5(MVSIZ,*),D6(MVSIZ,*),VOLGP(MVSIZ,*),
     .   VOLN(MVSIZ),DVOL(MVSIZ),VD2(MVSIZ),RHO0(MVSIZ),DELTAX(MVSIZ),
     .   RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ)    ,
     .   SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ)    ,
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ)    ,OFF(MVSIZ)    ,
     .   GAMA(MVSIZ,6),DT2T, MSSA(*) ,DMELS(*)    ,SSP(MVSIZ)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MAT_ELEM_) ,TARGET ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ(6),J1,J2,IR,NFUNC,NUVAR,NUPARAM,NUVARR,
     .        NVARF,IRUPT,NUMBER_FAILURE,IADBUF,IMAT,NTABL_FAIL,
     .        NIPARAM
      INTEGER IFUNC(MAXFUNC), LF_DAMMX
      INTEGER, DIMENSION(:), POINTER :: ITABL_FAIL, IPARAMF
      my_real
     .   QNEW(MVSIZ),VIS(MVSIZ),EPSD(MVSIZ),ET(MVSIZ),
     .    DPLA(8*MVSIZ), EPSP(8*MVSIZ),TSTAR(MVSIZ),SXX(MVSIZ),
     .    SYY(MVSIZ),SZZ(MVSIZ),SXY(MVSIZ),SYZ(MVSIZ),SZX(MVSIZ)   
      my_real,
     .  DIMENSION(:) ,POINTER :: UVARF,UPARAMF,DFMAX,TDELE
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE (BUF_LAY_),POINTER :: BUFLY
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      BUFLY => ELBUF_STR%BUFLY(1)
!
      DO I=1,6
        JJ(I) = NEL*(I-1)
      ENDDO
!
C     tangent module
      DO I=1,NEL
        ET(I)=ZERO
      ENDDO 
C
      DO I=1,NEL
        VIS(I)   = ZERO
        SVIS(I,1)= ZERO 
        SVIS(I,2)= ZERO 
        SVIS(I,3)= ZERO 
        SVIS(I,4)= ZERO 
        SVIS(I,5)= ZERO 
        SVIS(I,6)= ZERO 
        TSTAR(I) = ZERO        
      ENDDO 
C-----------------------------------------------------------------
      IF (MTN == 1) THEN
        CALL M1LAW8(
     1   PM,       OFF,      GBUF%SIG, GBUF%EINT,
     2   GBUF%RHO, GBUF%QVIS,GBUF%VOL, STIFN,
     3   NEL,      D1,       D2,       D3,
     4   D4,       D5,       D6,       VOLN,
     5   VOLGP,    DELTAX,   RHO0,     DVOL,
     6   VD2,      VIS,      MAT,      NC,
     7   NGL,      GEO,      PID,      DT2T,
     8   NELTST,   ITYPTST,  GBUF%OFF, MSSA,
     9   DMELS,    BUFLY,    SSP,      ITY,
     A   NPT,      JTUR,     JTHE,     JSMS)
      ELSEIF (MTN == 2) THEN
        CALL M2LAW8(
     1   PM,       OFF,      GBUF%SIG, GBUF%EINT,
     2   GBUF%RHO, GBUF%QVIS,GBUF%VOL, STIFN,
     3   NEL,      D1,       D2,       D3,
     4   D4,       D5,       D6,       VOLN,
     5   VOLGP,    DELTAX,   RHO0,     DVOL,
     6   VD2,      VIS,      MAT,      NC,
     7   NGL,      GEO,      PID,      GBUF%PLA,
     8   DT2T,     NELTST,   ITYPTST,  JPLASOL,
     9   GBUF%OFF, DPLA,     EPSP,     TSTAR,
     A   ET,       MSSA,     DMELS,    BUFLY,
     B   SSP,      ITY,      NPT,      JTUR,
     C   JTHE,     JSMS)

      ELSEIF (MTN == 3) THEN
        CALL M3LAW8(
     1   PM,       OFF,      GBUF%SIG, GBUF%PLA,
     2   GBUF%EINT,GBUF%RHO, D1,       D2,
     3   D3,       D4,       D5,       D6,
     4   VOLN,     VOLGP,    DVOL,     MAT,
     5   NGL,      JPLASOL,  DPLA,     EPSP,
     6   TSTAR,    BUFLY,    NEL,      NPT)
        CALL MEOS8(
     1   PM,       OFF,      GBUF%SIG, GBUF%EINT,
     2   GBUF%RHO, GBUF%QVIS,GBUF%VOL, GBUF%PLA,
     3   GBUF%TEMP,GBUF%RE,  STIFN,    NEL,
     4   D1,       D2,       D3,       VOLN,
     5   DELTAX,   RHO0,     DVOL,     VD2,
     6   VIS,      MAT,      NC,       NGL,
     7   GEO,      PID,      DT2T,     NELTST,
     8   ITYPTST,  GBUF%OFF, MSSA,     DMELS,
     9   BUFLY,    SSP,      ITY,      NPT,
     A   JTUR,     JTHE,     JSMS)
      ELSEIF (MTN == 41) THEN
      ELSEIF (MTN>=28) THEN
c     user-type laws
        NUVAR = 0
        DO I=1,NEL
          NUVAR  = MAX(NUVAR,IPM(8,MAT(I)))  
        ENDDO
c
        CALL MULAW8(TIMERS,
     1   1,        NEL,      MTN,
     2   NPT,      D1,       D2,       D3,
     3   D4,       D5,       D6,       PM,
     4   OFF,      GBUF%SIG, GBUF%EINT,GBUF%RHO,
     5   GBUF%QVIS,GBUF%VOL, GAMA,     BUFLY,
     6   BUFMAT,   TF,       NPF,      STIFN,
     7   VOLN,     VOLGP,    DELTAX,   RHO0,
     8   DVOL,     VD2,      VIS,      GBUF%EPSD,
     9   MAT,      NC,       NGL,      WXX,
     A   WYY,      WZZ,      GEO,      PID,
     B   DT2T,     NELTST,   ITYPTST,  JPLASOL,
     C   RX,       RY,       RZ,       SX,
     D   SY,       SZ,       TX,       TY,
     E   TZ,       ISMSTR,   IPM,      GBUF%OFF,
     F   ISORTH,   ET,       MSSA,     DMELS,
     G   TABLE,    IHET,     SSP,      ITASK,
     H   NEL,      ITY,      JSMS,     JSPH,
     I   JTHE,     JTUR,     MAT_ELEM%NUMMAT,
     J   MAT_ELEM%MAT_PARAM, SVIS,     SNPC,
     K   DT1,      TT,       MAXFUNC,  NPROPMI,
     L   NPROPG,   NPROPM,   IMON_MAT, NUMGEO,
     M   SBUFMAT,  STF,      NTABLE)
C-----
      ENDIF 
C---------------------------
C       Failure Model 
C --------------------------
      IF ((ITASK==0).AND.((IMON_MAT==1)))CALL STARTIME(TIMERS,121)
      IF (IFAILURE == 1)THEN
        IF (MTN < 28) THEN
        IMAT = MAT(1)
        NUMBER_FAILURE = MAT_ELEM%MAT_PARAM(IMAT)%NFAIL
c
        DO IR = 1,NUMBER_FAILURE 
          NUPARAM = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%NUPARAM
          NIPARAM = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%NIPARAM
          UPARAMF =>MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%UPARAM(1:NUPARAM)
          IPARAMF =>MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%IPARAM(1:NIPARAM)
          NFUNC   = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%NFUNC 
          IRUPT   = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%IRUPT
          IFUNC(1:NFUNC) = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%IFUNC(1:NFUNC)     
          NTABL_FAIL =  MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%NTABLE
          ITABL_FAIL => MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IR)%TABLE(1:NTABL_FAIL)
C----   
          DO J=1,NPT 
            LBUF   => BUFLY%LBUF(1,1,J)
            UVARF  => BUFLY%FAIL(1,1,J)%FLOC(IR)%VAR
            NVARF  =  BUFLY%FAIL(1,1,J)%FLOC(IR)%NVAR
            DFMAX  => BUFLY%FAIL(1,1,J)%FLOC(IR)%DAMMX
            TDELE  => BUFLY%FAIL(1,1,J)%FLOC(IR)%TDEL
            LF_DAMMX = BUFLY%FAIL(1,1,J)%FLOC(IR)%LF_DAMMX
            
            J1 = 1 + NEL*(J-1)
            DO I=1,NEL
              SXX(I) = LBUF%SIG(JJ(1)+I)
              SYY(I) = LBUF%SIG(JJ(2)+I)
              SZZ(I) = LBUF%SIG(JJ(3)+I)
              SXY(I) = LBUF%SIG(JJ(4)+I)
              SYZ(I) = LBUF%SIG(JJ(5)+I)
              SZX(I) = LBUF%SIG(JJ(6)+I)
            ENDDO
c----------
             IF (IRUPT == 1)THEN
C  --- Johnson cook
              CALL FAIL_JOHNSON(NEL ,NUPARAM,NVARF,
     2                   TT  ,DT1  ,UPARAMF,NGL ,
     4                   SXX  ,SYY  ,SZZ  ,SXY   ,SYZ   ,SZX,
     5                   DPLA(J1),EPSP(J1),TSTAR,UVARF,OFF,
     6                   DFMAX  ,TDELE)
             ELSEIF(IRUPT == 2)THEN
C --------  Tuler butcher
              CALL FAIL_TBUTCHER_S(NEL ,NUPARAM,NVARF,
     2                   TT  ,DT1  ,UPARAMF,NGL ,
     4                   SXX  ,SYY  ,SZZ  ,SXY   ,SYZ   ,SZX,
     5                   UVARF,OFF  ,DFMAX ,TDELE )
             ELSEIF(IRUPT == 3)THEN
C -----------wilkins
              CALL FAIL_WILKINS_S(NEL ,NUPARAM,NVARF,
     2                 TT  ,DT1  ,UPARAMF,NGL ,
     4                 SXX  ,SYY  ,SZZ  ,SXY   ,SYZ   ,SZX,
     5                 DPLA(J1),UVARF,OFF,DFMAX,TDELE)
             ELSEIF(IRUPT == 8)THEN
C---- JC  + spalling
              CALL FAIL_SPALLING_S(NEL ,NUPARAM,
     2                 TT  ,UPARAMF,NGL ,
     4                 SXX  ,SYY  ,SZZ  ,SXY   ,SYZ   ,SZX,
     5                 DPLA(J1),EPSP(J1),TSTAR,OFF,
     6                 LF_DAMMX,DFMAX ,TDELE,GBUF%OFF,
     7                 NIPARAM ,IPARAMF, MVSIZ)
             ELSEIF(IRUPT == 9)THEN
C---- wierzbicki
               CALL FAIL_WIERZBICKI_S(NEL ,NUPARAM,NVARF,
     2                 TT  ,DT1  ,UPARAMF,NGL ,
     4                 SXX  ,SYY  ,SZZ  ,SXY   ,SYZ   ,SZX,
     5                 DPLA(J1),LBUF%PLA,UVARF ,OFF   ,
     6                 DFMAX   ,TDELE)
C   tabulated failure model
             ELSEIF (IRUPT == 23) THEN
               CALL FAIL_TAB_S(
     1         NEL      ,NVARF    ,NPF      ,TF       ,TT       ,
     2         UPARAMF     ,NGL      ,DELTAX   ,
     3         SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX,
     4         DPLA(J1) ,EPSP(J1) ,TSTAR,UVARF ,NTABL_FAIL,ITABL_FAIL,
     5         OFF      ,TABLE    ,DFMAX,TDELE ,NFUNC     ,IFUNC )
c ---   extended mohr coulomb failure model
             ELSEIF (IRUPT == 27) THEN
               CALL FAIL_EMC(
     1         NEL      ,NVARF    ,TT       ,
     2         DT1      ,UPARAMF  ,NGL      ,
     3         SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX,
     4         LBUF%PLA ,DPLA(J1) ,EPSP(J1) ,UVARF,
     5         OFF      ,DFMAX    ,TDELE)
c ---   Biquadratic failure
             ELSEIF (IRUPT == 30) THEN
               CALL FAIL_BIQUAD_S(
     1         NEL      ,NUPARAM  ,NVARF    ,NFUNC    ,IFUNC    ,DELTAX   ,
     2         NPF      ,TF       ,TT       ,BUFMAT   ,TDELE    ,
     3         NGL      ,DPLA(J1) ,UVARF    ,OFF      ,DFMAX    ,LBUF%DMGSCL,
     4         SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX      )
c
              ELSEIF (IRUPT == 37) THEN
c ---       tabulated failure model (old, obsolete version)
                CALL FAIL_TAB_OLD_S(
     1         NEL      ,NVARF    ,NPF      ,TF       ,TT       ,
     2         UPARAMF  ,NGL      ,DELTAX   ,
     3         SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX,
     4         LBUF%PLA ,DPLA(J1) ,EPSP(J1) ,TSTAR,UVARF,
     5         OFF      ,DFMAX,TDELE ,
     6         NFUNC    ,IFUNC )
c
c ---   Orthotropic biquadratic failure
              ELSEIF (IRUPT == 38) THEN
                CALL FAIL_ORTHBIQUAD_S(
     1        NEL      ,NUPARAM  ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,UPARAMF,
     3        NGL      ,DPLA(J1) ,EPSP(J1) ,UVARF    ,OFF      ,
     4        SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX      ,
     5        DFMAX    ,TDELE    ,DELTAX   )
c
c  --- RTCL failure model
              ELSEIF (IRUPT == 40) THEN
                CALL FAIL_RTCL_S(
     1          NEL      ,NUPARAM  ,NVARF    ,TT       ,DT1      ,UPARAMF,
     2          SXX      ,SYY      ,SZZ      ,SXY      ,SYZ      ,SZX      ,
     3          NGL      ,DPLA(J1) ,UVARF    ,OFF      ,DFMAX    ,TDELE    )
              ENDIF
c
          DO I=1,NEL
            LBUF%SIG(JJ(1)+I) = SXX(I)
            LBUF%SIG(JJ(2)+I) = SYY(I)
            LBUF%SIG(JJ(3)+I) = SZZ(I)
            LBUF%SIG(JJ(4)+I) = SXY(I)
            LBUF%SIG(JJ(5)+I) = SYZ(I)
            LBUF%SIG(JJ(6)+I) = SZX(I)
          ENDDO

         ENDDO!J=1,NPT
        ENDDO  ! IR = 1,NUMBER_FAILURE
       ENDIF
      ENDIF

      IF ((ITASK==0).AND.((IMON_MAT==1)))CALL STOPTIME(TIMERS,121)
      IF (IMPL_S>0) CALL PUT_ETFAC(NEL     ,ET    ,MTN  )

      IF(BUFLY%L_SSP /=0 )THEN
        BUFLY%LBUF(1,1,1)%SSP(1:NEL) = SSP(1:NEL)
      ENDIF
C-----------                
      RETURN
      END
