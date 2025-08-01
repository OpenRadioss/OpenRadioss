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
!||    multi_fvm2fem   ../engine/source/multifluid/multi_fvm2fem.F
!||--- called by ------------------------------------------------------
!||    alemain         ../engine/source/ale/alemain.F
!||--- calls      -----------------------------------------------------
!||    a4mass3         ../engine/source/elements/solid/solide4/a4mass3.F
!||    a4mass3p        ../engine/source/elements/solid/solide4/a4mass3p.F
!||    amass3          ../engine/source/elements/solid/solide/amass3.F
!||    amass3p         ../engine/source/elements/solid/solide/amass3p.F
!||    initbuf         ../engine/share/resol/initbuf.F
!||    s4cumu3         ../engine/source/elements/solid/solide4/s4cumu3.F
!||    s4cumu3p        ../engine/source/elements/solid/solide4/s4cumu3p.F
!||    scumu3          ../engine/source/elements/solid/solide/scumu3.F
!||    scumu3p         ../engine/source/elements/solid/solide/scumu3p.F
!||    snorm3          ../engine/source/multifluid/snorm3.F
!||    snorm3t         ../engine/source/multifluid/snorm3t.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod     ../engine/share/resol/initbuf.F
!||    multi_fvm_mod   ../common_source/modules/ale/multi_fvm_mod.F90
!||====================================================================
      SUBROUTINE MULTI_FVM2FEM(TIMESTEP, ELBUF_TAB, ITASK, 
     .     IXS, IXQ, IPARG, XGRID, ACCELE, VEL, WGRID, MS, MSNF, VEUL,
     .     STIFN, FSKY, IADS, FSKYM, 
     .     CONDN, CONDNSKY, MULTI_FVM,NODADT_THERM)     
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD 
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C     G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: NODADT_THERM
      INTEGER, INTENT(IN) ::  ITASK, IPARG(NPARG, *), IXS(NIXS, *), IXQ(NIXQ, *),
     .     IADS(8, *)
      
      my_real, INTENT(IN) ::
     .     XGRID(3, *), WGRID(3, *), VEUL(*), VEL(3, *), TIMESTEP
      my_real, INTENT(INOUT) :: ACCELE(*)
      my_real, INTENT(INOUT) ::
     .     MS(*), MSNF(*)
      my_real, INTENT(INOUT) :: FSKYM(*), STIFN(*), FSKY(8,LSKY),
     .     CONDN(*), CONDNSKY(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM

C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, K, NF1, ISTRA, ISOLNOD, NSG, IPLA, NG, NEL, NVC
      my_real
     .     PRES(MVSIZ)
      my_real :: NORM(3, 6, MVSIZ), SURF(6, MVSIZ), WFAC(3, 6, MVSIZ)
      my_real
     .     XG(MVSIZ, 8, 3)
      my_real
     .     XX(3), YY(3)
      my_real
     .     STI(MVSIZ), FR_WAV(MVSIZ), THEM(MVSIZ,8), CONDE(MVSIZ)
      my_real
     .     MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .     MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .     MX3(MVSIZ),MY3(MVSIZ),MZ3(MVSIZ),
     .     MX4(MVSIZ),MY4(MVSIZ),MZ4(MVSIZ),
     .     MX5(MVSIZ),MY5(MVSIZ),MZ5(MVSIZ),
     .     MX6(MVSIZ),MY6(MVSIZ),MZ6(MVSIZ),
     .     MX7(MVSIZ),MY7(MVSIZ),MZ7(MVSIZ),
     .     MX8(MVSIZ),MY8(MVSIZ),MZ8(MVSIZ),    
     .     F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .     F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .     F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .     F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .     F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .     F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .     F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .     F18(MVSIZ),F28(MVSIZ),F38(MVSIZ),
     .     DMASS1(MVSIZ), DMASS2(MVSIZ), DMASS3(MVSIZ), DMASS4(MVSIZ), 
     .     DMASS5(MVSIZ), DMASS6(MVSIZ), DMASS7(MVSIZ), DMASS8(MVSIZ),
     .     X(3), XC(3)
      INTEGER
     .     NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .     NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      INTEGER :: IBID
      my_real
     .     AR, FR_WAVE, FTHE, FTHESKY, FFSKY, T1,T2,T3
      
      TYPE(G_BUFEL_) ,POINTER :: GBUF

C-----------------------------------------------
C     S o u r c e  L i n e s
C-----------------------------------------------

      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
         IF (TT > ZERO .AND. IPARG(76, NG) == 1) CYCLE ! --> OFF
         IF(IPARG(8,NG) /= 1) THEN
C     
            CALL INITBUF(IPARG    ,NG      ,
     2           MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3           NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4           JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5           NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6           IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7           ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            IF (MTN == 151) THEN
               GBUF => ELBUF_TAB(NG)%GBUF
               IF(JLAG /= 1 .AND. ITY<=2) THEN
                  NSG    =IPARG(10,NG)
                  NVC    =IPARG(19,NG)
                  ISOLNOD=IPARG(28,NG)
                  ISTRA  =IPARG(44,NG)
                  JSPH   =0
                  JPLASOL=IPLA
                  ISPH2SOL = 0
                  IPARTSPH = IPARG(69,NG)
                  LFT=1
                  LLT=NEL
                  NF1=NFT+1
                  IF (ITY == 1 .AND. ISOLNOD /= 4) THEN
C     HEXA CASE
                     GBUF => ELBUF_TAB(NG)%GBUF

                     CALL SNORM3(NEL, NFT, JALE, IXS, XGRID, WGRID, 
     .                    NORM(1:3, 1:6, 1:NEL), WFAC(1:3, 1:6, 1:NEL), SURF(1:6, 1:NEL))

                     F11(:) = ZERO ; F21(:) = ZERO ; F31(:) = ZERO
                     F12(:) = ZERO ; F22(:) = ZERO ; F32(:) = ZERO
                     F13(:) = ZERO ; F23(:) = ZERO ; F33(:) = ZERO
                     F14(:) = ZERO ; F24(:) = ZERO ; F34(:) = ZERO
                     F15(:) = ZERO ; F25(:) = ZERO ; F35(:) = ZERO
                     F16(:) = ZERO ; F26(:) = ZERO ; F36(:) = ZERO
                     F17(:) = ZERO ; F27(:) = ZERO ; F37(:) = ZERO
                     F18(:) = ZERO ; F28(:) = ZERO ; F38(:) = ZERO
C     Assemblage des forces nodales
                     DO II = LFT, LLT
                        I = II + NFT
                        NC1(II) = IXS(2, I)
                        NC2(II) = IXS(3, I)
                        NC3(II) = IXS(4, I)
                        NC4(II) = IXS(5, I)
                        NC5(II) = IXS(6, I)
                        NC6(II) = IXS(7, I)
                        NC7(II) = IXS(8, I)
                        NC8(II) = IXS(9, I)
C     Pressure
                        PRES(II) = THIRD * (GBUF%SIG(II) + GBUF%SIG(II + NEL) + GBUF%SIG(II + 2 * NEL))
C     Node 1
                        F11(II) = F11(II) - FOURTH * PRES(II) * 
     .                       (NORM(1, 1, II) * SURF(1, II) + NORM(1, 4, II) * SURF(4, II) + NORM(1, 6, II) * SURF(6, II))
                        F21(II) = F21(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 1, II) * SURF(1, II) + NORM(2, 4, II) * SURF(4, II) + NORM(2, 6, II) * SURF(6, II))
                        F31(II) = F31(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 1, II) * SURF(1, II) + NORM(3, 4, II) * SURF(4, II) + NORM(3, 6, II) * SURF(6, II))
C     Node 2
                        F12(II) = F12(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 1, II) * SURF(1, II) + NORM(1, 4, II) * SURF(4, II) + NORM(1, 5, II) * SURF(5, II))
                        F22(II) = F22(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 1, II) * SURF(1, II) + NORM(2, 4, II) * SURF(4, II) + NORM(2, 5, II) * SURF(5, II))
                        F32(II) = F32(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 1, II) * SURF(1, II) + NORM(3, 4, II) * SURF(4, II) + NORM(3, 5, II) * SURF(5, II))
C     Node 3
                        F13(II) = F13(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 1, II) * SURF(1, II) + NORM(1, 2, II) * SURF(2, II) + NORM(1, 5, II) * SURF(5, II))
                        F23(II) = F23(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 1, II) * SURF(1, II) + NORM(2, 2, II) * SURF(2, II) + NORM(2, 5, II) * SURF(5, II))
                        F33(II) = F33(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 1, II) * SURF(1, II) + NORM(3, 2, II) * SURF(2, II) + NORM(3, 5, II) * SURF(5, II))
C     Node 4
                        F14(II) = F14(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 1, II) * SURF(1, II) + NORM(1, 2, II) * SURF(2, II) + NORM(1, 6, II) * SURF(6, II))
                        F24(II) = F24(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 1, II) * SURF(1, II) + NORM(2, 2, II) * SURF(2, II) + NORM(2, 6, II) * SURF(6, II))
                        F34(II) = F34(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 1, II) * SURF(1, II) + NORM(3, 2, II) * SURF(2, II) + NORM(3, 6, II) * SURF(6, II))
C     Node 5
                        F15(II) = F15(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 3, II) * SURF(3, II) + NORM(1, 4, II) * SURF(4, II) + NORM(1, 6, II) * SURF(6, II))
                        F25(II) = F25(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 3, II) * SURF(3, II) + NORM(2, 4, II) * SURF(4, II) + NORM(2, 6, II) * SURF(6, II))
                        F35(II) = F35(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 3, II) * SURF(3, II) + NORM(3, 4, II) * SURF(4, II) + NORM(3, 6, II) * SURF(6, II))
C     Node 6
                        F16(II) = F16(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 3, II) * SURF(3, II) + NORM(1, 4, II) * SURF(4, II) + NORM(1, 5, II) * SURF(5, II))
                        F26(II) = F26(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 3, II) * SURF(3, II) + NORM(2, 4, II) * SURF(4, II) + NORM(2, 5, II) * SURF(5, II))
                        F36(II) = F36(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 3, II) * SURF(3, II) + NORM(3, 4, II) * SURF(4, II) + NORM(3, 5, II) * SURF(5, II))
C     Node 7
                        F17(II) = F17(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 2, II) * SURF(2, II) + NORM(1, 3, II) * SURF(3, II) + NORM(1, 5, II) * SURF(5, II))
                        F27(II) = F27(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 2, II) * SURF(2, II) + NORM(2, 3, II) * SURF(3, II) + NORM(2, 5, II) * SURF(5, II))
                        F37(II) = F37(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 2, II) * SURF(2, II) + NORM(3, 3, II) * SURF(3, II) + NORM(3, 5, II) * SURF(5, II))
C     Node 8
                        F18(II) = F18(II) - FOURTH * PRES(II) *  
     .                       (NORM(1, 2, II) * SURF(2, II) + NORM(1, 3, II) * SURF(3, II) + NORM(1, 6, II) * SURF(6, II))
                        F28(II) = F28(II) - FOURTH * PRES(II) *  
     .                       (NORM(2, 2, II) * SURF(2, II) + NORM(2, 3, II) * SURF(3, II) + NORM(2, 6, II) * SURF(6, II))
                        F38(II) = F38(II) - FOURTH * PRES(II) *  
     .                       (NORM(3, 2, II) * SURF(2, II) + NORM(3, 3, II) * SURF(3, II) + NORM(3, 6, II) * SURF(6, II))
                     ENDDO
                     
C--------------------------
C     UPDATE DES MASSES
C----------------------------
                     IF(IPARIT == 0)THEN
                        CALL AMASS3(
     1   MS,                GBUF%RHO,          VEUL(LVEUL*NFT+44),GBUF%TAG22,
     2   GBUF%VOL,          NC1,               NC2,               NC3,
     3   NC4,               NC5,               NC6,               NC7,
     4   NC8,               MSNF,              NVC,               GBUF%OFF,
     5   IXS,               NEL,               JEUL)
                     ELSE
                        CALL AMASS3P(
     1   FSKYM,             GBUF%RHO,          VEUL(LVEUL*NFT+44),GBUF%TAG22,
     2   GBUF%VOL,          IADS,              GBUF%OFF,          IXS,
     3   NEL,               NFT,               JEUL)
                     ENDIF

C     ASSEMBLE
                     STI(:) = ZERO
                     FR_WAV(:) = ZERO
                     IBID = 0
                     IF(IPARIT == 0)THEN
                        CALL SCUMU3(
     1   GBUF%OFF,ACCELE,  NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NC7,     NC8,     STIFN,   STI,
     4   F11,     F21,     F31,     F12,
     5   F22,     F32,     F13,     F23,
     6   F33,     F14,     F24,     F34,
     7   F15,     F25,     F35,     F16,
     8   F26,     F36,     F17,     F27,
     9   F37,     F18,     F28,     F38,
     A   NVC,     AR,      FR_WAVE, FR_WAV,
     B   MX1,     MY1,     MZ1,     MX2,
     C   MY2,     MZ2,     MX3,     MY3,
     D   MZ3,     MX4,     MY4,     MZ4,
     E   MX5,     MY5,     MZ5,     MX6,
     F   MY6,     MZ6,     MX7,     MY7,
     G   MZ7,     MX8,     MY8,     MZ8,
     H   THEM,    FTHE,    CONDN,   CONDE,
     I   NEL,     JTHE,    ISROT,   IPARTSPH,NODADT_THERM)
                     ELSE
                        CALL SCUMU3P(
     1   GBUF%OFF,STI,     FSKY,    FSKY,
     2   IADS,    F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     NC1,     NC2,     NC3,
     9   NC4,     NC5,     NC6,     NC7,
     A   NC8,     AR,      FR_WAVE, FR_WAV,
     B   MX1,     MY1,     MZ1,     MX2,
     C   MY2,     MZ2,     MX3,     MY3,
     D   MZ3,     MX4,     MY4,     MZ4,
     E   MX5,     MY5,     MZ5,     MX6,
     F   MY6,     MZ6,     MX7,     MY7,
     G   MZ7,     MX8,     MY8,     MZ8,
     H   THEM,    FTHESKY, CONDNSKY,CONDE,
     I   NEL,     NFT,     JTHE,    ISROT,
     J   IPARTSPH,NODADT_THERM)
                     ENDIF
                  ELSE IF (ITY == 1 .AND. ISOLNOD == 4) THEN
C     TETRA CASE
                     GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------------------------------
C     Compute nodal normals
C-----------------------------------------------
                     CALL SNORM3T(NEL, NFT, JALE, IXS, XGRID, WGRID, 
     .                    NORM(1:3, 1:6, 1:NEL), WFAC(1:3, 1:6, 1:NEL), SURF(1:6, 1:NEL))
                     F11(:) = ZERO ; F21(:) = ZERO ; F31(:) = ZERO
                     F12(:) = ZERO ; F22(:) = ZERO ; F32(:) = ZERO
                     F13(:) = ZERO ; F23(:) = ZERO ; F33(:) = ZERO
                     F14(:) = ZERO ; F24(:) = ZERO ; F34(:) = ZERO
                     DMASS1(:) = ZERO ; DMASS2(:) = ZERO ; DMASS3(:) = ZERO ; DMASS4(:) = ZERO

C     Assemblage des forces nodales
                        DO II = LFT, LLT
                           I = II + NFT
                           NC1(II) = IXS(2, I)
                           NC2(II) = IXS(4, I)
                           NC3(II) = IXS(7, I)
                           NC4(II) = IXS(6, I)
C     Pressure
                           PRES(II) = THIRD * (GBUF%SIG(II) + GBUF%SIG(II + NEL) + GBUF%SIG(II + 2 * NEL))
C     Node 1
                           F11(II) = F11(II) - THIRD * PRES(II) * 
     .                          (NORM(1, 4, II) * SURF(4, II) + NORM(1, 5, II) * SURF(5, II) + NORM(1, 6, II) * SURF(6, II))
                           F21(II) = F21(II) - THIRD * PRES(II) * 
     .                          (NORM(2, 4, II) * SURF(4, II) + NORM(2, 5, II) * SURF(5, II) + NORM(2, 6, II) * SURF(6, II))
                           F31(II) = F31(II) - THIRD * PRES(II) * 
     .                          (NORM(3, 4, II) * SURF(4, II) + NORM(3, 5, II) * SURF(5, II) + NORM(3, 6, II) * SURF(6, II))
C     Node 2
                           F12(II) = F12(II) - THIRD * PRES(II) * 
     .                          (NORM(1, 2, II) * SURF(2, II) + NORM(1, 5, II) * SURF(5, II) + NORM(1, 6, II) * SURF(6, II))
                           F22(II) = F22(II) - THIRD * PRES(II) * 
     .                          (NORM(2, 2, II) * SURF(2, II) + NORM(2, 5, II) * SURF(5, II) + NORM(2, 6, II) * SURF(6, II))
                           F32(II) = F32(II) - THIRD * PRES(II) * 
     .                          (NORM(3, 2, II) * SURF(2, II) + NORM(3, 5, II) * SURF(5, II) + NORM(3, 6, II) * SURF(6, II))
C     Node 3
                           F13(II) = F13(II) - THIRD * PRES(II) * 
     .                          (NORM(1, 4, II) * SURF(4, II) + NORM(1, 5, II) * SURF(5, II) + NORM(1, 2, II) * SURF(2, II))
                           F23(II) = F23(II) - THIRD * PRES(II) * 
     .                          (NORM(2, 4, II) * SURF(4, II) + NORM(2, 5, II) * SURF(5, II) + NORM(2, 2, II) * SURF(2, II))
                           F33(II) = F33(II) - THIRD * PRES(II) * 
     .                          (NORM(3, 4, II) * SURF(4, II) + NORM(3, 5, II) * SURF(5, II) + NORM(3, 2, II) * SURF(2, II))
C     Node 4
                           F14(II) = F14(II) - THIRD * PRES(II) * 
     .                          (NORM(1, 4, II) * SURF(4, II) + NORM(1, 2, II) * SURF(2, II) + NORM(1, 6, II) * SURF(6, II))
                           F24(II) = F24(II) - THIRD * PRES(II) * 
     .                          (NORM(2, 4, II) * SURF(4, II) + NORM(2, 2, II) * SURF(2, II) + NORM(2, 6, II) * SURF(6, II))
                           F34(II) = F34(II) - THIRD * PRES(II) * 
     .                          (NORM(3, 4, II) * SURF(4, II) + NORM(3, 2, II) * SURF(2, II) + NORM(3, 6, II) * SURF(6, II))
                     ENDDO
C--------------------------
C     UPDATE DES MASSES
C----------------------------
                     IF (IPARIT == 0) THEN
                        CALL A4MASS3(
     1   MS,      GBUF%RHO,GBUF%VOL,NC1,
     2   NC2,     NC3,     NC4,     MSNF,
     3   GBUF%OFF,NEL)
                     ELSE
                        CALL A4MASS3P(
     1   FSKYM,   GBUF%RHO,GBUF%VOL,IADS,
     2   GBUF%OFF,NEL,     NFT)
                     ENDIF
                     
C     ASSEMBLE
                STI(:) = ZERO
                FR_WAV(:) = ZERO
                IBID = 0
                IF(IPARIT == 0)THEN
                  CALL S4CUMU3(
     1                  GBUF%OFF,ACCELE,  NC1,     NC2,
     2                  NC3,     NC4,     STIFN,   STI,
     3                  F11,     F21,     F31,     F12,
     4                  F22,     F32,     F13,     F23,
     5                  F33,     F14,     F24,     F34,
     6                  THEM,    FTHE,    CONDN,   CONDE,
     7                  NEL,     JTHE,  NODADT_THERM)
                ELSE
                  CALL S4CUMU3P(
     1                 GBUF%OFF,STI,     FSKY,    FSKY,
     2                 IADS,    F11,     F21,     F31,
     3                 F12,     F22,     F32,     F13,
     4                 F23,     F33,     F14,     F24,
     5                 F34,     THEM,    FTHESKY, CONDNSKY,
     6                 CONDE,   NEL,     NFT,     JTHE,  NODADT_THERM)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C     
C-----------
      RETURN
      END SUBROUTINE MULTI_FVM2FEM

!||====================================================================
!||    vector_prod   ../engine/source/multifluid/multi_fvm2fem.F
!||====================================================================
      SUBROUTINE VECTOR_PROD(X, Y, RES, FAC)
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      my_real, INTENT(IN) :: X(3), Y(3), FAC
      my_real, INTENT(OUT) :: RES(3)
      
      RES(1) = FAC * (X(2) * Y(3) - X(3) * Y(2))
      RES(2) = FAC * (X(3) * Y(1) - X(1) * Y(3))
      RES(3) = FAC * (X(1) * Y(2) - X(2) * Y(1))
      END
