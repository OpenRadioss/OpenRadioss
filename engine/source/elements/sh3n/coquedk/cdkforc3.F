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
Chd|  CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        C3BILAN                       source/elements/sh3n/coque3n/c3bilan.F
Chd|        C3UPDT3                       source/elements/sh3n/coque3n/c3updt3.F
Chd|        C3UPDT3P                      source/elements/sh3n/coque3n/c3updt3.F
Chd|        CBAVISC                       source/elements/shell/coqueba/cbavisc.F
Chd|        CDKCOOR3                      source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CDKCURV3                      source/elements/sh3n/coquedk/cdkdefo3.F
Chd|        CDKDEFO3                      source/elements/sh3n/coquedk/cdkdefo3.F
Chd|        CDKDERI3                      source/elements/sh3n/coquedk/cdkderi3.F
Chd|        CDKDERIC3                     source/elements/sh3n/coquedk/cdkderi3.F
Chd|        CDKFCUM3                      source/elements/sh3n/coquedk/cdkfcum3.F
Chd|        CDKFINT3                      source/elements/sh3n/coquedk/cdkfint3.F
Chd|        CDKFINT_REG                   source/elements/sh3n/coquedk/cdkfint_reg.F
Chd|        CDKSTRA3                      source/elements/sh3n/coquedk/cdkstra3.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CNCOEF3                       source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNDT3                         source/elements/sh3n/coquedk/cndt3.F
Chd|        DTCDK_REG                     source/elements/sh3n/coquedk/dtcdk_reg.F
Chd|        DTTHERM                       source/elements/sh3n/coquedk/cndt3.F
Chd|        SET_FAILWAVE_NOD3             source/materials/fail/failwave/set_failwave_nod3.F
Chd|        SET_FAILWAVE_SH3N             source/materials/fail/failwave/upd_failwave_sh3n.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE CDKFORC3 (ELBUF_STR,
     1            JFT   ,JLT    ,PM     ,IXTG       ,X       ,
     2            F     ,M      ,V      ,R          ,FAILWAVE,
     3            NVC   ,MTN    ,GEO    ,TF         ,NPF     ,
     4            BUFMAT,PMSAV  ,DT2T   ,NELTST     ,ITYPTST ,
     5            STIFN ,STIFR  ,FSKY   ,IADTG      ,ITAB    ,
     6            EPSDOT,IPARTTG,THKE   ,GROUP_PARAM,F11     ,
     7            F12    ,F13   ,F21    ,F22    ,F23     ,
     8            F31    ,F32   ,F33    ,M11    ,M12     ,
     9            M13    ,M21   ,M22    ,M23    ,M31     ,
     A            M32    ,M33   ,MATPARAM_TAB,
     B            NEL    ,ISTRAIN,IHBE ,
     C            ITHK   ,IOFC   ,IPLA  ,NFT   ,ISMSTR   ,
     D            NPT    ,KFTS   ,
     E            IGEO   ,IPM    ,IFAILURE,GRESAV,GRTH   ,
     F            IGRTH  ,MSTG   ,DMELTG  ,JSMS  ,TABLE  ,
     G            IPARG  ,SENSOR_TAB,PTG    ,JTHE   ,
     H            CONDN  ,CONDNSKY,ISUBSTACK,STACK,ITASK ,
     I            DRAPE_SH3N  ,IPRI ,NLOC_DMG , INDX_DRAPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE TABLE_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MATPARAM_DEF_MOD
      USE NLOCAL_REG_MOD
      USE DRAPE_MOD
      USE SENSOR_MOD
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NVC, MTN,NELTST,ITYPTST,IUN,
     .        NEL,ISTRAIN,IHBE ,ITHK,IOFC,IPLA,NFT,ISMSTR ,
     .        NPT,KFTS,IFAILURE,JSMS,JTHE,ISUBSTACK,ITASK,IPRI
      INTEGER NPF(*),IXTG(NIXTG,*),IADTG(3,*),IGEO(NPROPGI,*),ITAB(*),
     .        IPM(NPROPMI,*),IPARTTG(*),GRTH(*),IGRTH(*),IPARG(*),INDX_DRAPE(SCDRAPE)
C     REAL
      my_real 
     .   PM(*), X(*), F(*), M(*), V(*), R(*),
     .   GEO(NPROPG,*), TF(*), BUFMAT(*), PMSAV(*),STIFN(*),
     .   STIFR(*),FSKY(*),EPSDOT(6,*),THKE(*),DT2T,
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ),
     .   GRESAV(*),MSTG(*), DMELTG(*),PTG(3,*),CONDN(*),CONDNSKY(*)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET  :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM
      TYPE(NLOCAL_STR_),  TARGET :: NLOC_DMG
      TYPE (DRAPE_), DIMENSION (NUMELTG_DRAPE):: DRAPE_SH3N
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c  indx utilise localement contrairement aux coques 4n 
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),INDX(MVSIZ),FWAVE_EL(NEL),
     .   IFLAG,LENF,LENM,LENS,IR,IS,IT,IPT,NPTT,N1,N2,N3,
     .   I,II,J,JJ,NG,NPG,NNOD,NPTR,NPTS,NLAY,L_DIRA,L_DIRB,IFAILWAVE,
     .   PT0,PT1,PT2,PT3,PTF,PTM,PTE,PTEP,PTS,IGTYP,IBID,J1,J2,
     .   IGMAT,ILAY,NPTTOT,IREP,KK(5),K,IDRAPE
      INTEGER, DIMENSION(NEL) :: OFFLY
      PARAMETER (NPG = 3)
      PARAMETER (NNOD = 3)
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),RHO(MVSIZ),
     .   SSP(MVSIZ),VISCMX(MVSIZ),AREA(MVSIZ),AREA2(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),
     .   PX2(MVSIZ),PY2(MVSIZ),  PX3(MVSIZ), PY3(MVSIZ),
     .   OFF(MVSIZ), SIGY(MVSIZ),THK0(MVSIZ),
     .   NU(MVSIZ) , SHF(MVSIZ), DT1C(MVSIZ),
     .   G(MVSIZ)   , YM(MVSIZ)  , A11(MVSIZ)   , A12(MVSIZ),
     .   VOL0(MVSIZ),THK02(MVSIZ),ZCFAC(MVSIZ,2), GS(MVSIZ),
     .   VOL00(MVSIZ),ALPE(MVSIZ),A_HAMMER(3,2),ONE_OVER_3,O2_3TH,
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),ALDT(MVSIZ),
     .   VLX(MVSIZ,2),VLY(MVSIZ,2),VLZ(MVSIZ,2),RLX(MVSIZ,3),RLY(MVSIZ,3),
     .   PX(MVSIZ,3),PY(MVSIZ,3),PXY(MVSIZ,3), PYY(MVSIZ,3),
     .   BZ1(MVSIZ,2),BZ2(MVSIZ,2),BZ3(MVSIZ,2), BRX1(MVSIZ,3),
     .   BRX2(MVSIZ,3),BRX3(MVSIZ,3),BRY1(MVSIZ,3),BRY2(MVSIZ,3),
     .   BRY3(MVSIZ,3),AMU(MVSIZ),CDET(MVSIZ),VDEF(MVSIZ,8),DIE(MVSIZ), 
     .   TEMPEL(MVSIZ),KRZ(MVSIZ),DIR1_CRK(NPT,MVSIZ),
     .   DIR2_CRK(NPT,MVSIZ),CONDE(MVSIZ),A11R(MVSIZ)
      my_real, 
     .   DIMENSION(1),TARGET :: BID
      my_real
     .   X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ),
     .   Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ),
     .   Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ),
     .   X2L(MVSIZ),Y2L(MVSIZ),X3L(MVSIZ),Y3L(MVSIZ)
      my_real, 
     :    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A,DIR_B,CRKDIR,DADV
      my_real, 
     .    ALLOCATABLE, DIMENSION(:,:,:) :: PLAPT_LAY,SIGPT_LAY
C--- Variables pour le non-local
      INTEGER :: NDDL, INOD(3),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), L_NLOC, IPOS(3),INLOC
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: DNL,UNL
      my_real
     .   KSI,ETA
C-----
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF1,LBUF2,LBUF3 ,LBUF
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR    
C-----------------------------------------------
      DATA A_HAMMER / 
     1 0.166666666666667,0.666666666666667,0.166666666666667,             
     2 0.166666666666667,0.166666666666667,0.666666666666667/
C=======================================================================
C GENERALIZED STRAINS ARE SAVED AT NB8 (GENERALIZED FOR ALL MATERIALS)
C
C     NB1    |5*JLT    | FORCES  
C     NB2    |3*JLT    | MOMENTS
C     NB3    |JLT      | THK0
C     NB4    |5*JLT    | UNUSED : ONLY FOR FINDING OFF
C     NB5    |2*JLT    | ENERGIES
C     NB6    |JLT      | OFF
C     NB8    |8*JLT    | DEFORMATIONS
C     NB10   |5*NPT*JLT         | Contraintes moyennes->post  
C     NB11   |max(1,npt)*JLT    | def plastique moyennes->post
C     NB12   |8*NPG*JLT         | contraintes generalisees 
C     NB13   |8*NPG*JLT         | def generalise
C     NB14   |max(1,npt)*NPG*JLT| def plastique
C     NB15   |5*NPG*NPT*JLT     | contraintes pour laws et npt >0
C     NB16   |2*NPT*JLT         | axe I  d'orthotropie/anisotropie
C     NB16A  |2*NPT*JLT         | axe II d'anisotropie
C=======================================================================
      GBUF   => ELBUF_STR%GBUF
      IDRAPE = ELBUF_STR%IDRAPE
C---
      IUN  = 1
      IBID = 0
      BID  = ZERO
      IGTYP = IGEO(11,IXTG(5,1))
      IREP = IPARG(35)
      INLOC= IPARG(78)
      SEDRAPE = STDRAPE
      NUMEL_DRAPE = NUMELTG_DRAPE
!
      DO J=1,5
        KK(J) = NEL*(J-1)
      ENDDO
!
C
      NLAY = ELBUF_STR%NLAY
c     NPT  --> set to = IPARG(6) , keeping it original to allow for NPT = 0 (global LAW_3
C
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL))
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IFAILWAVE = IPARG(79)
      IF (IFAILWAVE > 0) THEN
        FWAVE_EL(:) = ZERO
        OFFLY(:) = ELBUF_STR%BUFLY(1)%OFF(:)
        DO I=2,NLAY
          DO J=1,NEL
            OFFLY(J) = MAX(OFFLY(J), ELBUF_STR%BUFLY(I)%OFF(J))
          ENDDO
        ENDDO        
        DADV   => GBUF%DMG
        CALL SET_FAILWAVE_SH3N(FAILWAVE ,FWAVE_EL ,DADV     ,
     .      NEL      ,IXTG    ,ITAB     ,NGL      ,OFFLY    )
c
      ENDIF
c-------------------------------------
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
        ALLOCATE(DIRA(NPTTOT*NEL*L_DIRA))
        ALLOCATE(DIRB(NPTTOT*NEL*L_DIRB))
        IF (L_DIRA == 0) THEN
            CONTINUE
        ELSEIF (IREP == 0) THEN
           NPTTOT = 0
           DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                 J = NPTTOT + IT
                 LBUF_DIR =>  ELBUF_STR%BUFLY(ILAY)%LBUF_DIR(IT)
                 J1 = 1+(J-1)*L_DIRA*NEL
                 J2 = J*L_DIRA*NEL
                 DIRA(J1:J2) = LBUF_DIR%DIRA(1:NEL*L_DIRA)
              ENDDO
              NPTTOT = NPTTOT + NPTT
            ENDDO 
        ENDIF
        DIR_A => DIRA(1:NPTTOT*NEL*L_DIRA)
        DIR_B => DIRB(1:NPTTOT*NEL*L_DIRB)
      ELSE ! idrape
        ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
        ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
        DIRA=ZERO
        DIRB=ZERO
        IF (L_DIRA == 0) THEN
          CONTINUE
        ELSEIF (IREP == 0) THEN
           DO J=1,NLAY
              J1 = 1+(J-1)*L_DIRA*NEL
              J2 = J*L_DIRA*NEL
              DIRA(J1:J2) = ELBUF_STR%BUFLY(J)%DIRA(1:NEL*L_DIRA)
           ENDDO
         ENDIF
         DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
         DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
      ENDIF ! IDRAPE
C
       IF (IGTYP == 51 .OR. IGTYP == 52) THEN
         ALLOCATE(PLAPT_LAY(NLAY,NPG,MVSIZ))
         ALLOCATE(SIGPT_LAY(NLAY,NPG,5*MVSIZ))
         PLAPT_LAY = ZERO
         SIGPT_LAY = ZERO
       ELSE
         ALLOCATE(PLAPT_LAY(0,0,0))
         ALLOCATE(SIGPT_LAY(0,0,0))
       ENDIF
c-------------------------------------
      IGTYP = IGEO(11,IXTG(5,1))
      IGMAT = IGEO(98 ,IXTG(5,1))

C     Initialize MAT and PID (because they are used in CMATBUF3
      DO I=JFT,JLT
        MAT(I) = IXTG(1,I)
        PID(I) = IXTG(5,I)
        A11R(I) = ZERO
      ENDDO
      TEMPEL(JLT:JFT) = ZERO  ! transfert thermique pour les coques 3 noeud n'est pas dispo
C---
      CALL CDKCOOR3(ELBUF_STR,
     .              JFT,JLT,MAT,PID,NGL,X,V,R,IXTG,GBUF%OFF,
     .              OFF,R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .              X2L,Y2L,X3L,Y3L,GBUF%SMSTR,
     .              AREA,AREA2,CDET,VLX,VLY,VLZ,RLX,RLY,
     .              ISMSTR,IREP,NLAY,DIR_A,DIR_B,
     .              F11,F12,F13,F21,F22,F23,F32,F33,
     .              M11,M12,M13,M21,M22,M23,NEL)
      CALL CNCOEF3(JFT    ,JLT     ,PM     ,MAT     ,GEO     ,
     2             PID    ,OFF     ,AREA   ,SHF     ,THK0    ,
     3             THK02  ,NU     ,G       ,YM      , 
     4             A11    ,A12     ,GBUF%THK,THKE   ,SSP     ,
     5             RHO    ,VOL00   ,GS     ,MTN     ,ITHK    ,
     6             NPTTOT ,DT1C    ,DT1    ,IHBE    ,AMU     ,
     7             KRZ    ,IGEO    ,A11R   ,ISUBSTACK, STACK%PM)
      CALL CDKDERIC3(JFT ,JLT, X2L,Y2L,X3L,Y3L,AREA2,ALPE,ALDT,
     1              PX2,PY2,PX3,PY3,PX,PY,PXY,PYY,VOL0,VOL00,
     2              NU,THK02)
C
      CALL CDKDEFO3(JFT,JLT,VLX,VLY,PX2,PY2,PX3,PY3,EXX,EYY,EXY,
     1              EXZ, EYZ,DT1,EPSDOT,NFT,ISTRAIN,GBUF%STRA,VDEF,NEL)
C-----------------------------------------------
C     BOUCLE SUR POINTS D'INTEGRATION DE GAUSS
C-----------------------------------------------
      LENF = NEL*GBUF%G_FORPG/NPG
      LENM = NEL*GBUF%G_MOMPG/NPG
      LENS = NEL*GBUF%G_STRPG/NPG
      IT   = 1
c
      DO NG =1,NPG
        IR = NG
        IS = 1
        PTF = (NG-1)*LENF+1
        PTM = (NG-1)*LENM+1
        PTS = (NG-1)*LENS+1
c
       CALL CDKDERI3(JFT ,JLT,PX2,PY2,PX3,PY3,PX,PY,PXY,PYY,  
     1               BZ1,BZ2,BZ3,BRX1,BRX2,BRX3,BRY1,BRY2,BRY3,
     2               A_HAMMER(NG,1),A_HAMMER(NG,2))
       CALL CDKCURV3(JFT,JLT,BZ1,BZ2,BZ3,BRX1,BRX2,BRX3,BRY1,
     1               BRY2,BRY3,VLZ,RLX,RLY,KXX, KYY, KXY)
       CALL CDKSTRA3(JFT,JLT,GBUF%STRA,EXX,EYY,EXY,KXX, KYY, KXY, 
     1               EPSDOT,NFT,ISTRAIN,DT1,GBUF%STRPG(PTS),NEL)
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------     
       IF (INLOC > 0) THEN
         L_NLOC = NLOC_DMG%L_NLOC
         DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
         UNL  => NLOC_DMG%UNL(1:L_NLOC)
         ETA  = A_HAMMER(NG,1)
         KSI  = A_HAMMER(NG,2)
         VAR_REG(1:NEL,1:NDDL) = ZERO
         DO I = JFT,JLT
           NC1(I)  = IXTG(2,I)
           NC2(I)  = IXTG(3,I)
           NC3(I)  = IXTG(4,I)
         ENDDO
         DO K = 1,NDDL
#include "vectorize.inc"
           DO I = JFT,JLT
             INOD(1) = NLOC_DMG%IDXI(NC1(I))
             INOD(2) = NLOC_DMG%IDXI(NC2(I))
             INOD(3) = NLOC_DMG%IDXI(NC3(I))
             IPOS(1) = NLOC_DMG%POSI(INOD(1))
             IPOS(2) = NLOC_DMG%POSI(INOD(2))
             IPOS(3) = NLOC_DMG%POSI(INOD(3))
             VAR_REG(I,K) = (ONE-ETA-KSI)*DNL(IPOS(1)+K-1) +
     .                               ETA*DNL(IPOS(2)+K-1) +
     .                               KSI*DNL(IPOS(3)+K-1)
           ENDDO
         ENDDO
       ENDIF
C----------------------------------------------------------------------------
       CALL CMAIN3(
     1     ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2     NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3     PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4     SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5     CDET      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6     EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7     OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8     GBUF%FORPG(PTF),GBUF%MOMPG(PTM) ,GBUF%STRPG(PTS),FAILWAVE,FWAVE_EL,
     9     GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A     G         ,A11       ,A12       ,VOL0      ,INDX      ,
     B     NGL       ,ZCFAC     ,SHF       ,GS        ,GBUF%EPSD ,
     C     KFTS      ,IHBE      ,ALPE      ,
     D     DIR_A     ,DIR_B     ,IGEO      ,
     E     IPM       ,IFAILURE  ,NPG       ,
     F     TEMPEL    ,DIE       ,IBID      ,IBID      ,BID       ,
     G     IBID      ,BID       ,
     H     BID       ,BID       ,BID       ,BID       ,BID       ,
     I     BID       ,BID       ,BID       ,R11       ,R12       ,
     J     R13       ,R21       ,R22       ,R23       ,R31       ,
     K     R32       ,R33       ,NG        ,TABLE     ,IBID      ,
     L     BID       ,SENSOR_TAB,BID       ,IBID      ,
     M     BID       ,BID       ,ALDT      ,
     N     ISMSTR    ,IR        ,IS        ,NLAY      ,NPT       ,
     O     IBID      ,IBID      ,ISUBSTACK ,STACK     ,
     P     BID       ,ITASK     ,DRAPE_SH3N  ,VAR_REG   ,NLOC_DMG  ,
     R    INDX_DRAPE)
C----------------------------------------------------------------------------
C     THICKNESS CORRECTION 
C----------------------------
      IF (ITHK > 0) THEN
        DO I=JFT,JLT
          GBUF%THK(I) = GBUF%THK(I) - TWO_THIRD*(GBUF%THK(I)-THK0(I))  
          THK0(I)     = GBUF%THK(I)                                 
        ENDDO
      ENDIF
C----------------------------------------------------------------------------
C     FORCES VISCOCITE 
C----------------------------
C       uniquement membranaire pour l'instant--------
      CALL CBAVISC(JFT     ,JLT            ,VDEF           ,AMU ,OFF ,
     2             SHF     ,NU             ,RHO            ,SSP ,CDET,
     3             THK0    ,GBUF%FORPG(PTF),GBUF%MOMPG(PTM),IUN ,MTN ,
     4             IPARTTG ,PMSAV          ,DT1            ,NEL )
C----------------------------
C     FORCES INTERNES
C----------------------------
        CALL CDKFINT3(JFT,JLT,VOL0,THK0,GBUF%FORPG(PTF),GBUF%MOMPG(PTM),
     1                PX2,PY2,PX3,PY3,                      
     2                BZ1,BZ2,BZ3,                          
     3                BRX1,BRX2,BRX3,BRY1,BRY2,BRY3,        
     4                F11,F12,F13,F21,F22,F23,F32,F33,      
     5                M11,M12,M13,M21,M22,M23,
     6                NEL)           
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
        IF (INLOC > 0) THEN
          CALL CDKFINT_REG(NLOC_DMG,VAR_REG ,THK0    ,NEL     ,
     .                     GBUF%OFF,AREA    ,NC1     ,NC2     , 
     .                     NC3     ,PX2     ,PY2     ,PX3     ,
     .                     PY3     ,KSI     ,ETA     ,
     .                     ELBUF_STR%NLOC(IR,IS)     ,IXTG(1,JFT),
     .                     NDDL    ,ITASK   ,NG      ,DT2T    ,
     .                     GBUF%THK_I,GBUF%AREA)
        ENDIF 
c-------------------------------
      ENDDO  !  NG = 1,NPG
C----
C temporary storage for mean PLAPT, SIGPT
C----
      IF ((IGTYP == 51.OR. IGTYP == 52) .AND. NLAY > 1) THEN
        DO ILAY=1,NLAY
          BUFLY => ELBUF_STR%BUFLY(ILAY)
          NPTT = BUFLY%NPTT
          DO IT = 1,NPTT
            DO NG =1,NPG
              IR = NG
              IS = 1
              DO I=JFT,JLT
                IF (BUFLY%L_PLA > 0) THEN
                  PLAPT_LAY(ILAY,NG,I) = PLAPT_LAY(ILAY,NG,I) + 
     .                                   BUFLY%LBUF(IR,IS,IT)%PLA(I)/NPTT
                ELSE
                  PLAPT_LAY(ILAY,NG,I) = ZERO
                ENDIF
                II = (I-1)*5
                DO J=1,5
                  JJ = II+J
                  SIGPT_LAY(ILAY,NG,JJ) = SIGPT_LAY(ILAY,NG,JJ) + 
     .                                    BUFLY%LBUF(IR,IS,IT)%SIG(KK(J)+I)/NPTT
                ENDDO
              ENDDO ! DO I=JFT,JLT
            ENDDO ! DO NG =1,NPG
          ENDDO ! DO IT = 1,NPTT
        ENDDO ! DO ILAY=1,NLAY
      ENDIF  ! IF (IGTYP == 51) THEN
C----
C----------------------------------------------------------------------------
C     FIN DE BOUCLE DE 3 POINTS DE INTEGRATION------------
C----------------------------------------------------------------------------
C     POST-TRAITEMENT - valeurs moyennes
C----------------------------
C---
C    = FOR, MOM =
C---
      PT1 = 0
      PT2 = PT1 + LENF
      PT3 = PT2 + LENF
      DO I=JFT,JLT
        DO J=1,5
          GBUF%FOR(KK(J)+I) = THIRD*(GBUF%FORPG(PT1+KK(J)+I)
     .                             + GBUF%FORPG(PT2+KK(J)+I)
     .                             + GBUF%FORPG(PT3+KK(J)+I))
        ENDDO 
      ENDDO
      PT2 = PT1 + LENM
      PT3 = PT2 + LENM
      DO I=JFT,JLT
        DO J=1,3
          GBUF%MOM(KK(J)+I) = THIRD*(GBUF%MOMPG(PT1+KK(J)+I)
     .                             + GBUF%MOMPG(PT2+KK(J)+I)
     .                             + GBUF%MOMPG(PT3+KK(J)+I))	
         ENDDO
      ENDDO
C---
C    = PLAPT =
C---
      IF (NLAY > 1) THEN
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          DO IPT = 1,NLAY
            BUFLY => ELBUF_STR%BUFLY(IPT)
            IF (BUFLY%L_PLA > 0) THEN
              DO I=JFT,JLT  
                BUFLY%PLAPT(I) = THIRD*(PLAPT_LAY(IPT,1,I) + PLAPT_LAY(IPT,2,I) +
     .                                  PLAPT_LAY(IPT,3,I))
              ENDDO
            ENDIF
          ENDDO
        ELSE  ! (IGTYP /= 51)
          DO IPT = 1,NLAY
            BUFLY => ELBUF_STR%BUFLY(IPT)
            IF (BUFLY%L_PLA > 0) THEN
              LBUF1 => ELBUF_STR%BUFLY(IPT)%LBUF(1,1,1)
              LBUF2 => ELBUF_STR%BUFLY(IPT)%LBUF(2,1,1)
              LBUF3 => ELBUF_STR%BUFLY(IPT)%LBUF(3,1,1)
              DO I=JFT,JLT  
                BUFLY%PLAPT(I) = THIRD*(LBUF1%PLA(I) + LBUF2%PLA(I) +
     .                                  LBUF3%PLA(I))
              ENDDO
            ENDIF
          ENDDO
        ENDIF  !  IF (IGTYP == 51) THEN
      ELSE  !  (NLAY == 1)
        BUFLY => ELBUF_STR%BUFLY(1)
        DO IPT = 1,BUFLY%NPTT
          IF (BUFLY%L_PLA > 0) THEN
            LBUF1 => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
            LBUF2 => ELBUF_STR%BUFLY(1)%LBUF(2,1,IPT)
            LBUF3 => ELBUF_STR%BUFLY(1)%LBUF(3,1,IPT)
            II = (IPT-1)*NEL
            DO I=JFT,JLT                                                 
              BUFLY%PLAPT(II+I) = THIRD*(LBUF1%PLA(I) + LBUF2%PLA(I) +
     .                                   LBUF3%PLA(I))
            ENDDO
          ENDIF
        ENDDO
      ENDIF !  IF (NLAY > 1) THEN
C---
C    = SIGPT =
C---
      IF (NLAY > 1) THEN
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          DO IPT = 1,NLAY
            BUFLY => ELBUF_STR%BUFLY(IPT)
            DO I=JFT,JLT*5  
              BUFLY%SIGPT(I) = THIRD*(SIGPT_LAY(IPT,1,I) + SIGPT_LAY(IPT,2,I) +
     .                                SIGPT_LAY(IPT,3,I))
            ENDDO
          ENDDO
        ELSE  ! (IGTYP /= 51)
          DO IPT = 1,NLAY
            BUFLY => ELBUF_STR%BUFLY(IPT)
            LBUF1 => ELBUF_STR%BUFLY(IPT)%LBUF(1,1,1)
            LBUF2 => ELBUF_STR%BUFLY(IPT)%LBUF(2,1,1)
            LBUF3 => ELBUF_STR%BUFLY(IPT)%LBUF(3,1,1)
            DO I=JFT,JLT
              II = (I-1)*5
              DO J=1,5
               JJ = II+J
               BUFLY%SIGPT(JJ) = THIRD*(LBUF1%SIG(KK(J)+I) + LBUF2%SIG(KK(J)+I) +
     .                                  LBUF3%SIG(KK(J)+I))
              ENDDO
            ENDDO
          ENDDO
        ENDIF  !  IF (IGTYP == 51) THEN
      ELSE  !  (NLAY == 1)
        BUFLY => ELBUF_STR%BUFLY(1)
        DO IPT = 1,BUFLY%NPTT
          LBUF1 => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
          LBUF2 => ELBUF_STR%BUFLY(1)%LBUF(2,1,IPT)
          LBUF3 => ELBUF_STR%BUFLY(1)%LBUF(3,1,IPT)
          K = (IPT-1)*NEL*5
          DO I=JFT,JLT
            II = (I-1)*5
            DO J=1,5
              JJ = K+II+J
              BUFLY%SIGPT(JJ) = THIRD*(LBUF1%SIG(KK(J)+I) + LBUF2%SIG(KK(J)+I)
     .                               + LBUF3%SIG(KK(J)+I))
            ENDDO
          ENDDO
        ENDDO
      ENDIF !  IF (NLAY > 1) THEN
C-------------------------
C     ASSEMBLE
C-------------------------
      CALL CDKFCUM3(JFT,JLT,PX2,PY2,PX3,PY3,
     1              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     2              F11,F12,F13,F21,F22,F23,F31,F32,F33,
     3              M11,M12,M13,M21,M22,M23,M31,M32,M33)
C 
C--------------------------
C     PAS DE TEMPS
C--------------------------
      CALL CNDT3(
     1        JFT    ,JLT    ,OFF      ,DT2T    ,AMU     ,
     2        NELTST ,ITYPTST,STI      ,STIR    ,GBUF%OFF,
     3        SSP    ,VISCMX ,RHO      ,VOL00   ,THK0    ,THK02,
     4        A11    ,ALDT   ,ALPE     ,NGL     , ISMSTR,
     5        IOFC   ,NNOD   ,AREA     ,G       ,SHF   ,
     6        MSTG   ,DMELTG ,JSMS     ,PTG     ,IGTYP ,
     7        IGMAT  ,A11R   ,GBUF%G_DT, GBUF%DT,MTN   ,
     8        PM     ,MAT(JFT))
C--------------------------
C     THERMAL TIME STEP
C--------------------------
        IF(JTHE > 0.AND.IDT_THERM == 1)THEN
           CALL DTTHERM(JFT  ,JLT  ,PM  ,TEMPEL,GBUF%RE ,
     2                  RHO  ,GBUF%RK ,VOL0 ,ALDT,MAT ,
     3                  DT_THERM,OFF    ,CONDE) 
         ENDIF
C--------------------------
C     NON-LOCAL TIME STEP
      IF (INLOC > 0) THEN
        CALL DTCDK_REG(NLOC_DMG,THK0       ,NEL     ,GBUF%OFF,
     .                 ALDT    ,IXTG(1,JFT),NDDL    ,DT2T    )
      ENDIF 
C--------------------------
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
c      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IPRI>0) 
     1   CALL C3BILAN(JFT  ,JLT    ,PM   ,V    ,GBUF%THK,GBUF%EINT,
     2                PMSAV,IPARTTG,RHO  ,VOL00,IXTG   ,X         ,
     3                R    ,THK02  ,AREA ,GRESAV ,GRTH      ,
     4                IGRTH,OFF    ,IBID ,IBID ,IBID   ,IBID      ,
     5                IBID ,IBID   ,GBUF%EINTTH,ITASK  ,MAT ,GBUF%VOL)
c
      IF (IPARIT == 0) THEN
        CALL C3UPDT3(JFT  ,JLT  ,F   ,M   ,NVC  ,
     2             GBUF%OFF ,OFF      ,STI       ,STIR    ,STIFN  ,
     3             STIFR  ,IXTG ,F11  ,
     4             F12    ,F13  ,F21 ,F22 ,F23  ,
     5             F31    ,F32  ,F33 ,M11 ,M12  ,
     7             M13    ,M21  ,M22 ,M23 ,M31  ,
     8             M32      ,M33      ,IBID      ,BID     ,BID    ,
     9             GBUF%EINT,PM       ,AREA      ,GBUF%THK,
     A             PMSAV    ,MAT      ,IPARTTG   ,CONDN   ,CONDE  )
      ELSE
        CALL C3UPDT3P(JFT   ,JLT      ,GBUF%OFF ,OFF     ,STI     ,
     2             STIR   ,FSKY ,FSKY,IADTG ,F11,
     4             F12    ,F13  ,F21 ,F22 ,F23  ,
     5             F31    ,F32  ,F33 ,M11 ,M12  ,
     7             M13    ,M21  ,M22 ,M23 ,M31  ,
     8             M32    ,M33  ,IBID,BID ,BID,
     8             GBUF%EINT,PM       ,AREA     ,GBUF%THK,
     B             PMSAV  ,MAT  ,IPARTTG,CONDNSKY,
     C            CONDE)
      ENDIF
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IF (IFAILWAVE > 0) THEN
        CRKDIR => ELBUF_STR%BUFLY(1)%CRKDIR
c        
        CALL SET_FAILWAVE_NOD3(FAILWAVE   ,FWAVE_EL ,NGL      ,
     .       NEL      ,IXTG     ,ITAB     ,CRKDIR   ,DIR_A    ,
     .       L_DIRA   ,X2L      ,X3L      ,Y2L      ,Y3L      )
      ENDIF
C------------
      IF (ALLOCATED(DIRB)) DEALLOCATE(DIRB)                                                          
      IF (ALLOCATED(DIRA)) DEALLOCATE(DIRA)                                                          
      IF (ALLOCATED(PLAPT_LAY)) DEALLOCATE(PLAPT_LAY)
      IF (ALLOCATED(SIGPT_LAY)) DEALLOCATE(SIGPT_LAY)
      IF (ALLOCATED(VAR_REG))    DEALLOCATE(VAR_REG)
C------------
      RETURN
      END
