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
Chd|  MATINI                        source/materials/mat_share/matini.F
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SPINIT3                       source/elements/sph/spinit3.F 
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        AGRAD2                        source/ale/ale2d/agrad2.F     
Chd|        AGRAD3                        source/ale/ale3d/agrad3.F     
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ATHLEN                        source/ale/athlen.F           
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        M105INIT                      source/materials/mat/mat105/m105init.F
Chd|        M24IN2                        source/materials/mat/mat024/m24in2.F
Chd|        M24IN3                        source/materials/mat/mat024/m24in3.F
Chd|        M24INSPH                      source/elements/sph/m24insph.F
Chd|        M37INIT                       source/materials/mat/mat037/m37init.F
Chd|        M38INIT                       source/materials/mat/mat038/m38init.F
Chd|        M51INIT                       source/materials/mat/mat051/m51init.F
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|        M6IN                          source/materials/mat/mat006/m6in.F
Chd|        M70INIT                       source/materials/mat/mat070/m70init.F
Chd|        M75INIT                       source/materials/mat/mat075/m75init.F
Chd|        M77INIT                       source/materials/mat/mat077/m77init.F
Chd|        M95INIT                       source/materials/mat/mat095/m95init.F
Chd|        M97INIT                       source/materials/mat/mat097/m97init.F
Chd|        MATING                        source/materials/mat_share/mating.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MATINI(PM       ,IX               ,NIX         ,X         ,
     .                  GEO      ,ALE_CONNECTIVITY ,DETONATORS  ,IPARG     ,
     .                  SIGI     ,NEL              ,SKEW        ,IGEO      ,
     .                  IPART    ,IPARTEL          ,
     .                  MAT      ,IPM              ,NSIG        ,NUMS      ,PT     ,
     .                  IPT      ,NGL              ,NPF         ,TF        ,BUFMAT ,
     .                  GBUF     ,LBUF             ,MBUF        ,ELBUF_STR ,ILOADP ,
     .                  FACLOAD  ,DDELTAX)   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD   
      USE DETONATORS_MOD   
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr19_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NEL,NSIG,NUMS,IPT,JALE_FROM_PROP,JALE_FROM_MAT,JALE_MAX
      INTEGER IX(NIX,*), IPARG(*),IPART(LIPART1,*),IPARTEL(*),MAT(*),IPM(NPROPMI,*),PT(*), NGL(*),NPF(*)
      INTEGER,INTENT(IN) :: IGEO(NPROPGI,NUMGEO)
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real X(*), GEO(*), PM(NPROPM,*),SIGI(NSIG,*),SKEW(LSKEW,*),BUFMAT(*),TF(*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      my_real, INTENT(IN) :: DDELTAX(*)
      TYPE(G_BUFEL_), TARGET  :: GBUF     
      TYPE(L_BUFEL_), TARGET  :: LBUF     
      TYPE(BUF_MAT_) :: MBUF
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TARGET  :: BUFMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*nchartitle, TITR
      INTEGER I,IADBUF,NPAR,NFUNC,NUVAR,IFORM,NUMEL
      INTEGER IFUNC(MAXFUNC)
      INTEGER ID
      my_real RHO0(MVSIZ)
      my_real ,DIMENSION(MVSIZ) ,TARGET :: TMP,EPL,FILLO
      my_real ,DIMENSION(:) ,POINTER ::
     .   OFF,SIG,EINT,RHO,VOL,EPSD,DELTAX,TB,ANG,SF,VK,ROB,UVAR,EPLAS,FILL,DTEL,UPARAM,TEMP
C=======================================================================
      TMP(1:NEL)=ZERO
      EPL(1:NEL)=ZERO
      
      IF (IPT == 0) THEN
        OFF   => GBUF%OFF(1:NEL)
        SIG   => GBUF%SIG(1:NEL*6)
        EINT  => GBUF%EINT(1:NEL)
        EPSD  => GBUF%EPSD(1:NEL)
        RHO   => GBUF%RHO(1:NEL)
        VOL   => GBUF%VOL(1:NEL)
        IF(SIZE(GBUF%DELTAX)>0) DELTAX=> GBUF%DELTAX(1:NEL)
        IF(SIZE(GBUF%TB)>0) TB => GBUF%TB(1:NEL)
        DTEL  => GBUF%DT(1:NEL)
        IF (GBUF%G_TEMP > 0) THEN
          TEMP => GBUF%TEMP(1:NEL)
        ELSE
          TEMP => TMP(1:NEL)
        ENDIF
        IF (GBUF%G_PLA > 0) THEN
          EPLAS => GBUF%PLA(1:NEL)
        ELSE
          EPLAS => EPL(1:NEL)
        ENDIF
      ELSE
        OFF   => LBUF%OFF(1:NEL)
        SIG   => LBUF%SIG(1:NEL*6)
        EINT  => LBUF%EINT(1:NEL)
        EPSD  => LBUF%EPSD(1:NEL) 
        RHO   => LBUF%RHO(1:NEL)
        VOL   => LBUF%VOL(1:NEL)
        IF(SIZE(LBUF%DELTAX)>0) DELTAX=> LBUF%DELTAX(1:NEL)
        IF(SIZE(LBUF%TB)>0) TB    => LBUF%TB(1:NEL)
        IF (ELBUF_STR%BUFLY(1)%L_TEMP > 0) THEN
          TEMP => LBUF%TEMP(1:NEL) 
        ELSE
          TEMP => TMP(1:NEL)
        ENDIF               
        IF (ELBUF_STR%BUFLY(1)%L_PLA > 0) THEN
          EPLAS => LBUF%PLA(1:NEL) 
        ELSE
          EPLAS => EPL(1:NEL)
        ENDIF
      ENDIF
c
      IF(JSPH==0)THEN
         FILL  => GBUF%FILL(1:NEL)
      ELSE
         FILLO(1:NEL)=ONE
         FILL => FILLO(1:NEL)
      END IF
c
      CALL MATING(PM      ,VOL     ,OFF     ,EINT     ,RHO      ,
     .            SIG     ,IX      ,NIX     ,SIGI     ,EPLAS    ,
     .            NSIG    ,MAT     ,NUMS    ,PT       ,NEL      ,
     .            FILL    ,TEMP    )
     
C--------------------
      IF(JMULT <= 1)THEN  !reset only once
        IPARG(15)=0   !  FLAG REZONE SIG
        IPARG(16)=0   !  FLAG REZONE EPS PLAST.
        IPARG(63)=0   !  FLAG FOR FLUID MATERIAL
        IPARG(64)=0   !  FLAG FOR NON REFLECTING BOUNDARY      
      ENDIF
C---------------------------      
      IF (MTN == 1) THEN
        IPARG(15)=1
      ELSEIF (MTN == 2.OR.MTN == 3.OR.MTN == 4) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN == 5) THEN
        IPARG(16)=1   !  FLAG REZONE EPS PLAST. (BURNING TIME)
        IF(N2D == 0)THEN
          CALL M5IN3 (PM,MAT,0,DETONATORS,TB,NGL,IPARG,X,IX,NIX)
        ELSE
          CALL M5IN2 (PM,MAT,0,DETONATORS,TB,NGL,X,IX,NIX)
        ENDIF
        IPARG(63)=1 !Loi fluide
      ELSEIF (MTN == 6) THEN
        IF(ISGIFL > 0) THEN
          CALL M6IN(PM,MAT,SIG,RHO,NEL)
        ENDIF
        IPARG(15)=0
        IPARG(63)=1 !Loi fluide
      ELSEIF (MTN == 10) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN == 11) THEN
        IPARG(63)=1 !Loi fluide
        IPARG(64)=1 !Loi frontiere
        IF(N2D == 0)THEN
          NUMEL=NUMELS
        ELSE
          NUMEL=NUMELQ+NUMELTG
        ENDIF
        JALE_FROM_PROP = IGEO(62,IABS(IX(NIX-1,1))) !ALE/EULER from /PROP/TYPE14(SOLID)
        JALE_FROM_MAT = IPARG(7)+IPARG(11)          !ALE/EULER from /ALE/MAT or /EULER/MAT
        JALE_MAX = MAX(JALE_FROM_PROP, JALE_FROM_MAT)
        ID = IPM(1,MAT(1))
        CALL MAT11CHECK(PM,NIX,IX,ALE_CONNECTIVITY,NUMEL,JALE_MAX,NEL,NFT,ID,NUMMAT,NPROPM)
      ELSEIF (MTN == 12) THEN
        IPARG(15)=1
        IPARG(16)=1
        IF (N2D /= 0)THEN
          WRITE(IOUT,'(A)') ' LAW 12 IS NOT AVAILABLE IN 2D ANALYSIS'
          CALL ARRET(2)
        ENDIF
      ELSEIF (MTN == 14) THEN
        IPARG(15)=1
        IPARG(16)=1
        IF (N2D /= 0)THEN
          WRITE(IOUT,'(A)') ' LAW 14 IS NOT AVAILABLE IN 2D ANALYSIS'
          CALL ARRET(2)
        ENDIF
      ELSEIF (MTN == 16) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN == 17) THEN
        IF(ISGIFL > 0) THEN
          CALL M6IN(PM,MAT,SIG,RHO,NEL)
        ENDIF
        IPARG(15)=0
        IPARG(63)=1 !Loi fluide
      ELSEIF (MTN == 18) THEN
        CALL ATHLEN(DELTAX, DDELTAX)
        IF(JSPH == 0)THEN
          IF(N2D == 0)THEN
            CALL AGRAD3(IX,X,ALE_CONNECTIVITY,SIG,NEL)
          ELSE
            CALL AGRAD2(IX,X,ALE_CONNECTIVITY,SIG,NEL)
          ENDIF
        ENDIF
      ELSEIF (MTN == 20) THEN
        IF(IPARG(5)/=2)THEN
          CALL ANCMSG(MSGID=129,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
          CALL ARRET(2)                 
                 ENDIF
      ELSEIF (MTN == 21.OR.MTN == 22.OR.MTN == 23) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN == 24) THEN
        IPARG(15)=1
        IPARG(16)=1
        ANG => LBUF%ANG(1:NEL*6)
        SF  => LBUF%SF(1:NEL*3)
        VK  => LBUF%VK(1:NEL)
        ROB => LBUF%ROB(1:NEL)
        IF (JSPH == 0) THEN
          IF (N2D == 0) THEN
            CALL M24IN3(PM  ,IX  ,ANG ,SF ,VK  ,ROB, NEL)
         ELSE
            CALL M24IN2(PM  ,IX  ,ANG ,SF ,VK  ,ROB, NEL)
         ENDIF
        ELSE
          CALL M24INSPH(PM      ,ANG     ,SF    ,VK     ,ROB    ,
     .                  IPART   ,IPARTEL ,NEL   )
        ENDIF
      ELSEIF (MTN == 26) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN == 46.OR.MTN == 47) THEN
        IPARG(63)=1 !Loi fluide
      ELSEIF (MTN == 49) THEN
        IPARG(15)=1
        IPARG(16)=1
      ELSEIF (MTN >= 28) THEN
C     User-type laws
        IF(MTN /= 67)THEN
          IPARG(15)=1
          IPARG(16)=1
        END IF
c
        NUVAR  = IPM(8,MAT(1))
        NPAR   = IPM(9,MAT(1))
        IADBUF = IPM(7,MAT(1))
        IADBUF = MAX(1,IADBUF)
        UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)
c        
        DO I=LFT,LLT
          RHO0(I)= PM( 1,MAT(I))
        END DO
C       attention loi 36
        NFUNC  = IPM(10,MAT(1))
        DO I=1,NFUNC
          IFUNC(I) = IPM(10+I,MAT(1))
        ENDDO
c
        IF (MTN == 37) THEN
          IPARG(63) = 1 !Loi fluide
          CALL M37INIT(IPM     ,PM      ,
     .                 NGL     ,NUVAR   ,MBUF%VAR ,UPARAM   ,X      ,
     .                 MAT     ,IPARG   ,IFORM    ,IX       ,NIX    ,
     .                 ILOADP  ,FACLOAD  ,GBUF     ,NEL)
        ELSEIF (MTN == 38) THEN 
          CALL M38INIT(
     1       NEL   ,NPAR    ,NUVAR         ,NFUNC  ,IFUNC  ,
     2       NPF   ,TF      ,BUFMAT(IADBUF),RHO0   ,VOL    ,
     3       EINT  ,MBUF%VAR )
        ELSEIF (MTN == 51) THEN
          CALL M51INIT(IPM                ,DETONATORS ,PM  ,
     .                 NGL                ,NUVAR      ,MBUF%VAR ,UPARAM     ,X   ,
     .                 MAT                ,IPARG      ,IFORM    ,IX         ,NIX ,
     .                 ALE_CONNECTIVITY   ,BUFMAT     ,RHO0     ,
     .                 GBUF               ,NEL        ,SIG)
        ELSEIF (MTN == 70) THEN
          CALL M70INIT(NEL   ,NPAR   ,NUVAR    ,UPARAM   ,MBUF%VAR)

        ELSEIF (MTN == 75) THEN
          ID=IPM(1,MAT(1))
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,MAT(1)),LTITR)
          CALL M75INIT(
     1       NEL   ,NPAR    ,NUVAR   ,NFUNC  ,IFUNC  ,
     2       NPF   ,TF      ,UPARAM  ,RHO0   ,VOL    ,
     3       EINT  ,MBUF%VAR,PM      ,IPM    ,ID     ,TITR   )
        ELSEIF (MTN == 77) THEN
          CALL M77INIT(
     1       NEL   ,NPAR    ,NUVAR   ,NFUNC  ,IFUNC  ,
     2       NPF   ,TF      ,UPARAM  ,RHO0   ,VOL    ,
     3       EINT  ,MBUF%VAR)
c
        ELSEIF (MTN == 95) THEN
          CALL M95INIT(NEL   ,NUVAR   ,MBUF%VAR)
        ELSEIF (MTN == 97)THEN
          IPARG(16)=1   !  FLAG REZONE (BURNING TIME same index for EPS PLAST.)
          IPARG(63)=1   !  FLAG FOR FLUID MATERIAL
          CALL M97INIT(
     .                 IPM     ,DETONATORS,PM  ,
     .                 NGL     ,NUVAR      ,MBUF%VAR  ,BUFMAT(IADBUF) ,X   ,
     .                 MAT     ,IPARG      ,IFORM     ,IX             ,NIX ,
     .                 BUFMAT     ,RHO0      ,TB)
        ELSEIF (MTN == 102) THEN
          IPARG(15)=1
          IPARG(16)=1
        ELSEIF (MTN == 105)THEN
          IPARG(16)=1   !  FLAG REZONE (BURNING TIME same index for EPS PLAST.)
          IPARG(63)=1   !  FLAG FOR FLUID MATERIAL
          CALL M105INIT(
     .                 IPM     ,DETONATORS,PM  ,
     .                 NGL     ,NUVAR     ,MBUF%VAR ,BUFMAT(IADBUF) ,X   ,
     .                 MAT     ,IPARG     ,IFORM    ,IX             ,NIX ,
     .                 BUFMAT    ,RHO0     ,TB)

        ENDIF

      ENDIF
      
      !must be set, once m51init() was called
      IF(ELBUF_STR%BUFLY(1)%L_SSP /= 0)THEN
        DO I=1,NEL
          LBUF%SSP(I)=PM(27,MAT(I))
        ENDDO
      ENDIF
      
C------------
      RETURN
      END
