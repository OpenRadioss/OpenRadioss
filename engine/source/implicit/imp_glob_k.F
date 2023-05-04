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
Chd|  IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|-- calls ---------------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        PKE3                          source/elements/beam/pke3.F   
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|        R4KE3                         source/elements/spring/r4ke3.F
Chd|        R8KE3                         source/elements/spring/r8ke3.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        TKE3                          source/elements/truss/tke3.F  
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE IMP_GLOB_K(
     1    PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3    IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     4    IXS20     ,IXS16     ,IPARG     ,TF        ,NPC       ,
     5    FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6    RBY       ,SKEW      ,X         ,
     7    WA        ,IDDL      ,NDOF      ,K_DIAG    ,K_LT      ,
     8    IADK      ,JDIK      ,IKGEO     ,ETAG      ,ELBUF_TAB ,
     9    STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "vect01_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*) ,
     .   IPM(NPROPMI,*),IGEO(NPROPGI,*),IKGEO
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),
     .   IXP(NIXP,*),IXR(NIXR,*), IXTG(NIXTG,*),
     .   NPC(*), IPARG(NPARG,*),
     .   IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG1(4,*), ETAG(*)
C     REAL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*) ,TF(*) ,
     .   FR_WAVE(*) ,ELBUF(*) ,THKE(*),RBY(*),SKEW(LSKEW,*),
     .   BUFGEO(*),W16(*),X(3,*),WA(*)
      my_real
     .   K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK ,IPLA ,
     .   K1, K2, KAD,IAD2,NF1,IPRI, NELEM, OFFSET, NSGRP, K,
     .   K0, K3, K5, K6, K7, K8, K9, NSG, NEL, KFTS,IOFC, ISTRA,
     .   JJ19,NPE,NIPMAX,ICNOD,NFT1,NF2,MPT,
     .   L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,
     .   L17,L18,L19,L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,L30,
     .   SEDRAPE,NUMEL_DRAPE
      INTEGER INDXOF(MVSIZ),ISH3N,IPRMES_EL(50)
      INTEGER ICP,ICS,IEXPAN,IETY,IG,ISUBSTACK
      SAVE
     .     IPRMES_EL
      my_real
     .   OFF(MVSIZ)
C----6---------------------------------------------------------------7-2
      DO NG = 1, NGROUP
c            NG = IGROUC(IG)
c            IF(NGDONE>NGROUP) GOTO 250
c            NGDONE = NG + 1
C
          IF(IPARG(8,NG)==1)GOTO 250
          IF (IDDW>0) CALL STARTIMEG(NG)
          IF (NG==1) THEN
              DO I=1,50
                  IPRMES_EL(I)=0
              ENDDO
          END IF !(NG==1) THEN
          ITY   =IPARG(5,NG)
          OFFSET  = 0
          MLW     = IPARG(1,NG)
          IF (MLW == 0 .OR. MLW == 13) GOTO 250
          CALL INITBUF(IPARG    ,NG      ,
     2      MLW     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3      NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4      JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5      NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6      IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7      ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
c
          ICNOD   = IPARG(11,NG)
          NSG     = IPARG(10,NG)
          ICP     = IPARG(10,NG)
          ICS     = IPARG(17,NG)
          ISTRA   = IPARG(44,NG)
          NVC     = IPARG(19,NG)
          ITHK    = IPARG(28,NG)
          ISOLNOD = IPARG(28,NG)
          KFTS    = IPARG(30,NG)
          IEXPAN  = IPARG(49,NG)
          ISH3N   = IPARG(23,NG)
          ISUBSTACK=IPARG(71,NG)
          IF(ITY==1.OR.ITY==2) JPLASOL=IPLA
          IFORMDT = 0
          LFT   = 1
          LLT   = MIN(NVSIZ,NEL)
          MTN   = MLW
          JFT=LFT
          JLT=LLT
          NF1 = NFT+1
          IAD = KAD
C
          JSPH=0
C----6---------------------------------------------------------------7---------8
          IF(ITY==1 .AND. JLAG==1)THEN
              IGTYP = NINT(GEO(12,IXS(10,NF1)))
              IF(ISOLNOD==4)THEN
                  IETY=1
                  IF (ISROT > 0 .AND. ISPMD==0) THEN
                      IF (IPRMES_EL(IETY)==0) THEN
                          WRITE(IOUT,1005)ISROT
                          IPRMES_EL(IETY)=1
                      ENDIF
                  ENDIF
                  CALL S4KE3(
     1            PM,           GEO,          IXS,          X,
     2            ELBUF_TAB(NG)%GBUF,        ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            ISMSTR,       JHBE,         IREP,         ISORTH,
     7            IFORMDT)

              ELSEIF(ISOLNOD==10)THEN
                  CALL  S10KE3(
     1            PM,           GEO,          IXS,          IXS10,
     2            X,            ELBUF_TAB(NG),ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            NPT,          ISMSTR,       JHBE,         IREP,
     7            ISORTH,       JLAG)

              ELSEIF(ISOLNOD==20)THEN
                  CALL S20KE3(
     1            PM,           GEO,          IXS,          IXS20,
     2            X,            ELBUF_TAB(NG),ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            ISMSTR,       JHBE,         IREP,         IGTYP,
     7            ISORTH)
              ELSEIF(ISOLNOD==16)THEN
                  IETY=2
                  IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                      WRITE(IOUT,1001)'  S16 SOLID'
                      WRITE(ISTDO,1001)'  S16 SOLID'
                      IPRMES_EL(IETY)=1
                  ENDIF
              ELSEIF(JHBE==15.AND.ISOLNOD==6)THEN
                  CALL S6CKE3(
     1            PM,           GEO,          IXS,          X,
     2            ELBUF_TAB(NG),ETAG,         IDDL,         NDOF,
     3            K_DIAG,       K_LT,         IADK,         JDIK,
     4            NEL,          ICP,          ICS,          IPM,
     5            IGEO,         IKGEO,        BUFMAT,       NFT,
     6            MTN,          JHBE,         ISORTH,       ISORTHG,
     7            ISMSTR)
C
              ELSEIF(ISOLNOD==8)THEN
C              NIPMAX = 729
C              L1 = 1
C              L2 = L1 + MVSIZ * NIPMAX
C              L3 = L2 + MVSIZ * NIPMAX
                  IF (JHBE/=14.AND.JHBE/=15.AND.JHBE/=17) THEN
                      IF (NCYCLE==1.AND.IMCONV==1)THEN
                          IF(JHBE==24)THEN
                              IETY=3
                              IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                                  WRITE(IOUT,1002)JHBE
                                  IPRMES_EL(IETY)=1
                              ENDIF
                          ELSEIF(JHBE==12.OR.JHBE==112)THEN
                              IETY=4
                              IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                                  WRITE(IOUT,1002)JHBE
                                  IPRMES_EL(IETY)=1
                              ENDIF
                          ELSEIF(JHBE==0)THEN
                              IETY=5
                              IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                                  WRITE(IOUT,1002)JHBE
                                  IPRMES_EL(IETY)=1
                              ENDIF
                          ELSE
                              IETY=6
                              IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                                  WRITE(IOUT,1002)JHBE
                                  IPRMES_EL(IETY)=1
                              ENDIF
                          ENDIF
                      ENDIF
                  ENDIF
c
                  IF (JHBE == 14 .AND.
     .               (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22)) THEN
                      CALL S8CKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         IPM,
     5                IGEO,         IKGEO,        BUFMAT,       NFT,
     6                MTN,          JHBE,         JCVT,         IGTYP,
     7                ISORTH,       IREP,         ISMSTR)
                  ELSE IF(JHBE == 17 .AND.  IPARG(36,NG) == 2) THEN
                      MPT = 222
                      CALL S8SKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         MPT,
     5                IPM,          IGEO,         IKGEO,        BUFMAT,
     6                NFT,          MTN,          JHBE,         JCVT,
     7                IGTYP,        ISORTH)
                  ELSE
                      MPT = 222
                      CALL S8ZKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         MPT,
     5                IPM,          IGEO,         IKGEO,        BUFMAT,
     6                NFT,          MTN,          ISMSTR,       JHBE,
     7                JCVT,         IGTYP,        ISORTH,       IREP)
                  ENDIF

C         OPEN(UNIT=16,FILE='KE_S.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
C         CALL IMPKSOUT( IXS,NFT,NEL,16,
C     1                   K11   ,K12   ,K13    ,K14    ,K15   ,
C     2                   K16   ,K17   ,K18    ,K22    ,K23   ,
C     3                   K24   ,K25   ,K26    ,K27    ,K28   ,
C     4                   K33   ,K34   ,K35    ,K36    ,K37   ,
C     5                   K38   ,K44   ,K45    ,K46    ,K47   ,
C     6                   K48   ,K55   ,K56    ,K57    ,K58   ,
C     7                   K66   ,K67   ,K68    ,K77    ,K78   ,
C     8                   K88   )
C----6---------------------------------------------------------------7---------8
              ELSEIF(IGTYP>=29)THEN
                  IETY=7
                  IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                      WRITE(IOUT,1001)'   USERS   '
                      WRITE(ISTDO,1001)'   USERS   '
                      IPRMES_EL(IETY)=1
                  ENDIF
C               CALL SUKE3(
              ELSEIF(NPT==1)THEN
                  IF(NG/=NGROUP)THEN
                      IAD2 = IPARG(4,NG+1) - 21 * NEL
                  ELSE
                      IAD2 = LBUFEL - 21 * NEL + 1
                  ENDIF
                  IF(JHBE==24)THEN
C---------------ca ne riske pas de entrer ici pour l'instant
C               CALL SZKE3(
C     1                   PM    ,GEO   ,IXS    ,X  ,ELBUF(KAD),
C     1                   K11   ,K12   ,K13    ,K14    ,K15   ,
C     2                   K16   ,K17   ,K18    ,K22    ,K23   ,
C     3                   K24   ,K25   ,K26    ,K27    ,K28   ,
C     4                   K33   ,K34   ,K35    ,K36    ,K37   ,
C     5                   K38   ,K44   ,K45    ,K46    ,K47   ,
C     6                   K48   ,K55   ,K56    ,K57    ,K58   ,
C     7                   K66   ,K67   ,K68    ,K77    ,K78   ,
C     8                   K88   ,NEL   ,LIAD   ,ICP    ,ICSIG ,
C     9                   OFFSET,ELBUF(IAD2),OFF)
                      IETY=8
                      IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                          WRITE(IOUT,1001)' HEPH SOLID'
                          WRITE(ISTDO,1001)' HEPH SOLID'
                          IPRMES_EL(IETY)=1
                      ENDIF
                  ELSE
                      IETY=9
                      IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                          WRITE(IOUT,1001)'  S8  SOLID'
                          WRITE(ISTDO,1001)'  S8  SOLID'
                          IPRMES_EL(IETY)=1
                      ENDIF
                  ENDIF
              ELSEIF(NPT==8.AND.MTN/=0 .AND. ISOLNOD/=20)THEN
C              CALL S8KE3(
                  IETY=10
                  IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                      WRITE(IOUT,1001)'  S8  SOLID'
                      WRITE(ISTDO,1001)'  S8  SOLID'
                      IPRMES_EL(IETY)=1
                  ENDIF
              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==2.AND.JMULT==0.AND.JLAG==1)THEN
              IETY=11
              IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                  WRITE(IOUT,1001)'  QUAD 2D  '
                  WRITE(ISTDO,1001)'  QUAD 2D '
                  IPRMES_EL(IETY)=1
              ENDIF
C             CALL QFORC2(
          ELSEIF(ITY==3)THEN
              IOFC = 0
              IF(NG/=NGROUP)THEN
                  IAD2 = IPARG(4,NG+1) - 6 * NEL - 27 * ISUB * NEL
              ELSE
                  IAD2 = LBUFEL - 6 * NEL + 1 - 27 * ISUB * NEL
              ENDIF
              IF (JHBE<11) THEN
                  IF (NCYCLE==1.AND.IMCONV==1) THEN
                      IF(JHBE==4)THEN
                          IETY=12
                          IF (IPRMES_EL(IETY)==0) THEN
                              WRITE(IOUT,1003)JHBE
                              IPRMES_EL(IETY)=1
                          ENDIF
                      ELSEIF(JHBE==3)THEN
                          IETY=13
                          IF (IPRMES_EL(IETY)==0) THEN
                              WRITE(IOUT,1003)JHBE
                              IPRMES_EL(IETY)=1
                          ENDIF
                      ELSEIF(JHBE==1)THEN
                          IETY=14
                          IF (IPRMES_EL(IETY)==0) THEN
                              WRITE(IOUT,1003)JHBE
                              IPRMES_EL(IETY)=1
                          ENDIF
                      ELSE
                          IETY=15
                          IF (IPRMES_EL(IETY)==0) THEN
                              WRITE(IOUT,1003)JHBE
                              IPRMES_EL(IETY)=1
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
              IF(JHBE>=11.AND.JHBE<=19) THEN

                  NUMEL_DRAPE = NUMELC_DRAPE
                  SEDRAPE = SCDRAPE
                  CALL CBAKE3 (
     1            JFT        ,JLT       ,NFT       ,IABS(NPT) ,MLW        ,
     2            ITHK       ,NCYCLE    ,
     3            ISTRA      ,IPLA      ,PM        ,GEO       ,IXC(1,NF1) ,
     4            ELBUF_TAB(NG),BUFMAT   ,OFFSET    ,INDXOF     ,
     1            ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     7            JHBE       ,THKE(NF1)  ,ISMSTR   ,X         ,IKGEO      ,
     8            IPM        ,IGEO     ,IEXPAN    ,IPARG(1,NG),ISUBSTACK,
     9            STACK      ,DRAPE_SH4N  ,DRAPEG%INDX_SH4N, SEDRAPE,NUMEL_DRAPE)
              ELSE
C

                  NUMEL_DRAPE = NUMELC_DRAPE
                  SEDRAPE = SCDRAPE
                  CALL CZKE3 (
     1            JFT        ,JLT       ,NFT       ,IABS(NPT) ,MLW        ,
     2            ITHK       ,NCYCLE     ,
     3            ISTRA      ,IPLA      ,PM        ,GEO       ,IXC(1,NF1) ,
     4            ELBUF_TAB(NG),BUFMAT   ,OFFSET    ,INDXOF     ,
     1            ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     7            JHBE       ,THKE(NF1)  ,ISMSTR   ,X         ,IKGEO      ,
     8            IPM        ,IGEO     ,IEXPAN    ,IPARG(1,NG),ISUBSTACK,
     9            STACK      ,DRAPE_SH4N     ,DRAPEG%INDX_SH4N, SEDRAPE,NUMEL_DRAPE)
CTMP             ELSE
CTMP              WRITE(IOUT,1001)'  Q4  SHELL'
CTMP              WRITE(ISTDO,1001)'  Q4  SHELL'
C              CALL CKE3(
              ENDIF
c         OPEN(UNIT=13,FILE='KE.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c              CALL IMPKCOUT( IXC,NFT,NEL,13,
c     1                    KC11   ,KC12   ,KC13   ,KC14   ,KC22 ,
c     2                    KC23   ,KC24   ,KC33   ,KC34   ,KC44 )
c              CALL KELAMDA( IXC,NIXC,NFT,NEL,13,
c     1                    KC11   ,KC12   ,KC13   ,KC14   ,KC22 ,
c     2                    KC23   ,KC24   ,KC33   ,KC34   ,KC44 )

C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==4)THEN

              CALL TKE3( JFT     ,JLT            ,PM    ,GEO         ,IXT(1,NF1) ,
     2                   X       ,ELBUF_TAB(NG)  ,NEL   ,OFFSET      ,IKGEO ,
     3                   ETAG    , IDDL          ,NDOF  ,K_DIAG      ,K_LT  ,
     4                   IADK    ,JDIK  )

C
C              WRITE(IOUT,1001)'   TRUSS   '
C              WRITE(ISTDO,1001)'   TRUSS   '
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==5)THEN

              CALL PKE3(  JFT    ,JLT        ,NEL  , MTN           , ISMSTR,
     1                    PM     ,IXP(1,NF1) ,X    , ELBUF_TAB(NG) , GEO   ,
     2                    OFFSET ,IKGEO      ,ETAG , IDDL          , NDOF  ,
     3                    K_DIAG ,K_LT       ,IADK , JDIK   )


c         OPEN(UNIT=16,FILE='KE_P.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c              CALL IMPKPOUT(NIXPL,IXP,NFT,NEL,16,KC11   ,KC12 ,  KC22 )
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==6)THEN
              IGTYP = NINT(GEO(12,IXR(1,NF1)))
              K1=1 + 6*(NUMELC+NUMELTG)*IEPSDOT + 15*(NUMELT+NUMELP+NFT)

              IF (IGTYP==4)THEN
                  CALL R4KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                      GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                      TF     ,SKEW  ,OFFSET,FR_WAVE,
     3                      IKGEO  ,IGEO,
     1                      ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                      IADK  ,JDIK   )


              ELSEIF (IGTYP==8)THEN
                  CALL R8KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                       TF     ,SKEW  ,OFFSET,FR_WAVE,IGEO   ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )

              ELSEIF (IGTYP==12)THEN
                  CALL R12KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X  ,ELBUF_TAB(NG),NPC   ,
     2                       TF     ,SKEW  ,OFFSET,FR_WAVE,IGEO  ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )

              ELSEIF (IGTYP==13)THEN
                  CALL R13KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                       TF  ,SKEW  ,OFFSET,FR_WAVE,IKGEO ,IGEO ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )


C        OPEN(UNIT=16,FILE='KE_SP.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
C              CALL IMPKPOUT( NIXR,IXR,NFT,NEL,16,KC11   ,KC12 ,  KC22 )
              ELSE
                  IETY=16
                  IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                      WRITE(IOUT,1001)' THIS SPRING'
                      WRITE(ISTDO,1001)' THIS SPRING'
                      IPRMES_EL(IETY)=1
                  ENDIF
              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==7)THEN
              IOFC = 0
              IF(NG/=NGROUP)THEN
                  IAD2 = IPARG(4,NG+1) - 6 * NEL - 27 * ISUB * NEL
              ELSE
                  IAD2 = LBUFEL - 6 * NEL + 1 - 27 * ISUB * NEL
              ENDIF
              NF1 = NFT + 1
              IF(ICNOD==6)THEN

                  IETY=17
                  IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                      WRITE(IOUT,1001)' S3N6 SHELL'
                      WRITE(ISTDO,1001)' S3N6 SHELL'
                      IPRMES_EL(IETY)=1
                  ENDIF

              ELSE
                  IF (ISH3N == 30) THEN
                      IF (NCYCLE==1.AND.IMCONV==1) THEN
                          IETY=18
                          IF (IPRMES_EL(IETY)==0.AND.ISPMD==0) THEN
                              WRITE(IOUT,1004)ISH3N
                              IPRMES_EL(IETY)=1
                          ENDIF
                      ENDIF
                  ENDIF
                  NUMEL_DRAPE = NUMELTG_DRAPE
                  SEDRAPE = STDRAPE
                  CALL C3KE3 (
     1                JFT    ,JLT    ,NFT    ,IABS(NPT),MTN    ,
     2                ITHK   ,NCYCLE ,
     3                ISTRA  ,IPLA   ,PM     ,GEO    ,IXTG(1,NF1),
     4                ELBUF_TAB(NG),BUFMAT ,OFFSET ,INDXOF ,
     5                ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     6                JHBE   ,THKE(NUMELC+NF1),ISMSTR ,X     ,
     7                IKGEO  ,IPM    ,IGEO   ,IEXPAN  ,IPARG(1,NG),
     8                ISUBSTACK, STACK, DRAPE_SH3N  ,DRAPEG%INDX_SH3N,
     9                SEDRAPE,NUMEL_DRAPE)

              ENDIF
C----6---------------------------------------------------------------7---------8

          ENDIF
  250     CONTINUE
      END DO
C----6---------------------------------------------------------------7---------8
 1001 FORMAT(' *****WARNING : IMPLICITE FORMULATION IS NOT AVAILABLE
     . WITH '/,2X,A11,' ELEMENT : STIFFNESS IGNORED')
 1002 FORMAT(' *****WARNING : ELEMENT FORMULATION ISOLID= ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING;'
     .       ,'USING GENERIC ONE INSTEAD, POSSIBLE CONVERGING ISSUE.')
 1003 FORMAT(' *****WARNING : ELEMENT FORMULATION ISHELL= ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING;'
     .       ,'USING GENERIC ONE INSTEAD, POSSIBLE CONVERGING ISSUE.')
 1004 FORMAT(' *****WARNING : ELEMENT FORMULATION ISH3N = ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING;'
     .       ,'USING GENERIC ONE INSTEAD, POSSIBLE CONVERGING ISSUE.')
 1005 FORMAT(' *****WARNING : TETRA ELEMENT FORMULATION W/ ITETRA= ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING;'
     .       ,'USING ITETRA=0 INSTEAD, POSSIBLE CONVERGING ISSUE.')
      RETURN
      END
Chd|====================================================================
Chd|  GET_KII                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KII(NI  ,IDDL ,IADK,K_DIAG,K_LT ,KII,ND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI,IDDL(*)  ,IADK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*) ,KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,IK,ID,JD,L
C----6----------KII is always triag_sup whatever IKPAT------------------------
      ID = IDDL(NI)
      IF (IKPAT==0) THEN
          DO K=1,ND
              KII(K,K) = K_DIAG(ID+K)
              JD = IADK(ID+K)-1
              DO L=K+1,ND
                  IK = JD+L-K
                  KII(K,L) = K_LT(IK)
              ENDDO
          ENDDO
      ELSE
          DO K=1,ND
              KII(K,K) = K_DIAG(ID+K)
              JD = IADK(ID+K+1)-K
              DO L=1,K-1
                  IK = JD+L
                  KII(L,K) = K_LT(IK)
              ENDDO
          ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  PUT_KII                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|        IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|        IND_SLD                       source/mpi/implicit/imp_fri.F 
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUT_KII(NI  ,IDDL ,IADK,K_DIAG,K_LT ,KII,ND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI,IDDL(*)  ,IADK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,IK,ID,JD,L
C----6----------KII is always triag_sup whatever IKPAT------------------------
      ID = IDDL(NI)
      IF (IKPAT==0) THEN
          DO K=1,ND
              K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
              JD = IADK(ID+K)-1
              DO L=K+1,ND
                  IK = JD+L-K
                  K_LT(IK) = K_LT(IK) + KII(K,L)
              ENDDO
          ENDDO
      ELSE
          DO K=1,ND
              K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
              JD = IADK(ID+K+1)-K
              DO L=1,K-1
                  IK = JD+L
                  K_LT(IK) = K_LT(IK) + KII(L,K)
              ENDDO
          ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPM                       source/interfaces/interf/i2_imp1.F
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KIJ( NI ,NJ ,IDDL  ,IADK,JDIK,K_LT ,KIJ ,NK,NL ,
     .                   IERR)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NK,NL  , IERR
      INTEGER NI,NJ,IDDL(*)  ,IADK(*),JDIK(*)
C     REAL
      my_real
     .   K_LT(*)   ,KIJ(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,EP,ID,JD,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
      IERR = 0
      I = IDDL(NI)
      J = IDDL(NJ)
      IF (IKPAT==0) THEN
          ID = MIN(I,J)
          JD = MAX(I,J)+1
      ELSE
          ID = MAX(I,J)
          JD = MIN(I,J)+1
      ENDIF
      JDL=0
      IF (I==ID) THEN
          DO K=1,NK
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 100
                  ENDIF
              ENDDO
              IF (JDL==0) THEN
                  IERR = 1
                  DO I=1,NK
                      DO J=1,NL
                          KIJ(I,J)=ZERO
                      ENDDO
                  ENDDO
                  RETURN
              ENDIF
  100         DO L=1,NL
                  KIJ(K,L)=K_LT(JDL+L)
              ENDDO
          ENDDO
      ELSE
          DO K=1,NL
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 200
                  ENDIF
              ENDDO
              IF (JDL==0) THEN
                  IERR = 1
                  DO I=1,NK
                      DO J=1,NL
                          KIJ(I,J)=ZERO
                      ENDDO
                  ENDDO
                  RETURN
              ENDIF
  200         DO L=1,NK
                  KIJ(L,K)=K_LT(JDL+L)
              ENDDO
          ENDDO
      ENDIF
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2UPDKM1                      source/interfaces/interf/i2_imp1.F
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUT_KIJ( NI ,NJ ,IDDL  ,IADK,JDIK,K_LT,KIJ,NK,NL ,
     .                   IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NK,NL   ,IERR
      INTEGER NI,NJ,IDDL(*)  ,IADK(*),JDIK(*)
C     REAL
      my_real
     .   K_LT(*)   ,KIJ(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,EP,ID,JD,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
      IERR = 0
      I = IDDL(NI)
      J = IDDL(NJ)
      IF (IKPAT==0) THEN
          ID = MIN(I,J)
          JD = MAX(I,J)+1
      ELSE
          ID = MAX(I,J)
          JD = MIN(I,J)+1
      ENDIF
      IF (I==ID) THEN
          DO K=1,NK
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 100
                  ENDIF
              ENDDO
              IERR = 1
              RETURN
  100         DO L=1,NL
                  K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L)
              ENDDO
          ENDDO
      ELSE
          DO K=1,NL
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 200
                  ENDIF
              ENDDO
              IERR = 1
              RETURN
  200         DO L=1,NK
                  K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K)
              ENDDO
          ENDDO
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  PRINT_WKIJ                    source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        I2UPDK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2UPDK1                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|        RBE2_IMP1                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPL                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMP1                     source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMP1                      source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPM                      source/constraints/general/rbody/rby_imp0.F
Chd|        RM_IMP1                       source/model/remesh/rm_imp0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRINT_WKIJ( NI ,NJ ,IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI ,NJ ,IFLAG
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF (IFLAG==1) THEN
          WRITE(IOUT,1001)NI,NJ
          WRITE(ISTDO,1001)NI,NJ
      ELSEIF (IFLAG==2) THEN
          WRITE(IOUT,1002)NI,NJ
          WRITE(ISTDO,1002)NI,NJ
      ELSEIF (IFLAG==3) THEN
          WRITE(IOUT,1003)NI,NJ
          WRITE(ISTDO,1003)NI,NJ
      ELSEIF (IFLAG==4) THEN
          WRITE(IOUT,1004)NI,NJ
          WRITE(ISTDO,1004)NI,NJ
      ELSEIF (IFLAG==5) THEN
          WRITE(IOUT,1005)NI,NJ
          WRITE(ISTDO,1005)NI,NJ
      ELSE
          WRITE(IOUT,1000)NI,NJ
          WRITE(ISTDO,1000)NI,NJ
      ENDIF
C----6---------------------------------------------------------------7---------8
 1000 FORMAT(' *** WARNING : IN OPTION ? :'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
 1001 FORMAT(' *** WARNING : IN RIGID BODY CONDENSATION:'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
 1002 FORMAT(' *** WARNING : IN INTERFACE TYPE 2 CONDENSATION:'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
 1003 FORMAT(' *** WARNING : IN REMESH KINEMATIC CONDENSATION:'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
 1004 FORMAT(' *** WARNING : IN RBE3 CONDENSATION:'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
 1005 FORMAT(' *** WARNING : IN RBE2 CONDENSATION:'/,
     .        '*** NO CONNECTIVITY BETWEEN NODES:',2I10)
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        ASSEM_INT                     source/implicit/assem_int.F   
Chd|        ASSEM_INT11                   source/implicit/assem_int.F   
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KSL                     source/mpi/implicit/imp_fri.F 
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        ASSEM_Q4                      source/implicit/assem_q4.F    
Chd|        ASSEM_R3                      source/implicit/assem_r3.F    
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|        ASSEM_S20                     source/implicit/assem_s20.F   
Chd|        ASSEM_S4                      source/implicit/assem_s4.F    
Chd|        ASSEM_S6                      source/implicit/assem_s6.F    
Chd|        ASSEM_S8                      source/implicit/assem_s8.F    
Chd|        ASS_SPMD                      source/implicit/assem_int.F   
Chd|        ASS_SPMD11                    source/implicit/assem_int.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSEM_KII(NI    ,NEL   ,IDDL  ,IADK  ,K_DIAG,
     1                     K_LT  ,KII   ,ND    ,OFF   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI(*),NEL   ,IDDL(*)  , IADK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KII(ND,ND,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K,EP,IK,ID,JD,L
C----6----------KII is always triag_sup whatever IKPAT---------------7--------
Cel lock par element trop penalisant pour perf
#include "lockon.inc"
      DO EP = 1,NEL
          IF (OFF(EP)>ZERO.AND.NI(EP)>0) THEN
              N = NI(EP)
              ID = IDDL(N)
              IF (IKPAT==0) THEN
                  DO K=1,ND
c#include "lockon.inc"
                      K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K,EP)
c#include "lockoff.inc"
                      JD = IADK(ID+K)-1
                      DO L=K+1,ND
                          IK = JD+L-K
c#include "lockon.inc"
                          K_LT(IK) = K_LT(IK) + KII(K,L,EP)
c#include "lockoff.inc"
                      ENDDO
                  ENDDO
              ELSE
                  DO K=1,ND
c#include "lockon.inc"
                      K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K,EP)
c#include "lockoff.inc"
                      JD = IADK(ID+K+1)-K
                      DO L=1,K-1
                          IK = JD+L
c#include "lockon.inc"
                          K_LT(IK) = K_LT(IK) + KII(L,K,EP)
c#include "lockoff.inc"
                      ENDDO
                  ENDDO
              ENDIF
          ENDIF
      ENDDO
#include "lockoff.inc"
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        ASSEM_INT                     source/implicit/assem_int.F   
Chd|        ASSEM_INT11                   source/implicit/assem_int.F   
Chd|        ASSEM_P                       source/implicit/assem_p.F     
Chd|        ASSEM_Q4                      source/implicit/assem_q4.F    
Chd|        ASSEM_R3                      source/implicit/assem_r3.F    
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|        ASSEM_S20                     source/implicit/assem_s20.F   
Chd|        ASSEM_S4                      source/implicit/assem_s4.F    
Chd|        ASSEM_S6                      source/implicit/assem_s6.F    
Chd|        ASSEM_S8                      source/implicit/assem_s8.F    
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_KIJ( NI ,NJ ,NEL  ,IDDL  ,IADK,JDIK,
     1                      K_DIAG,K_LT  ,KIJ  ,ND    ,OFF )
C----6---------------------------------------------------------------7---------8
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
#include      "comlock.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI(*),NJ(*),NEL   ,IDDL(*)  ,IADK(*),JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*),K_LT(*)   ,KIJ(ND,ND,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,EP,ID,JD,JDL,L,JJ,NN(MVSIZ),NELD
      my_real
     .   KIJD(ND,ND,MVSIZ),OFFD(MVSIZ)
C---- s'il y a des elms degeneres--------------
      NELD=0
      DO EP = 1,NEL
          IF (NI(EP)==NJ(EP).AND.OFF(EP)>ZERO.AND.NI(EP)>0) THEN
              NELD=NELD+1
              NN(NELD)=NI(EP)
              OFFD(NELD)=OFF(EP)
              DO I=1,ND
                  DO J=I,ND
                      KIJD(I,J,NELD)=KIJ(I,J,EP)+KIJ(J,I,EP)
                  ENDDO
              ENDDO
          ENDIF
      ENDDO
      IF (NELD>0)
     . CALL ASSEM_KII(NN    ,NELD  ,IDDL  ,IADK  ,K_DIAG,
     .                       K_LT  ,KIJD  ,ND    ,OFFD  )
C----6---------------------------------------------------------------7---------8
#include "lockon.inc"
      IF (IKPAT==0) THEN
          DO EP = 1,NEL
              IF (OFF(EP)>ZERO.AND.NI(EP)/=NJ(EP).AND.
     .            NI(EP)>0.AND.NJ(EP)>0) THEN
                  I = IDDL(NI(EP))
                  J = IDDL(NJ(EP))
                  ID = MIN(I,J)
                  JD = MAX(I,J)+1
                  IF (I==ID) THEN
                      DO K=1,ND
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 100
                              ENDIF
                          ENDDO
  100                     DO L=1,ND
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ELSE
                      DO K=1,ND
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 200
                              ENDIF
                          ENDDO
  200                     DO L=1,ND
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO
      ELSE
          DO EP = 1,NEL
              IF (OFF(EP)>ZERO.AND.NI(EP)/=NJ(EP).AND.
     .            NI(EP)>0.AND.NJ(EP)>0) THEN
                  I = IDDL(NI(EP))
                  J = IDDL(NJ(EP))
                  ID = MAX(I,J)
                  JD = MIN(I,J)+1
                  IF (I==ID) THEN
                      DO K=1,ND
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 300
                              ENDIF
                          ENDDO
  300                     DO L=1,ND
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ELSE
                      DO K=1,ND
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 400
                              ENDIF
                          ENDDO
  400                     DO L=1,ND
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO
      ENDIF
#include "lockoff.inc"
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  PUT_KMII                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUT_KMII(ID    ,IADK  ,K_DIAG,K_LT  ,KII   ,
     .                    ND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER ID,IADK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KII(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,IK,JD,L,IDM
C----6----------KII is always triag_sup whatever IKPAT------------------------
      IF (IKPAT==0) THEN
          DO K=1,ND
              K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
              JD = IADK(ID+K)-1
              DO L=K+1,ND
                  IK = JD+L-K
                  K_LT(IK) = K_LT(IK) + KII(K,L)
              ENDDO
          ENDDO
      ELSE
          DO K=1,ND
              K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
              JD = IADK(ID+K+1)-K
              DO L=1,K-1
                  IK = JD+L
                  K_LT(IK) = K_LT(IK) + KII(L,K)
              ENDDO
          ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  PUT_KMIJ                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|        RBE3_FR1                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUT_KMIJ(INI ,INJ ,IADK,JDIK,K_LT,
     .                    KIJ ,NK  ,NL  ,IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NK,NL   ,IERR
      INTEGER INI,INJ,IADK(*),JDIK(*)
C     REAL
      my_real
     .   K_LT(*)   ,KIJ(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,EP,ID,JD,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
      IERR = 0
      I = INI
      J = INJ
      IF (IKPAT==0) THEN
          ID = MIN(I,J)
          JD = MAX(I,J)+1
      ELSE
          ID = MAX(I,J)
          JD = MIN(I,J)+1
      ENDIF
      IF (I==ID) THEN
          DO K=1,NK
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 100
                  ENDIF
              ENDDO
              IERR = 1
              RETURN
  100         DO L=1,NL
                  K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L)
              ENDDO
          ENDDO
      ELSE
          DO K=1,NL
              DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                  IF (JDIK(JJ)==JD) THEN
                      JDL = JJ-1
                      GOTO 200
                  ENDIF
              ENDDO
              IERR = 1
              RETURN
  200         DO L=1,NK
                  K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K)
              ENDDO
          ENDDO
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMPKSOUT                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITEKS                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE IMPKSOUT( IXS,NFT,NEL,IUGEO,
     1                   K11   ,K12   ,K13    ,K14    ,K15   ,
     2                   K16   ,K17   ,K18    ,K22    ,K23   ,
     3                   K24   ,K25   ,K26    ,K27    ,K28   ,
     4                   K33   ,K34   ,K35    ,K36    ,K37   ,
     5                   K38   ,K44   ,K45    ,K46    ,K47   ,
     6                   K48   ,K55   ,K56    ,K57    ,K58   ,
     7                   K66   ,K67   ,K68    ,K77    ,K78   ,
     8                   K88   )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),NFT,NEL,IUGEO
C     REAL
      my_real
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*) ,K14(3,3,*) ,K15(3,3,*),
     .   K16(3,3,*),K17(3,3,*),K18(3,3,*) ,K22(3,3,*) ,K23(3,3,*),
     .   K24(3,3,*),K25(3,3,*),K26(3,3,*) ,K27(3,3,*) ,K28(3,3,*),
     .   K33(3,3,*),K34(3,3,*),K35(3,3,*) ,K36(3,3,*) ,K37(3,3,*),
     .   K38(3,3,*),K44(3,3,*),K45(3,3,*) ,K46(3,3,*) ,K47(3,3,*),
     .   K48(3,3,*),K55(3,3,*),K56(3,3,*) ,K57(3,3,*) ,K58(3,3,*),
     .   K66(3,3,*),K67(3,3,*),K68(3,3,*) ,K77(3,3,*) ,K78(3,3,*),
     .   K88(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ)
C=======================================================================
C     SOLID
C=======================================================================
      DO N= 1,NEL
          NT=N+NFT
          IG(N)=IXS(NIXS,NT)
      ENDDO
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K11',K11)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K12',K12)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K13',K13)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K14',K14)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K15',K15)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K16',K16)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K17',K17)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K18',K18)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K22',K22)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K23',K23)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K24',K24)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K25',K25)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K26',K26)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K27',K27)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K28',K28)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K33',K33)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K34',K34)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K35',K35)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K36',K36)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K37',K37)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K38',K38)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K44',K44)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K45',K45)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K46',K46)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K47',K47)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K48',K48)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K55',K55)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K56',K56)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K57',K57)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K58',K58)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K66',K66)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K67',K67)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K68',K68)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K77',K77)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K78',K78)
      CALL WRITEKS(IUGEO,NFT,NEL,IG,'K88',K88)
C
      RETURN
      END
C  |----nouvell routine -----
Chd|====================================================================
Chd|  WRITEKS                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMPKSOUT                      source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITEKS( IN,NFT,NEL,IG,CH,KIJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IG(*),NFT,NEL,IN
      CHARACTER CH*3
C     REAL
      my_real
     .   KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT
      CHARACTER KEY*10,KEY1*23
C-----------------------------------------------
      KEY='/SOLID_'//CH
      KEY1='#3d Solid Elements '//CH
      WRITE(IN,'(A)') KEY
      WRITE(IN,'(A)')KEY1
      WRITE(IN,'(A)')
     .    '#FORMAT: (2I8,1P4E16.9,6(/,1P5E16.9)) '
      WRITE(IN,'(2A)')'# SYSSOL  USRSOL  K(I,J)  I=1,3;J=1,3'
      DO N= 1,NEL
          NT=N+NFT
          WRITE(IN,'(2I8,1P4E16.9,6(/,1P5E16.9))'
     .      )NT,IG(N),((KIJ(I,J,N),I=1,3),J=1,3)
      ENDDO
      RETURN
      END
C
C  |----nouvell routine -----
Chd|====================================================================
Chd|  IMPKCOUT                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITEKC                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE IMPKCOUT( IXC,NFT,NEL,IUGEO,
     1                    KE11   ,KE12   ,KE13   ,KE14   ,KE22 ,
     2                    KE23   ,KE24   ,KE33   ,KE34   ,KE44 )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),NFT,NEL,IUGEO
C     REAL
      my_real
     .   KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),KE44(6,6,*),
     .   KE12(6,6,*),KE13(6,6,*),KE14(6,6,*),KE23(6,6,*),
     .   KE24(6,6,*),KE34(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ)
C=======================================================================
C     SHELL
C=======================================================================
      DO N= 1,NEL
          NT=N+NFT
          IG(N)=IXC(NIXC,NT)
      ENDDO
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K11',KE11)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K12',KE12)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K13',KE13)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K14',KE14)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K22',KE22)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K23',KE23)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K24',KE24)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K33',KE33)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K34',KE34)
      CALL WRITEKC(IUGEO,NFT,NEL,IG,'K44',KE44)
C
      RETURN
      END
C  |----nouvell routine -----
Chd|====================================================================
Chd|  WRITEKC                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMPKCOUT                      source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITEKC( IN,NFT,NEL,IG,CH,KIJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IG(*),NFT,NEL,IN
      CHARACTER CH*3
C     REAL
      my_real
     .   KIJ(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT
      CHARACTER KEY*10,KEY1*23
C-----------------------------------------------
      KEY='/SHELL_'//CH
      KEY1='#3d Shell Elements '//CH
      WRITE(IN,'(A)') KEY
      WRITE(IN,'(A)')KEY1
      WRITE(IN,'(A)')
     .    '#FORMAT: (2I8,1P4E16.9,6(/,1P5E16.9),/,1P2E16.9) '
      WRITE(IN,'(2A)')'#SYSSHEL USRSHEL  K(I,J)  I=1,6;J=1,6'
      DO N= 1,NEL
          NT=N+NFT
          WRITE(IN,'(2I8,1P4E16.9,6(/,1P5E16.9),/,1P2E16.9)'
     .      )NT,IG(N),((KIJ(I,J,N),I=1,6),J=1,6)
      ENDDO
      RETURN
      END
C
C  |----nouvell routine -----
Chd|====================================================================
Chd|  IMPKPOUT                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITEKP                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE IMPKPOUT( NIXPL,IXP,NFT,NEL,IUGEO,KE11,KE12,KE22 )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIXPL
      INTEGER IXP(NIXPL,*),NEL,IUGEO,NFT
C     REAL
      my_real
     .   KE11(6,6,*),KE22(6,6,*),KE12(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ)
C=======================================================================
C     SHELL
C=======================================================================
      DO N= 1,NEL
          IG(N)=IXP(NIXPL,N)
      ENDDO
      CALL WRITEKP(IUGEO,NFT,NEL,IG,'K11',KE11)
      CALL WRITEKP(IUGEO,NFT,NEL,IG,'K12',KE12)
      CALL WRITEKP(IUGEO,NFT,NEL,IG,'K22',KE22)
C
      RETURN
      END
C  |----nouvell routine -----
Chd|====================================================================
Chd|  WRITEKP                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMPKPOUT                      source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITEKP( IN,NFT,NEL,IG,CH,KIJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IG(*),NFT,NEL,IN
      CHARACTER CH*3
C     REAL
      my_real
     .   KIJ(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT
      CHARACTER KEY*10,KEY1*23
C-----------------------------------------------
      KEY='/BEAM_'//CH
      KEY1='#3d Beam Elements '//CH
      WRITE(IN,'(A)') KEY
      WRITE(IN,'(A)')KEY1
      WRITE(IN,'(A)')
     .    '#FORMAT: (2I8,1P4E16.9,6(/,1P5E16.9),/,1P2E16.9) '
      WRITE(IN,'(2A)')'#SYSSHEL USRSHEL  K(I,J)  I=1,6;J=1,6'
      DO N= 1,NEL
          NT=N+NFT
          WRITE(IN,'(2I8,1P4E16.9,6(/,1P5E16.9),/,1P2E16.9)'
     .      )NT,IG(N),((KIJ(I,J,N),I=1,6),J=1,6)
      ENDDO
      RETURN
      END
C
C  |----nouvell routine -----
Chd|====================================================================
Chd|  IMPKIOUT                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITEKI                       source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE IMPKIOUT( NIXPL,IXP,NFT,NEL,IUGEO,KE11,KE12,KE22 )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIXPL
      INTEGER IXP(NIXPL,*),NEL,IUGEO,NFT
C     REAL
      my_real
     .   KE11(3,3,*),KE22(3,3,*),KE12(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ)
C=======================================================================
C     SHELL
C=======================================================================
      DO N= 1,NEL
          IG(N)=IXP(NIXPL,N)
      ENDDO
      CALL WRITEKI(IUGEO,NFT,NEL,IG,'K11',KE11)
      CALL WRITEKI(IUGEO,NFT,NEL,IG,'K12',KE12)
      CALL WRITEKI(IUGEO,NFT,NEL,IG,'K22',KE22)
C
      RETURN
      END
C  |----nouvell routine -----
Chd|====================================================================
Chd|  WRITEKI                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMPKIOUT                      source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITEKI( IN,NFT,NEL,IG,CH,KIJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IG(*),NFT,NEL,IN
      CHARACTER CH*3
C     REAL
      my_real
     .   KIJ(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT
      CHARACTER KEY*10,KEY1*23
C-----------------------------------------------
      KEY='/TRUSS_'//CH
      KEY1='#3d TRUSS Elements '//CH
      WRITE(IN,'(A)') KEY
      WRITE(IN,'(A)')KEY1
      WRITE(IN,'(A)')
     .    '#FORMAT: (2I8,1P4E16.9,6(/,1P5E16.9)) '
      WRITE(IN,'(2A)')'#SYSSHEL USRSHEL  K(I,J)  I=1,3;J=1,3'
      DO N= 1,NEL
          NT=N+NFT
          WRITE(IN,'(2I8,1P4E16.9,6(/,1P5E16.9))'
     .      )NT,IG(N),((KIJ(I,J,N),I=1,3),J=1,3)
      ENDDO
      RETURN
      END
C
C  |----nouvell routine -----
Chd|====================================================================
Chd|  KELAMDA                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        JACOBIEN                      source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE KELAMDA( IXC,NIXCL,NFT,NEL,IUGEO,
     1                    KE11   ,KE12   ,KE13   ,KE14   ,KE22 ,
     2                    KE23   ,KE24   ,KE33   ,KE34   ,KE44 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIXCL
      INTEGER IXC(NIXCL,*),NFT,NEL,IUGEO
C     REAL
      my_real
     .   KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),KE44(6,6,*),
     .   KE12(6,6,*),KE13(6,6,*),KE14(6,6,*),KE23(6,6,*),
     .   KE24(6,6,*),KE34(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ),I2,I3,I4,J2,J3,J4
      my_real
     .   KE(24,24,MVSIZ),EW(24,MVSIZ),TOL,LAMDA(MVSIZ),
     .   A,B,C,LAMDAS(MVSIZ),EV(24,24),KTMP(2,2)
C
      TOL=EM5
      DO N= 1,NEL
          NT=N+NFT
          IG(N)=IXC(NIXCL,NT)
      ENDDO
      DO I=1,6
          I2=I+6
          I3=I2+6
          I4=I3+6
          DO J=I,6
              J2=J+6
              J3=J2+6
              J4=J3+6
              DO N= 1,NEL
                  KE(I,J,N)=KE11(I,J,N)
                  KE(I2,J2,N)=KE22(I,J,N)
                  KE(I3,J3,N)=KE33(I,J,N)
                  KE(I4,J4,N)=KE44(I,J,N)
              ENDDO
          ENDDO
      ENDDO
      DO I=1,6
          I2=I+6
          I3=I2+6
          I4=I3+6
          DO J=1,6
              J2=J+6
              J3=J2+6
              J4=J3+6
              DO N= 1,NEL
                  KE(I,J2,N)=KE12(I,J,N)
                  KE(I,J3,N)=KE13(I,J,N)
                  KE(I,J4,N)=KE14(I,J,N)
                  KE(I2,J3,N)=KE23(I,J,N)
                  KE(I2,J4,N)=KE24(I,J,N)
                  KE(I3,J4,N)=KE34(I,J,N)
              ENDDO
          ENDDO
      ENDDO
      DO N= 1,NEL
          CALL JACOBIEN(KE(1,1,N),24,EW(1,N),EV,TOL,LAMDA(N))
          A=HALF*(KE11(1,1,N)+KE11(2,2,N))
          B=HALF*(KE11(1,1,N)-KE11(2,2,N))
          C=A+SQRT(B*B+KE11(1,2,N)*KE11(1,2,N))
          LAMDAS(N)=C
          A=HALF*(KE22(1,1,N)+KE22(2,2,N))
          B=HALF*(KE22(1,1,N)-KE22(2,2,N))
          C=A+SQRT(B*B+KE22(1,2,N)*KE22(1,2,N))
          IF(C>LAMDAS(N))LAMDAS(N)=C
          A=HALF*(KE33(1,1,N)+KE33(2,2,N))
          B=HALF*(KE33(1,1,N)-KE33(2,2,N))
          C=A+SQRT(B*B+KE33(1,2,N)*KE33(1,2,N))
          IF(C>LAMDAS(N))LAMDAS(N)=C
          A=HALF*(KE44(1,1,N)+KE44(2,2,N))
          B=HALF*(KE44(1,1,N)-KE44(2,2,N))
          C=A+SQRT(B*B+KE44(1,2,N)*KE44(1,2,N))
          IF(C>LAMDAS(N))LAMDAS(N)=C
      ENDDO
      WRITE(IUGEO,'(A)') '#SHELL EIGENVALUES'
      WRITE(IUGEO,'(A)')
     .    '#FORMAT: (2I8,1P3E16.9,/,4(/,1P5E16.9),/,1P4E16.9) '
      WRITE(IUGEO,'(2A)')
     . '#SYSSHEL USRSHEL  LAMDA1,LAMDAS,FAC, LAMDA(I),I=24'
      DO N= 1,NEL
          NT=N+NFT
          WRITE(IUGEO,'(2I8,1P3E16.9,/,4(/,1P5E16.9),/,1P4E16.9)'
     .      )NT,IG(N),LAMDA(N),LAMDAS(N),LAMDA(N)/LAMDAS(N),
     .      (EW(I,N),I=1,24)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  JACOBIEN                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        KELAMDA                       source/implicit/imp_glob_k.F  
Chd|        MINV_K                        source/implicit/imp_solv.F    
Chd|        PVP_K                         source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  JACOBIEN(A,N,EW,EV,TOL,LAMDA)
C-------------------------------------------------------KK141189-
C COMPUTATION OF ALL EIGENVALUES AND EIGENVECTORS OF A SYMMETRIC
C MATRIX A BY THE JACOBI ALGORITHM
C
C A(N,N)      EIGENWERTPROBLEM
C N           DIMENSION OF A
C EW(N)       EIGENVALUES
C EV(N,N)     EIGENVEKTORS
C NROT        NUMBER OF ROTATIONS
C MAXA        MAXIMUM ELEMENT OF A
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER     N
      my_real
     . A(N,N), EW(N), EV(N,N)
     . , B(N), Z(N),TOL,LAMDA
      INTEGER IZ,IS,ITER,J,NROT
      my_real
     . SUMRS,EPS,G,H,T,C,S,TAU,THETA,R,LAMDA0
C----------------------------------------------------------------
      DO 130 IZ=1,N
          DO 120 IS=1,N
              IF(IZ<IS) A(IS,IZ) = A(IZ,IS)
              EV(IZ,IS)=ZERO
  120     CONTINUE
          B(IZ)=A(IZ,IZ)
          EW(IZ)=B(IZ)
          Z(IZ)=0.
          EV(IZ,IZ)=ONE
c        EV(IZ,IZ)=ZERO
  130 CONTINUE

      NROT=0
      R=EP30
      ITER =0

      SUMRS = ZERO

      LAMDA0=ZERO

C SUM OF THE OFF DIAGONALS
      DO 150 IZ=1,N-1
          DO 140 IS=IZ+1,N
              SUMRS=SUMRS+ABS(A(IZ,IS))
  140     CONTINUE
  150 CONTINUE

      IF (SUMRS ==ZERO) GOTO 9000
      IF (ITER > 4)   THEN
          EPS = ZERO
      ELSE
          EPS = ONE_FIFTH*SUMRS/N**2
      ENDIF
C START ITERATION

      DO WHILE (R>TOL)
c      DO 240 ITER = 1,50
          ITER =ITER+ 1

          DO 220 IZ=1,N-1
              DO 210 IS=IZ+1,N
                  G = 100. * ABS(A(IZ,IS))
                  IF (ITER>4 .AND. ABS(EW(IZ))+G==ABS(EW(IZ))
     &            .AND. ABS(EW(IS))+G==ABS(EW(IS))) THEN
                      A(IZ,IS)=ZERO
                  ELSE IF (ABS(A(IZ,IS)) > EPS) THEN
                      H = EW(IS)-EW(IZ)
                      IF (ABS(H)+G==ABS(H)) THEN
                          T = A(IZ,IS)/H
                      ELSE
                          THETA = HALF*H/A(IZ,IS)
                          T=ONE/(ABS(THETA)+SQRT(ONE+THETA**2))
                          IF (THETA < ZERO) T=-T
                      ENDIF
                      C=ONE/SQRT(ONE+T**2)
                      S=T*C
                      TAU=S/(ONE+C)
                      H=T*A(IZ,IS)
                      Z(IZ)=Z(IZ)-H
                      Z(IS)=Z(IS)+H
                      EW(IZ)=EW(IZ)-H
                      EW(IS)=EW(IS)+H
                      A(IZ,IS)=ZERO
                      DO 160 J=1,IZ-1
                          G=A(J,IZ)
                          H=A(J,IS)
                          A(J,IZ)=G-S*(H+G*TAU)
                          A(J,IS)=H+S*(G-H*TAU)
  160                 CONTINUE
                      DO 170 J=IZ+1,IS-1
                          G=A(IZ,J)
                          H=A(J,IS)
                          A(IZ,J)=G-S*(H+G*TAU)
                          A(J,IS)=H+S*(G-H*TAU)
  170                 CONTINUE
                      DO 180 J=IS+1,N
                          G=A(IZ,J)
                          H=A(IS,J)
                          A(IZ,J)=G-S*(H+G*TAU)
                          A(IS,J)=H+S*(G-H*TAU)
  180                 CONTINUE
                      DO 190 J=1,N
                          G=EV(J,IZ)
                          H=EV(J,IS)
                          EV(J,IZ)=G-S*(H+G*TAU)
                          EV(J,IS)=H+S*(G-H*TAU)
  190                 CONTINUE
                      NROT=NROT+1
                  ENDIF
  210         CONTINUE
  220     CONTINUE
          DO 230 IZ=1,N
              B(IZ)=B(IZ)+Z(IZ)
              IF (B(IZ)>LAMDA)LAMDA=B(IZ)
              EW(IZ)=B(IZ)
              Z(IZ)=ZERO
  230     CONTINUE
c        R=ABS(LAMDA-LAMDA0)
          R=ABS(LAMDA/MAX(EM20,LAMDA0)-ONE)
          LAMDA0=LAMDA
c      write(*,*)'iter,lamda,R=',iter,lamda,R
          LAMDA=ZERO
c  240 CONTINUE
      ENDDO

      LAMDA=LAMDA0
c      write(*,*)'n_iter,nrot=',iter,nrot,eps
 9000 CONTINUE

      RETURN

      END
Chd|====================================================================
Chd|  ELEOFF                        source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ELEOFF(JFT , JLT , IX, NIX ,NN   ,ETAG, OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX
      INTEGER JFT, JLT, IX(NIX,*), ETAG(*),NN
      my_real
     .        OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J  ,N,N1,NALL,IUN
C
      IUN = 1
      DO I=JFT,JLT
          N1=IX(2,I)
          NALL=ETAG(N1)
          DO J=3,NN+1
              N=IX(J,I)
              NALL=NALL*ETAG(N)
          ENDDO
          NALL=MIN(NALL,IUN)
          OFF(I)=NALL
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        KP4_INI                       source/implicit/imp_glob_k.F  
Chd|        KPQUAD                        source/implicit/imp_glob_k.F  
Chd|        KPTRIA                        source/implicit/imp_glob_k.F  
Chd|        PUT_KIJ                       source/implicit/imp_glob_k.F  
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE IMP_KPRES(IB    ,FAC    ,NPC   ,TF    ,X     ,
     2                     SKEW  ,NSENSOR,SENSOR_TAB,WEIGHT,IADC  ,
     3                     IDDL  ,NDOF   ,IADK  ,JDIK  ,K_DIAG,
     4                     K_LT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IB(NIBCLD,*)
      INTEGER WEIGHT(*), IADC(4,*)
      INTEGER IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
C     REAL
      my_real
     .   FAC(LFACCLD,*), TF(*), X(3,*), SKEW(LSKEW,*),
     .   K_DIAG(*) ,K_LT(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, K1, K2, K3, ISENS,K,LL,
     .        ICODE,IAD,N_OLD,IPRES4,IERR,ND,I,J
C     REAL
      my_real
     .   AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .   SIXTH,X_OLD, F1, F2,XSENS,FCX,FCY,SCALN
      my_real
     .        VKSI(4,4),VETA(4,4),VF4(4,4),
     .        K11(6,6),K22(6,6),K33(6,6),K44(6,6),K12(6,6),
     .        K13(6,6),K14(6,6),K23(6,6),K24(6,6),K34(6,6)
      my_real FINTER
      EXTERNAL FINTER
C=======================================================================
C-------IPRES4=0:no pressure,1:pressure with 3n; 2: with 4N
      IPRES4=0
      ND =3
      SCALN=HALF
C       IF (ILINE==0) SCALN=ONEP1
C       IF (NBUCK>0) SCALN=-ONE
      DO NL=1,NCONLD
          N4=IB(4,NL)
          ISENS=0
          XSENS = ONE
          DO K=1,NSENSOR
              IF(IB(6,NL)==SENSOR_TAB(K)%SENS_ID) ISENS=K
          ENDDO
          IF(ISENS==0)THEN
              TS=TT
          ELSE
              TS = TT-SENSOR_TAB(ISENS)%TSTART
              IF(TS < ZERO) CYCLE
          ENDIF
C
          IF(N4==-1)THEN
C----------------
C       FORCE CONCENTREE
C----------------
          ELSE
C----------------
C       PRESSION
C----------------
              IF (XSENS==ZERO) CYCLE
C
              IF(N2D==0)THEN
C        ANALYSE 3D
                  IF(N4/=0)THEN
                      IPRES4=2
                      GOTO 100
                  ELSE
C         true triangles.
                      IPRES4=1
                  ENDIF
              ENDIF
          ENDIF
      END DO
C
  100 CONTINUE
      IF (IPRES4==0) RETURN
      IF (IPRES4>1) CALL KP4_INI(VKSI,VETA,VF4)
      N_OLD = 0
      X_OLD = ZERO
      DO 10 NL=1,NCONLD
          N1=IB(1,NL)
          N2=IB(2,NL)
          N3=IB(3,NL)
          N4=IB(4,NL)
          N5=IB(5,NL)
          FCY = FAC(1,NL)
          FCX = FAC(2,NL)
          ISENS=0
          XSENS = ONE
          DO K=1,NSENSOR
              IF(IB(6,NL)==SENSOR_TAB(K)%SENS_ID) ISENS=K
          ENDDO
          IF(ISENS==0)THEN
              TS=TT
          ELSE
              TS = TT-SENSOR_TAB(ISENS)%TSTART
              IF(TS < ZERO) GOTO 10
          ENDIF
C
          IF(N4==-1)THEN
C----------------
C       FORCE CONCENTREE
C----------------
          ELSE
C----------------
C       PRESSION
C----------------
              IF(N_OLD/=N5.OR.X_OLD/=TS) THEN
                  F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
                  N_OLD = N5
                  X_OLD = TS
              ENDIF
              AA = -SCALN*FCY*F1*XSENS
              IF (AA==ZERO) CYCLE
C
              IF(N2D==0)THEN
C        ANALYSE 3D
                  IF(N4/=0)THEN
                      CALL KPQUAD(N1,N2,N3,N4,AA,X,VKSI,VETA,VF4,
     .                           K11,K22,K33,K44,K12,K13,K14,K23,K24,K34)
C-----------add in ind_glob_k in case element has been deleted---
c          CALL PUT_KII(N1  ,IDDL ,IADK,K_DIAG,K_LT ,K11,ND  )
c          CALL PUT_KII(N2  ,IDDL ,IADK,K_DIAG,K_LT ,K22,ND  )
c          CALL PUT_KII(N3  ,IDDL ,IADK,K_DIAG,K_LT ,K33,ND  )
c          CALL PUT_KII(N4  ,IDDL ,IADK,K_DIAG,K_LT ,K44,ND  )
C
                      CALL PUT_KIJ(N1  ,N2  ,IDDL ,IADK,JDIK,K_LT,K12,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N1  ,N3  ,IDDL ,IADK,JDIK,K_LT,K13,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N1  ,N4  ,IDDL ,IADK,JDIK,K_LT,K14,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N2  ,N3  ,IDDL ,IADK,JDIK,K_LT,K23,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N2  ,N4  ,IDDL ,IADK,JDIK,K_LT,K24,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N3  ,N4  ,IDDL ,IADK,JDIK,K_LT,K34,ND ,ND ,
     .                             IERR)
                  ELSE
C        triangles.
                      CALL KPTRIA(N1,N2,N3,AA,X,
     .                            K11,K22,K33,K12,K13,K23)
c          CALL PUT_KII(N1  ,IDDL ,IADK,K_DIAG,K_LT ,K11,ND  )
c          CALL PUT_KII(N2  ,IDDL ,IADK,K_DIAG,K_LT ,K22,ND  )
c          CALL PUT_KII(N3  ,IDDL ,IADK,K_DIAG,K_LT ,K33,ND  )
C
                      CALL PUT_KIJ(N1  ,N2  ,IDDL ,IADK,JDIK,K_LT,K12,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N1  ,N3  ,IDDL ,IADK,JDIK,K_LT,K13,ND ,ND ,
     .                             IERR)
                      CALL PUT_KIJ(N2  ,N3  ,IDDL ,IADK,JDIK,K_LT,K23,ND ,ND ,
     .                             IERR)
                  ENDIF
              ELSE
C        ANALYSE 2D
              ENDIF
          ENDIF
   10 CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  KPQUAD                        source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KPQUAD(N1,N2,N3,N4,P,X,VKSI,VETA,VF4,
     .                 K11,K22,K33,K44,K12,K13,K14,K23,K24,K34)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4
      my_real
     .        P,X(3,*),VKSI(4,4),VETA(4,4),VF4(4,4),
     .        K11(6,6),K22(6,6),K33(6,6),K44(6,6),K12(6,6),
     .        K13(6,6),K14(6,6),K23(6,6),K24(6,6),K34(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J  ,K,NP
      MY_REAL
     .    PG,J0,J1,J2,DETA(4),X1,Y1,S1,PG2,
     .    KSIX,KSIY,KSIZ,ETAX,ETAY,ETAZ,HX,HY,HZ,
     .    G1X(4),G1Y(4),G1Z(4),G2X(4),G2Y(4),G2Z(4)
      DATA   PG/.577350269189626/
C
      KSIX=(-X(1,N1)+X(1,N2)+X(1,N3)-X(1,N4))*FOURTH
      KSIY=(-X(2,N1)+X(2,N2)+X(2,N3)-X(2,N4))*FOURTH
      KSIZ=(-X(3,N1)+X(3,N2)+X(3,N3)-X(3,N4))*FOURTH
C
      ETAX=(-X(1,N1)-X(1,N2)+X(1,N3)+X(1,N4))*FOURTH
      ETAY=(-X(2,N1)-X(2,N2)+X(2,N3)+X(2,N4))*FOURTH
      ETAZ=(-X(3,N1)-X(3,N2)+X(3,N3)+X(3,N4))*FOURTH
C
      HX=(X(1,N1)-X(1,N2)+X(1,N3)-X(1,N4))*FOURTH
      HY=(X(2,N1)-X(2,N2)+X(2,N3)-X(2,N4))*FOURTH
      HZ=(X(3,N1)-X(3,N2)+X(3,N3)-X(3,N4))*FOURTH
C
      G1X(1)=KSIX-PG*HX
      G1Y(1)=KSIY-PG*HY
      G1Z(1)=KSIZ-PG*HZ
      G1X(3)=KSIX+PG*HX
      G1Y(3)=KSIY+PG*HY
      G1Z(3)=KSIZ+PG*HZ
      G1X(2)=G1X(1)
      G1Y(2)=G1Y(1)
      G1Z(2)=G1Z(1)
      G1X(4)=G1X(3)
      G1Y(4)=G1Y(3)
      G1Z(4)=G1Z(3)
C
      G2X(1)=ETAX-PG*HX
      G2Y(1)=ETAY-PG*HY
      G2Z(1)=ETAZ-PG*HZ
      G2X(2)=ETAX+PG*HX
      G2Y(2)=ETAY+PG*HY
      G2Z(2)=ETAZ+PG*HZ
      G2X(3)=G2X(2)
      G2Y(3)=G2Y(2)
      G2Z(3)=G2Z(2)
      G2X(4)=G2X(1)
      G2Y(4)=G2Y(1)
      G2Z(4)=G2Z(1)
C
C       DO I =1,4
c        CALL PRODUITV(G1X,G1Y,G1Z,G2X,G2Y,G2Z,DETA(I))
C       END DO
C
      DO J =1,3
          DO K =J,3
              K11(J,K)=ZERO
              K22(J,K)=ZERO
              K33(J,K)=ZERO
              K44(J,K)=ZERO
          END DO
      END DO
C
      DO J =1,3
          DO K =1,3
              K12(J,K)=ZERO
              K13(J,K)=ZERO
              K14(J,K)=ZERO
              K23(J,K)=ZERO
              K24(J,K)=ZERO
              K34(J,K)=ZERO
          END DO
      END DO
C
      S1=HALF*P
      DO NP =1,4
c        K11(1,2)=K11(1,2) + S1*VF4(1,NP)*
c     .           (VETA(1,NP)*G2Z(NP)-VKSI(1,NP)*G1Z(NP))
c        K11(1,3)=K11(1,3) - S1*VF4(1,NP)*
c     .           (VETA(1,NP)*G2Y(NP)-VKSI(1,NP)*G1Y(NP))
c        K11(2,3)=K11(2,3) + S1*VF4(1,NP)*
c     .           (VETA(1,NP)*G2X(NP)-VKSI(1,NP)*G1X(NP))
c        K22(1,2)=K22(1,2) + S1*VF4(2,NP)*
c     .           (VETA(2,NP)*G2Z(NP)-VKSI(2,NP)*G1Z(NP))
c        K22(1,3)=K22(1,3) - S1*VF4(2,NP)*
c     .           (VETA(2,NP)*G2Y(NP)-VKSI(2,NP)*G1Y(NP))
c        K22(2,3)=K22(2,3) + S1*VF4(2,NP)*
c     .           (VETA(2,NP)*G2X(NP)-VKSI(2,NP)*G1X(NP))
c        K33(1,2)=K33(1,2) + S1*VF4(3,NP)*
c     .           (VETA(3,NP)*G2Z(NP)-VKSI(3,NP)*G1Z(NP))
c        K33(1,3)=K33(1,3) - S1*VF4(3,NP)*
c     .           (VETA(3,NP)*G2Y(NP)-VKSI(3,NP)*G1Y(NP))
c        K33(2,3)=K33(2,3) + S1*VF4(3,NP)*
c     .           (VETA(3,NP)*G2X(NP)-VKSI(3,NP)*G1X(NP))
c        K44(1,2)=K44(1,2) + S1*VF4(4,NP)*
c     .           (VETA(4,NP)*G2Z(NP)-VKSI(4,NP)*G1Z(NP))
c        K44(1,3)=K44(1,3) - S1*VF4(4,NP)*
c     .           (VETA(4,NP)*G2Y(NP)-VKSI(4,NP)*G1Y(NP))
c        K44(2,3)=K44(2,3) + S1*VF4(4,NP)*
c     .           (VETA(4,NP)*G2X(NP)-VKSI(4,NP)*G1X(NP))
C
          K12(1,2)=K12(1,2) + S1*VF4(1,NP)*
     .             (VKSI(2,NP)*G2Z(NP)-VETA(2,NP)*G1Z(NP))
          K12(1,3)=K12(1,3) - S1*VF4(1,NP)*
     .             (VKSI(2,NP)*G2Y(NP)-VETA(2,NP)*G1Y(NP))
          K12(2,3)=K12(2,3) + S1*VF4(1,NP)*
     .             (VKSI(2,NP)*G2X(NP)-VETA(2,NP)*G1X(NP))
          K13(1,2)=K13(1,2) + S1*VF4(1,NP)*
     .             (VKSI(3,NP)*G2Z(NP)-VETA(3,NP)*G1Z(NP))
          K13(1,3)=K13(1,3) - S1*VF4(1,NP)*
     .             (VKSI(3,NP)*G2Y(NP)-VETA(3,NP)*G1Y(NP))
          K13(2,3)=K13(2,3) + S1*VF4(1,NP)*
     .             (VKSI(3,NP)*G2X(NP)-VETA(3,NP)*G1X(NP))
          K14(1,2)=K14(1,2) + S1*VF4(1,NP)*
     .             (VKSI(4,NP)*G2Z(NP)-VETA(4,NP)*G1Z(NP))
          K14(1,3)=K14(1,3) - S1*VF4(1,NP)*
     .             (VKSI(4,NP)*G2Y(NP)-VETA(4,NP)*G1Y(NP))
          K14(2,3)=K14(2,3) + S1*VF4(1,NP)*
     .             (VKSI(4,NP)*G2X(NP)-VETA(4,NP)*G1X(NP))
          K23(1,2)=K23(1,2) + S1*VF4(2,NP)*
     .             (VKSI(3,NP)*G2Z(NP)-VETA(3,NP)*G1Z(NP))
          K23(1,3)=K23(1,3) - S1*VF4(2,NP)*
     .             (VKSI(3,NP)*G2Y(NP)-VETA(3,NP)*G1Y(NP))
          K23(2,3)=K23(2,3) + S1*VF4(2,NP)*
     .             (VKSI(3,NP)*G2X(NP)-VETA(3,NP)*G1X(NP))
          K24(1,2)=K24(1,2) + S1*VF4(2,NP)*
     .             (VKSI(4,NP)*G2Z(NP)-VETA(4,NP)*G1Z(NP))
          K24(1,3)=K24(1,3) - S1*VF4(2,NP)*
     .             (VKSI(4,NP)*G2Y(NP)-VETA(4,NP)*G1Y(NP))
          K24(2,3)=K24(2,3) + S1*VF4(2,NP)*
     .             (VKSI(4,NP)*G2X(NP)-VETA(4,NP)*G1X(NP))
          K34(1,2)=K34(1,2) + S1*VF4(3,NP)*
     .             (VKSI(4,NP)*G2Z(NP)-VETA(4,NP)*G1Z(NP))
          K34(1,3)=K34(1,3) - S1*VF4(3,NP)*
     .             (VKSI(4,NP)*G2Y(NP)-VETA(4,NP)*G1Y(NP))
          K34(2,3)=K34(2,3) + S1*VF4(3,NP)*
     .             (VKSI(4,NP)*G2X(NP)-VETA(4,NP)*G1X(NP))
      END DO
C
      DO NP =1,4
          K12(1,2)=K12(1,2) - S1*VF4(2,NP)*
     .             (VKSI(1,NP)*G2Z(NP)-VETA(1,NP)*G1Z(NP))
          K12(1,3)=K12(1,3) + S1*VF4(2,NP)*
     .             (VKSI(1,NP)*G2Y(NP)-VETA(1,NP)*G1Y(NP))
          K12(2,3)=K12(2,3) - S1*VF4(2,NP)*
     .             (VKSI(1,NP)*G2X(NP)-VETA(1,NP)*G1X(NP))
          K13(1,2)=K13(1,2) - S1*VF4(3,NP)*
     .             (VKSI(1,NP)*G2Z(NP)-VETA(1,NP)*G1Z(NP))
          K13(1,3)=K13(1,3) + S1*VF4(3,NP)*
     .             (VKSI(1,NP)*G2Y(NP)-VETA(1,NP)*G1Y(NP))
          K13(2,3)=K13(2,3) - S1*VF4(3,NP)*
     .             (VKSI(1,NP)*G2X(NP)-VETA(1,NP)*G1X(NP))
          K14(1,2)=K14(1,2) - S1*VF4(4,NP)*
     .             (VKSI(1,NP)*G2Z(NP)-VETA(1,NP)*G1Z(NP))
          K14(1,3)=K14(1,3) + S1*VF4(4,NP)*
     .             (VKSI(1,NP)*G2Y(NP)-VETA(1,NP)*G1Y(NP))
          K14(2,3)=K14(2,3) - S1*VF4(4,NP)*
     .             (VKSI(1,NP)*G2X(NP)-VETA(1,NP)*G1X(NP))
          K23(1,2)=K23(1,2) - S1*VF4(3,NP)*
     .             (VKSI(2,NP)*G2Z(NP)-VETA(2,NP)*G1Z(NP))
          K23(1,3)=K23(1,3) + S1*VF4(3,NP)*
     .             (VKSI(2,NP)*G2Y(NP)-VETA(2,NP)*G1Y(NP))
          K23(2,3)=K23(2,3) - S1*VF4(3,NP)*
     .             (VKSI(2,NP)*G2X(NP)-VETA(2,NP)*G1X(NP))
          K24(1,2)=K24(1,2) - S1*VF4(4,NP)*
     .             (VKSI(2,NP)*G2Z(NP)-VETA(2,NP)*G1Z(NP))
          K24(1,3)=K24(1,3) + S1*VF4(4,NP)*
     .             (VKSI(2,NP)*G2Y(NP)-VETA(2,NP)*G1Y(NP))
          K24(2,3)=K24(2,3) - S1*VF4(4,NP)*
     .             (VKSI(2,NP)*G2X(NP)-VETA(2,NP)*G1X(NP))
          K34(1,2)=K34(1,2) - S1*VF4(4,NP)*
     .             (VKSI(3,NP)*G2Z(NP)-VETA(3,NP)*G1Z(NP))
          K34(1,3)=K34(1,3) + S1*VF4(4,NP)*
     .             (VKSI(3,NP)*G2Y(NP)-VETA(3,NP)*G1Y(NP))
          K34(2,3)=K34(2,3) - S1*VF4(4,NP)*
     .             (VKSI(3,NP)*G2X(NP)-VETA(3,NP)*G1X(NP))
      END DO
      K12(2,1)=-K12(1,2)
      K12(3,1)=-K12(1,3)
      K12(3,2)=-K12(2,3)
      K13(2,1)=-K13(1,2)
      K13(3,1)=-K13(1,3)
      K13(3,2)=-K13(2,3)
      K14(2,1)=-K14(1,2)
      K14(3,1)=-K14(1,3)
      K14(3,2)=-K14(2,3)
      K23(2,1)=-K23(1,2)
      K23(3,1)=-K23(1,3)
      K23(3,2)=-K23(2,3)
      K24(2,1)=-K24(1,2)
      K24(3,1)=-K24(1,3)
      K24(3,2)=-K24(2,3)
      K34(2,1)=-K34(1,2)
      K34(3,1)=-K34(1,3)
      K34(3,2)=-K34(2,3)
C
      RETURN
      END
Chd|====================================================================
Chd|  PRODUITV                      source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODUITV(RX, RY, RZ, SX, SY, SZ,DET)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   RX , RY , RZ,SX , SY, SZ, DET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C                                                                     12
      MY_REAL
     .   E3X,E3Y,E3Z
      E3X = RY * SZ - RZ * SY
      E3Y = RZ * SX - RX * SZ
      E3Z = RX * SY - RY * SX
      DET= SQRT(E3X*E3X + E3Y*E3Y + E3Z*E3Z)
C
      RETURN
      END
Chd|====================================================================
Chd|  KP4_INI                       source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KP4_INI(VKSI,VETA,VF4)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        VKSI(4,4),VETA(4,4),VF4(4,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J  ,K
      MY_REAL
     .    PG,PG2
      DATA   PG/.577350269189626/
C------------NI,ksi=VKSI,NI,eta=VETA--------------------------------
      VKSI(1,1)=-FOURTH*(ONE+PG)
      VKSI(2,1)=-VKSI(1,1)
      VKSI(3,1)= FOURTH*(ONE-PG)
      VKSI(4,1)=-VKSI(3,1)
      VETA(1,1)=-FOURTH*(ONE+PG)
      VETA(2,1)=-FOURTH*(ONE-PG)
      VETA(3,1)=-VETA(2,1)
      VETA(4,1)=-VETA(1,1)
      VKSI(1,2)= VKSI(1,1)
      VKSI(2,2)=-VKSI(1,2)
      VKSI(3,2)= VKSI(3,1)
      VKSI(4,2)=-VKSI(3,2)
      VETA(1,2)= VETA(2,1)
      VETA(2,2)= VETA(1,1)
      VETA(3,2)=-VETA(2,2)
      VETA(4,2)=-VETA(1,2)
      VKSI(1,3)=-VKSI(3,1)
      VKSI(2,3)=-VKSI(1,3)
      VKSI(3,3)=-VKSI(1,1)
      VKSI(4,3)=-VKSI(3,3)
      VETA(1,3)= VETA(1,2)
      VETA(2,3)= VETA(2,2)
      VETA(3,3)=-VETA(2,3)
      VETA(4,3)=-VETA(1,3)
      VKSI(1,4)= VKSI(1,3)
      VKSI(2,4)=-VKSI(1,4)
      VKSI(3,4)= VKSI(3,3)
      VKSI(4,4)=-VKSI(3,4)
      VETA(1,4)= VETA(1,1)
      VETA(2,4)= VETA(2,1)
      VETA(3,4)=-VETA(2,4)
      VETA(4,4)=-VETA(1,4)
      PG2=FOURTH*PG*PG
      DO I =1,4
          VF4(I,1)=FOURTH+(-VKSI(I,1)-VETA(I,1))*PG
          VF4(I,2)=FOURTH+(VKSI(I,2)-VETA(I,2))*PG
          VF4(I,3)=FOURTH+(VKSI(I,3)+VETA(I,3))*PG
          VF4(I,4)=FOURTH+(-VKSI(I,4)+VETA(I,4))*PG
      END DO
      VF4(1,1)=VF4(1,1)-PG2
      VF4(2,1)=VF4(2,1)+PG2
      VF4(3,1)=VF4(3,1)-PG2
      VF4(4,1)=VF4(4,1)+PG2
      VF4(1,2)=VF4(1,2)+PG2
      VF4(2,2)=VF4(2,2)-PG2
      VF4(3,2)=VF4(3,2)+PG2
      VF4(4,2)=VF4(4,2)-PG2
      VF4(1,3)=VF4(1,3)-PG2
      VF4(2,3)=VF4(2,3)+PG2
      VF4(3,3)=VF4(3,3)-PG2
      VF4(4,3)=VF4(4,3)+PG2
      VF4(1,4)=VF4(1,4)+PG2
      VF4(2,4)=VF4(2,4)-PG2
      VF4(3,4)=VF4(3,4)+PG2
      VF4(4,4)=VF4(4,4)-PG2
      RETURN
      END
Chd|====================================================================
Chd|  KPTRIA                        source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KPTRIA(N1,N2,N3,P,X,
     .                  K11,K22,K33,K12,K13,K23)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,N3
      my_real
     .        P,X(3,*),
     .        K11(6,6),K22(6,6),K33(6,6),K12(6,6),
     .        K13(6,6),K23(6,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J  ,K
      MY_REAL
     .    X21,Y21,Z21,X31,Y31,Z31,S1,G1X,G1Y,G1Z,G2X,G2Y,G2Z
C
      X21=X(1,N2)-X(1,N1)
      Y21=X(2,N2)-X(2,N1)
      Z21=X(3,N2)-X(3,N1)
      X31=X(1,N3)-X(1,N1)
      Y31=X(2,N3)-X(2,N1)
      Z31=X(3,N3)-X(3,N1)
C
      G1X=X21
      G1Y=Y21
      G1Z=Z21
      G2X=X31
      G2Y=Y31
      G2Z=Z31
C       CALL PRODUITV(G1X,G1Y,G1Z,G2X,G2Y,G2Z,DETA2)
      DO K =1,3
          K11(K,K)=ZERO
          K22(K,K)=ZERO
          K33(K,K)=ZERO
          K12(K,K)=ZERO
          K13(K,K)=ZERO
          K23(K,K)=ZERO
      END DO
C
      S1 = ONE_OVER_6*P*HALF
C        K11(1,2)=-S1*(G2Z-G1Z)
C        K11(1,3)=S1*(G2Y-G1Y)
C        K11(2,3)=-S1*(G2X-G1X)
C        K22(1,2)=S1*G2Z
C        K22(1,3)=-S1*G2Y
C        K22(2,3)=S1*G2X
C        K33(1,2)=-S1*G1Z
C        K33(1,3)=S1*G1Y
C        K33(2,3)=-S1*G1X
      K11(1,2)=ZERO
      K11(1,3)=ZERO
      K11(2,3)=ZERO
      K22(1,2)=ZERO
      K22(1,3)=ZERO
      K22(2,3)=ZERO
      K33(1,2)=ZERO
      K33(1,3)=ZERO
      K33(2,3)=ZERO
C----------Kij(1,2) =0.5(Kij(1,2)+Kji(2,1))
      K12(1,2)=S1*(G2Z+G2Z-G1Z)
      K12(1,3)=-S1*(G2Y+G2Y-G1Y)
      K12(2,3)=S1*(G2X+G2X-G1X)
      K13(1,2)=-S1*(G1Z-G2Z+G1Z)
      K13(1,3)=S1*(G1Y-G2Y+G1Y)
      K13(2,3)=-S1*(G1X-G2X+G1X)
      K23(1,2)=-S1*(G1Z+G2Z)
      K23(1,3)=S1*(G1Y+G2Y)
      K23(2,3)=-S1*(G1X+G2X)
C
      K12(2,1)=-K12(1,2)
      K12(3,1)=-K12(1,3)
      K12(3,2)=-K12(2,3)
      K13(2,1)=-K13(1,2)
      K13(3,1)=-K13(1,3)
      K13(3,2)=-K13(2,3)
      K23(2,1)=-K23(1,2)
      K23(3,1)=-K23(1,3)
      K23(3,2)=-K23(2,3)
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEMC_KII                    source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        ASSEMC_KIJ                    source/implicit/imp_glob_k.F  
Chd|        ASSEM_C3                      source/implicit/assem_c3.F    
Chd|        ASSEM_C4                      source/implicit/assem_c4.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSEMC_KII(NI    ,NEL   ,IDDL  ,IADK  ,K_DIAG,
     1                     K_LT  ,KII   ,ND     ,OFF   ,NDOF  )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI(*),NEL   ,IDDL(*)  , IADK(*),NDOF(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KII(ND,ND,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----6---used only for shell due to npt=1 -> NDOF=3-------
      INTEGER N,K,EP,IK,ID,JD,L
C----6----------KII is always triag_sup whatever IKPAT---------------7--------
Cel lock par element trop penalisant pour perf
#include "lockon.inc"
      DO EP = 1,NEL
          IF (OFF(EP)>ZERO.AND.NI(EP)>0) THEN
              N = NI(EP)
              ID = IDDL(N)
              IF (IKPAT==0) THEN
                  DO K=1,NDOF(N)
c#include "lockon.inc"
                      K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K,EP)
c#include "lockoff.inc"
                      JD = IADK(ID+K)-1
                      DO L=K+1,NDOF(N)
                          IK = JD+L-K
c#include "lockon.inc"
                          K_LT(IK) = K_LT(IK) + KII(K,L,EP)
c#include "lockoff.inc"
                      ENDDO
                  ENDDO
              ELSE
                  DO K=1,NDOF(N)
c#include "lockon.inc"
                      K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K,EP)
c#include "lockoff.inc"
                      JD = IADK(ID+K+1)-K
                      DO L=1,K-1
                          IK = JD+L
c#include "lockon.inc"
                          K_LT(IK) = K_LT(IK) + KII(L,K,EP)
c#include "lockoff.inc"
                      ENDDO
                  ENDDO
              ENDIF
          ENDIF
      ENDDO
#include "lockoff.inc"
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEMC_KIJ                    source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        ASSEM_C3                      source/implicit/assem_c3.F    
Chd|        ASSEM_C4                      source/implicit/assem_c4.F    
Chd|-- calls ---------------
Chd|        ASSEMC_KII                    source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEMC_KIJ( NI ,NJ ,NEL  ,IDDL  ,IADK,JDIK,
     1                      K_DIAG,K_LT  ,KIJ  ,ND    ,OFF ,
     2                      NDOF  )
C----6---------------------------------------------------------------7---------8
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
#include      "comlock.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER NI(*),NJ(*),NEL   ,IDDL(*)  ,IADK(*),JDIK(*) ,NDOF(*)
C     REAL
      my_real
     .   K_DIAG(*),K_LT(*)   ,KIJ(ND,ND,*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,EP,ID,JD,JDL,L,JJ,NN(MVSIZ),NELD,N,N1,N2
      my_real
     .   KIJD(ND,ND,MVSIZ),OFFD(MVSIZ)
C---- s'il y a des elms degeneres--------------
      NELD=0
      DO EP = 1,NEL
          IF (NI(EP)==NJ(EP).AND.OFF(EP)>ZERO.AND.NI(EP)>0) THEN
              NELD=NELD+1
              N=NI(EP)
              NN(NELD)=N
              OFFD(NELD)=OFF(EP)
              DO I=1,NDOF(N)
                  DO J=I,NDOF(N)
                      KIJD(I,J,NELD)=KIJ(I,J,EP)+KIJ(J,I,EP)
                  ENDDO
              ENDDO
          ENDIF
      ENDDO
      IF (NELD>0)
     . CALL ASSEMC_KII(NN    ,NELD  ,IDDL  ,IADK  ,K_DIAG,
     .                 K_LT  ,KIJD  ,ND    ,OFFD  ,NDOF  )
C----6---------------------------------------------------------------7---------8
Cel lock par element trop penalisant pour perf
#include "lockon.inc"
      IF (IKPAT==0) THEN
          DO EP = 1,NEL
              IF (OFF(EP)>ZERO.AND.NI(EP)/=NJ(EP).AND.
     .            NI(EP)>0.AND.NJ(EP)>0) THEN
                  N1=NI(EP)
                  N2=NJ(EP)
                  I = IDDL(N1)
                  J = IDDL(N2)
                  ID = MIN(I,J)
                  JD = MAX(I,J)+1
                  IF (I==ID) THEN
                      DO K=1,NDOF(N1)
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 100
                              ENDIF
                          ENDDO
  100                     DO L=1,NDOF(N1)
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ELSE
                      DO K=1,NDOF(N2)
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 200
                              ENDIF
                          ENDDO
  200                     DO L=1,NDOF(N2)
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO
      ELSE
          DO EP = 1,NEL
              IF (OFF(EP)>ZERO.AND.NI(EP)/=NJ(EP).AND.
     .            NI(EP)>0.AND.NJ(EP)>0) THEN
                  N1=NI(EP)
                  N2=NJ(EP)
                  I = IDDL(N1)
                  J = IDDL(N2)
                  ID = MAX(I,J)
                  JD = MIN(I,J)+1
                  IF (I==ID) THEN
                      DO K=1,NDOF(N1)
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 300
                              ENDIF
                          ENDDO
  300                     DO L=1,NDOF(N1)
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ELSE
                      DO K=1,NDOF(N2)
                          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
                              IF (JDIK(JJ)==JD) THEN
                                  JDL = JJ-1
                                  GOTO 400
                              ENDIF
                          ENDDO
  400                     DO L=1,NDOF(N2)
c#include "lockon.inc"
                              K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K,EP)
c#include "lockoff.inc"
                          ENDDO
                      ENDDO
                  ENDIF
              ENDIF
          ENDDO
      ENDIF
#include "lockoff.inc"
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
C-----------------------------------------------
Chd|====================================================================
Chd|  GRPREORDER                    source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        ETFAC_INI                     source/implicit/imp_init.F    
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  GRPREORDER(IPARG, IGROUC)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IGROUC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, ITY, NGROUC
C-----------------------------------------------
C-------------first shell+ other
      NGROUC = 0
      DO NG = 1, NGROUP
          ITY   =IPARG(5,NG)
          IF(ITY==3.OR.ITY==7)THEN
              NGROUC = NGROUC + 1
              IGROUC(NGROUC)=NG
          END IF
      END DO
      DO NG = 1, NGROUP
          ITY   =IPARG(5,NG)
          IF(ITY==3.OR.ITY==7)THEN
          ELSE
              NGROUC = NGROUC + 1
              IGROUC(NGROUC)=NG
          END IF
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        GRPREORDER                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE IMP_GLOB_KHP(
     1    PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3    IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     4    IXS20     ,IXS16     ,IPARG     ,TF        ,NPC       ,
     5    FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6    RBY       ,SKEW      ,X         ,
     7    WA        ,IDDL      ,NDOF      ,K_DIAG    ,K_LT      ,
     8    IADK      ,JDIK      ,IKGEO     ,ETAG      ,ITASK0    ,
     9    ELBUF_TAB ,STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*) ,
     .   IPM(NPROPMI,*),IGEO(NPROPGI,*),IKGEO,ITASK0
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),
     .   IXP(NIXP,*),IXR(NIXR,*), IXTG(NIXTG,*),
     .   NPC(*), IPARG(NPARG,*),
     .   IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG1(4,*), ETAG(*)
C     REAL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*) ,TF(*) ,
     .   FR_WAVE(*) ,ELBUF(*) ,THKE(*),RBY(*),SKEW(LSKEW,*),
     .   BUFGEO(*),W16(*),X(3,*),WA(*)
      my_real
     .   K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_), TARGET  :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,ITASK
      INTEGER IGROUC(NGROUP),IPRMES_EL(40)
C
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C----6---------------------------------------------------------------7-2
      IF (NCYCLE==1.AND.INCONV==1) THEN
          DO I=1,40
              IPRMES_EL(I)=0
          ENDDO
C        /---------------/
      END IF
      CALL  GRPREORDER(IPARG, IGROUC)
!$OMP PARALLEL PRIVATE(ITASK)
      ITASK = OMP_GET_THREAD_NUM()

c
      CALL IMP_GLOB_K0(
     1   PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2   IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3   IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     4   IXS20     ,IXS16     ,IPARG     ,TF        ,NPC       ,
     5   FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6   RBY       ,SKEW      ,X         ,
     7   WA        ,IDDL      ,NDOF      ,K_DIAG    ,K_LT      ,
     8   IADK      ,JDIK      ,IKGEO     ,ETAG      ,ITASK     ,
     9   ELBUF_TAB ,IGROUC    ,IPRMES_EL ,STACK     ,DRAPE_SH4N, DRAPE_SH3N  ,
     A   DRAPEG )
C
!$OMP END PARALLEL
      IF (NCYCLE==1.AND.INCONV==1) THEN
          IF (NSPMD>1) THEN
              DO I=1,19
                  CALL SPMD_MAX_I(IPRMES_EL(I))
              ENDDO
          ENDIF
          IF (ISPMD == 0 ) THEN
              DO I=1,19
                  IF (IPRMES_EL(I)>0) THEN
                      SELECT CASE (I)
C
                        CASE(1)
                          WRITE(IOUT,1006)
                          WRITE(ISTDO,1006)
                        CASE(2)
                          WRITE(IOUT,1001)'  S16 SOLID'
                          WRITE(ISTDO,1001)'  S16 SOLID'
                        CASE(3)
                          N = 24
                          WRITE(IOUT,1002) N
                        CASE(4)
                          N = 12
                          WRITE(IOUT,1002) N
                        CASE(5)
                          N = 0
                          WRITE(IOUT,1002) N
                        CASE(6)
                          N = IPRMES_EL(I)
                          WRITE(IOUT,1002) N
                        CASE(7)
                          WRITE(IOUT,1001)'   USERS   '
                          WRITE(ISTDO,1001)'   USERS   '
                        CASE(8)
                          WRITE(IOUT,1001)' HEPH SOLID'
                        CASE(9,10)
                          WRITE(IOUT,1001)'  S8  SOLID'
                        CASE(11)
                          WRITE(IOUT,1001)'  QUAD 2D  '
                          WRITE(ISTDO,1001)'  QUAD 2D '
                        CASE(12)
                          N = 4
                          WRITE(IOUT,1003) N
                        CASE(13)
                          N = 3
                          WRITE(IOUT,1003) N
                        CASE(14)
                          N = 1
                          WRITE(IOUT,1003) N
                        CASE(15)
                          N = IPRMES_EL(I)
                          WRITE(IOUT,1003) N
                        CASE(16)
                          N = IPRMES_EL(I)
                          WRITE(IOUT,1005) N
                          WRITE(ISTDO,1005) N
                        CASE(17)
                          WRITE(IOUT,1001)' S3N6 SHELL'
                          WRITE(ISTDO,1001)' S3N6 SHELL'
                        CASE(18)
                          N = IPRMES_EL(I)
                          WRITE(IOUT,1004) N
                        CASE(19)
                          WRITE(IOUT,1001)'USER-SPRING'
                          WRITE(ISTDO,1001)'USER-SPRING'
                      END SELECT
                  ENDIF
              ENDDO
          ENDIF
      END IF !(NCYCLE==1.AND.INCONV==1) THEN
C----6---------------------------------------------------------------7---------8
 1001 FORMAT('  ***** WARNING : IMPLICIT FORMULATION IS NOT AVAILABLE
     . WITH '/,2X,A11,' ELEMENT : STIFFNESS IGNORED *****')
 1002 FORMAT('  ***** WARNING : ELEMENT FORMULATION ISOLID= ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING,'
     .       ,' USING GENERIC ONE INSTEAD'/
     .       ,5X,' POSSIBLE CONVERGING ISSUE. *****')
 1003 FORMAT('  ***** WARNING : ELEMENT FORMULATION ISHELL= ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING,'
     .       ,' USING GENERIC ONE INSTEAD'/
     .       ,5X,' POSSIBLE CONVERGING ISSUE. *****')
 1004 FORMAT('  ***** WARNING : ELEMENT FORMULATION ISH3N = ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING,'
     .       ,' USING GENERIC ONE INSTEAD'/
     .       ,5X,' POSSIBLE CONVERGING ISSUE. *****')
 1005 FORMAT('  ***** WARNING : SPRING ELEMENT PROP.TYPE = ',
     .       I4/,5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING,'
     .       ,' STIFFNESS IGNORED *****')
 1006 FORMAT(' *****WARNING : TETRA ELEMENT FORMULATION W/ ITETRA>0 '/,
     .        5X,'IS NOT AVAILABLE FOR STIFFNESS MATRIX BUILDING;'/,
     .        5X,'USING ITETRA=0 INSTEAD, POSSIBLE CONVERGING ISSUE.')
      RETURN
      END
Chd|====================================================================
Chd|  IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- called by -----------
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        PKE3                          source/elements/beam/pke3.F   
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|        R4KE3                         source/elements/spring/r4ke3.F
Chd|        R8KE3                         source/elements/spring/r8ke3.F
Chd|        RUSER32KE3                    source/elements/spring/ruser32ke3.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        TKE3                          source/elements/truss/tke3.F  
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE IMP_GLOB_K0(
     1    PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3    IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     4    IXS20     ,IXS16     ,IPARG     ,TF        ,NPC       ,
     5    FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6    RBY       ,SKEW      ,X         ,
     7    WA        ,IDDL      ,NDOF      ,K_DIAG    ,K_LT      ,
     8    IADK      ,JDIK      ,IKGEO     ,ETAG      ,ITASK     ,
     9    ELBUF_TAB ,IGROUC    ,IPRMES_EL ,STACK     ,DRAPE_SH4N, DRAPE_SH3N ,
     A    DRAPEG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "vect01_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*) ,
     .   IPM(NPROPMI,*),IGEO(NPROPGI,*),IKGEO,ITASK,IGROUC(NGROUP)
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),
     .   IXP(NIXP,*),IXR(NIXR,*), IXTG(NIXTG,*),
     .   NPC(*), IPARG(NPARG,*),
     .   IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG1(4,*), ETAG(*),
     .   IPRMES_EL(*)
C     REAL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*) ,TF(*) ,
     .   FR_WAVE(*) ,ELBUF(*) ,THKE(*),RBY(*),SKEW(LSKEW,*),
     .   BUFGEO(*),W16(*),X(3,*),WA(*)
      my_real
     .   K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK ,IPLA ,
     .   K1, K2, KAD,IAD2,NF1,IPRI, NELEM, OFFSET, NSGRP, K,
     .   K0, K3, K5, K6, K7, K8, K9, NSG, NEL, KFTS,IOFC, ISTRA,
     .   JJ19,NPE,NIPMAX,ICNOD,NFT1,LIAD,INPT,NF2,MPT,
     .   L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,
     .   L17,L18,L19,L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,L30,
     .   SEDRAPE, NUMEL_DRAPE
      INTEGER INDXOF(MVSIZ),ISH3N
      INTEGER ICP,ICS,IEXPAN,IETY,IG,ISUBSTACK
      my_real
     .   OFF(MVSIZ)
C----6---------------------------------------------------------------7-2
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUP
          NG = IGROUC(IG)
C---------temporarily used to avoid pass KTBUF_STR everywhere
          NG_IMP = NG
c            IF(NGDONE>NGROUP) GOTO 250
c            NGDONE = NG + 1
C
          IF(IPARG(8,NG)==1)GOTO 250
          IF (IDDW>0) CALL STARTIMEG(NG)
          ITY   =IPARG(5,NG)
          OFFSET  = 0
          MLW     = IPARG(1,NG)
C
          IF (MLW == 0 .OR. MLW == 13) GOTO 250
          CALL INITBUF(IPARG    ,NG      ,
     2      MLW     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3      NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4      JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5      NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6      IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7      ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          ICNOD   = IPARG(11,NG)
          NSG     = IPARG(10,NG)
          ICP     = IPARG(10,NG)
          ICS     = IPARG(17,NG)
          ISTRA   = IPARG(44,NG)
          NVC     = IPARG(19,NG)
          ITHK    = IPARG(28,NG)
          ISOLNOD = IPARG(28,NG)
          KFTS    = IPARG(30,NG)
          IEXPAN  = IPARG(49,NG)
          ISH3N   = IPARG(23,NG)
          ISUBSTACK=IPARG(71,NG)
          IF(ITY==1) THEN
              LIAD   = NVAUX
          ENDIF
          IF(ITY==1.OR.ITY==2) JPLASOL=IPLA
          IFORMDT = 0
          LFT   = 1
          LLT   = MIN(NVSIZ,NEL)
          MTN   = MLW
          JFT=LFT
          JLT=LLT
          NF1 = NFT+1
          IAD = KAD
C
          JSPH=0
C----6---------------------------------------------------------------7---------8
          IF(ITY==1 .AND. JLAG==1)THEN
              IGTYP = NINT(GEO(12,IXS(10,NF1)))
              IF(ISOLNOD==4)THEN
                  IETY=1
                  IF (ISROT > 0 .AND. ISPMD==0) THEN
                      IF (IPRMES_EL(IETY)==0) THEN
                          IPRMES_EL(IETY)=1
                      ENDIF
                  ENDIF
                  CALL S4KE3(
     1            PM,           GEO,          IXS,          X,
     2            ELBUF_TAB(NG)%GBUF,        ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            ISMSTR,       JHBE,         IREP,         ISORTH,
     7            IFORMDT)
              ELSEIF(ISOLNOD==10)THEN
                  NF2=NF1-NUMELS8
                  CALL  S10KE3(
     1            PM,           GEO,          IXS,          IXS10,
     2            X,            ELBUF_TAB(NG),ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            NPT,          ISMSTR,       JHBE,         IREP,
     7            ISORTH,       JLAG)

              ELSEIF(ISOLNOD==20)THEN
                  CALL S20KE3(
     1            PM,           GEO,          IXS,          IXS20,
     2            X,            ELBUF_TAB(NG),ETAG,         IDDL,
     3            NDOF,         K_DIAG,       K_LT,         IADK,
     4            JDIK,         NEL,          IPM,          IGEO,
     5            IKGEO,        BUFMAT,       NFT,          MTN,
     6            ISMSTR,       JHBE,         IREP,         IGTYP,
     7            ISORTH)


              ELSEIF(ISOLNOD==16)THEN
                  IETY=2
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
              ELSEIF(JHBE==15.AND.ISOLNOD==6)THEN
                  CALL S6CKE3(
     1            PM,           GEO,          IXS,          X,
     2            ELBUF_TAB(NG),ETAG,         IDDL,         NDOF,
     3            K_DIAG,       K_LT,         IADK,         JDIK,
     4            NEL,          ICP,          ICS,          IPM,
     5            IGEO,         IKGEO,        BUFMAT,       NFT,
     6            MTN,          JHBE,         ISORTH,       ISORTHG,
     7            ISMSTR)
C
              ELSEIF(ISOLNOD==8)THEN
                  IF (JHBE/=14.AND.JHBE/=15.AND.JHBE/=17) THEN
                      IF (NCYCLE==1.AND.IMCONV==1)THEN
                          IF(JHBE==24)THEN
                              IETY=3
                              IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                          ELSEIF(JHBE==12.OR.JHBE==112)THEN
                              IETY=4
                              IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                          ELSEIF(JHBE==0)THEN
                              IETY=5
                              IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                          ELSE
                              IETY=6
                              IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=JHBE
                          ENDIF
                      ENDIF
                  ENDIF
c
                  IF (JHBE == 14 .AND.
     .               (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22)) THEN
                      CALL S8CKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         IPM,
     5                IGEO,         IKGEO,        BUFMAT,       NFT,
     6                MTN,          JHBE,         JCVT,         IGTYP,
     7                ISORTH,       IREP,         ISMSTR)
                  ELSE IF(JHBE == 17 .AND.  IPARG(36,NG) == 3) THEN
                      MPT = 222
                      CALL S8SKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         MPT,
     5                IPM,          IGEO,         IKGEO,        BUFMAT,
     6                NFT,          MTN,          JHBE,         JCVT,
     7                IGTYP,        ISORTH)
                  ELSE
                      MPT = 222
                      CALL S8ZKE3(
     1                PM,           GEO,          IXS,          X,
     2                ELBUF_TAB(NG),NEL,          ICP,          ICS,
     3                ETAG,         IDDL,         NDOF,         K_DIAG,
     4                K_LT,         IADK,         JDIK,         MPT,
     5                IPM,          IGEO,         IKGEO,        BUFMAT,
     6                NFT,          MTN,          ISMSTR,       JHBE,
     7                JCVT,         IGTYP,        ISORTH,       IREP)
                  ENDIF
c
C         OPEN(UNIT=16,FILE='KE_S.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
C         CALL IMPKSOUT( IXS,NFT,NEL,16,
C     1                   K11   ,K12   ,K13    ,K14    ,K15   ,
C     2                   K16   ,K17   ,K18    ,K22    ,K23   ,
C     3                   K24   ,K25   ,K26    ,K27    ,K28   ,
C     4                   K33   ,K34   ,K35    ,K36    ,K37   ,
C     5                   K38   ,K44   ,K45    ,K46    ,K47   ,
C     6                   K48   ,K55   ,K56    ,K57    ,K58   ,
C     7                   K66   ,K67   ,K68    ,K77    ,K78   ,
C     8                   K88   )
C----6---------------------------------------------------------------7---------8
              ELSEIF(IGTYP>=29)THEN
                  IETY=7
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
C               CALL SUKE3(
              ELSEIF(NPT==1)THEN
                  IF(JHBE==24)THEN
C---------------ca ne riske pas de entrer ici pour l'instant
C               CALL SZKE3(
C     1                   PM    ,GEO   ,IXS    ,X  ,ELBUF(KAD),
C     1                   K11   ,K12   ,K13    ,K14    ,K15   ,
C     2                   K16   ,K17   ,K18    ,K22    ,K23   ,
C     3                   K24   ,K25   ,K26    ,K27    ,K28   ,
C     4                   K33   ,K34   ,K35    ,K36    ,K37   ,
C     5                   K38   ,K44   ,K45    ,K46    ,K47   ,
C     6                   K48   ,K55   ,K56    ,K57    ,K58   ,
C     7                   K66   ,K67   ,K68    ,K77    ,K78   ,
C     8                   K88   ,NEL   ,LIAD   ,ICP    ,ICSIG ,
C     9                   OFFSET,ELBUF(IAD2),OFF)
                      IETY=8
                      IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                  ELSE
                      IETY=9
                      IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                  ENDIF
              ELSEIF(NPT==8.AND.MTN/=0 .AND. ISOLNOD/=20)THEN
C              CALL S8KE3(
                  IETY=10
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==2.AND.JMULT==0.AND.JLAG==1)THEN
              IF ((N2D==2.AND.JHBE==17) .OR.
     .            (N2D==1.AND.JHBE==22)) THEN
                  INPT = IABS(NPT)
                  ICP = IPARG(10,NG)
                  CALL Q4KE2(
     1            PM,           GEO,          IXQ,          X,
     2            ELBUF_TAB(NG),NEL,          LIAD,         ICP,
     3            ICS,          ETAG,         IDDL,         NDOF,
     4            K_DIAG,       K_LT,         IADK,         JDIK,
     5            INPT,         IPM,          IGEO,         IKGEO,
     6            BUFMAT,       NFT,          MTN,          JMULT,
     7            JHBE,         JCVT,         IGTYP,        ISORTH,
     8            ISMSTR)
              ELSE
                  IETY=11
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
C             CALL QFORC2(
              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==3)THEN
              IOFC = 0
              IF(NG/=NGROUP)THEN
                  IAD2 = IPARG(4,NG+1) - 6 * NEL - 27 * ISUB * NEL
              ELSE
                  IAD2 = LBUFEL - 6 * NEL + 1 - 27 * ISUB * NEL
              ENDIF
              IF (JHBE<11) THEN
                  IF (NCYCLE==1.AND.IMCONV==1) THEN
                      IF(JHBE==4)THEN
                          IETY=12
                          IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                      ELSEIF(JHBE==3)THEN
                          IETY=13
                          IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                      ELSEIF(JHBE==1)THEN
                          IETY=14
                          IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
                      ELSE
                          IETY=15
                          IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=JHBE
                      ENDIF
                  ENDIF
              ENDIF

              IF(JHBE>=11.AND.JHBE<=19) THEN
                  NUMEL_DRAPE = NUMELC_DRAPE
                  SEDRAPE = SCDRAPE
                  CALL CBAKE3 (
     1            JFT        ,JLT       ,NFT       ,IABS(NPT) ,MLW        ,
     2            ITHK       ,NCYCLE    ,
     3            ISTRA      ,IPLA      ,PM        ,GEO       ,IXC(1,NF1) ,
     4            ELBUF_TAB(NG),BUFMAT   ,OFFSET    ,INDXOF     ,
     1            ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     7            JHBE       ,THKE(NF1)  ,ISMSTR   ,X         ,IKGEO      ,
     8            IPM        ,IGEO     ,IEXPAN    ,IPARG(1,NG),ISUBSTACK ,
     9            STACK      ,DRAPE_SH4N  ,DRAPEG%INDX_SH4N, SEDRAPE, NUMEL_DRAPE)
              ELSE
C

                  NUMEL_DRAPE = NUMELC_DRAPE
                  SEDRAPE = SCDRAPE
                  CALL CZKE3 (
     1            JFT        ,JLT       ,NFT       ,IABS(NPT) ,MLW        ,
     2            ITHK       ,NCYCLE     ,
     3            ISTRA      ,IPLA      ,PM        ,GEO       ,IXC(1,NF1) ,
     4            ELBUF_TAB(NG),BUFMAT   ,OFFSET    ,INDXOF     ,
     1            ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     7            JHBE       ,THKE(NF1)  ,ISMSTR   ,X         ,IKGEO      ,
     8            IPM        ,IGEO     ,IEXPAN    ,IPARG(1,NG),ISUBSTACK  ,
     9            STACK      ,DRAPE_SH4N    ,DRAPEG%INDX_SH4N , SEDRAPE, NUMEL_DRAPE)
C              CALL CKE3(
              ENDIF
c         OPEN(UNIT=13,FILE='KE.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c              CALL IMPKCOUT( IXC,NFT,NEL,13,
c     1                    KC11   ,KC12   ,KC13   ,KC14   ,KC22 ,
c     2                    KC23   ,KC24   ,KC33   ,KC34   ,KC44 )
c              CALL KELAMDA( IXC,NIXC,NFT,NEL,13,
c     1                    KC11   ,KC12   ,KC13   ,KC14   ,KC22 ,
c     2                    KC23   ,KC24   ,KC33   ,KC34   ,KC44 )

C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==4)THEN
              CALL TKE3(
     1        JFT         ,JLT         ,PM          ,GEO         ,IXT(1,NF1) ,
     2        X           ,ELBUF_TAB(NG)  ,NEL         ,OFFSET      ,IKGEO,
     3        ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     4        IADK  ,JDIK  )


C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==5)THEN
              CALL PKE3(JFT   ,JLT    ,NEL  ,MTN    ,ISMSTR,
     1                   PM   ,IXP(1,NF1),X    ,ELBUF_TAB(NG),
     2                   GEO  ,OFFSET , IKGEO,
     3                   ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     4                   IADK  ,JDIK   )


c         OPEN(UNIT=16,FILE='KE_P.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c              CALL IMPKPOUT(NIXPL,IXP,NFT,NEL,16,KC11   ,KC12 ,  KC22 )
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==6)THEN
              IGTYP = NINT(GEO(12,IXR(1,NF1)))
              K1=1 + 6*(NUMELC+NUMELTG)*IEPSDOT + 15*(NUMELT+NUMELP+NFT)
              IF (IGTYP==4)THEN
                  CALL R4KE3 (JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                       TF     ,SKEW  ,OFFSET,FR_WAVE,
     3                       IKGEO  ,IGEO,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK   )

              ELSEIF (IGTYP==32)THEN
                  CALL RUSER32KE3 (JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                          GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                          TF     ,SKEW  ,OFFSET,FR_WAVE,
     3                          IKGEO  ,IGEO,
     1                          ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                          IADK  ,JDIK  )

              ELSEIF (IGTYP==8)THEN
                  CALL R8KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                       TF     ,SKEW  ,OFFSET,FR_WAVE,IGEO   ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )

              ELSEIF (IGTYP==12)THEN
                  CALL R12KE3(JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X  ,ELBUF_TAB(NG),NPC   ,
     2                       TF     ,SKEW  ,OFFSET,FR_WAVE,IGEO  ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )

              ELSEIF (IGTYP==13)THEN
                  CALL R13KE3 (JFT    ,JLT   ,NEL   ,MTN    ,PM    ,
     1                       GEO     ,IXR(1,NF1),X     ,ELBUF_TAB(NG),NPC   ,
     2                       TF  ,SKEW  ,OFFSET,FR_WAVE,IKGEO ,IGEO ,
     1                       ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                       IADK  ,JDIK  )


C        OPEN(UNIT=16,FILE='KE_SP.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
C              CALL IMPKPOUT( NIXR,IXR,NFT,NEL,16,KC11   ,KC12 ,  KC22 )
              ELSE
                  IETY=16
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=IGTYP
              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==7)THEN
              IOFC = 0
              IF(NG/=NGROUP)THEN
                  IAD2 = IPARG(4,NG+1) - 6 * NEL - 27 * ISUB * NEL
              ELSE
                  IAD2 = LBUFEL - 6 * NEL + 1 - 27 * ISUB * NEL
              ENDIF
              NF1 = NFT + 1
              IF(ICNOD==6)THEN
                  IETY=17
                  IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
              ELSE
                  IF (ISH3N >= 30) THEN
                      IETY=18
                      IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=ISH3N
                  ENDIF
                  NUMEL_DRAPE = NUMELTG_DRAPE
                  SEDRAPE = STDRAPE
                  CALL C3KE3 (
     1                JFT    ,JLT    ,NFT    ,IABS(NPT),MTN    ,
     2                ITHK   ,NCYCLE ,
     3                ISTRA  ,IPLA   ,PM     ,GEO    ,IXTG(1,NF1),
     4                ELBUF_TAB(NG),BUFMAT ,OFFSET ,INDXOF ,
     5                ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     6                JHBE   ,THKE(NUMELC+NF1),ISMSTR ,X     ,
     7                IKGEO  ,IPM    ,IGEO   ,IEXPAN  ,IPARG(1,NG),
     8                ISUBSTACK , STACK     , DRAPE_SH3N, DRAPEG%INDX_SH3N,
     9                SEDRAPE, NUMEL_DRAPE )

              ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(ITY==50)THEN
              IETY=19
              IF (IPRMES_EL(IETY) == 0 ) IPRMES_EL(IETY)=1
          ENDIF
  250     CONTINUE
      END DO
!$OMP END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
