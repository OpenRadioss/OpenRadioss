Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    r2r_split              ../starter/source/coupling/rad2rad/r2r_split.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    constit                ../starter/source/elements/nodes/constit.F
!||    nlocal                 ../starter/source/spmd/node/ddtools.F
!||    prelecsec              ../starter/source/tools/sect/prelecsec.F
!||    r2r_monvol             ../starter/source/coupling/rad2rad/r2r_prelec.F
!||    tagelem_r2r            ../starter/source/coupling/rad2rad/tagelem_r2r.F
!||--- uses       -----------------------------------------------------
!||    front_mod              ../starter/share/modules1/front_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    nod2el_mod             ../starter/share/modules1/nod2el_mod.F
!||    r2r_mod                ../starter/share/modules1/r2r_mod.F
!||    restmod                ../starter/share/modules1/restart_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE R2R_SPLIT(
     1           NB_LINE,
     2           NB_SURF,FLAG,EANI2,BUF_NOD,IXR_KJ,
     3           INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4           SUBSET ,IGRSURF,IGRNOD  ,IGRBRIC,IGRQUAD,
     5           IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6           IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN,IWORKSH,
     7           SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MY_ALLOC_MOD
        USE RESTMOD
        USE R2R_MOD
        USE NOD2EL_MOD
        USE FRONT_MOD
        USE MESSAGE_MOD
        USE GROUPDEF_MOD
        USE SUBMODEL_MOD
        USE ALE_MOD
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY
        use element_mod , only : nixs,nixq,nixc,nixt,nixp,nixr,nixtg
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "tabsiz_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
        INTEGER BUF_NOD(*),
     .     NB_SURF,FLAG,
     .     EANI2(*),NB_LINE,
     .     IXR_KJ(*),INOM_OPT(*),RESERVEP(*),NALE_R2R(ALE%GLOBAL%SNALE),
     .     NSPCOND0,RBY_MSN(2,*)
        INTEGER, INTENT(INOUT) ::  IWORKSH(3,NUMELC+NUMELTG)
        INTEGER ,INTENT(IN) :: NB_SEATBELT_SHELLS
        INTEGER ,INTENT(IN) :: SEATBELT_SHELL_TO_SPRING(NUMELC,2)
        TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
        TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
        TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
        TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
        TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
        TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
        TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
        TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
        TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
        TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER  NLOCAL
        EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, K, J, ADD, CUR_ID, CCPL
        INTEGER COMPT, NSEG, IDOM, NUMNOD_OLD
        INTEGER NTOT, NB_NOD_SUB, NB_NOD_CPL
        INTEGER MAXN, MAXANUS, SIXSN, NUL, TAG, COMPTB
        INTEGER COMPT10,COMPT20,COMPT16,COMPT8,J10,J20,J16,JJ
        INTEGER NEW_ID, SIPART0, SIPARTTH, COMPT_IP, COMPT_IP_TMP, L0
        INTEGER ID_PROP,COEFF,NUMSPHA,NSPHRESN,FIRST_CELL,NOD_ID,PART_RES,INOD
        INTEGER JJB,IUN,NRB,NRBODY_OLD
        CHARACTER TITR*40
        INTEGER, DIMENSION(:), POINTER :: PART1,PART2
        INTEGER, DIMENSION(:), ALLOCATABLE :: BUF_TEMP,ITAB_TEMP,IX_TEMP
        INTEGER, DIMENSION(:), ALLOCATABLE :: CORESN,CORESC,CORESTG,COREST
        INTEGER, DIMENSION(:), ALLOCATABLE :: CORESPA,CORESR,CORESP,CORESS,CORESSP
        INTEGER, DIMENSION(:), ALLOCATABLE :: IPART_TEMP,IWA_TEMP
        INTEGER, DIMENSION(:), ALLOCATABLE :: IPM_TEMP,IGEO_TEMP,CORESMA
        INTEGER, DIMENSION(:), ALLOCATABLE :: CORESPRO,TAGNO_TEMP
        INTEGER, DIMENSION(:), ALLOCATABLE :: IX10_TEMP,IX20_TEMP
        INTEGER, DIMENSION(:), ALLOCATABLE :: IX16_TEMP,CORESQ,ITAB_SUP
        INTEGER, DIMENSION(:), ALLOCATABLE :: KXSP_TEMP,RES_TEMP,NALE_R2R_TEMP
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: RBY_MSN_TEMP,IWORKSH_TEMP
        my_real, DIMENSION(:,:), ALLOCATABLE :: X_TEMP
        my_real, DIMENSION(:), ALLOCATABLE :: THK_TMP,PM_TEMP
        my_real, DIMENSION(:), ALLOCATABLE :: EANI_TEMP,GEO_TEMP
        CHARACTER MESS*40
        CHARACTER(LEN=NCHARKEY) :: OPT,KEY
        DATA MESS/'MULTIDOMAIN INITIALIZATION'/
        DATA IUN/1/
C===================================================================================

C----------------------------------------------------------------------------------C
C--------------------------------Initialisation------------------------------------C
C----------------------------------------------------------------------------------C

        N_PART = NPART

C----------------------------------------------------------------------------------C
C-----------------------First pass -> tag and count--------------------------------C
C----------------------------------------------------------------------------------C

        IF (FLAG==0) THEN

C----------------------------------------------------------------------------------C
C---------------------Allocation and initialisation of array tags------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(TAG_MAT, NUMMAT, "TAG_MAT")
          call my_alloc(TAG_PROP, NUMGEO, "TAG_PROP")
          call my_alloc(TAG_SUBS, NSUBS, "TAG_SUBS")
          call my_alloc(TAG_SURF, NUMELC+NUMELTG+NUMELS+NPART, "TAG_SURF")

          TAG_SURF(:) = 0
          TAG_SUBS(:) = 0
          TAG_PART(:) = 0

C------------Temporarily all mat and properties are kept --------------------------C
          TAG_MAT(:) = 1
          TAG_PROP(:) = 1

C------------TAG_EL must be stored-------------------------------------------------C
          call my_alloc(TAG_ELCF2, NUMELC, "TAG_ELCF2")
          call my_alloc(TAG_ELSF2, NUMELS, "TAG_ELSF2")
          TAG_ELCF2 = 0
          TAG_ELSF2 = 0
          DO I=1,NUMELS
            TAG_ELSF2(I) = TAG_ELS(I+NPART)
          END DO
          DO I=1,NUMELC
            TAG_ELCF2(I) = TAG_ELC(I+NPART)
          END DO

C----------------------------------------------------------------------------------C
C-----------------------------tag of data of the subdomain-------------------------C
C----------------------------------------------------------------------------------C

          IF (IDDOM/=0) THEN
            ADD = ISUBDOM(3,IDDOM)
            DO K=1,NPART
              DO I=1,ISUBDOM(1,IDDOM)
                IF(K == ISUBDOM_PART(I+ADD))THEN
                  TAG_PART(K)=1
                ENDIF
              ENDDO
            END DO
          ENDIF

C----------------Full domain --> tag of parts of all subdomains--------------------C

          IF (IDDOM == 0) THEN

            DO IDOM=1,NSUBDOM
              ADD = ISUBDOM(3,IDOM)
              DO K=1,NPART
                DO I=1,ISUBDOM(1,IDOM)
                  IF(K == ISUBDOM_PART(I+ADD))THEN
                    TAG_PART(K)=1
                  ENDIF
                ENDDO
              END DO
            END DO

C-        ----->Full domain --> inversion of tag of parts<---
            DO K=1,NPART
              IF(TAG_PART(K) == 1) THEN
                TAG_PART(K)=0
              ELSE
                TAG_PART(K)=1
              ENDIF
            END DO

          ENDIF

C---------------Tag of subsets-----------------------------------------------------C
!
C     --- TAG_SUBS  ---> ATTENTION not used
!
!        DO J=1,NSUBS
!    IAD3 = ISUBS(LISUB1*(J-1)+9)
!          DO K=1,ISUBS(LISUB1*(J-1)+8)
!      IF (TAG_PART(BUF_NOD(IAD3))>0) TAG_SUBS(J)=1
!      IAD3 = IAD3+1
!    END DO
!        END DO
!---
          DO J=1,NSUBS
            DO K=1,SUBSET(J)%NTPART
              IF (TAG_PART(SUBSET(J)%TPART(K))>0) TAG_SUBS(J)=1
!
            ENDDO
          ENDDO
!---

C---------------Tag of parts for tag of elements-----------------------------------C
          DO K=1,NPART
            IF(TAG_PART(K) == 1) THEN
              TAG_ELS(K)=1
              TAG_ELQ(K)=1
              TAG_ELC(K)=1
              TAG_ELT(K)=1
              TAG_ELP(K)=1
              TAG_ELR(K)=1
              TAG_ELG(K)=1
              TAG_ELSP(K)=1
              TAG_SURF(K)=1
C---------------Tag of materials and properties------------------------------------C
              TAG_MAT(IPART(LIPART1*(K-1)+1))=1
              TAG_PROP(IPART(LIPART1*(K-1)+2))=1
            ENDIF
          END DO

C---------------Nodes are already taged in TAGNO-----------------------------------C

C---------------Tag of elements----------------------------------------------------C

          SIPART0 = LIPART1*NPART+LIPART1*NTHPART
          SIPARTTH= 2*9*NPART+2*9*NTHPART
          L0 =SIPARTTH+SIPART0+1

          IF (NUMELS>0) CALL TAGELEM_R2R(NUMELS,IPART(L0),TAG_ELS,NPART)
          L0 = L0+NUMELS

          IF (NUMELQ>0) CALL TAGELEM_R2R(NUMELQ,IPART(L0),TAG_ELQ,NPART)
          L0 = L0+NUMELQ

          IF (NUMELC>0) CALL TAGELEM_R2R(NUMELC,IPART(L0),TAG_ELC,NPART)
          L0 = L0+NUMELC

          IF (NUMELT>0) CALL TAGELEM_R2R(NUMELT,IPART(L0),TAG_ELT,NPART)
          L0 = L0+NUMELT

          IF (NUMELP>0) CALL TAGELEM_R2R(NUMELP,IPART(L0),TAG_ELP,NPART)
          L0 = L0+NUMELP

          IF (NUMELR>0) CALL TAGELEM_R2R(NUMELR,IPART(L0),TAG_ELR,NPART)
          L0 = L0+NUMELR

          IF (NUMELTG>0)CALL TAGELEM_R2R(NUMELTG,IPART(L0),TAG_ELG,NPART)
          L0 = L0+NUMELTG+NUMELX

          IF (NUMSPH>0) CALL TAGELEM_R2R(NUMSPH,IPART(L0),TAG_ELSP,NPART)

C---------------Tag of injectors and corresponding material and property-----------C
          CALL R2R_MONVOL(TAG_MAT,TAG_PROP,IGRSURF,LSUBMODEL)

C----------------------------------------------------------------------------------C
C--------------------------First pass -> counting----------------------------------C
C----------------------------------------------------------------------------------C

          COMPT = 0
          NNODN = 0
          NODSUPR = 0
          NSPHN = 0
          NELCN = 0
          NELTGN = 0
          NELTN = 0
          NELRN = 0
          NELPN = 0
          NELQN = 0
          NELSN = 0
          NELS10N = 0
          NELS20N = 0
          NELS16N = 0
          NINLETN = 0
          SIZ_IPM_NEW = NPROPMI
          SIZ_PM_NEW = NPROPM
          SIZ_IGEO_NEW = NPROPGI
          SIZ_GEO_NEW = NPROPG

C ---------> counting of materials  -------------------------------------------C
          DO J=1,NUMMAT
            IF (TAG_MAT(J)/=0)THEN
              SIZ_IPM_NEW = SIZ_IPM_NEW + NPROPMI
              SIZ_PM_NEW = SIZ_PM_NEW + NPROPM
            ENDIF
          ENDDO
C ---------> counting of properties  -------------------------------------------C
          DO J=1,NUMGEO
            IF (TAG_PROP(J)/=0)THEN
              SIZ_IGEO_NEW = SIZ_IGEO_NEW + NPROPGI
              SIZ_GEO_NEW = SIZ_GEO_NEW + NPROPG
            ENDIF
          ENDDO
C ---------> counting of nodes and deleted nodes-----------------------------------C
          DO J=1,NUMNOD
            IF (TAGNO(J+NPART)>=0)THEN
              NNODN = NNODN+1
            ELSE
              NODSUPR = NODSUPR+1
            ENDIF
          ENDDO
C ---------> counting of parts  ---------------------------------------------------C
          DO J=1,NPART
            IF (TAG_PART(J)==1) THEN
              NPARN = NPARN+1
            ENDIF
          ENDDO
C ---------> DUmmy nodes  ---------------------------------------------------C
          NNODN = NNODN+1
C ---------> counting of shells  ---------------------------------------------------C
          DO J=1,NUMELC
            IF (TAG_ELC(J+NPART)/=0) THEN
              NELCN = NELCN+1
            ENDIF
          ENDDO
C ---------> counting of sh3n  ---------------------------------------------------C
          DO J=1,NUMELTG
            IF (TAG_ELG(J+NPART)/=0) THEN
              NELTGN = NELTGN+1
            ENDIF
          ENDDO
C ---------> counting of truss  ---------------------------------------------------C
          DO J=1,NUMELT
            IF (TAG_ELT(J+NPART)/=0) THEN
              NELTN = NELTN+1
            ENDIF
          ENDDO
C ---------> counting of springs---------------------------------------------------C
          DO J=1,NUMELR
            IF (TAG_ELR(J+NPART)/=0) THEN
              NELRN = NELRN+1
            ENDIF
          ENDDO
C ---------> counting of beams----------------------------------------------------C
          DO J=1,NUMELP
            IF (TAG_ELP(J+NPART)/=0) THEN
              NELPN = NELPN+1
            ENDIF
          ENDDO
C ---------> counting of beams  ---------------------------------------------------C
          DO J=1,NUMELQ
            IF (TAG_ELQ(J+NPART)/=0) THEN
              NELQN = NELQN+1
            ENDIF
          ENDDO
C ---------> counting of solids  ---------------------------------------------------C
          DO J=1,NUMELS
            IF (TAG_ELS(J+NPART)/=0) THEN
              NELSN = NELSN+1
              IF (EANI2(J)==10) NELS10N = NELS10N+1
              IF (EANI2(J)==20) NELS20N = NELS20N+1
              IF (EANI2(J)==16) NELS16N = NELS16N+1
            ENDIF
          ENDDO
C ---------> counting of SPH particles---------------------------------------------C
          DO J=1,NUMSPH
            IF (TAG_ELSP(J+NPART)/=0) THEN
              NSPHN = NSPHN+1
            ENDIF
          ENDDO
C ---------> counting of particles reserve-----------------------------------------C
          FIRST_CELL = FIRST_SPHRES
          DO J=1,NBPARTINLET
            IF (TAG_ELSP(FIRST_CELL+NPART)/=0) THEN
              NINLETN = NINLETN + 1
            ENDIF
            FIRST_CELL = FIRST_CELL + RESERVEP(J)
          ENDDO

C----------------------------------------------------------------------------------C
        ENDIF ! IF (FLAG==0) THEN

C----------------------------------------------------------------------------------C
C-------------------------Second pass -> split of arrays --------------------------C
C----------------------------------------------------------------------------------C

        IF (FLAG==1) THEN

C----------------------------------------------------------------------------------C
C-------------Split of materials --------------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IPM_TEMP, NPROPMI*NUMMAT, "IPM_TEMP")
          call my_alloc(PM_TEMP, NPROPM*NUMMAT, "PM_TEMP")
          call my_alloc(CORESMA, NUMMAT, "CORESMA")
          DO I=1,NUMMAT
            DO J=1,NPROPMI
              IPM_TEMP(NPROPMI*(I-1)+J)=IPM(NPROPMI*(I-1)+J)
            END DO
          END DO
          DO I=1,NUMMAT
            DO J=1,NPROPM
              PM_TEMP(NPROPM*(I-1)+J)=PM(NPROPM*(I-1)+J)
            END DO
          END DO
          CALL MY_DEALLOC(IPM)
          CALL MY_DEALLOC(PM)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IPM, SIZ_IPM_NEW, "IPM")
          call my_alloc(PM, SIZ_PM_NEW, "PM")
          COMPT = 0
          DO J=1,NUMMAT
            IF ((TAG_MAT(J)/=0).OR.(J==NUMMAT)) THEN
              COMPT = COMPT+1
              CORESMA(J)=COMPT
              DO K=1,NPROPMI
                IPM(NPROPMI*(COMPT-1)+K)=IPM_TEMP(NPROPMI*(J-1)+K)
              END DO
              DO K=1,NPROPM
                PM(NPROPM*(COMPT-1)+K)=PM_TEMP(NPROPM*(J-1)+K)
              END DO
            ENDIF
          ENDDO

          NUMMAT = COMPT
          CALL MY_DEALLOC(IPM_TEMP)
          CALL MY_DEALLOC(PM_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of properties -------------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IGEO_TEMP, NPROPGI*NUMGEO, "IGEO_TEMP")
          call my_alloc(GEO_TEMP, NPROPG*NUMGEO, "GEO_TEMP")
          call my_alloc(CORESPRO, NUMGEO, "CORESPRO")
          DO I=1,NUMGEO
            DO J=1,NPROPGI
              IGEO_TEMP(NPROPGI*(I-1)+J)=IGEO(NPROPGI*(I-1)+J)
            END DO
          END DO
          DO I=1,NUMGEO
            DO J=1,NPROPG
              GEO_TEMP(NPROPG*(I-1)+J)=GEO(NPROPG*(I-1)+J)
            END DO
          END DO
          CALL MY_DEALLOC(IGEO)
          CALL MY_DEALLOC(GEO)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IGEO, SIZ_IGEO_NEW, "IGEO")
          call my_alloc(GEO, SIZ_GEO_NEW, "GEO")
          COMPT = 0
          MAXANUS = 0
          DO J=1,NUMGEO
            IF (TAG_PROP(J)/=0) THEN
              COMPT = COMPT+1
              CORESPRO(J)=COMPT
              DO K=1,NPROPGI
                MAXANUS = NPROPGI*(COMPT-1)+K
                IGEO(NPROPGI*(COMPT-1)+K)=IGEO_TEMP(NPROPGI*(J-1)+K)
              END DO
              DO K=1,NPROPG
                GEO(NPROPG*(COMPT-1)+K)=GEO_TEMP(NPROPG*(J-1)+K)
              END DO
            ENDIF
          ENDDO

          NUMGEO = COMPT
          CALL MY_DEALLOC(IGEO_TEMP)
          CALL MY_DEALLOC(GEO_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of PARTS-------------------------------------------------------C
C----------------------------------------------------------------------------------C

          DO J=1,NPART
            IPART(LIPART1*(J-1)+1)=CORESMA(IPART(LIPART1*(J-1)+1))
            IPART(LIPART1*(J-1)+2)=CORESPRO(IPART(LIPART1*(J-1)+2))
          ENDDO

C----------------------------------------------------------------------------------C
C-------------Split of nodes-------------------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(CORESN, NUMNOD, "CORESN")
          call my_alloc(X_TEMP, 3, NUMNOD, "X_TEMP")
          call my_alloc(ITAB_TEMP, NUMNOD, "ITAB_TEMP")
          DO J=1,NUMNOD
            ITAB_TEMP(J)=ITAB(J)
            X_TEMP(1,J)=X(3*(J-1)+1)
            X_TEMP(2,J)=X(3*(J-1)+2)
            X_TEMP(3,J)=X(3*(J-1)+3)
          END DO
          CALL MY_DEALLOC(ITAB)
          CALL MY_DEALLOC(X)

C-------------Split----------------------------------------------------------------C
          call my_alloc(ITAB, NNODN, "ITAB")
          call my_alloc(X, 3*NNODN, "X")
          call my_alloc(ITAB_SUP, NODSUPR, "ITAB_SUP")
          call my_alloc(FRONT_R2R, NNODN, "FRONT_R2R")
          call my_alloc(FLAGKIN_R2R, NNODN, "FLAGKIN_R2R")
          FLAGKIN_R2R(1:NNODN)=0
          FRONT_R2R(1:NNODN)=0
          COMPT = 0
          COMPTB = 0
          MAXN=0
          DO J=1,NUMNOD
            IF (TAGNO(J+NPART)>=0)THEN
              COMPT = COMPT+1
              ITAB(COMPT)=ITAB_TEMP(J)
              IF (ITAB(COMPT)>MAXN) MAXN = ITAB(COMPT)
              CORESN(J)=COMPT
              X(3*(COMPT-1)+1)=X_TEMP(1,J)
              X(3*(COMPT-1)+2)=X_TEMP(2,J)
              X(3*(COMPT-1)+3)=X_TEMP(3,J)
c     FRONT_R2R(COMPT)=FRONT(J,1)
              FRONT_R2R(COMPT) = NLOCAL(J,1)
              IF(FLAGKIN(J)==1)THEN
                FLAGKIN_R2R(COMPT)=1
              ENDIF
c     ELSE
c       FRONT_R2R(COMPT) = 0
c     ENDIF
              IF (TAGNO(J+NPART)>1) THEN
                MS(COMPT)=1e-20
                IF (IRODDL==1) IN(COMPT)=1e-20
              ENDIF
            ELSE
              COMPTB = COMPTB+1
              ITAB_SUP(COMPTB)=ITAB_TEMP(J)
            ENDIF
          ENDDO

C -----------update of skews-------------------------------------------------------C
          DO J=1,NUMSKW
            DO K=1,3
              IF (ISKWN(LISKN*J+K)>0)
     .              ISKWN(LISKN*J+K)=CORESN(ISKWN(LISKN*J+K))
            END DO
          ENDDO

C -----------update of frames------------------------------------------------------C
          JJ = SISKWN-SIFRAME
          IF (NSPHN==NUMSPH) THEN
            DO J=1,NUMFRAM
              DO K=1,3
                IF (ISKWN(JJ+LISKN*J+K)>0)
     .                ISKWN(JJ+LISKN*J+K)=CORESN(ISKWN(JJ+LISKN*J+K))
              END DO
            ENDDO
          ELSE
C--         advance of fraomes if SPH particles are removed
            JJB = SISKWN-SIFRAME-MIN(IUN,NSPCOND0)*(NUMSPH-NSPHN)*LISKN
            DO J=1,NUMFRAM
              DO K=1,3
                IF (ISKWN(JJ+LISKN*J+K)>0) THEN
                  ISKWN(JJB+LISKN*J+K)=CORESN(ISKWN(JJ+LISKN*J+K))
                ENDIF
              END DO
              DO K=4,LISKN
                ISKWN(JJB+LISKN*J+K)=ISKWN(JJ+LISKN*J+K)
              END DO
            END DO
          ENDIF

          NUMNOD_OLD = NUMNOD
          NUMNOD = COMPT
          NUMNOD0 = COMPT
          CALL MY_DEALLOC(ITAB_TEMP)
          CALL MY_DEALLOC(X_TEMP)

C-------------Reconstitution of ITABM1---------------------------------------------C
          PART1  => ITABM1(1:2*NUMNOD)
          CALL CONSTIT(ITAB,PART1,NUMNOD)

C-------------A second list of removed nodes is generated for error detection
          IF (NODSUPR/=0) THEN
            PART2  => ITABM1(2*NUMNOD+1:2*NUMNOD_OLD)
            CALL CONSTIT(ITAB_SUP,PART2,NODSUPR)
            CALL MY_DEALLOC(ITAB_SUP)
          ENDIF

C----------------------------------------------------------------------------------C
C-------------Split of NALE_R2R----------------------------------------------------C
C----------------------------------------------------------------------------------C

          IF (ALE%GLOBAL%SNALE>0) THEN
C
            call my_alloc(NALE_R2R_TEMP, ALE%GLOBAL%SNALE, "NALE_R2R_TEMP")
            DO J=1,NUMNOD_OLD
              NALE_R2R_TEMP(J)=NALE_R2R(J)
            END DO

C-------------Split----------------------------------------------------------------C
            NALE_R2R(:) = 0
            COMPT = 0
            DO J=1,NUMNOD_OLD
              IF (TAGNO(J+NPART)>=0) THEN
                COMPT = COMPT+1
                NALE_R2R(COMPT) =  NALE_R2R_TEMP(J)
              ENDIF
            END DO
            CALL MY_DEALLOC(NALE_R2R_TEMP)

          ENDIF

C----------------------------------------------------------------------------------C
C-------------Split of EANI--------------------------------------------------------C
C----------------------------------------------------------------------------------C

          NTOT = NELSN+NELCN+NELTGN+NELQN
          call my_alloc(EANI_TEMP, SEANI, "EANI_TEMP")
          COMPT = 0

          DO J=1,SEANI
            EANI_TEMP(J)=EANI2(J)
            EANI2(J)=0
          END DO

          SEANI = NTOT

          DO J=1,NUMELS
            IF (TAG_ELS(J+NPART)/=0) THEN
              COMPT = COMPT + 1
              EANI2(COMPT)=EANI_TEMP(J)
            ENDIF
          END DO

          COMPT = NELSN+NELCN+NELQN
          DO J=1,NUMELTG
            IF (TAG_ELG(J+NPART)/=0) THEN
              COMPT = COMPT + 1
              EANI2(COMPT)=EANI_TEMP(NUMELS+NUMELQ+NUMELC+J)
            ENDIF
          END DO

C----------------------------------------------------------------------------------C
C-------------INITIALIZATION OF IPART----------------------------------------------C
C----------------------------------------------------------------------------------C

          SIPART0 = LIPART1*NPART+LIPART1*NTHPART
          SIPARTTH= 2*9*NPART+2*9*NTHPART
          call my_alloc(IPART_TEMP, SIPART, "IPART_TEMP")

          DO J=1,SIPART
            IPART_TEMP(J)=IPART(J)
          END DO

          CALL MY_DEALLOC(IPART)
          SIPART  = SIPART0+SIPARTTH+NELSN+NELQN+NELCN+NELTN+NELPN
     .        + NELRN+NELTGN+NUMELX+NUMSPH
          call my_alloc(IPART, SIPART, "IPART")

          DO J=1,SIPART0+SIPARTTH
            IPART(J)=IPART_TEMP(J)
          END DO

          COMPT_IP = SIPART0+SIPARTTH
          COMPT_IP_TMP = SIPART0+SIPARTTH

C----------------------------------------------------------------------------------C
C-------------Split of SOLID elements----------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, SIXS, "IX_TEMP")
          call my_alloc(CORESS, NUMELS, "CORESS")

          DO J=1,NUMELS
            DO K=1,NIXS
              IX_TEMP(NIXS*(J-1)+K)=IXS(NIXS*(J-1)+K)
            END DO
          END DO

C-------------Storage of additional terms IXS10,IXS20,IXS16------------------------C

          DO J=NUMELS+1,SIXS
            IX_TEMP(J)=IXS(J)
          END DO

          CALL MY_DEALLOC(IXS)

C-------------Split----------------------------------------------------------------C

          SIXSN = NELSN*NIXS+NELS10N*6+NELS20N*12+NELS16N*8
          call my_alloc(IXS, SIXSN, "IXS")
          call my_alloc(TAG_ELSF, NELSN, "TAG_ELSF")
          TAG_ELSF = 0
          COMPT = 0
          COMPT8 = 0
          COMPT10 = 0
          COMPT20 = 0
          COMPT16 = 0
          J10 = 0
          J20 = 0
          J16 = 0

          DO J=1,NUMELS
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (EANI_TEMP(J)==10) J10 = J10+1
            IF (EANI_TEMP(J)==20) J20 = J20+1
            IF (EANI_TEMP(J)==16) J16 = J16+1
            IF (TAG_ELS(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESS(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXS
                IXS(NIXS*(COMPT-1)+K)=IX_TEMP(NIXS*(J-1)+K)
              END DO
              IXS(NIXS*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXS*(J-1)+1))
              IXS(NIXS*(COMPT-1)+10)=CORESPRO(IX_TEMP(NIXS*(J-1)+10))
              DO K=2,9
                IXS(NIXS*(COMPT-1)+K)=CORESN(IX_TEMP(NIXS*(J-1)+K))
              END DO
              IF (TAG_ELSF2(J)>1) TAG_ELSF(COMPT) = 1
              IF (EANI_TEMP(J)==10) THEN
                COMPT10 = COMPT10+1
                DO K=1,6
                  IXS(NIXS*NELSN+6*(COMPT10-1)+K)=
     .                          CORESN(IX_TEMP(NIXS*NUMELS+6*(J10-1)+K))
                END DO
              ELSEIF (EANI_TEMP(J)==20) THEN
                COMPT20 = COMPT20+1
                DO K=1,12
                  IXS((NIXS*NELSN+6*NELS10N)+12*(COMPT20-1)+K)=
     .                             CORESN(IX_TEMP((NIXS*NUMELS+6*NUMELS10)+
     .                             12*(J20-1)+K))
                END DO
              ELSEIF (EANI_TEMP(J)==16) THEN
                COMPT16 = COMPT16+1
                DO K=1,8
                  IXS((NIXS*NELSN+6*NELS10N+12*NELS20N)+8*(COMPT16-1)+K)=
     .                             CORESN(IX_TEMP((NIXS*NUMELS+6*NUMELS10+
     .                             12*NUMELS20)+8*(J16-1)+K))
                END DO
              ELSE
                COMPT8 = COMPT8+1
              ENDIF

            ENDIF
          ENDDO

          NUMELS8 = COMPT8
          NUMELS10 = COMPT10
          NUMELS20 = COMPT20
          NUMELS16 = COMPT16
          NUMELS = COMPT

          CALL MY_DEALLOC(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------Split of QUAD elements-----------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, NUMELQ*NIXQ, "IX_TEMP")
          call my_alloc(CORESQ, NUMELQ, "CORESQ")
          DO J=1,NUMELQ
            DO K=1,NIXQ
              IX_TEMP(NIXQ*(J-1)+K)=IXQ(NIXQ*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXQ)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IXQ, NELQN*NIXQ, "IXQ")
          COMPT = 0
          DO J=1,NUMELQ
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELQ(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESQ(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXQ
                IXQ(NIXQ*(COMPT-1)+K)=IX_TEMP(NIXQ*(J-1)+K)
              END DO
              IXQ(NIXQ*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXQ*(J-1)+1))
              IXQ(NIXQ*(COMPT-1)+6)=CORESPRO(IX_TEMP(NIXQ*(J-1)+6))
              DO K=2,5
                IXQ(NIXQ*(COMPT-1)+K)=CORESN(IX_TEMP(NIXQ*(J-1)+K))
              END DO
            ENDIF
          ENDDO

          NUMELQ = COMPT
          CALL MY_DEALLOC(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of SHELL elements----------------------------------------------C
C----------------------------------------------------------------------------------C

          NUMELC0 = NUMELC
          call my_alloc(IX_TEMP, NUMELC*NIXC, "IX_TEMP")
          call my_alloc(CORESC, NUMELC, "CORESC")
          CALL MY_ALLOC(IWORKSH_TEMP,3,NUMELC,"IWORKSH_TEMP")
          DO J=1,NUMELC
            DO K=1,NIXC
              IX_TEMP(NIXC*(J-1)+K)=IXC(NIXC*(J-1)+K)
            END DO
            IWORKSH_TEMP(1,J) = IWORKSH(1,J)
            IWORKSH_TEMP(2,J) = IWORKSH(2,J)
            IWORKSH_TEMP(3,J) = IWORKSH(3,J)
          END DO
          CALL MY_DEALLOC(IXC)

C-------------Split----------------------------------------------------------------C
          call my_alloc(IXC, NELCN*NIXC, "IXC")
          call my_alloc(TAG_ELCF, NELCN, "TAG_ELCF")
          TAG_ELCF = 0
          COMPT = 0
          DO J=1,NUMELC
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELC(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESC(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXC
                IXC(NIXC*(COMPT-1)+K)=IX_TEMP(NIXC*(J-1)+K)
              END DO
              IXC(NIXC*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXC*(J-1)+1))
              IXC(NIXC*(COMPT-1)+6)=CORESPRO(IX_TEMP(NIXC*(J-1)+6))
              DO K=2,5
                IXC(NIXC*(COMPT-1)+K)=CORESN(IX_TEMP(NIXC*(J-1)+K))
              END DO
              IF (TAG_ELCF2(J) > 1) TAG_ELCF(COMPT) = 1
              IF (TAG_PART(IPART(COMPT_IP)) /= 0) THEN
                IWORKSH(1,COMPT)=IWORKSH_TEMP(1,J)
                IWORKSH(2,COMPT)=IWORKSH_TEMP(2,J)
                IWORKSH(3,COMPT)=IWORKSH_TEMP(3,J)
              ELSE
                IWORKSH(1,COMPT)=ZERO
                IWORKSH(2,COMPT)=ZERO
                IWORKSH(3,COMPT)=ZERO
              ENDIF
            ENDIF
          ENDDO

          NUMELC = COMPT
          CALL MY_DEALLOC(IX_TEMP)
          CALL MY_DEALLOC(IWORKSH_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of TRUSS elements----------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, NUMELT*NIXT, "IX_TEMP")
          call my_alloc(COREST, NUMELT, "COREST")
          DO J=1,NUMELT
            DO K=1,NIXT
              IX_TEMP(NIXT*(J-1)+K)=IXT(NIXT*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXT)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IXT, NELTN*NIXT, "IXT")
          COMPT = 0
          DO J=1,NUMELT
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELT(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              COREST(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXT
                IXT(NIXT*(COMPT-1)+K)=IX_TEMP(NIXT*(J-1)+K)
              END DO
              IXT(NIXT*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXT*(J-1)+1))
              IXT(NIXT*(COMPT-1)+4)=CORESPRO(IX_TEMP(NIXT*(J-1)+4))
              DO K=2,3
                IXT(NIXT*(COMPT-1)+K)=CORESN(IX_TEMP(NIXT*(J-1)+K))
              END DO
            ENDIF
          ENDDO

          NUMELT = COMPT
          CALL MY_DEALLOC(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------Split of BEAM elements-----------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, NUMELP*NIXP, "IX_TEMP")
          call my_alloc(CORESP, NUMELP, "CORESP")
          DO J=1,NUMELP
            DO K=1,NIXP
              IX_TEMP(NIXP*(J-1)+K)=IXP(NIXP*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXP)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IXP, NELPN*NIXP, "IXP")
          COMPT = 0
          DO J=1,NUMELP
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELP(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESP(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXP
                IXP(NIXP*(COMPT-1)+K)=IX_TEMP(NIXP*(J-1)+K)
              END DO
              IXP(NIXP*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXP*(J-1)+1))
              IXP(NIXP*(COMPT-1)+5)=CORESPRO(IX_TEMP(NIXP*(J-1)+5))
              DO K=2,4
                IXP(NIXP*(COMPT-1)+K)=CORESN(IX_TEMP(NIXP*(J-1)+K))
              END DO
            ENDIF
          ENDDO

          NUMELP = COMPT
          CALL MY_DEALLOC(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------Split of SPRINGS and KJOINTS-----------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, NUMELR*5+1, "IX_TEMP")
          DO J=1,NUMELR
            DO K=1,5
              IX_TEMP(5*(J-1)+K)=IXR_KJ(5*(J-1)+K)
              IXR_KJ(5*(J-1)+K) = 0
            END DO
          END DO

C-------------Split----------------------------------------------------------------C

          COMPT = 0
          COMPTB = 0
          DO J=1,NUMELR
            ID_PROP = CORESPRO(IXR(NIXR*(J-1)+1))
            IF (IGEO(NPROPGI*(ID_PROP-1)+11)==45) THEN
              COMPTB = COMPTB + 1
              IF (TAG_ELR(J+NPART)/=0) THEN
                COMPT = COMPT + 1
                DO K=1,3
                  IXR_KJ(5*(COMPT-1)+K)=CORESN(IX_TEMP(5*(COMPTB-1)+K))
                END DO
                IXR_KJ(5*(COMPT-1)+4)=IX_TEMP(5*(COMPTB-1)+4)
                IXR_KJ(5*(COMPT-1)+5)=0
              ENDIF
            ENDIF
          ENDDO

          IXR_KJ(5*NELRN+1) = COMPT
          CALL MY_DEALLOC(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of SPRING elements---------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(IX_TEMP, NUMELR*NIXR, "IX_TEMP")
          call my_alloc(CORESR, NUMELR, "CORESR")
          DO J=1,NUMELR
            DO K=1,NIXR
              IX_TEMP(NIXR*(J-1)+K)=IXR(NIXR*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXR)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IXR, NELRN*NIXR, "IXR")
          COMPT = 0
          DO J=1,NUMELR
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELR(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESR(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXR
                IXR(NIXR*(COMPT-1)+K)=IX_TEMP(NIXR*(J-1)+K)
              END DO
              IXR(NIXR*(COMPT-1)+1)=CORESPRO(IX_TEMP(NIXR*(J-1)+1))
              DO K=2,3
                IXR(NIXR*(COMPT-1)+K)=CORESN(IX_TEMP(NIXR*(J-1)+K))
              END DO
              IF (IXR(NIXR*(COMPT-1)+4)/=0) THEN
                IXR(NIXR*(COMPT-1)+4)=CORESN(IX_TEMP(NIXR*(J-1)+4))
              ENDIF
            ENDIF
          ENDDO

          NUMELR = COMPT
          CALL MY_DEALLOC(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of SH3N elements-----------------------------------------------C
C----------------------------------------------------------------------------------C

          NUMELTG0 = NUMELTG
          call my_alloc(IX_TEMP, NUMELTG*NIXTG, "IX_TEMP")
          call my_alloc(CORESTG, NUMELTG, "CORESTG")
          DO J=1,NUMELTG
            DO K=1,NIXTG
              IX_TEMP(NIXTG*(J-1)+K)=IXTG(NIXTG*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXTG)

C-------------Split----------------------------------------------------------------C

          call my_alloc(IXTG, NELTGN*NIXTG, "IXTG")
          COMPT = 0
          DO J=1,NUMELTG
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELG(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESTG(J)=COMPT
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NIXTG
                IXTG(NIXTG*(COMPT-1)+K)=IX_TEMP(NIXTG*(J-1)+K)
              END DO
              IXTG(NIXTG*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXTG*(J-1)+1))
              IXTG(NIXTG*(COMPT-1)+5)=CORESPRO(IX_TEMP(NIXTG*(J-1)+5))
              DO K=2,4
                IXTG(NIXTG*(COMPT-1)+K)=CORESN(IX_TEMP(NIXTG*(J-1)+K))
              END DO
            ENDIF
          ENDDO

          NUMELTG = COMPT
          CALL MY_DEALLOC(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of /SPH/RESERVE------------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(RES_TEMP, NBPARTINLET, "RES_TEMP")
          DO J=1,NBPARTINLET
            RES_TEMP(J) = RESERVEP(J)
          END DO

C-------------Split----------------------------------------------------------------C
          COMPT = 0
          PART_RES = 0
          FIRST_CELL = FIRST_SPHRES
          DO J=1,NBPARTINLET
            IF (TAG_ELSP(FIRST_CELL+NPART)/=0) THEN
              COMPT = COMPT + 1
              RESERVEP(COMPT) = RES_TEMP(J)
            ENDIF
            DO K=1,RES_TEMP(J)
              INOD = KXSP(NISP*(FIRST_CELL-1)+3)
              FIRST_CELL = FIRST_CELL+1
            ENDDO
          ENDDO

          NBPARTINLET = COMPT
          CALL MY_DEALLOC(RES_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of SPH particles-----------------------------------------------C
C----------------------------------------------------------------------------------C

          COEFF = 0
          IF (NSPHN>0) COEFF = 1
          NUMSPHA = NUMSPH - NSPHRES
C
          call my_alloc(KXSP_TEMP, NISP*NUMSPH, "KXSP_TEMP")
          call my_alloc(CORESSP, NUMSPH, "CORESSP")
          DO J=1,NUMSPH
            DO K=1,NISP
              KXSP_TEMP(NISP*(J-1)+K)=KXSP(NISP*(J-1)+K)
            END DO
          END DO
          CALL MY_DEALLOC(IXSP)
          CALL MY_DEALLOC(KXSP)
          CALL MY_DEALLOC(NOD2SP)
          CALL MY_DEALLOC(SPBUF)

C-------------Split----------------------------------------------------------------C
          call my_alloc(IXSP, KVOISPH, NSPHN, "IXSP")
          call my_alloc(KXSP, NISP*NSPHN, "KXSP")
          call my_alloc(NOD2SP, COEFF*NUMNOD, "NOD2SP")
          call my_alloc(SPBUF, NSPBUF*NSPHN, "SPBUF")
          COMPT = 0
          NSPHRESN = 0
          DO J=1,NUMSPH
            COMPT_IP_TMP=COMPT_IP_TMP+1
            IF (TAG_ELSP(J+NPART)/=0) THEN
              COMPT_IP=COMPT_IP+1
              COMPT = COMPT+1
              CORESSP(J)=COMPT
              IF (J>=FIRST_SPHRES) NSPHRESN=NSPHRESN+1
              IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
              DO K=1,NISP
                KXSP(NISP*(COMPT-1)+K)=KXSP_TEMP(NISP*(J-1)+K)
              END DO
              KXSP(NISP*(COMPT-1)+3)=CORESN(KXSP_TEMP(NISP*(J-1)+3))
              NOD2SP(CORESN(KXSP_TEMP(NISP*(J-1)+3))) = COMPT
            ENDIF
          ENDDO
          IF ((COMPT/=0).AND.(COMPT/=NUMSPH)) THEN
            CALL ANCMSG(MSGID=1061,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1)
          ENDIF
          NUMSPH = COMPT
          NUMSPHA = COMPT - NSPHRESN
          NSPHRES =  NSPHRESN
          FIRST_SPHRES = NUMSPHA + 1
          CALL MY_DEALLOC(KXSP_TEMP)

C----------------------------------------------------------------------------------C

          CALL MY_DEALLOC(IPART_TEMP)

C----------------------------------------------------------------------------------C
C-------------Split of THKE for shell and sh3n elements----------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(THK_TMP, NUMELTG0+NUMELC0, "THK_TMP")
          DO J=1,NUMELTG0+NUMELC0
            THK_TMP(J)=THKE(J)
          END DO
          CALL MY_DEALLOC(THKE)

C-------------Split----------------------------------------------------------------C

          call my_alloc(THKE, NUMELTG+NUMELC, "THKE")
          DO J=1,NUMELC0
            IF (TAG_ELC(J+NPART)/=0) THEN
              THKE(CORESC(J))=THK_TMP(J)
            ENDIF
          ENDDO
          DO J=1,NUMELTG0
            IF (TAG_ELG(J+NPART)/=0) THEN
              THKE(CORESTG(J)+NUMELC)=THK_TMP(J+NUMELC0)
            ENDIF
          ENDDO

          CALL MY_DEALLOC(THK_TMP)

C----------------------------------------------------------------------------------C
C-------------Split of GROUPS------------------------------------------------------C
C----------------------------------------------------------------------------------C

!!          ALLOCATE(BUF_TEMP(SIZE))
!!          DO I=1,SIZE
!!            BUF_TEMP(I)=BUF_NOD(I)
!!          END DO

C ---------> groups of elements----------------------------------------------------C
C             --> solids <--
          DO I=1,NGRBRIC
            COMPT = 0
            IGRBRIC(I)%R2R_SHARE = 0
            DO J=1,IGRBRIC(I)%NENTITY
              CUR_ID = IGRBRIC(I)%ENTITY(J)
              IF (TAG_ELS(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRBRIC(I)%ENTITY(COMPT) = CORESS(CUR_ID)
                IF (TAG_ELS(CUR_ID+NPART)>1)
     .               IGRBRIC(I)%R2R_SHARE = IGRBRIC(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRBRIC(I)%R2R_ALL = IGRBRIC(I)%NENTITY
            IGRBRIC(I)%NENTITY = COMPT
          ENDDO
C             --> quads <--
          DO I=1,NGRQUAD
            COMPT = 0
            IGRQUAD(I)%R2R_SHARE = 0
            DO J=1,IGRQUAD(I)%NENTITY
              CUR_ID = IGRQUAD(I)%ENTITY(J)
              IF (TAG_ELQ(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRQUAD(I)%ENTITY(COMPT) = CORESQ(CUR_ID)
                IF (TAG_ELQ(CUR_ID+NPART)>1)
     .               IGRQUAD(I)%R2R_SHARE = IGRQUAD(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRQUAD(I)%R2R_ALL = IGRQUAD(I)%NENTITY
            IGRQUAD(I)%NENTITY = COMPT
          ENDDO
C             --> sh4n <--
          DO I=1,NGRSHEL
            COMPT = 0
            IGRSH4N(I)%R2R_SHARE = 0
            DO J=1,IGRSH4N(I)%NENTITY
              CUR_ID = IGRSH4N(I)%ENTITY(J)
              IF (TAG_ELC(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRSH4N(I)%ENTITY(COMPT) = CORESC(CUR_ID)
                IF (TAG_ELC(CUR_ID+NPART)>1)
     .               IGRSH4N(I)%R2R_SHARE = IGRSH4N(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRSH4N(I)%R2R_ALL = IGRSH4N(I)%NENTITY
            IGRSH4N(I)%NENTITY = COMPT
          ENDDO
C             --> truss <--
          DO I=1,NGRTRUS
            COMPT = 0
            IGRTRUSS(I)%R2R_SHARE = 0
            DO J=1,IGRTRUSS(I)%NENTITY
              CUR_ID = IGRTRUSS(I)%ENTITY(J)
              IF (TAG_ELT(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRTRUSS(I)%ENTITY(COMPT) = COREST(CUR_ID)
                IF (TAG_ELT(CUR_ID+NPART)>1)
     .               IGRTRUSS(I)%R2R_SHARE = IGRTRUSS(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRTRUSS(I)%R2R_ALL = IGRTRUSS(I)%NENTITY
            IGRTRUSS(I)%NENTITY = COMPT
          ENDDO
C             --> beams <--
          DO I=1,NGRBEAM
            COMPT = 0
            IGRBEAM(I)%R2R_SHARE = 0
            DO J=1,IGRBEAM(I)%NENTITY
              CUR_ID = IGRBEAM(I)%ENTITY(J)
              IF (TAG_ELP(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRBEAM(I)%ENTITY(COMPT) = CORESP(CUR_ID)
                IF (TAG_ELP(CUR_ID+NPART)>1)
     .               IGRBEAM(I)%R2R_SHARE = IGRBEAM(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRBEAM(I)%R2R_ALL = IGRBEAM(I)%NENTITY
            IGRBEAM(I)%NENTITY = COMPT
          ENDDO
C             --> springs <--
          DO I=1,NGRSPRI
            COMPT = 0
            IGRSPRING(I)%R2R_SHARE = 0
            DO J=1,IGRSPRING(I)%NENTITY
              CUR_ID = IGRSPRING(I)%ENTITY(J)
              IF (TAG_ELR(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRSPRING(I)%ENTITY(COMPT) = CORESR(CUR_ID)
                IF (TAG_ELR(CUR_ID+NPART)>1)
     .               IGRSPRING(I)%R2R_SHARE = IGRSPRING(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRSPRING(I)%R2R_ALL = IGRSPRING(I)%NENTITY
            IGRSPRING(I)%NENTITY = COMPT
          ENDDO
C             --> sh3n <--
          DO I=1,NGRSH3N
            COMPT = 0
            IGRSH3N(I)%R2R_SHARE = 0
            DO J=1,IGRSH3N(I)%NENTITY
              CUR_ID = IGRSH3N(I)%ENTITY(J)
              IF (TAG_ELG(CUR_ID+NPART)/=0)THEN
                COMPT = COMPT+1
                IGRSH3N(I)%ENTITY(COMPT) = CORESTG(CUR_ID)
                IF (TAG_ELG(CUR_ID+NPART)>1)
     .               IGRSH3N(I)%R2R_SHARE = IGRSH3N(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
            IGRSH3N(I)%R2R_ALL = IGRSH3N(I)%NENTITY
            IGRSH3N(I)%NENTITY = COMPT
          ENDDO

C ---------> groups of parts-----------------------------------------------------C

          DO I=1,NGRPART
            COMPT = 0
            DO J=1,IGRPART(I)%NENTITY
              CUR_ID = IGRPART(I)%ENTITY(J)
              IF (TAG_PART(CUR_ID)==1)THEN
                COMPT = COMPT+1
                IGRPART(I)%ENTITY(COMPT) = CUR_ID
              ENDIF
            ENDDO
            IGRPART(I)%R2R_ALL = IGRPART(I)%NENTITY
            IGRPART(I)%NENTITY = COMPT
          ENDDO
C----------------------------------------------------------------------------------C
C-------------Split of surfaces----------------------------------------------------C
C----------------------------------------------------------------------------------C

          DO I=1,NB_SURF
            NSEG = 0
            CCPL = 0
            DO J=1,IGRSURF(I)%NSEG
              NB_NOD_SUB=0
              NB_NOD_CPL=0
              TAG = 0
              CUR_ID = IGRSURF(I)%ELEM(J)
              IF (IGRSURF(I)%ELTYP(J) == 1) THEN
C---------------> case face of solid <--
                IF (TAG_ELS(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESS(CUR_ID)
                  TAG = 1
                ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) == 2) THEN
C---------------> case quad <--
                IF (TAG_ELQ(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESQ(CUR_ID)
                  TAG = 1
                ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) == 3) THEN
C---------------> case shell <--
                IF (TAG_ELC(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESC(CUR_ID)
                  TAG = 1
                ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) == 7) THEN
C---------------> case sh3n <--
                IF (TAG_ELG(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESTG(CUR_ID)
                  TAG = 1
                ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) > 10) THEN
C---------------> case of surface defined by segments and associated elements <--
                IF (IGRSURF(I)%ELTYP(J) == 11) THEN
                  IF (TAG_ELS(CUR_ID+NPART)/=0) TAG=1
                ELSEIF (IGRSURF(I)%ELTYP(J) == 13) THEN
                  IF (TAG_ELC(CUR_ID+NPART)/=0) TAG=1
                ELSEIF (IGRSURF(I)%ELTYP(J) == 17) THEN
                  IF (TAG_ELG(CUR_ID+NPART)/=0) TAG=1
                ENDIF
C                  ---> resef of type of segment <--
                IGRSURF(I)%ELTYP(J) = 0
                NEW_ID = 0
              ELSEIF (IGRSURF(I)%ELTYP(J) == 0) THEN
C---------------> case of surface defined by segments with nodes <--
                DO K=1,4
                  NOD_ID = IGRSURF(I)%NODES(J,K)
                  IF (TAGNO(NOD_ID+NPART)/=-1) NB_NOD_CPL=NB_NOD_CPL+1
                END DO
                IF (NB_NOD_CPL==4) THEN
                  TAG = 1
                  NEW_ID = 0
                ENDIF
              ENDIF
C---------------> TAG = 1, segment is kept, update of nodes <--
              IF (TAG == 1) THEN
                NSEG = NSEG + 1
                DO K=1,4
                  CUR_ID = IGRSURF(I)%NODES(J,K)
                  IF (TAGNO(CUR_ID+NPART)>1) NB_NOD_CPL=NB_NOD_CPL+1
                  IGRSURF(I)%NODES(NSEG,K) = CORESN(CUR_ID)
                END DO
                IF (NB_NOD_CPL==4) CCPL=CCPL+1
                IGRSURF(I)%ELTYP(NSEG) = IGRSURF(I)%ELTYP(J)
                IGRSURF(I)%ELEM(NSEG) = NEW_ID
              ENDIF
            END DO
            ISURF_R2R(2,I)  = IGRSURF(I)%NSEG
            IGRSURF(I)%NSEG = NSEG
            ISURF_R2R(1,I)  = CCPL
          END DO

C----------------------------------------------------------------------------------C
C-------------Split of Lines-------------------------------------------------------C
C----------------------------------------------------------------------------------C
          DO I=1,NB_LINE
            NSEG = 0
            DO J=1,IGRSLIN(I)%NSEG
              NB_NOD_SUB=0
              TAG = 0
              CUR_ID = IGRSLIN(I)%ELEM(J)
              IF (IGRSLIN(I)%ELTYP(J)==1) THEN
C---------------> case face of solid <--
                IF (TAG_ELS(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESS(CUR_ID)
                  TAG = 1
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==2) THEN
C---------------> case quad <--
                IF (TAG_ELQ(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESQ(CUR_ID)
                  TAG = 2
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==3) THEN
C---------------> case shell <--
                IF (TAG_ELC(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESC(CUR_ID)
                  TAG = 3
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==4) THEN
C---------------> case truss <--
                IF (TAG_ELT(CUR_ID+NPART)/=0) THEN
                  NEW_ID = COREST(CUR_ID)
                  TAG = 4
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==5) THEN
C---------------> case beam <--
                IF (TAG_ELP(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESP(CUR_ID)
                  TAG = 5
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==6) THEN
C---------------> cas spring <--
                IF (TAG_ELR(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESR(CUR_ID)
                  TAG = 6
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==7) THEN
C---------------> cas sh3n <--
                IF (TAG_ELG(CUR_ID+NPART)/=0) THEN
                  NEW_ID = CORESTG(CUR_ID)
                  TAG = 7
                ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==0) THEN
C---------------> No element, count of taged nodes <--
                NEW_ID = 0
                DO K=1,2
                  CUR_ID = IGRSLIN(I)%NODES(J,K)
                  IF (TAGNO(CUR_ID+NPART)>=0) NB_NOD_SUB=NB_NOD_SUB+1
                END DO
                IF (NB_NOD_SUB==2) TAG = 8
              ENDIF
C---------------> TAG = 1, segment is kept, update of nodes <--
              IF (TAG > 0) THEN
                NSEG = NSEG + 1
                DO K=1,2
                  CUR_ID = IGRSLIN(I)%NODES(J,K)
                  IGRSLIN(I)%NODES(NSEG,K) = CORESN(CUR_ID)
                END DO
                IGRSLIN(I)%ELTYP(NSEG) = IGRSLIN(I)%ELTYP(J)
                IGRSLIN(I)%ELEM(NSEG)  = NEW_ID
              ENDIF
            END DO
            IGRSLIN(I)%NSEG_R2R_ALL = IGRSLIN(I)%NSEG
            IGRSLIN(I)%NSEG = NSEG
          END DO

C----------------------------------------------------------------------------------C
C-------------Split of groups of nodes---------------------------------------------C
C----------------------------------------------------------------------------------C

          DO I=1,NGRNOD
! ---------> groups of nodes
            COMPT = 0
            CCPL = 0
            DO J=1,IGRNOD(I)%NENTITY
              CUR_ID = IGRNOD(I)%ENTITY(J)
              IF (TAGNO(CUR_ID+NPART) >= 0) THEN
                COMPT = COMPT + 1
                IGRNOD(I)%ENTITY(COMPT) = CORESN(CUR_ID)
              ENDIF
              IF (TAGNO(CUR_ID+NPART)>1) CCPL=CCPL+1
            ENDDO
            IGRNOD(I)%R2R_ALL   = IGRNOD(I)%NENTITY
            IGRNOD(I)%R2R_SHARE = CCPL
            IGRNOD(I)%NENTITY   = COMPT
          ENDDO ! DO I=1,NGRNOD

C----------------------------------------------------------------------------------C
C--------------Update of TAGNO-----------------------------------------------------C
C----------------------------------------------------------------------------------C

          call my_alloc(TAGNO_TEMP, 2*NUMNOD_OLD+NPART, "TAGNO_TEMP")
          DO J=1,NPART+2*NUMNOD_OLD
            TAGNO_TEMP(J)=TAGNO(J)
          END DO

          CALL MY_DEALLOC(TAGNO)
          call my_alloc(TAGNO, 2*NUMNOD+NPART, "TAGNO")
          DO J=1,NPART
            TAGNO(J)=TAGNO_TEMP(J)
          END DO
          COMPT=0
          DO J=1,NUMNOD_OLD
            IF (TAGNO_TEMP(J+NPART)>=0)THEN
              COMPT=COMPT+1
              TAGNO(COMPT+NPART)=TAGNO_TEMP(J+NPART)
              TAGNO(COMPT+NPART+NUMNOD)=TAGNO_TEMP(J+NPART+NUMNOD_OLD)
            ENDIF
          ENDDO

C----------------------------------------------------------------------------------C
C------Prereading and tag of SECTIONS----------------------------------------------C
C----------------------------------------------------------------------------------C

          CALL PRELECSEC(
     1    NUL,NUL,ITABM1,2,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2    IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N  ,IGRTRUSS,
     3    IGRBEAM ,IGRSPRING ,IGRNOD, LSUBMODEL ,SEATBELT_SHELL_TO_SPRING,
     4    NB_SEATBELT_SHELLS)

C----------------------------------------------------------------------------------C
C------Split of GAUGES-------------------------------------------------------------C
C----------------------------------------------------------------------------------C

          COMPT = 0
          DO I=1,NBGAUGE
            IF ((TAGGAU(I)>0).AND.(NUMELS>0)) THEN
C---------------> Case of GAUGES <--
              COMPT = COMPT + 1
            ELSEIF ((TAGGAU(I)<0).AND.(NUMSPH>0)) THEN
C---------------> Case of GAUGE/SPH <--
              TAGGAU(I) = ABS(TAGGAU(I))
              COMPT = COMPT + 1
            ELSE
              TAGGAU(I) = 0
            ENDIF
          ENDDO

          NBGAUGE = COMPT

C----------------------------------------------------------------------------------C
C------Split of RBY_MSN (used for /INIVEL/AXIS) -----------------------------------C
C----------------------------------------------------------------------------------C

          NRBODY_OLD = SIZE(TAGRBY)
          call my_alloc(RBY_MSN_TEMP, 2, NRBODY_OLD, "RBY_MSN_TEMP")
          DO I=1,NRBODY_OLD
            RBY_MSN_TEMP(1,I) = RBY_MSN(1,I)
            RBY_MSN_TEMP(2,I) = RBY_MSN(2,I)
            RBY_MSN(1,I) = 0
            RBY_MSN(2,I) = 0
          ENDDO

C-------------Split----------------------------------------------------------------C

          NRB =0
          DO I=1,NRBODY_OLD
            IF(TAGRBY(I) > 0) THEN
              NRB = NRB + 1
              RBY_MSN(1,NRB) = RBY_MSN_TEMP(1,I)
              RBY_MSN(2,NRB) = CORESN(RBY_MSN_TEMP(2,I))
            END IF
          ENDDO

C------------------------------------------------------------------------------------C

          FLG_SPLIT = 1
!!         DEALLOCATE(BUF_TEMP)
          CALL MY_DEALLOC(CORESC)
          CALL MY_DEALLOC(CORESN)
          CALL MY_DEALLOC(CORESTG)
          CALL MY_DEALLOC(COREST)
          CALL MY_DEALLOC(CORESPRO)
          CALL MY_DEALLOC(CORESR)
          CALL MY_DEALLOC(CORESP)
          CALL MY_DEALLOC(CORESS)
          CALL MY_DEALLOC(CORESQ)
          CALL MY_DEALLOC(EANI_TEMP)
          CALL MY_DEALLOC(TAGNO_TEMP)

C------------------------------------------------------------------------------------C

          call my_dealloc(TAG_ELC)
          call my_dealloc(TAG_ELS)
          call my_dealloc(TAG_ELP)
          call my_dealloc(TAG_ELQ)
          call my_dealloc(TAG_ELR)
          call my_dealloc(TAG_ELT)
          CALL MY_DEALLOC(TAG_SURF)
          CALL MY_DEALLOC(TAG_PROP)
          CALL MY_DEALLOC(TAG_ELCF2)
          CALL MY_DEALLOC(TAG_ELSF2)
          call my_dealloc(TAG_ELG)

C----------------------------------------------------------------------------------C
C------Rewritinf of TAGMAT and TAGPROP for grouping--------------------------------C
C----------------------------------------------------------------------------------C
          DO K=1,NPART
            IF(TAG_PART(K) == 0) THEN
              TAG_MAT(IPART(LIPART1*(K-1)+1))=0
            ENDIF
          END DO

C------------------------------------------------------------------------------------

        ENDIF
        RETURN

      END SUBROUTINE R2R_SPLIT
