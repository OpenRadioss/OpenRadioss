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
Chd|  HM_READ_SURF                  source/groups/hm_read_surf.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEALLOCATE_SURF_ELM           source/groups/init_surf_elm.F
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SUBMODPART                 source/groups/hm_submodpart.F
Chd|        HM_SURFGR2                    source/groups/hm_surfgr2.F
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F
Chd|        INIT_SURF_ELM                 source/groups/init_surf_elm.F
Chd|        QSURFTAG                      source/groups/qsurftag.F
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F
Chd|        SEGSURF                       source/groups/tsurftag.F
Chd|        SSURFTAG                      source/groups/ssurftag.F
Chd|        SSURFTAGIGEO                  source/groups/ssurftagigeo.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        SURFTAG                       source/groups/surftag.F
Chd|        SURFTAGADM                    source/groups/surftag.F
Chd|        SURFTAGE                      source/groups/surftage.F
Chd|        TSURFTAG                      source/groups/tsurftag.F
Chd|        UDOUBLE_IGR                   source/system/sysfus.F
Chd|        USR2SYS                       source/system/sysfus.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F
Chd|        SURF_MOD                      share/modules1/surf_mod.F
Chd|====================================================================
      SUBROUTINE HM_READ_SURF(
     1           ITAB    ,ITABM1  ,
     2           IGRSURF ,IXS     ,IXQ     ,IXC     ,IXT     ,
     3           IXP     ,IXR     ,IXTG
     4                   ,IPART   ,IPARTS  ,IPARTQ  ,IPARTC  ,
     5           IPARTT  ,IPARTP  ,IPARTR  ,IPARTTG ,X       ,
     6           MFI     ,ISKN    ,SKEW    ,
     7           BUFSF   ,KNOD2ELS,NOD2ELS ,SH4TREE ,SH3TREE ,
     8           ISUBMOD ,FLAG    ,UNITAB  ,IBOX    ,
     9           IXS10   ,IXS16   , IXS20  ,RTRANS  ,
     A           LSUBMODEL,KNOD2ELC,NOD2ELC,KNOD2ELTG,NOD2ELTG,
     B           KXIG3D  ,IXIG3D  ,IPARTIG3D,
     C           KNOT    ,IGEO    ,WIGE    ,KNOD2ELIG3D,NOD2ELIG3D,
     D           V      ,NIGE    ,RIGE     ,XIGE   ,
     E           VIGE    ,IADTABIGE,DECALIGEO,IADBOXMAX,KNOD2ELQ,
     F           NOD2ELQ ,SUBSET  ,IGRBRIC ,IGRSH4N  ,IGRSH3N,
     G           KNOTLOCPC,KNOTLOCEL,NSETS,MAP_TABLES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MY_ALLOC_MOD
        USE UNITAB_MOD
        USE SUBMODEL_MOD
        USE MESSAGE_MOD
        USE GROUPDEF_MOD
        USE OPTIONDEF_MOD
        USE SURF_MOD
        USE HM_OPTION_READ_MOD
        USE MAPPING_OPTION_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "ige3d_c.inc"
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
        INTEGER ITABM1(SITABM1),
     .          IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXC(NIXC,NUMELX),IXT(NIXT,NUMELT),
     .          IXP(NIXP,NUMELP),IXR(NIXR,NUMELR),IXTG(NIXTG,nUMELTG),IPARTS(NUMELS),
     .          IPARTQ(NUMELQ),IPARTC(NUMELC),IPARTT(*),IPARTP(NUMELP),IPARTR(NUMELR),
     .          IPARTTG(NUMELTG),IPART(LIPART1,NPART+NTHPART),ITAB(NUMNOD),
     .          ISKN(LISKN,SISKWN/LISKN),MFI,KNOD2ELS(NUMNOD+1),
     .          NOD2ELS(8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16),
     .          SH4TREE(KSH4TREE*NUMELC),SH3TREE(KSH3TREE*NUMELTG),ISUBMOD(NSUBMOD),
     .          IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .          KNOD2ELC(NUMNOD+1),NOD2ELC(4*NUMELC),KNOD2ELTG(NUMNOD+1),NOD2ELTG(3*NUMELTG+3*NUMELTG6),
     .          KXIG3D(NIXIG3D,NUMELIG3D0+ADDELIG3D),IPARTIG3D(NUMELIG3D0+ADDELIG3D),IXIG3D(*),
     .          KNOD2ELIG3D(NUMNOD+1),NOD2ELIG3D(*),
     .          NIGE(*),IGEO(NPROPGI,NUMGEO),
     .          KNOD2ELQ(NUMNOD+1),NOD2ELQ(4*NUMELQ)
        INTEGER FLAG,IADTABIGE,DECALIGEO,
     .          IADBOXMAX,NSETS
        my_real X(3,NUMNOD),SKEW(LSKEW,SSKEW/LSKEW),BUFSF(LISURF1*NSURF),
     .          RTRANS(NTRANSF,NRTRANS),V(3,NUMNOD),RIGE(*),XIGE(*),VIGE(*),
     .          WIGE(*),KNOT(*),KNOTLOCPC(*),KNOTLOCEL(*)
        TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
        TYPE(MAPPING_STRUCT_), INTENT(IN) :: MAP_TABLES
C-----------------------------------------------
        TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
        TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
        TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
        TYPE (SURF_)   , DIMENSION(NSURF+NSETS)   :: IGRSURF
        TYPE (BOX_)    , DIMENSION(NBBOX)   :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J,JJ,I,K,L,II,KK,ISU,ID,NSEG,NOSYS,NTOT,
     .          ITER,IGS,IGRS,NSU,CONT,IAD0,IADV,
     .          IADFIN,IT0,IT1,IT2,IT3,IT4,IT5,IT6,IT7,IPP,N1,N2,
     .          NSEGV,NE,ITYP,ISKEW,MAD,SRFTYP,REFMAD,DGR,DGR1,
     .          JC, IEXT,UID,IFLAGUNIT,
     .          ISK,BOXTYPE,J2(2),IT8,SBUFBOX,IT9,IADPL,SUB_ID,
     .          IFRE,NUMEL,INTMAX,IBUFSIZ,NINDX,STAT,NSEGIGE,
     .          IADBOX,N3,N4,NSEG0,
     .          LIST_SURF(NSURF),NSEG_TOT,NN,NENTITY,
     .          SEGID
        my_real
     .    XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,BID,
     .    S_A,S_B,S_C,XG,YG,ZG,FAC_L,DIAM,XP1,YP1,ZP1,XP2,YP2,ZP2
        CHARACTER TITR*nchartitle,MESS*40,KEY*ncharkey,KEY2*ncharkey,
     .            KEY3*ncharkey,STRING*nchartitle
        INTEGER, DIMENSION(:),ALLOCATABLE :: BUFBOX, BUFTMP, INDX ,TAGSHELLBOXC,TAGSHELLBOXG
        my_real :: VECTX,VECTY,VECTZ,VECT
        DOUBLE PRECISION RSBUFBOX
        CHARACTER*nchartitle,TITR1
        LOGICAL :: FLAG_GRBRIC, lFOUND, IS_AVAILABLE, IS_ENCRYPTED, lERROR, l1104
        INTEGER :: ID_PART,MODE
        INTEGER :: IBID
        INTEGER :: NINDX_SOL, NINDX_SOL10
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_SOL, INDX_SOL10
        TYPE(PART_TYPE), DIMENSION(:), ALLOCATABLE :: SURF_ELM
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       FLAG_GRBRIC : flag to initialize the INDX_SOL(10) arrays
!                     and optimize an old and expensive treatment in SSURFTAG
!       NINDX_SOL(10) : number of the tagged solid(10) element
!                      --> need to split solid and solid10
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       INDX_SOL(10) : ID of the tagged solid(10) element
!                      --> need to split solid and solid10
!                      for a treatment in the SSURFTAG routine
!                      only useful for /SURF/GRBRIC
!       MODE : integer
!              switch to initialize solid/shell/shell3n or truss/beam/spring
!       SURF_ELM : PART_TYPE structure
!                  %Nxxx : number of element per part
!                  %xxx_PART : ID of the element
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
        DATA MESS/'SURFACE DEFINITION                      '/
        DATA INTMAX /2147483647/
C-----------------------------------------------
!   IGRSURF(IGS)%ID   :: SURFACE identifier
!   IGRSURF(IGS)%TITLE   :: SURF title
!   IGRSURF(IGS)%NSEG   :: Number of surfaces within  /SURF
!   IGRSURF(IGS)%NSEG_IGE   :: Number of iso-surfaces
!   IGRSURF(IGS)%TYPE   ::  OPEN / CLOSED surface flag
!                        SURF_TYPE = 0         : SEGMENTS
!                        SURF_TYPE = 100       : HYPER-ELLIPSOIDE MADYMO.
!                        SURF_TYPE = 101       : HYPER-ELLIPSOIDE RADIOSS.
!                        SURF_TYPE = 200       : INFINITE PLANE
!   IGRSURF(IGS)%ID_MADYMO :: Coupled madimo surface identifier
!                             (computed in Radioss Engine, when receiving Datas from MaDyMo).
!                             ID MaDyMo - for entity type which impose surface movement:
!                             No systeme MaDyMo for entity type which impose surface movement
!   IGRSURF(IGS)%NB_MADYMO   :: No de l'entite qui impose le mvt de la surface.
!                                --> No systeme Radioss ou MaDyMO.
!   IGRSURF(IGS)%TYPE_MADYMO   :: Entity type which impose surface movement.
!                                  = 1 : Rigid Body.
!                                  = 2 : MADYMO Hyper-ellipsoide.
!   IGRSURF(IGS)%IAD_BUFR   :: Analytical Surfaces address (reals BUFSF - temp)
!   IGRSURF(IGS)%LEVEL   :: FLAG "SUBLEVEL DONE" FOR SURFACES OF SURFACES
!                                 = 0 ! initialized surface
!                                 = 1 ! uninitialized surface
!   IGRSURF(IGS)%TH_SURF   :: FLAG for /TH/SURF
!                                 = 0 ! unsaved surface for /TH/SURF
!                                 = 1 ! saved surface for /TH/SURF
!   IGRSURF(IGS)%ISH4N3N   ::  FLAG = 1 (only SH4N and SH3N considered - for airbags)
!   IGRSURF(IGS)%NSEG_R2R_ALL   :: Multidomaines -> number of segments before split
!   IGRSURF(IGS)%NSEG_R2R_SHARE :: shared on boundary subdomain segments
!   IGRSURF(IGS)%ELTYP(J)   :: type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
!   IGRSURF(IGS)%ELEM(J) :: element attached to the segment(J) of the surface
!   IGRSURF(IGS)%NODES(J,4) :: 4 nodes of the segment for /SURF
C=======================================================================
        IT0=0
        IT1=0
        IT2=0
        IT3=0
        IT4=0
        IT5=0
        IT6=0
        IT7=0
        IT8=0
        IT9=0
        IEXT=0
        IFRE=0
        IBUFSIZ=NUMELC+NUMELTG+6*NUMELS+NPART
        ALLOCATE(BUFTMP(IBUFSIZ),INDX(IBUFSIZ),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='BUFTMP')
        NINDX=0
        BUFTMP(1:IBUFSIZ)=0    ! init to 0 only one time

C=======================================================================
C SURFACE TYPE SEGMENT + init ISURF(1,IGS)
C=======================================================================
        CALL HM_OPTION_START('/SURF')
        TITR1='SURFACE'
        DO IGS=1,NSURF
          lERROR=.FALSE.
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR  ,
     .                            UNIT_ID     = UID,
     .                            KEYWORD2    = KEY   ,
     .                            KEYWORD3    = KEY2)
          NSEG = 0
          ! initialized variables:
          IF (FLAG == 0) THEN
            IGRSURF(IGS)%ID = 0
            IGRSURF(IGS)%NSEG = 0
            IGRSURF(IGS)%NSEG_IGE = 0
            IGRSURF(IGS)%IAD_IGE = 0
            IGRSURF(IGS)%TYPE = 0
            IGRSURF(IGS)%ID_MADYMO = 0
            IGRSURF(IGS)%IAD_BUFR = 0
            IGRSURF(IGS)%NB_MADYMO = 0
            IGRSURF(IGS)%TYPE_MADYMO = 0
            IGRSURF(IGS)%LEVEL = 0
            IGRSURF(IGS)%TH_SURF = 0
            IGRSURF(IGS)%ISH4N3N = 0
            IGRSURF(IGS)%NSEG_R2R_ALL = 0
            IGRSURF(IGS)%NSEG_R2R_SHARE = 0
          ENDIF
          IGRSURF(IGS)%ID=ID
          IGRSURF(IGS)%TYPE=0
          IGRSURF(IGS)%TITLE=TITR
          IF(KEY(1:4)=='SURF' .OR. KEY(1:5)=='DSURF')THEN
C             tag for surfaces defined from surface list
            IGRSURF(IGS)%NSEG=-1
            IGRSURF(IGS)%LEVEL=0
            IT0=IT0+1
          ELSEIF(KEY(1:3)=='SEG')THEN
            IT1=IT1+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
              NSEG0 = IGRSURF(IGS)%NSEG
              CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
              IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
              CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
              IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
              CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
              IGRSURF(IGS)%ELEM(1:NSEG0) = 0
            ENDIF
            IGRSURF(IGS)%LEVEL=1
            CALL HM_GET_INTV  ('segmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
            DO KK=1,NENTITY
              CALL HM_GET_INT_ARRAY_INDEX('SEGidArray',SEGID,KK,IS_AVAILABLE,LSUBMODEL)
              IF (FLAG == 1) THEN
                CALL HM_GET_INT_ARRAY_INDEX('N1',N1,KK,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('N2',N2,KK,IS_AVAILABLE,LSUBMODEL)
                N1 = USR2SYS(N1,ITABM1,MESS,ID)
                N2 = USR2SYS(N2,ITABM1,MESS,ID)
              ENDIF
              IF(NUMELS10>0.OR.FLAG==1) THEN
                CALL HM_GET_INT_ARRAY_INDEX('N3',N3,KK,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('N4',N4,KK,IS_AVAILABLE,LSUBMODEL)
                IF(N2D == 0) THEN
                  N3 = USR2SYS(N3,ITABM1,MESS,ID)
                  IF(N4/=0) THEN
                    N4 = USR2SYS(N4,ITABM1,MESS,ID)
                  ELSE
                    N4 = N3
                  ENDIF
                ELSE
                  N3 = 0
                  N4 = 0
                ENDIF
              ENDIF
              IF(NUMELS10 > 0.AND.N2D==0.AND.N3==N4.AND.N3/=0) THEN
                NSEG0 = IGRSURF(IGS)%NSEG
                IF (FLAG == 0) THEN
                  CALL HM_GET_INT_ARRAY_INDEX('N1',N1,KK,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('N2',N2,KK,IS_AVAILABLE,LSUBMODEL)
                  N1 = USR2SYS(N1,ITABM1,MESS,ID)
                  N2 = USR2SYS(N2,ITABM1,MESS,ID)
                ENDIF
                CALL TSURFTAG(IXS     ,IXS10   ,IGRSURF(IGS),FLAG  ,NSEG ,
     2                        KNOD2ELS,NOD2ELS ,N1          ,N2    ,N3   ,
     3                        NSEG0   )
              ELSE
                NSEG = NSEG +1
                IF (FLAG == 1) THEN
                  NSEG0 = IGRSURF(IGS)%NSEG
                  CALL SEGSURF(
     .                   N1  ,N2                ,N3                ,N4     ,NSEG0,
     .                   NSEG,IGRSURF(IGS)%NODES,IGRSURF(IGS)%ELTYP,IGRSURF(IGS)%ELEM,0,0)
                ENDIF
              ENDIF
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
            ENDDO

          ELSEIF(KEY(1:6)=='SUBSET'.OR. KEY(1:4)=='PART'.OR.
     .           KEY(1:3)=='MAT'   .OR. KEY(1:4)=='PROP'.OR.
     .           KEY(1:6)=='GRBRIC')THEN
C             surf de SUBSET PART MAT OU PROP
            IT2=IT2+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IGRSURF(IGS)%LEVEL=1
          ELSEIF(KEY(1:3) == 'BOX'.AND.NBBOX == 0 .AND.
     .     (KEY2(1:5) /= 'RECTA'.AND.
     .      KEY2(1:5) /= 'CYLIN'.AND.KEY2(1:5) /= 'SPHER'))THEN
C             surf dans un box (old box)
            lERROR=.TRUE.
          ELSEIF(KEY(1:2)=='GR')THEN
C             surf d'un group d'elements
            IT4=IT4+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IGRSURF(IGS)%LEVEL=1
          ELSEIF(KEY(1:6)=='ELLIPS'.OR.KEY(1:8)=='MDELLIPS')THEN
C             surface with formal equation (non-meshed).
            IT5=IT5+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=1
            IGRSURF(IGS)%LEVEL=1
            IF (FLAG == 1) THEN
              NSEG0 = IGRSURF(IGS)%NSEG
              CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
              IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
              CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
              IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
              CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
              IGRSURF(IGS)%ELEM(1:NSEG0) = 0
            ENDIF
          ELSEIF(KEY(1:6)=='SUBMOD')THEN
C             une surface d'un submodel.
            IT6=IT6+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IGRSURF(IGS)%LEVEL=1
          ELSEIF(KEY(1:3)=='BOX'.AND.(KEY2(1:5) == 'RECTA'.OR.
     .      KEY2(1:5) == 'CYLIN'.OR.KEY2(1:5) == 'SPHER'))THEN
C             old /grnod/box (not /BOX/BOX)
C             surf inside a box (classical box, parallelepiped (oriented), cylindrical, spherical)
            lERROR=.TRUE.
          ELSEIF(KEY(1:3) == 'BOX' .AND. NBBOX > 0)THEN
C             multi box (box of boxes)
            IT8=IT8+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IGRSURF(IGS)%LEVEL=1
          ELSEIF(KEY(1:6)=='PLANE')THEN
C             infinite plane (non-meshed)
            IT9=IT9+1
            IF (FLAG == 0) IGRSURF(IGS)%NSEG=0
            IGRSURF(IGS)%LEVEL=1
          ELSE
            lERROR=.TRUE.
          ENDIF

          IF(lERROR)THEN
            !INVALID KEYWORD
            STRING=' '
            STRING = "/SURF/"//KEY(1:LEN_TRIM(KEY))
            IF(LEN_TRIM(KEY2)>1)STRING = STRING//KEY2(1:LEN_TRIM(KEY2))
            CALL ANCMSG(MSGID=686,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID, C1=TITR, C2=STRING)
          ENDIF


        ENDDO   ! I=1,NLINE(KCUR)
c----------------------------
        NUMEL = NUMELC+NUMELTG
C
C-------------------------------------
C Searching for double IDs
C-------------------------------------
        IF (FLAG == 0) THEN
          DO IGS=1,NSURF
            LIST_SURF(IGS) = IGRSURF(IGS)%ID
          ENDDO
          CALL UDOUBLE_IGR(LIST_SURF,NSURF,MESS,0,BID)
        ENDIF
C=======================================================================
C BOX (OLD)
C=======================================================================
        IF (IT3/=0)THEN
          !no longer supported with new reader based on CFG files
        ENDIF
C=======================================================================
C BOX (parallelepiped, cylindrical, spherical) - old one (10SA1)
C=======================================================================
        IF (IT7/=0)THEN
          !no longer supported with new reader based on CFG files
        ENDIF
C=======================================================================
C NEW BOX OPTION (MULTI BOX COMBINATION)
C=======================================================================
        IF (IT8/=0) THEN
          ALLOCATE(TAGSHELLBOXC(NUMELC),STAT=stat)
          ALLOCATE(TAGSHELLBOXG(NUMELTG),STAT=stat)
          TAGSHELLBOXC(1:NUMELC) = 0
          TAGSHELLBOXG(1:NUMELTG) = 0
          IADBOX = 1
          IF (FLAG == 0) THEN
            ALLOCATE(BUFBOX(1))
            BUFBOX = 0
          ELSEIF (FLAG == 1) THEN
            ALLOCATE(BUFBOX(IADBOXMAX))
            BUFBOX(1:IADBOXMAX) = 0
          ENDIF
          SBUFBOX = INT(INTMAX)
C
          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
            NN = 0
            NSEG=0
            IF(KEY(1:3) == 'BOX'.AND. NBBOX > 0)THEN
              NSEG=0
              IADBOX = 1
              IFLAGUNIT = 0
              DO J=1,NUNITS
                IF (UNITAB%UNIT_ID(J) == UID) THEN
                  FAC_L = UNITAB%FAC_L(J)
                  IFLAGUNIT = 1
                  EXIT
                ENDIF
              ENDDO
              IF (UID/=0.AND.IFLAGUNIT==0) THEN
                CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      I2=UID,I1=ID,C1='SURFACE',
     .                 C2='SURFACE',
     .                 C3=TITR)
              ENDIF
!
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
              ENDIF
!
              NUMEL = NUMELC
              IF(NUMEL > 0)
     .         CALL HM_BIGSBOX(NUMEL ,IXC   ,NIXC   ,2    ,5     ,3   ,
     .                      X     ,       NSEG ,FLAG  ,SKEW,
     .                      ISKN  ,1     ,ITABM1 ,IBOX  ,
     .                      ID    ,BUFBOX,IGRSURF(IGS),IADBOX,KEY ,
     .                      SBUFBOX,TITR ,MESS  ,TAGSHELLBOXC,
     .                      NN, LSUBMODEL )
C---
              IADBOXMAX = MAX(IADBOX,IADBOXMAX)
C---
              IF (IADBOX>SBUFBOX .OR. IADBOX<0)
     .          CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
C---
              NUMEL = NUMELTG
              IF(NUMEL > 0)
     .         CALL HM_BIGSBOX(NUMEL  ,IXTG  ,NIXTG  ,2    ,4     ,7   ,
     .                      X      ,       NSEG ,FLAG  ,SKEW,
     .                      ISKN   ,1     ,ITABM1 ,IBOX  ,
     .                      ID     ,BUFBOX,IGRSURF(IGS),IADBOX,KEY ,
     .                      SBUFBOX,TITR  ,MESS  ,TAGSHELLBOXG,
     .                      NN, LSUBMODEL  )
              IF (IADBOX>SBUFBOX .OR. IADBOX<0)
     .          CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
C---
              IADBOXMAX = MAX(IADBOX,IADBOXMAX)
C---
              IEXT=0
              IF(KEY2(1:3)=='EXT')THEN
                IEXT=1
              ELSEIF(KEY2(1:3)=='ALL')THEN
                IEXT=2
              END IF
C---
              IF (NUMELS > 0) THEN
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL SBOXBOXSURF(IXS  ,X      ,NSEG,
     .                     KNOD2ELS  ,NOD2ELS,IEXT ,FLAG,
     .                     IXS10     ,IXS16  ,IXS20,SKEW        ,IBOX,
     .                     ID        ,BUFBOX,IADBOX     ,KEY ,
     .                     SBUFBOX   ,TITR   ,KNOD2ELC,NOD2ELC  ,IXC ,
     .                     TAGSHELLBOXC ,KNOD2ELTG ,NOD2ELTG ,IXTG   ,
     .                     TAGSHELLBOXG,IGRSURF(IGS),NN,NSEG0,LSUBMODEL)
              ENDIF
C---
              IADBOXMAX = MAX(IADBOX,IADBOXMAX)
C---
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ELSEIF (FLAG == 1) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
            ENDIF
            IF (IADBOX>SBUFBOX .OR. IADBOX<0)
     .        CALL ANCMSG(MSGID=1007, MSGTYPE=MSGERROR,ANMODE=ANSTOP)
          ENDDO
          IF(ALLOCATED(BUFBOX))DEALLOCATE(BUFBOX)
          DEALLOCATE(TAGSHELLBOXC,TAGSHELLBOXG)
        ENDIF  ! IT8/=0
C
C=======================================================================
C groups of SUBSETS,PART,MAT,PROP,BRIC
C=======================================================================

        IF(IT2/=0.OR.IT6/=0)THEN
          ALLOCATE( SURF_ELM(NPART) )
          MODE = 1
          CALL INIT_SURF_ELM(NUMELS  ,NUMELS8,NUMELS10,NUMELC ,NUMELTG ,
     1                       IBID    ,IBID   ,IBID    ,NPART  ,IPARTS  ,
     2                       IPARTC  ,IPARTTG,IBID    ,IBID   ,IBID    ,
     3                       SURF_ELM,MODE )
        ENDIF

        IF(IT2/=0)THEN
          NINDX_SOL = 0
          NINDX_SOL10 = 0
          ALLOCATE( INDX_SOL(NUMELS) )
          ALLOCATE( INDX_SOL10(NUMELS) )
          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
            NSEG=0
            NSEGIGE=0
            IEXT=0
            NSEG0 = IGRSURF(IGS)%NSEG
            IF (KEY(1:6)=='GRBRIC')THEN
              IF(KEY2(1:3)=='EXT')THEN
                IFRE=0
                IEXT=1
              END IF
              IF(KEY2(1:4)=='FREE')THEN
                IFRE=1
                IEXT=1
              END IF
              IF(IEXT==0.AND.IFRE==0)THEN !only /grbric/ext is treated
                CALL ANCMSG(MSGID=479,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR)
              ENDIF
!
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
              ENDIF
!
              NUMEL=NUMELS8+NUMELS10
              FLAG_GRBRIC = .TRUE.
              CALL HM_SURFGR2(NGRBRIC ,KEY(1:6),NUMEL ,IGRSURF(IGS)%ID,
     2                    IGRBRIC ,BUFTMP  ,TITR  ,TITR1          ,
     3                    INDX    ,NINDX   ,FLAG  ,NINDX_SOL,NINDX_SOL10,
     4                    INDX_SOL,INDX_SOL10 ,FLAG_GRBRIC,LSUBMODEL)
              CALL SSURFTAG(IXS     ,IPARTS  ,NSEG0    ,IGRSURF(IGS),BUFTMP,
     2                      NSEG    ,KNOD2ELS,NOD2ELS  ,IEXT    ,FLAG   ,
     3                      IXS10   ,IXS16   ,IXS20    ,IFRE    ,KEY    ,
     4                      KNOD2ELC,NOD2ELC ,KNOD2ELTG,NOD2ELTG,
     5                      IXC     ,IXTG    ,IPARTC   ,IPARTTG ,NINDX,
     6                      NINDX_SOL, NINDX_SOL10, INDX, INDX_SOL, INDX_SOL10,
     7                      SURF_ELM)
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
            ENDIF

            IF (KEY(1:4)=='PART'.OR.KEY(1:6)=='SUBSET'.OR.
     .          KEY(1:3)=='MAT' .OR.KEY(1:4)=='PROP') THEN
              IF(KEY2(1:3)=='EXT')THEN
                IEXT=1
              ELSEIF(KEY2(1:3)=='ALL')THEN
                IEXT=2
              END IF
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
!             ISOGEO
                NSEG0 = IGRSURF(IGS)%NSEG_IGE
                CALL MY_ALLOC(IGRSURF(IGS)%NODES_IGE,NSEG0,4)
                IGRSURF(IGS)%NODES_IGE(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP_IGE,NSEG0)
                IGRSURF(IGS)%ELTYP_IGE(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM_IGE,NSEG0)
                IGRSURF(IGS)%ELEM_IGE(1:NSEG0) = 0
!
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
!
                IF (NVOLU + NMONVOL > 0) THEN
                  NSEG0 = IGRSURF(IGS)%NSEG
                  !Keep track of the "reversed surface" -> when /SURF/PART comes
                  !with a negative part_id
                  CALL MY_ALLOC(IGRSURF(IGS)%REVERSED, NSEG0)
                ENDIF
              ENDIF
!
              CALL HM_TAGPART2(BUFTMP,IPART ,KEY   ,
     .                      IGRSURF(IGS)%ID,TITR,TITR1,INDX,NINDX ,
     .                      FLAG  ,SUBSET, LSUBMODEL,MAP_TABLES%IPARTM)
              IF (NADMESH==0)THEN
                NUMEL = NUMELC
                CALL SURFTAG(NUMEL,IXC,NIXC,2,5,3,IPARTC,
     .                       BUFTMP,IGRSURF(IGS),NSEG,FLAG,NINDX,
     .                       INDX,SURF_ELM)
                NUMEL = NUMELTG
                CALL SURFTAG(NUMEL,IXTG,NIXTG,2,4,7,IPARTTG,
     .                       BUFTMP,IGRSURF(IGS),NSEG,FLAG,NINDX,
     .                       INDX,SURF_ELM)
              ELSE
                NUMEL = NUMELC
                CALL SURFTAGADM(NUMEL,IXC,NIXC,2,5,3,IPARTC,
     .                 BUFTMP,IGRSURF(IGS),NSEG,IPART,
     .                 KSH4TREE,SH4TREE,FLAG)
                NUMEL = NUMELTG
                CALL SURFTAGADM(NUMEL,IXTG,NIXTG,2,4,7,IPARTTG,
     .                 BUFTMP,IGRSURF(IGS),NSEG,IPART,
     .                 KSH3TREE,SH3TREE,FLAG)
              END IF
              IF(IEXT==0)THEN
                l1104=.FALSE.
                DO II=1,NUMELS
                  IF (IABS(BUFTMP(IPARTS(II)))==1)THEN
                    l1104=.TRUE.
                    CALL ANCMSG(MSGID=1104,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            PRMOD=MSG_CUMU,
     .                            I1=IPARTS(II),
     .                            I2=IXS(11,II))
                  ENDIF
                ENDDO
                IF(l1104)CALL ANCMSG(MSGID=1104,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        PRMOD=MSG_PRINT,
     .                        I1=ID,
     .                        C1=TITR     )

              ELSE
                DO II=NUMELS8+NUMELS10+1,NUMELS
                  IF (IABS(BUFTMP(IPARTS(II)))==1)THEN
                    TITR = IGRSURF(IGS)%TITLE
                    CALL ANCMSG(MSGID=651,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                  ENDIF
                ENDDO
              END IF
!
              NSEG0 = IGRSURF(IGS)%NSEG
              CALL SSURFTAG(IXS     ,IPARTS  ,NSEG0    ,IGRSURF(IGS),BUFTMP ,
     2                      NSEG    ,KNOD2ELS,NOD2ELS  ,IEXT    ,FLAG   ,
     3                      IXS10   ,IXS16   ,IXS20    ,IFRE    ,KEY    ,
     4                      KNOD2ELC,NOD2ELC ,KNOD2ELTG,NOD2ELTG,
     5                      IXC     ,IXTG    ,IPARTC   ,IPARTTG ,NINDX,
     6                      NINDX_SOL, NINDX_SOL10, INDX, INDX_SOL, INDX_SOL10,
     7                      SURF_ELM)
              IF(NUMELIG3D/=0) THEN
                CALL SSURFTAGIGEO(IXIG3D,IPARTIG3D,KXIG3D,
     2                            BUFTMP ,NSEG   ,
     3                            IEXT  ,FLAG     ,IFRE   ,KEY    ,
     4                            NSEGIGE,KNOT    ,IGEO   ,WIGE   ,
     5                            X     ,V,  KNOD2ELIG3D,NOD2ELIG3D   ,
     6                            NIGE,RIGE,XIGE,VIGE,IADTABIGE,DECALIGEO,
     7                            IGRSURF(IGS),KNOTLOCPC,KNOTLOCEL)
              ENDIF

C------------/SURF/PART/EXT FOR QUADS --------------------
              CALL QSURFTAG(IXQ    ,IPARTQ  , NSEG0 ,IGRSURF(IGS),BUFTMP ,
     2                      NSEG   ,KNOD2ELQ,NOD2ELQ,IEXT        ,FLAG   ,
     3                      X)

              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
                IGRSURF(IGS)%NSEG_IGE = NSEGIGE
                NUMFAKENODIGEO=NUMFAKENODIGEO+16*NSEGIGE/9 ! same functionnality as IADTABIGE
              ENDIF
            ENDIF
C reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
            NINDX_SOL = 0
            NINDX_SOL10 = 0
          ENDDO

          DEALLOCATE( INDX_SOL )
          DEALLOCATE( INDX_SOL10 )
        ENDIF
C=======================================================================
C surfaces from SUBMODELS
C=======================================================================
        IF (IT6 > 0)THEN
          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
            NSEG=0
            IEXT=0
            IF (KEY(1:6)=='SUBMOD') THEN
              IF(KEY2(1:3)=='EXT')THEN
                IEXT=1
              ELSEIF(KEY2(1:3)=='ALL')THEN
                IEXT=2
              END IF
!
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
                IF (NVOLU + NMONVOL > 0) THEN
                  NSEG0 = IGRSURF(IGS)%NSEG
                  !Keep track of the "reversed surface" -> when /SURF/PART comes
                  !with a negative part_id
                  CALL MY_ALLOC(IGRSURF(IGS)%REVERSED, NSEG0)
                ENDIF
              ENDIF
!
              CALL HM_SUBMODPART(ISUBMOD,BUFTMP ,IPART ,ID   ,FLAG ,
     .                        MESS   ,TITR   ,TITR1 ,INDX ,NINDX,
     .                        LSUBMODEL)
C-----------------------                                                         --
              IF (NADMESH==0) THEN
                NUMEL = NUMELC
                CALL SURFTAG(NUMEL,IXC,NIXC,2,5,3,IPARTC,
     .                       BUFTMP,IGRSURF(IGS),NSEG,FLAG,NINDX,
     .                       INDX,SURF_ELM)
                NUMEL = NUMELTG
                CALL SURFTAG(NUMEL,IXTG,NIXTG,2,4,7,IPARTTG,
     .                       BUFTMP,IGRSURF(IGS),NSEG,FLAG,NINDX,
     .                       INDX,SURF_ELM)
              ELSE
                NUMEL = NUMELC
                CALL SURFTAGADM(NUMEL,IXC,NIXC,2,5,3,IPARTC,
     .                          BUFTMP,IGRSURF(IGS),NSEG,IPART,
     .                 KSH4TREE,SH4TREE,FLAG)
                NUMEL = NUMELTG
                CALL SURFTAGADM(NUMEL,IXTG,NIXTG,2,4,7,IPARTTG,
     .                          BUFTMP,IGRSURF(IGS),NSEG,IPART,
     .                          KSH3TREE,SH3TREE,FLAG)
              END IF
C-----------------------                                                         --
              IF(IEXT==0)THEN
                l1104=.FALSE.
                DO II=1,NUMELS
                  IF(IABS(BUFTMP(IPARTS(II)))==1)THEN
                    l1104=.TRUE.
                    CALL ANCMSG(MSGID=1104,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            PRMOD=MSG_CUMU,
     .                            I1=IPARTS(II),
     .                            I2=IXS(11,II))
                  ENDIF
                ENDDO
                IF(l1104)CALL ANCMSG(MSGID=1104,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        PRMOD=MSG_PRINT)
              ELSE
                DO II=NUMELS8+NUMELS10+1,NUMELS
                  IF(IABS(BUFTMP(IPARTS(II)))==1)THEN
                    CALL ANCMSG(MSGID=651,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                  ENDIF
                ENDDO
              END IF
C-------------------------
              NSEG0 = IGRSURF(IGS)%NSEG
              CALL SSURFTAG(IXS     ,IPARTS  ,NSEG0    ,IGRSURF(IGS),BUFTMP,
     2                      NSEG    ,KNOD2ELS,NOD2ELS  ,IEXT    ,FLAG  ,
     3                      IXS10   ,IXS16   ,IXS20    ,IFRE    ,KEY   ,
     4                      KNOD2ELC,NOD2ELC ,KNOD2ELTG,NOD2ELTG,
     5                      IXC     ,IXTG    ,IPARTC   ,IPARTTG ,NINDX,
     6                      NINDX_SOL, NINDX_SOL10, INDX, INDX_SOL, INDX_SOL10,
     7                      SURF_ELM)
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
C-------------------------
            ENDIF
C reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
          ENDDO
        ENDIF
C=======================================================================
C SURFACE FROM GROUP OF SHELLS (4N ET 3N)
C=======================================================================
        IF (IT4 /= 0) THEN
          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2)
            NSEG=0
            NSEG_TOT=0
            CONT=1
C-----------                                              --
            IF (KEY(1:6)=='GRSHEL') THEN
!
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
!
                IF (NVOLU + NMONVOL > 0) THEN
                  NSEG0 = IGRSURF(IGS)%NSEG
                  !     Keep track of the "reversed surface" -> when /SURF/PART comes
                  !     with a negative part_id
                  CALL MY_ALLOC(IGRSURF(IGS)%REVERSED, NSEG0)
                ENDIF
              ENDIF
!
              NUMEL=NUMELC
              FLAG_GRBRIC=.FALSE.
              CALL HM_SURFGR2(NGRSHEL ,KEY(1:6),NUMEL ,IGRSURF(IGS)%ID,
     .                    IGRSH4N ,BUFTMP  ,TITR  ,TITR1          ,
     .                    INDX    ,NINDX   ,FLAG  ,NINDX_SOL,NINDX_SOL10,
     .                    INDX_SOL,INDX_SOL10 ,FLAG_GRBRIC,LSUBMODEL)
              CALL SURFTAGE(NUMEL,IXC,NIXC,2,5,3,
     .                      BUFTMP,IGRSURF(IGS),NSEG,FLAG,
     .                      INDX,NINDX,NSEG_TOT)
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
C---
            ELSEIF (KEY(1:6)=='GRSH3N' .OR. KEY(1:6)=='GRTRIA') THEN
!
              IF (FLAG == 1) THEN ! NSEG counted at FLAG = 0
                NSEG0 = IGRSURF(IGS)%NSEG
                CALL MY_ALLOC(IGRSURF(IGS)%NODES,NSEG0,4)
                IGRSURF(IGS)%NODES(1:NSEG0,1:4) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELTYP,NSEG0)
                IGRSURF(IGS)%ELTYP(1:NSEG0) = 0
                CALL MY_ALLOC(IGRSURF(IGS)%ELEM,NSEG0)
                IGRSURF(IGS)%ELEM(1:NSEG0) = 0
!
                IF (NVOLU + NMONVOL > 0) THEN
                  NSEG0 = IGRSURF(IGS)%NSEG
                  !     Keep track of the "reversed surface" -> when /SURF/PART comes
                  !     with a negative part_id
                  CALL MY_ALLOC(IGRSURF(IGS)%REVERSED, NSEG0)
                ENDIF
              ENDIF
!
              NUMEL=NUMELTG
              FLAG_GRBRIC=.FALSE.
              CALL HM_SURFGR2(NGRSH3N ,KEY(1:6),NUMEL ,IGRSURF(IGS)%ID,
     .                    IGRSH3N ,BUFTMP  ,TITR  ,TITR1          ,
     .                    INDX    ,NINDX   ,FLAG  ,NINDX_SOL,NINDX_SOL10,
     .                    INDX_SOL,INDX_SOL10 ,FLAG_GRBRIC,LSUBMODEL)
              CALL SURFTAGE(NUMEL,IXTG,NIXTG,2,4,7,
     .                      BUFTMP,IGRSURF(IGS),NSEG,FLAG,
     .                      INDX,NINDX,NSEG_TOT)
              IF (FLAG == 0) THEN
                IGRSURF(IGS)%NSEG = NSEG
              ENDIF
            ENDIF
            !reset BUFTMP to 0 (only where it was set to 1/-1)
            DO II=1,NINDX
              BUFTMP(INDX(II))=0
            END DO
            NINDX=0
C-----------
          ENDDO!next IGS
        ENDIF!(IT4 /= 0)

C=======================================================================
C SURFACE WITH FORMAL EQUATION (ETC).
C=======================================================================
        MAD=0
        IF (IT5 /= 0 .AND. FLAG == 1)THEN

          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2  ,
     .                              SUBMODEL_ID = SUB_ID)
            IGRSURF(IGS)%TITLE = TITR
            IF(KEY(1:6)=='ELLIPS')THEN
              IGRSURF(IGS)%ID = ID
              IGRSURF(IGS)%TYPE = 101
              IGRSURF(IGS)%IAD_BUFR = MAD
              MFI=MFI+36
              DGR1=0
              CALL HM_GET_INTV  ('SKEW' ,ISKEW,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INTV  ('n' ,DGR1,IS_AVAILABLE,LSUBMODEL)
              !skew:temporary storage of user id
              IGRSURF(IGS)%ID_MADYMO = ISKEW
              !get internal id from user id
              lFOUND=.FALSE.
              DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
                IF(ISKEW==ISKN(4,J+1)) THEN
                  ISKEW=J+1
                  lFOUND=.TRUE.
                  EXIT
                ENDIF
              END DO
              IF(.NOT.lFOUND)THEN
                CALL ANCMSG(MSGID=184,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='SURFACE',
     .                      I1=ID,
     .                      C2='SURFACE',
     .                      C3=TITR,
     .                      I2=ISKEW)
C
              ELSE
C           Init surface rotation
                DO J=1,9
                  BUFSF(MAD+7+J-1)=SKEW(J,ISKEW)
                END DO
              ENDIF
C
              CALL HM_GET_FLOATV  ('Xc' ,XG,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Yc' ,YG,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Zc' ,ZG,IS_AVAILABLE,LSUBMODEL,UNITAB)
              IF(SUB_ID /= 0)CALL SUBROTPOINT(XG,YG,ZG,RTRANS,SUB_ID,LSUBMODEL)
              BUFSF(MAD+4)=XG
              BUFSF(MAD+5)=YG
              BUFSF(MAD+6)=ZG
              !Init application point for force and momentum
              !/* ellipsoides : defining center ! */
              BUFSF(MAD+16)=XG
              BUFSF(MAD+17)=YG
              BUFSF(MAD+18)=ZG
              DGR=0

              CALL HM_GET_FLOATV  ('a' ,S_A,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('b' ,S_B,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('c' ,S_C,IS_AVAILABLE,LSUBMODEL,UNITAB)
              DGR = 0
              IF ( S_A==0. .OR. S_B==0. .OR. S_C==0.) THEN
                CALL ANCMSG(MSGID=185,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR)
              ENDIF
              IF (DGR==0.AND.DGR1==0) THEN
                DGR1=2
              ELSEIF (DGR1==0) THEN
                DGR1=DGR
              ENDIF

              BUFSF(MAD+1)=S_A
              BUFSF(MAD+2)=S_B
              BUFSF(MAD+3)=S_C
              BUFSF(MAD+36)=DGR1

              MAD=MAD+36
            ELSEIF (KEY(1:8)=='MDELLIPS')THEN
              IGRSURF(IGS)%ID = ID
              IGRSURF(IGS)%TYPE = 100
              IGRSURF(IGS)%IAD_BUFR = MAD
              MFI=MFI+43
              CALL HM_GET_INTV  ('MDELLIPS' ,REFMAD,IS_AVAILABLE,LSUBMODEL)
              !ID MaDyMo of entity which imposes the surface movment
              IGRSURF(IGS)%ID_MADYMO = REFMAD
              !Madymo syst id of entity which imposes the surface movment
              !(computed in Radioss Engine, when receiving Datas from MaDyMo).
              IGRSURF(IGS)%NB_MADYMO = 0
              MAD=MAD+43
            ENDIF
          ENDDO
        ENDIF
C=======================================================================
C INFINITE PLANE
C=======================================================================
        IADPL = MAD
        IF (IT9 /= 0 .AND. FLAG == 1)THEN
          CALL HM_OPTION_START('/SURF')
          DO IGS=1,NSURF
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID   = ID,
     .                              OPTION_TITR = TITR  ,
     .                              UNIT_ID     = UID,
     .                              KEYWORD2    = KEY   ,
     .                              KEYWORD3    = KEY2,
     .                              SUBMODEL_ID = SUB_ID)
            IGRSURF(IGS)%TITLE = TITR
            IFLAGUNIT = 0
            DO J=1,NUNITS
              IF (UNITAB%UNIT_ID(J) == UID) THEN
                FAC_L = UNITAB%FAC_L(J)
                IFLAGUNIT = 1
                EXIT
              ENDIF
            ENDDO
            IF (UID/=0.AND.IFLAGUNIT==0) THEN
              CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I2=UID,I1=ID,C1='SURFACE',
     .                 C2='SURFACE',
     .                 C3=TITR)
            ENDIF

            IF(KEY(1:6)=='PLANE')THEN
              IGRSURF(IGS)%ID = ID
              IGRSURF(IGS)%TYPE = 200
              IGRSURF(IGS)%IAD_BUFR = IADPL
              MFI=MFI+6

              XP1 = ZERO
              YP1 = ZERO
              ZP1 = ZERO
              XP2 = ZERO
              YP2 = ZERO
              ZP2 = ZERO

              CALL HM_GET_FLOATV  ('X_A' ,XP1,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Y_A' ,YP1,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Z_A' ,ZP1,IS_AVAILABLE,LSUBMODEL,UNITAB)
              IF(SUB_ID /= 0)CALL SUBROTPOINT(XP1,YP1,ZP1,RTRANS,SUB_ID,LSUBMODEL)

              CALL HM_GET_FLOATV  ('X_B' ,XP2,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Y_B' ,YP2,IS_AVAILABLE,LSUBMODEL,UNITAB)
              CALL HM_GET_FLOATV  ('Z_B' ,ZP2,IS_AVAILABLE,LSUBMODEL,UNITAB)
              IF(SUB_ID /= 0)CALL SUBROTPOINT(XP2,YP2,ZP2,RTRANS,SUB_ID,LSUBMODEL)

              VECTX = (XP2-XP1)*(XP2-XP1)
              VECTY = (YP2-YP1)*(YP2-YP1)
              VECTZ = (ZP2-ZP1)*(ZP2-ZP1)
              VECT  = SQRT(VECTX+VECTY+VECTZ)
              IF(VECT <= EM10)THEN
                CALL ANCMSG(MSGID=891,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR)
              ENDIF
C             Normal Vector
              BUFSF(IADPL+1)=XP1
              BUFSF(IADPL+2)=YP1
              BUFSF(IADPL+3)=ZP1
              BUFSF(IADPL+4)=XP2
              BUFSF(IADPL+5)=YP2
              BUFSF(IADPL+6)=ZP2
C
              IADPL=IADPL+6
            ENDIF
          ENDDO
!
          MAD = IADPL
        ENDIF
C=======================================================================
        DEALLOCATE(BUFTMP,INDX)
        IF(IT2/=0.OR.IT6/=0)THEN
          MODE = 1
          CALL DEALLOCATE_SURF_ELM(NPART,SURF_ELM,MODE)
          DEALLOCATE( SURF_ELM )
        ENDIF
        RETURN

        CALL ANCMSG(MSGID=189,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=IGRSURF(IGS)%ID)
        RETURN
      END

