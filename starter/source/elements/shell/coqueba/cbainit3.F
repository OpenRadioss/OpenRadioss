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
!||    cbainit3                  ../starter/source/elements/shell/coqueba/cbainit3.F
!||--- called by ------------------------------------------------------
!||    initia                    ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                    ../starter/source/output/message/message.F
!||    c1buf3                    ../starter/source/elements/shell/coque/c1buf3.F
!||    cbufxfe                   ../starter/source/elements/xfem/cbufxfe.F
!||    ccoori                    ../starter/source/elements/shell/coque/ccoori.F
!||    cderii                    ../starter/source/elements/shell/coque/cderii.F
!||    cepschk                   ../starter/source/elements/shell/coque/cepsini.F
!||    cfailini                  ../starter/source/elements/shell/coque/cfailini.F
!||    cfailini4                 ../starter/source/elements/shell/coque/cfailini.F
!||    cinmas                    ../starter/source/elements/shell/coque/cinmas.F
!||    cm35in3                   ../starter/source/materials/mat/mat035/cm35in3.F
!||    cmaini3                   ../starter/source/elements/sh3n/coquedk/cmaini3.F
!||    cmatini                   ../starter/source/materials/mat_share/cmatini.F
!||    cmatini4                  ../starter/source/materials/mat_share/cmatini4.F
!||    cndleni                   ../starter/source/elements/shell/coqueba/cndleni.F
!||    cnepsini                  ../starter/source/elements/shell/coqueba/cnepsini.F
!||    cneveci                   ../starter/source/elements/shell/coqueba/cneveci.F
!||    corth3                    ../starter/source/elements/shell/coque/corth3.F
!||    csigini4                  ../starter/source/elements/shell/coqueba/scigini4.F
!||    csms11_ini                ../starter/source/elements/shell/coque/cinit3.F
!||    cstraini4                 ../starter/source/elements/shell/coqueba/cstraini4.F
!||    cuserini                  ../starter/source/elements/shell/coque/cuserini.F
!||    cuserini4                 ../starter/source/elements/shell/coqueba/cuserini4.F
!||    cveok3                    ../starter/source/elements/shell/coque/cveok3.F
!||    fail_windshield_init      ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
!||    fractal_dmg_init          ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
!||    fretitl2                  ../starter/source/starter/freform.F
!||    initemp_shell             ../starter/source/materials/therm/initemp_shell.F90
!||    layini1                   ../starter/source/elements/shell/coqueba/layini1.F
!||    thickini                  ../starter/source/elements/shell/coqueba/thickini.F
!||--- uses       -----------------------------------------------------
!||    drape_mod                 ../starter/share/modules1/drape_mod.F
!||    fractal_dmg_init_mod      ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
!||    initemp_shell_mod         ../starter/source/materials/therm/initemp_shell.F90
!||    message_mod               ../starter/share/message_module/message_mod.F
!||    stack_mod                 ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE CBAINIT3(
     1           ELBUF_STR,IXC      ,PM       ,X       ,GEO     ,
     2           XMAS     ,IN       ,NVC      ,DTELEM  ,IGRSH4N ,
     3           XREFC    ,NEL      ,ITHK     ,IHBE    ,IGRSH3N ,
     4           THKE     ,ISIGSH   ,SIGSH    ,STIFN   ,STIFR   ,
     5           PARTSAV  ,V        ,IPART    ,MSC     ,INC     ,
     6           SKEW     ,I8MI     ,NSIGSH   ,IGEO    ,IPM     ,
     7           IUSER    ,ETNOD    ,NSHNOD   ,STC     ,PTSHEL  ,
     8           BUFMAT   ,SH4TREE  ,MCP      ,MCPS    ,TEMP    ,  
     9           MS_LAYER ,ZI_LAYER ,ITAG     ,ITAGEL  ,IPARG   ,
     A           MS_LAYERC,ZI_LAYERC,PART_AREA,CPT_ELTENS,
     B           MSZ2C    ,ZPLY     ,ITAGN    ,ITAGE   ,IXFEM   ,
     C           NPF      ,TF       ,XFEM_STR ,ISUBSTACK,STACK  ,
     D           RNOISE   ,DRAPE    ,SH4ANG   ,IDDLEVEL,GEO_STACK,
     E           IGEO_STACK,STRC    ,PERTURB  ,IYLDINI ,ELE_AREA,
     F           NLOC_DMG ,NG       ,GROUP_PARAM, IDRAPE , DRAPEG,
     G           MAT_PARAM,FAIL_FRACTAL,FAIL_BROKMANN,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE NLOCAL_REG_MOD
      USE GROUP_PARAM_MOD      
      USE DRAPE_MOD        
      USE RANDOM_WALK_DEF_MOD
      USE FRACTAL_DMG_INIT_mod
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use brokmann_random_def_mod
      use glob_therm_mod
      use initemp_shell_mod
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scry_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC,NEL,ITHK,IHBE,ISIGSH,IXFEM,NSIGSH,IUSER,IYLDINI,
     .        ISUBSTACK,NG,IDRAPE
      INTEGER IXC(NIXC,*),IPART(*),PTSHEL(*),ITAG(*),ITAGEL(*), 
     .        IGEO(NPROPGI,*), IPM(NPROPMI,*), NSHNOD(*),NPF(*),
     .        SH4TREE(*),IPARG(*),CPT_ELTENS,ITAGN(*),ITAGE(*),
     .        IGEO_STACK(*),PERTURB(NPERTURB)
      INTEGER ,INTENT(IN) :: IDDLEVEL
      INTEGER *8 I8MI(6,*)
      my_real
     .   PM(NPROPM,*), X(3,*), GEO(NPROPG,*), XMAS(*), IN(*),
     .   DTELEM(*), XREFC(4,3,*),THKE(*), SIGSH(NSIGSH,*),
     .   STIFN(*),STIFR(*),PARTSAV(20,*), V(*) ,MSC(*) ,INC(*),
     .   SKEW(LSKEW,*), ETNOD(*), STC(*),BUFMAT(*),MCP(*),MCPS(*),
     .   TEMP(*),MS_LAYER(*),ZI_LAYER(*),MS_LAYERC(*),ZI_LAYERC(*),
     .   PART_AREA(*),MSZ2C(*),ZPLY(*),TF(*),RNOISE(*),
     .   SH4ANG(*),GEO_STACK(*),STRC(*),ELE_AREA(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,*):: XFEM_STR
      !   when XFEM is ON, XFEM_STR's dimension = NGROUP,NXEL
      TYPE (STACK_PLY) :: STACK
      TYPE (NLOCAL_STR_)   :: NLOC_DMG
      TYPE (GROUP_PARAM_)  :: GROUP_PARAM
      TYPE (DRAPE_)  ::  DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_) ::  DRAPEG
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
      TYPE (FAIL_FRACTAL_)  ,INTENT(IN) :: FAIL_FRACTAL
      TYPE (FAIL_BROKMANN_) ,INTENT(IN) :: FAIL_BROKMANN
      TYPE(glob_therm_)     ,intent(in) :: glob_therm
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,IUN,NDEPAR,IGTYP,IGMAT,NUVAR,IMAT,IPROP,PROPID,
     .   IPG,NPG,PTF,PTM,PTS,IXEL,IREP,NLAY,NPTR,NPTS,NPTT,IFAIL,
     .   IL,IR,IS,IT,LENF,LENM,LENS,
     .   LENFP,LENMP,LENEPINCHXZ,LENEPINCHYZ,LENEPINCHZZ,
     .   IPANG,IPTHK,IPPOS, ILAY,I4,MPT,LAYNPT_MAX,LAY_MAX,NPT_ALL
      INTEGER JJ(9)
      INTEGER , DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4,IORTHLOC,MAT,PID,NGL
c
      my_real , DIMENSION(MVSIZ) :: AREA,ALDT,DTEL,PX1G,PX2G,PY1G,PY2G,
     .                              X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                              E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                              X2S,Y2S,X3S,Y3S,X4S,Y4S,
     .                              X2L,X3L,X4L,Y2L,Y3L,Y4L
      CHARACTER(LEN=NCHARTITLE)::TITR
c
      my_real, ALLOCATABLE, DIMENSION(:) :: DIR_A,DIR_B
      my_real , DIMENSION(:) ,POINTER   :: UVAR
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER, DIMENSION(:),ALLOCATABLE::MATLY
      my_real, DIMENSION(:,:),ALLOCATABLE :: POSLY
C-----------------------------------------------
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C=======================================================================
      ! Allocate buffers
      CALL MY_ALLOC(MATLY,MVSIZ*LAY_MAX)
      CALL MY_ALLOC(POSLY,MVSIZ,LAY_MAX*LAYNPT_MAX)

      GBUF => ELBUF_STR%GBUF
c
      IMAT  = IXC(1,1+NFT)         ! mat N   
      IPROP = IXC(NIXC-1,1+NFT)    ! property N   
      PROPID= IGEO(1 ,IPROP)       ! property ID
      IGTYP = IGEO(11,IPROP)
      IGMAT = IGEO(98,IPROP)       ! global mat flag
      IREP  = IPARG(35) 
      IFAIL = IPARG(43)
C
      DO I=LFT,LLT
        N = I+NFT
        MAT(I) = IXC(1,N)   ! imat  = mxt
        PID(I) = IXC(6,N)   ! iprop = mxg
      ENDDO
C      
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
      IORTHLOC = 0
c
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      NXEL = ELBUF_STR%NXEL
      NPG  = NPTR*NPTS
      LENF = NEL*GBUF%G_FORPG/NPG
      LENM = NEL*GBUF%G_MOMPG/NPG
C      
      LENFP = NEL*GBUF%G_FORPGPINCH/NPG
      LENMP = NEL*GBUF%G_MOMPGPINCH/NPG
      LENEPINCHXZ = NEL*GBUF%G_EPGPINCHXZ/NPG
      LENEPINCHYZ = NEL*GBUF%G_EPGPINCHYZ/NPG
      LENEPINCHZZ = NEL*GBUF%G_EPGPINCHZZ/NPG
C
      LENS = NEL*GBUF%G_STRPG/NPG
    
      NPT_ALL = 0
      DO IL=1,NLAY
           NPT_ALL = NPT_ALL + ELBUF_STR%BUFLY(IL)%NPTT
      ENDDO   
      MPT  = MAX(1,NPT_ALL)
      IF(NPT_ALL == 0 ) NPT_ALL = NLAY
      IF (IPARG(6) == 0.OR.NPT==0) MPT=0
      IF((IGTYP == 51 .OR. IGTYP == 52) .AND. IDRAPE > 0) THEN
         ALLOCATE(DIR_A(NPT_ALL*NEL*2))
         ALLOCATE(DIR_B(NPT_ALL*NEL*2))
         DIR_A = ZERO
         DIR_B = ZERO
      ELSE
         ALLOCATE(DIR_A(NLAY*NEL*2))
         ALLOCATE(DIR_B(NLAY*NEL*2))
         DIR_A = ZERO
         DIR_B = ZERO
         NPT_ALL = NLAY 
      ENDIF
!
      DO I=1,9  ! length max of GBUF%G_SMSTR = 9
        JJ(I) = NEL*(I-1)
      ENDDO
      NPT_ALL = MAX(NLAY, NPT_ALL)
!
C
      IF (ISHXFEM_PLY > 0) THEN
        DO  I=LFT,LLT
          N = I+NFT
          ITAG(IXC(2,N)) =1
          ITAG(IXC(3,N)) =1
          ITAG(IXC(4,N)) =1
          ITAG(IXC(5,N)) =1
          ITAGEL(N) = 1
        ENDDO
      ENDIF
C
      IF (IXFEM > 0) THEN
        DO  I=LFT,LLT
          N = I+NFT
          ITAGN(IXC(2,N)) =1
          ITAGN(IXC(3,N)) =1
          ITAGN(IXC(4,N)) =1
          ITAGN(IXC(5,N)) =1
          ITAGE(N) = 1
        ENDDO
      ENDIF
c-------------------------------------------------
      CALL CCOORI(X,XREFC(1,1,NFT+1),IXC(1,NFT+1),
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .            IX1 ,IX2 ,IX3 ,IX4 ,NGL )
c
      CALL CVEOK3(NVC,4,IX1,IX2,IX3,IX4)
c
      CALL CNEVECI(LFT ,LLT ,AREA,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
C------------
C Tags total area of the part (needed in /ADMAS for shells)
C------------
      IF ((IMASADD > 0).OR.(NLOC_DMG%IMOD > 0)) THEN
        DO I=LFT,LLT
          J = IPART(I+NFT)
C         PART_AREA(J) = PART_AREA(J) + AREA(I)
          ELE_AREA(I+NFT) = AREA(I)
          IF (GBUF%G_AREA > 0) GBUF%AREA(I) = AREA(I)
        ENDDO
      ENDIF
C------------
!
!     Initialize element temperature from /initemp
!      
      IF (JTHE == 0 .and. GLOB_THERM%NINTEMP > 0) THEN
        CALL INITEMP_SHELL(ELBUF_STR,TEMP,NEL,NUMNOD,NUMELC,4,NIXC,IXC)    
      END IF
C------------
      CALL CINMAS(X,XREFC(1,1,NFT+1),IXC,GEO,PM,
     .            XMAS,IN,THKE,IHBE,PARTSAV,
     .            V,IPART(NFT+1),MSC(NFT+1),INC(NFT+1),AREA    , 
     .            I8MI ,IGEO  ,ETNOD ,IMAT ,IPROP,NSHNOD ,STC(NFT+1),
     .            SH4TREE ,MCP  , MCPS(NFT+1)  ,TEMP    ,
     .            MS_LAYER, ZI_LAYER,MS_LAYERC,ZI_LAYERC,
     .            MSZ2C,ZPLY,ISUBSTACK,NLAY,ELBUF_STR,STACK,
     .            GBUF%THK_I,RNOISE   ,DRAPE       ,GLOB_THERM%NINTEMP,
     .            PERTURB,IX1     ,IX2      ,IX3     ,IX4     ,
     .            IDRAPE  ,DRAPEG%INDX)
C-----------------------------------------------
      IF (MTN == 1 .OR. MTN == 3 .OR. MTN == 23 .OR. MTN == 91) NPT = 0
C---------------------------
      CALL CDERII(PX1G,PX2G,PY1G,PY2G,
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .            E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .            X2L ,X3L ,X4L ,Y2L ,Y3L ,Y4L )
      CALL CNDLENI(PM      ,GEO     ,STIFN   ,STIFR   ,IXC(1,NFT+1),
     .            THKE     ,IHBE    ,IGEO    ,SH4TREE ,ALDT  ,
     .            BUFMAT   ,IPM     ,NLAY    ,STACK%PM,ISUBSTACK,
     .            STRC(NFT+1),AREA  ,IMAT    ,IPROP   ,DTEL     , 
     .            X2L ,X3L ,X4L ,Y2L ,Y3L ,Y4L ,
     .            STACK%IGEO ,GROUP_PARAM)
      CALL C1BUF3(GEO,GBUF%THK,GBUF%OFF,THKE,KSH4TREE,SH4TREE)
c---------------------------
      IF (IXFEM > 0) THEN
        DO IXEL=1,NXEL
          CALL C1BUF3(GEO,XFEM_STR(NG,IXEL)%GBUF%THK,
     .                XFEM_STR(NG,IXEL)%GBUF%OFF,THKE,KSH4TREE,SH4TREE)
          DO I=LFT,LLT
            XFEM_STR(NG,IXEL)%GBUF%THK(I) = THKE(I)
            XFEM_STR(NG,IXEL)%GBUF%OFF(I) = -ONE
          END DO
        ENDDO
      ENDIF
c---------------------------
      IF (MTN == 35) THEN
        CALL CM35IN3(ELBUF_STR,THKE,AREA,NEL,NLAY,
     .               NPTR,NPTS,NPTT,IGTYP)
      ENDIF
C      
       IF (( ISIGSH/=0 .OR. ITHKSHEL == 2) .and. MPT>0) THEN
           CALL LAYINI1(
     .        ELBUF_STR  ,LFT        ,LLT        ,GEO        ,IGEO      ,
     .        MAT        ,PID        ,MATLY      ,POSLY      ,IGTYP     ,
     .        NLAY       ,MPT        ,ISUBSTACK  ,STACK      ,DRAPE     ,
     .        NFT        ,GBUF%THK   ,NEL        ,IDRAPE     ,SCDRAPE    ,
     .        DRAPEG%INDX)
        END IF
c---------------------------
c     Loop over Gauss points
c---------------------------
      DO IS = 1,NPTS
        DO IR = 1,NPTR
          IPG = NPTR*(IS-1) + IR
          PTF = (IPG-1)*LENF
          PTM = (IPG-1)*LENM
          PTS = (IPG-1)*LENS
c

          CALL CMAINI3(ELBUF_STR,PM       ,GEO      ,NEL       ,NLAY     ,
     .                 SKEW     ,IGEO     ,IXC(1,NFT+1),NIXC   ,NUMELC   ,
     .                 NSIGSH   ,SIGSH    ,PTSHEL   ,IGTYP     ,IORTHLOC ,
     .                 IPM      ,PROPID   ,ALDT     ,MAT_PARAM,
     .                 IR       ,IS       ,ISUBSTACK,STACK     ,IREP     ,
     .                 DRAPE    ,SH4ANG(NFT+1),GEO_STACK,IGEO_STACK,
     .                 IGMAT    ,IMAT     ,IPROP   ,NUMMAT,
     .                 X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                 Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                 E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,X ,
     .                 NPT_ALL,IDRAPE ,SCDRAPE , DRAPEG%INDX)
c
          IF (( ISIGSH/=0 .OR. ITHKSHEL == 2) .and. IHBE == 11) THEN
            IF (MPT>0) 
     .       CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,LFT    ,LLT    ,
     .             NLAY     ,IREP    ,NEL     ,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             IDRAPE, IGTYP)
           CALL CSIGINI4(ELBUF_STR,IHBE     ,
     1          LFT      ,LLT      ,NFT      ,MPT      ,ISTRAIN,
     2          GBUF%THK ,GBUF%EINT,GBUF%STRPG(PTS+1),GBUF%HOURG,
     3          GBUF%FORPG(PTF+1),GBUF%MOMPG(PTM+1),SIGSH   ,NSIGSH ,NUMELC   ,
     4          IXC      ,NIXC     ,NUMSHEL   ,PTSHEL ,IGEO     ,
     5          IR       ,IS       ,IPG       ,NPG    ,GBUF%G_PLA,
     6          GBUF%PLA,THKE    ,IGTYP    ,NEL   ,ISIGSH  ,
     7          E1X   ,E2X      ,E3X      ,E1Y   ,E2Y  ,E3Y,
     8          E1Z   ,E2Z      ,E3Z     ,DIR_A  ,DIR_B,POSLY )
           IF (ITHKSHEL == 2) THEN
             DO I=LFT,LLT
               GBUF%STRA(I+JJ(1:8))=GBUF%STRA(I+JJ(1:8))+
     .                              FOURTH*GBUF%STRPG(PTS+I+JJ(1:8))
             END DO
           END IF 
          ELSEIF ( ITHKSHEL == 1 .AND. IHBE == 11 ) THEN
           CALL THICKINI(LFT     ,LLT   ,NFT    ,PTSHEL,NUMELC,
     2                   GBUF%THK,THKE  ,IXC    ,NIXC  ,NSIGSH,
     3                   SIGSH   )
          ENDIF
c
          IF (IUSER == 1. AND. MTN>=28 .AND. IHBE == 11) THEN
            NPG = 4
            CALL CUSERINI4(ELBUF_STR,
     1                   LFT     ,LLT    ,NFT   ,NEL    ,ISTRAIN  ,
     2                   SIGSH   ,NSIGSH ,NUMELC,IXC    ,NIXC     ,
     3                   NUMSHEL ,PTSHEL ,IR    ,IS     ,NPT      ,
     4                   IGTYP   ,IGEO   ,NLAY  ,NPG    ,IPG      )
          ENDIF
c
          IF (IYLDINI == 1 .AND. (MTN== 36.OR. MTN==87).AND. IHBE == 11) THEN
            NPG = 4
            CALL CMATINI4(ELBUF_STR,
     1                   LFT     ,LLT    ,NFT   ,NEL    ,ISTRAIN  ,
     2                   SIGSH   ,NSIGSH ,NUMELC,IXC    ,NIXC     ,
     3                   NUMSHEL ,PTSHEL ,IR    ,IS     ,NPT      ,
     4                   IGTYP   ,IGEO   ,NLAY  ,NPG    ,IPG      )
          ENDIF
c
        ENDDO  ! IR = 1,NPTR
      ENDDO   ! IS = 1,NPTS
c-----------------------------------------------------------------------
c     Failure model initialisation
c-----------------------------------------------------------------------
c     set element damage initialized in /FAIL/FRACTAL_DMG
      IF (FAIL_FRACTAL%NFAIL > 0) THEN
        CALL FRACTAL_DMG_INIT(ELBUF_STR,MAT_PARAM,FAIL_FRACTAL,
     .       NUMMAT ,NUMELC  ,NEL      ,NFT      ,NGL    ,ITY      )
      ENDIF
c     tag edge elements in local UVAR for /FAIL/ALTER
      IF (IFAIL > 0 .and. IDDLEVEL == 1) THEN
        CALL FAIL_WINDSHIELD_INIT(ELBUF_STR,MAT_PARAM,FAIL_BROKMANN,
     .       NEL      ,NFT      ,ITY      ,IGRSH4N  ,IGRSH3N  ,
     .       ALDT     ,THKE     ,NGL      )
      ENDIF
c
      IF (IHBE == 11) THEN
C to be checked for IGTYP = 51
        CALL CFAILINI4(ELBUF_STR,NPTR    ,NPTS    ,NPTT    ,NLAY    ,
     .                 SIGSH    ,NSIGSH  ,PTSHEL  ,RNOISE  ,PERTURB ,
     .                 MAT_PARAM,ALDT    ,THKE    )
      ELSEIF (IHBE > 20 .AND. IHBE < 29) THEN
C to be checked for IGTYP = 51
        CALL CFAILINI(ELBUF_STR,MAT_PARAM,
     .                NPTT     ,NLAY     ,SIGSH   ,NSIGSH  ,PTSHEL  ,
     .                RNOISE   ,PERTURB  ,ALDT    ,THKE    )
      ENDIF
c
C-----------------------------------------------------------------------
C     CALCUL DES DEFORMATIONS INITIALES (MEMBRANE)
c-----------------------------------------------------------------------
      IF (IHBE > 20 .AND. IHBE < 29) THEN   !   1 Gauss point
c
        IR  = 1
        IS  = 1
        NPG = 1
c----
        IF (ISTRAIN == 1 .AND. NXREF > 0) THEN
c         REFERENCE STATE, QEPH
c
          UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR  !
          CALL CNEPSINI(ELBUF_STR,MAT_PARAM(IMAT),
     .        LFT      ,LLT     ,ISMSTR     ,MTN      ,ITHK   ,
     .        PM       ,GEO     ,IXC(1,NFT+1),X ,XREFC(1,1,NFT+1),
     .        NLAY     ,GBUF%FOR ,GBUF%THK,GBUF%EINT  ,GBUF%STRA,
     .        PX1G     ,PX2G    ,PY1G       ,PY2G     ,X2S      ,
     .        Y2S      ,X3S     ,Y3S        ,X4S      ,Y4S      ,
     .        GBUF%OFF ,UVAR    ,IPM      ,IMAT     ,
     .        IGEO     ,NEL     ,DIR_A      ,DIR_B    ,GBUF%SIGI,
     .        NPF      ,TF      ,IREP       )
c
          CALL CEPSCHK(LFT, LLT,NFT, PM, GEO,IXC(1,NFT+1),
     .                 GBUF%STRA,THKE,NEL     ,CPT_ELTENS)
          IF (ISMSTR == 1 .AND. MTN==19) IPARG(9)=11
          IF (MTN==58 .AND. NPT_ALL==1) THEN
C-------IINT to say NXREF > 0      
              IPARG(36)=1
            DO I=LFT,LLT                          
              II = NFT + I            
              ELBUF_STR%GBUF%HOURG(JJ(1)+I) = XREFC(2,1,II)-XREFC(1,1,II)  
              ELBUF_STR%GBUF%HOURG(JJ(2)+I) = XREFC(2,2,II)-XREFC(1,2,II)  
              ELBUF_STR%GBUF%HOURG(JJ(3)+I) = XREFC(2,3,II)-XREFC(1,3,II)  
              ELBUF_STR%GBUF%HOURG(JJ(4)+I) = XREFC(3,1,II)-XREFC(1,1,II)  
              ELBUF_STR%GBUF%HOURG(JJ(5)+I) = XREFC(3,2,II)-XREFC(1,2,II)  
              ELBUF_STR%GBUF%HOURG(JJ(6)+I) = XREFC(3,3,II)-XREFC(1,3,II)  
              ELBUF_STR%GBUF%HOURG(JJ(7)+I) = XREFC(4,1,II)-XREFC(1,1,II)  
              ELBUF_STR%GBUF%HOURG(JJ(8)+I) = XREFC(4,2,II)-XREFC(1,2,II)  
              ELBUF_STR%GBUF%HOURG(JJ(9)+I) = XREFC(4,3,II)-XREFC(1,3,II)  
            ENDDO                                 
           END IF
c
        ELSEIF (ISMSTR == 11 .OR. (ISMSTR==1 .AND. MTN==19) ) THEN
c
          CALL CSMS11_INI(
     .            LFT      ,LLT      ,IXC(1,NFT+1),X      ,
     .            X2S,Y2S,X3S,Y3S,X4S,Y4S)
        ENDIF
c----
        IF (ISMSTR == 10 ) THEN
          DO I=LFT,LLT                          
            II = NFT + I            
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X(1,IXC(3,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = X(2,IXC(3,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X(3,IXC(3,II))-X(3,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = X(1,IXC(4,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X(2,IXC(4,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = X(3,IXC(4,II))-X(3,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(7)+I) = X(1,IXC(5,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(8)+I) = X(2,IXC(5,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(9)+I) = X(3,IXC(5,II))-X(3,IXC(2,II))  
          ENDDO                                 
        ELSEIF (ISMSTR == 11 .OR. (ISMSTR==1 .AND. MTN==19) ) THEN
          DO I=LFT,LLT                          
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I)  
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = Y2S(I)  
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X3S(I)  
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = Y3S(I)  
            ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X4S(I)  
            ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = Y4S(I)  
          ENDDO                                 
        ENDIF
c---
        IF (ISIGSH/=0 .OR. ITHKSHEL == 2) THEN
          IPG = 0
            IF (MPT>0) 
     .       CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,LFT    ,LLT    ,
     .             NLAY     ,IREP    ,NEL     ,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             IDRAPE, IGTYP)
          CALL CSIGINI4(ELBUF_STR,IHBE   ,
     1         LFT     ,LLT    ,NFT      ,MPT      ,ISTRAIN,
     2         GBUF%THK,GBUF%EINT,GBUF%STRA,GBUF%HOURG,
     3         GBUF%FOR,GBUF%MOM,SIGSH    ,NSIGSH ,NUMELC  ,
     4         IXC    ,NIXC     ,NUMSHEL ,PTSHEL ,IGEO     ,
     5         IR     ,IS       ,IPG      ,NPG   ,GBUF%G_PLA,
     6         GBUF%PLA,THKE    ,IGTYP    ,NEL   ,ISIGSH  ,
     7          E1X   ,E2X      ,E3X      ,E1Y   ,E2Y  ,E3Y,
     8          E1Z   ,E2Z      ,E3Z     ,DIR_A  ,DIR_B,POSLY )
C--------to fill GBUF%STRPG     
         IF (ITHKSHEL == 2.AND.GBUF%G_STRPG>GBUF%G_STRA) 
     1     CALL CSTRAINI4(LFT   ,LLT    ,NFT    ,NEL    ,NUMSHEL,
     2           ISTRAIN,GBUF%STRPG,SIGSH ,NSIGSH ,NUMELC ,
     4           IXC    ,NIXC   ,PTSHEL   ,THKE   ,GBUF%STRA,
     7           E1X    ,E2X   ,E3X   ,E1Y   ,E2Y  ,E3Y,
     8           E1Z    ,E2Z   ,E3Z   )
        ELSEIF ( ITHKSHEL == 1 ) THEN
           CALL THICKINI(LFT     ,LLT   ,NFT    ,PTSHEL,NUMELC,
     2                   GBUF%THK,THKE  ,IXC    ,NIXC  ,NSIGSH,
     3                   SIGSH   )
        ENDIF
        IF (IUSER == 1 .AND. MTN >= 29) THEN
          CALL CUSERINI(ELBUF_STR,
     1         LFT    ,LLT    ,NFT      ,NEL   , NPT   ,
     2         ISTRAIN,SIGSH  ,NUMELC   ,IXC    ,NIXC  ,
     3         NSIGSH ,NUMSHEL,PTSHEL   ,IR    ,IS     ,
     4         NLAY   )
        ENDIF
C-----
        IF (IYLDINI == 1 .AND. (MTN== 36.OR. MTN==87)) THEN
          CALL CMATINI(ELBUF_STR,
     1         LFT    ,LLT    ,NFT      ,NEL   , NPT   ,
     2         ISTRAIN,SIGSH  ,NUMELC   ,IXC    ,NIXC  ,
     3         NSIGSH ,NUMSHEL,PTSHEL   ,IR    ,IS     ,
     4         NLAY   )
        ENDIF
c
c-----
      ELSEIF (IHBE == 11) THEN  ! Batoz, NPG = 4
c-----
c
        IF (ISTRAIN == 1 .AND. NXREF > 0) THEN
c         REFERENCE STATE, Batoz
c
          IR = 1
          IS = 1     
c         using only one pt, suppose constant  
          UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR  ! loi 58 => nlay=1
c
          CALL CNEPSINI(ELBUF_STR,MAT_PARAM(IMAT),
     .       LFT     ,LLT      ,ISMSTR     ,MTN      ,ITHK      ,
     .       PM      ,GEO      ,IXC(1,NFT+1),X       ,XREFC(1,1,NFT+1),
     .       NLAY    ,GBUF%FOR ,GBUF%THK   ,GBUF%EINT,GBUF%STRA ,
     .       PX1G    ,PX2G     ,PY1G       ,PY2G     ,X2S       ,
     .       Y2S     ,X3S      ,Y3S        ,X4S      ,Y4S       ,
     .       GBUF%OFF,UVAR     ,IPM        ,IMAT      ,
     .       IGEO    ,NEL      ,DIR_A      ,DIR_B    ,GBUF%SIGI ,
     .       NPF     ,TF       ,IREP      )
c          
          IF (ISMSTR /= 4) THEN 
            DO I=LFT,LLT                         
              ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I) 
              ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = Y2S(I) 
              ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X3S(I) 
              ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = Y3S(I) 
              ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X4S(I) 
              ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = Y4S(I) 
            ENDDO                                
          ENDIF                                  
c
          CALL CEPSCHK(LFT, LLT,NFT, PM, GEO,IXC(1,NFT+1),
     .                 GBUF%STRA,THKE,NEL     ,CPT_ELTENS)
          IF (ISMSTR == 1 .AND. MTN==19) IPARG(9)=11
c-----------------------------------------------------------
          DO IS =1,NPTS
            DO IR =1,NPTR
              IPG = NPTR*(IS-1) + IR
              PTF = (IPG-1)*LENF
              PTM = (IPG-1)*LENM
              PTS = (IPG-1)*LENS
              DO I=LFT,LLT
                GBUF%FORPG(PTF+JJ(1)+I) = GBUF%FOR(JJ(1)+I)
                GBUF%FORPG(PTF+JJ(2)+I) = GBUF%FOR(JJ(2)+I)
                GBUF%FORPG(PTF+JJ(3)+I) = GBUF%FOR(JJ(3)+I)
!
                GBUF%MOMPG(PTM+JJ(1)+I) = GBUF%MOM(JJ(1)+I)
                GBUF%MOMPG(PTM+JJ(2)+I) = GBUF%MOM(JJ(2)+I)
                GBUF%MOMPG(PTM+JJ(3)+I) = GBUF%MOM(JJ(3)+I)
!
                GBUF%STRPG(PTS+JJ(1)+I) = GBUF%STRA(JJ(1)+I)
                GBUF%STRPG(PTS+JJ(2)+I) = GBUF%STRA(JJ(2)+I)
                GBUF%STRPG(PTS+JJ(3)+I) = GBUF%STRA(JJ(3)+I)
              ENDDO
              DO J=1,NLAY
                IF (ELBUF_STR%BUFLY(J)%ILAW == 58) THEN
                  DO K = 1,ELBUF_STR%BUFLY(J)%NPTT
                    UVAR => ELBUF_STR%BUFLY(J)%MAT(IR,IS,K)%VAR
                    NUVAR = ELBUF_STR%BUFLY(J)%NVAR_MAT
                    DO I=1,NEL*NUVAR
                      UVAR(I) = ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR(I)
                    ENDDO
                  ENDDO
                END IF
              ENDDO
            ENDDO  !  DO IR =1,NPTR
          ENDDO  !  DO IS =1,NPTS
C       
C---------- ISMSTR == 10 in global system :9 + 9 place  + 3   
        ELSEIF (ISMSTR == 10 ) THEN
          DO I=LFT,LLT                          
            II = NFT + I            
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X(1,IXC(3,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = X(2,IXC(3,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X(3,IXC(3,II))-X(3,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = X(1,IXC(4,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X(2,IXC(4,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = X(3,IXC(4,II))-X(3,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(7)+I) = X(1,IXC(5,II))-X(1,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(8)+I) = X(2,IXC(5,II))-X(2,IXC(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(9)+I) = X(3,IXC(5,II))-X(3,IXC(2,II))  
          ENDDO                                 
        ELSEIF (ISMSTR == 11 .OR. (ISMSTR==1 .AND. MTN==19) ) THEN
C to be checked for IGTYP = 51
          CALL CSMS11_INI(
     .           LFT      ,LLT     ,IXC(1,NFT+1),X      ,X2S     ,
     .           Y2S      ,X3S     ,Y3S         ,X4S    ,Y4S     )
          DO I=LFT,LLT                          
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = Y2S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X3S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = Y3S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X4S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = Y4S(I)
          ENDDO                                 
        ENDIF
c
      ENDIF  ! IHBE
C-------------------------------------------
c     CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
         IF (IGTYP /= 1  .AND. IGTYP /= 7  .AND.
     .       IGTYP /= 9  .AND. IGTYP /= 10 .AND.
     .       IGTYP /= 11 .AND. IGTYP /= 0  .AND.
     .       IGTYP /= 16 .AND. IGTYP /= 17 .AND.
     .       IGTYP /= 51 .AND. IGTYP /= 52 ) THEN
           CALL ANCMSG(MSGID=25,
     .                 ANMODE=ANINFO,
     .                 MSGTYPE=MSGERROR,
     .                 I1=PROPID,
     .                 C1=TITR,
     .                 I2=IPROP)
         ENDIF
       NDEPAR=NUMELS+NFT
       DO I=LFT,LLT
c         IGTYP = GEO(12,IXC(6,I+NFT))
         DTELEM(NDEPAR+I) = DTEL(I)
       END DO
c-----------
      IF (IXFEM > 0) THEN
        CALL CBUFXFE(ELBUF_STR,XFEM_STR ,ISUBSTACK,STACK    ,
     .               IGEO     ,GEO      ,LFT      ,LLT      ,MAT     ,
     .               PID      ,NPT      ,NPTT     ,NLAY     ,IR      ,
     .               IS       ,IXFEM    ,MTN      ,NG)
      ENDIF
C------------
      ! Compute the initial volume
      DO I=LFT,LLT
        IF (GBUF%G_VOL > 0) GBUF%VOL(I) = AREA(I)*GBUF%THK(I)
      ENDDO
      IF (IXFEM > 0) THEN
        DO IXEL=1,NXEL
          DO I=LFT,LLT
            IF (XFEM_STR(NG,IXEL)%GBUF%G_VOL > 0) 
     .      XFEM_STR(NG,IXEL)%GBUF%VOL(I) = AREA(I)*GBUF%THK(I)
          END DO
        ENDDO
      ENDIF
c-----------
      DEALLOCATE(DIR_A)
      DEALLOCATE(DIR_B)      
      DEALLOCATE(MATLY)
      DEALLOCATE(POSLY)

      RETURN
      END
