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
Chd|  CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C1BUF3                        source/elements/shell/coque/c1buf3.F
Chd|        CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|        CCOORI                        source/elements/shell/coque/ccoori.F
Chd|        CDERII                        source/elements/shell/coque/cderii.F
Chd|        CDLENI                        source/elements/shell/coque/cdleni.F
Chd|        CEPSCHK                       source/elements/shell/coque/cepsini.F
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CFAILINI                      source/elements/shell/coque/cfailini.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CM27IN3                       source/materials/mat/mat027/cm27in3.F
Chd|        CM35IN3                       source/materials/mat/mat035/cm35in3.F
Chd|        CM58IN3                       source/materials/mat/mat058/cm58in3.F
Chd|        CMATINI                       source/materials/mat_share/cmatini.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        CORTHDIR                      source/elements/shell/coque/corthdir.F
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSMS11_INI                    source/elements/shell/coque/cinit3.F
Chd|        CUSERINI                      source/elements/shell/coque/cuserini.F
Chd|        CVEOK3                        source/elements/shell/coque/cveok3.F
Chd|        FAIL_WINDSHIELD_INIT          source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        LAW158_INIT                   source/materials/mat/mat158/law158_init.F
Chd|        LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|        THICKINI                      source/elements/shell/coqueba/thickini.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        GROUP_PARAM_MOD               ../common_source/modules/mat_elem/group_param_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CINIT3(ELBUF_STR,IXC     ,PM      ,X       ,GEO     ,
     2                  XMAS     ,IN      ,NVC     ,DTELEM  ,IGRSH4N ,
     3                  XREFC    ,NEL     ,ITHK    ,IHBE    ,IGRSH3N ,
     4                  THK      ,ISIGSH  ,SIGSH   ,STIFN   ,STIFR   ,
     5                  PARTSAV  ,V       ,IPART   ,MSC     ,INC     ,
     8                  SKEW     ,IPARG   ,I8MI    ,NSIGSH  ,IGEO    ,
     9                  IUSER    ,ETNOD   ,NSHNOD  ,STC     ,PTSHEL  ,
     A                  IPM      ,BUFMAT  ,SH4TREE ,MCP     ,MCPS    ,
     B                  TEMP     ,CPT_ELTENS,PART_AREA,ITAGN,ITAGE   ,
     C                  IXFEM    ,NPF     ,TF      ,XFEM_STR,ISUBSTACK,
     D                  STACK    ,RNOISE  ,DRAPE  ,SH4ANG  ,
     E                  GEO_STACK,IGEO_STACK,STRC  ,PERTURB ,IYLDINI ,
     F                  ELE_AREA ,NG      ,GROUP_PARAM      ,NLOC_DMG,
     G                  IDRAPE   ,DRAPEG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE GROUP_PARAM_MOD
      USE NLOCAL_REG_MOD    
      USE DRAPE_MOD        
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC,NEL,ITHK,IHBE,ISIGSH,IXFEM,NSIGSH,IUSER,IYLDINI
      INTEGER IXC(NIXC,*),IPART(*),IPARG(*),IGEO(NPROPGI,*), NSHNOD(*),
     .  PTSHEL(*),IPM(NPROPMI,*), SH4TREE(*),ITAGN(*),ITAGE(*),NPF(*),
     .  ISUBSTACK,IGEO_STACK(*),PERTURB(NPERTURB),NG,IDRAPE
      INTEGER *8 I8MI(6,*)
      my_real
     .   PM(NPROPM,*), X(3,*), GEO(NPROPG,*), XMAS(*), IN(*),
     .   DTELEM(*), XREFC(4,3,*),THK(*), SIGSH(NSIGSH,*),
     .   STIFN(*),STIFR(*),PARTSAV(20,*), V(*) ,MSC(*) ,INC(*),
     .   SKEW(LSKEW,*), ETNOD(*), STC(*),BUFMAT(*),MCP(*),MCPS(*),
     .   TEMP(*),PART_AREA(*),TF(*),RNOISE(*),
     .   SH4ANG(*),GEO_STACK(*),STRC(*),ELE_AREA(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,*):: XFEM_STR
      !   when XFEM is ON, XFEM_STR's dimension = NGROUP,NXEL
      TYPE (STACK_PLY) :: STACK
      TYPE (GROUP_PARAM_)  :: GROUP_PARAM
      TYPE (NLOCAL_STR_) :: NLOC_DMG
      TYPE (DRAPE_) :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*nchartitle, TITR,TITR1
      INTEGER I,J,N,IP,NDEPAR,IGTYP,NUVAR,ID,NLAY,II,IREP,IPROP,NUPARAM,
     .   IL,IR,IS,IT,CPT_ELTENS,IUN,NPTR,NPTS,NPTT,IXEL,ILAW,IMAT,IFAIL,
     .   IGMAT,JJ(9),NPT_ALL,MPT,LAYNPT_MAX,LAY_MAX
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4,IORTHLOC,MAT,PID,NGL
      my_real :: BID
      my_real, DIMENSION(MVSIZ) :: PX1G,PX2G,PY1G,PY2G,AREA,ALDT,
     .                             DT,VX,VY,VZ,
     .                             X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                             E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                             X2S,Y2S,X3S,Y3S,X4S,Y4S,
     .                             X2L,X3L,X4L,Y2L,Y3L,Y4L
      my_real, DIMENSION(:) ,POINTER :: UVAR,DIR1,DIR2
      my_real, ALLOCATABLE, DIMENSION(:) :: DIR_A,DIR_B
      my_real, DIMENSION(:), ALLOCATABLE :: PHI1,PHI2,COOR1,COOR2,COOR3,COOR4
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX)
      my_real
     .   POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(L_BUFEL_DIR_), POINTER :: LBUF_DIR
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
c
      IMAT  = IXC(1,1+NFT)         ! mat N
      IPROP = IXC(NIXC-1,1+NFT)    ! property N
      IGTYP = IGEO(11,IPROP)
      ID    = IGEO(1,IPROP)
      IGMAT = IGEO(98,IXC(6,1+NFT))
      IREP  = IPARG(35)
      IFAIL = IPARG(43)
c      
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
      VX   = ZERO
      VY   = ZERO
      VZ   = ZERO
      IORTHLOC = 0
      BID    = ZERO
C
      IUN = 1
      IR  = 1
      IS  = 1
      NLAY = ELBUF_STR%NLAY
      NXEL = ELBUF_STR%NXEL
      NPTT = ELBUF_STR%NPTT
      IDRAPE = ELBUF_STR%IDRAPE
      NPT_ALL = 0
      IF(IGTYP == 51 .OR. IGTYP == 52) THEN
          DO IL=1,NLAY
            NPT_ALL = NPT_ALL + ELBUF_STR%BUFLY(IL)%NPTT
          ENDDO
      ENDIF   
      MPT  = MAX(1,NPT_ALL)
      IF(NPT_ALL == 0 .OR. IDRAPE == 0) NPT_ALL = NLAY
C----- NPT=0 for some cases      
      IF((IGTYP == 51 .OR. IGTYP == 52) .AND. IDRAPE > 0) THEN
         ALLOCATE(PHI1(MVSIZ*NPT_ALL))
         ALLOCATE(PHI2(NVSIZ*NPT_ALL))
         ALLOCATE(DIR_A(NPT_ALL*NEL*2))
         ALLOCATE(DIR_B(NPT_ALL*NEL*2))
         PHI1  = ZERO
         PHI2  = ZERO
         DIR_A = ZERO
         DIR_B = ZERO
      ELSE
         ALLOCATE(PHI1(NLAY*MVSIZ))
         ALLOCATE(PHI2(NLAY*MVSIZ))
         ALLOCATE(DIR_A(NLAY*NEL*2))
         ALLOCATE(DIR_B(NLAY*NEL*2))
         PHI1  = ZERO
         PHI2  = ZERO
         DIR_A = ZERO
         DIR_B = ZERO
         NPT_ALL = NLAY
      ENDIF
      ALLOCATE(COOR1(NPT_ALL*MVSIZ))
      COOR1 = ZERO
      ALLOCATE(COOR2(NPT_ALL*MVSIZ))
      COOR2 = ZERO
      ALLOCATE(COOR3(NPT_ALL*MVSIZ))
      COOR3 = ZERO
      ALLOCATE(COOR4(NPT_ALL*MVSIZ))
      COOR4 = ZERO
C      
      IF (IPARG(6) == 0.OR.NPT==0) MPT=0
C
      DO J=1,9
        JJ(J) = NEL*(J-1)
      ENDDO
C
      DO I=LFT,LLT
        N = I+NFT
        MAT(I) = IXC(1,N)
        PID(I) = IXC(6,N)
      ENDDO
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
C
      CALL CCOORI(X,XREFC(1,1,NFT+1),IXC(1,NFT+1),
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .            IX1 ,IX2 ,IX3 ,IX4 ,NGL )
c
      CALL CVEOK3(NVC,4,IX1,IX2,IX3,IX4)
c--------------------------------------
      CALL CEVECI(LFT,LLT ,AREA,
     .           X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .           Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .           E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
C------------
C Tags total area of the part (needed in /ADMAS for shells)
C------------
      IF ((IMASADD > 0).OR.(NLOC_DMG%IMOD > 0)) THEN
        DO I=LFT,LLT
          IP = IPART(I+NFT)
C         PART_AREA(IP) = PART_AREA(IP) + AREA(I)
          ELE_AREA(I+NFT) = AREA(I)
          IF (GBUF%G_AREA > 0) GBUF%AREA(I) = AREA(I)
        ENDDO
      ENDIF
C------------
      CALL CINMAS(X     ,XREFC(1,1,NFT+1),IXC       ,GEO       ,PM,
     .            XMAS  ,IN              ,THK       ,IHBE       ,PARTSAV,
     .            V     ,IPART(NFT+1)    ,MSC(NFT+1),INC(NFT+1) ,AREA    ,
     .            I8MI  ,IGEO       ,ETNOD ,IMAT       ,IPROP      ,
     .            NSHNOD ,STC(NFT+1)     ,SH4TREE   ,MCP    ,MCPS(NFT+1),
     .            TEMP   ,BID            ,BID       ,BID        ,BID,
     .            BID    ,BID            ,ISUBSTACK ,NLAY  ,ELBUF_STR,
     .            STACK  ,GBUF%THK_I     ,RNOISE    ,DRAPE   ,
     .            PERTURB,IX1            ,IX2       ,IX3     ,IX4       ,
     .            IDRAPE ,DRAPEG%INDX)
C-----------------------------------------------
      CALL CDERII(PX1G,PX2G,PY1G,PY2G,
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .            E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .            X2L ,X3L ,X4L ,Y2L ,Y3L ,Y4L )
      CALL CDLENI(PM      ,GEO     ,STIFN   ,STIFR   ,IXC(1,NFT+1),
     .            PX1G    ,PX2G    ,PY1G    ,PY2G    ,THK         ,
     .            IGEO    ,DT      ,SH4TREE ,ALDT    ,BUFMAT      ,
     .            IPM     ,NLAY    ,STACK%PM,ISUBSTACK,STRC(NFT+1),
     .            AREA    ,IMAT    ,IPROP   ,
     .            X2L     ,X3L     ,X4L     ,Y2L     ,Y3L     ,Y4L ,
     .            STACK%IGEO,GROUP_PARAM)
      CALL C1BUF3(GEO,GBUF%THK,GBUF%OFF,THK,KSH4TREE,SH4TREE)
C-----------------------------------------------
      IF (IXFEM > 0) THEN
        DO IXEL=1,NXEL
          DO I=LFT,LLT
            XFEM_STR(NG,IXEL)%GBUF%THK(I) = THK(I)
            XFEM_STR(NG,IXEL)%GBUF%OFF(I) = -ONE
          END DO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
c     PHI, COOR used only with dimension(NLAY,MVSIZ); !! now corrected
      CALL CORTHINI(
     .   LFT        ,LLT       ,NFT       ,NLAY       ,NUMELC     ,
     .   NSIGSH     ,NIXC      ,IXC(1,NFT+1),IGEO     ,GEO        ,
     .   SKEW       ,SIGSH     ,PTSHEL    ,PHI1       ,PHI2       ,
     .   VX         ,VY        ,VZ        ,COOR1      ,COOR2      ,
     .   COOR3      ,COOR4     ,IORTHLOC  ,ISUBSTACK  ,STACK      ,
     .   IREP       ,ELBUF_STR ,DRAPE     ,SH4ANG(NFT+1),X        ,
     .   GEO_STACK  ,IGEO_STACK,E3X       ,E3Y        ,E3Z        ,
     .   GBUF%BETAORTH,X1      ,X2        ,Y1         ,Y2         ,
     .   Z1         ,Z2        ,NEL       ,GBUF%G_ADD_NODE,GBUF%ADD_NODE,
     .   NPT_ALL    ,IDRAPE    ,DRAPEG%INDX) 
c---     
C-----
      IF(IGTYP == 51 .OR. IGTYP == 52 .AND. IGMAT > 0) THEN
c
        CALL CORTHDIR(ELBUF_STR,
     .                IGEO     ,GEO       ,VX         ,VY      ,VZ       ,
     .                PHI1     ,PHI2      ,COOR1      ,COOR2   ,COOR3    ,
     .                COOR4    ,IORTHLOC  ,NLAY       ,IREP    ,ISUBSTACK,
     .                STACK    ,GEO_STACK ,IGEO_STACK ,IR      ,IS       ,
     .                NEL      ,IMAT      ,IPROP      ,     
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                NPT_ALL  ,IDRAPE)
c
      ELSEIF (MTN == 27) THEN
        CALL CM27IN3(ELBUF_STR,
     .               GEO ,IGEO ,PM ,IPM ,IXC(1,1+NFT) ,NIXC,
     .               NLAY,IR   ,IS ,IMAT )
      ELSEIF (MTN == 35) THEN
        NPTR = ELBUF_STR%NPTR
        NPTS = ELBUF_STR%NPTS
        NPTT = ELBUF_STR%NPTT
        CALL CM35IN3(ELBUF_STR,THK,AREA,NEL,NLAY,
     .               NPTR,NPTS,NPTT,IGTYP)
      ELSEIF (MTN==15 .or. MTN==19 .or. MTN==25 .or. MTN>=28) THEN
        IF (MTN == 19 .AND. IGTYP /= 9) THEN
          CALL ANCMSG(MSGID=5,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=IGEO(1,IXC(NIXC-1,NFT+1)))
        ENDIF
c
        CALL CORTHDIR(ELBUF_STR,
     .                IGEO     ,GEO       ,VX         ,VY      ,VZ       ,
     .                PHI1     ,PHI2      ,COOR1      ,COOR2   ,COOR3    ,
     .                COOR4    ,IORTHLOC  ,NLAY       ,IREP    ,ISUBSTACK,
     .                STACK    ,GEO_STACK ,IGEO_STACK ,IR      ,IS       ,
     .                NEL      ,IMAT      ,IPROP      ,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .                NPT_ALL  ,IDRAPE)
      ENDIF
c
      IF ((MTN == 58 .or. MTN == 158) .AND. 
     .    IGTYP /= 16 .AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN  
          CALL ANCMSG(MSGID=658,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=MTN,
     .                I3=IGTYP)
      ELSEIF (MTN == 58 .or. MTN == 158 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
C
        IF (IDRAPE == 0) THEN 
          DO IL = 1,NLAY
            NPTT =  ELBUF_STR%BUFLY(IL)%NPTT
            IMAT =  ELBUF_STR%BUFLY(IL)%IMAT
            ILAW =  ELBUF_STR%BUFLY(IL)%ILAW
            NUPARAM = IPM(9,IMAT) 
            IF (ILAW == 58) THEN
              DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
              DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                CALL CM58IN3(
     .               IREP     ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .               BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
              ENDDO
            ELSE IF (ILAW == 158) THEN
              DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
              DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                CALL LAW158_INIT(
     .               NUPARAM  ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .               BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
              ENDDO
            ENDIF ! IF (ILAW == 58) THEN
          ENDDO ! DO IL = 1,NLAY
        ELSE
          DO IL = 1,NLAY
            NPTT =  ELBUF_STR%BUFLY(IL)%NPTT
            IMAT =  ELBUF_STR%BUFLY(IL)%IMAT
            ILAW =  ELBUF_STR%BUFLY(IL)%ILAW
            NUPARAM = IPM(9,IMAT) 
            IF (ILAW == 58) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                LBUF_DIR => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)    
                DIR1 => LBUF_DIR%DIRA
                DIR2 => LBUF_DIR%DIRB
                CALL CM58IN3(
     .               IREP     ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .               BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
              ENDDO
            ELSE IF (ILAW == 158) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                LBUF_DIR => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)    
                DIR1 => LBUF_DIR%DIRA
                DIR2 => LBUF_DIR%DIRB
                CALL LAW158_INIT(
     .               NUPARAM  ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .               BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
              ENDDO
            ENDIF ! IF (ILAW == 58) THEN
          ENDDO ! DO IL = 1,NLAY      
        ENDIF 
      ENDIF
C-----------------------------------------------------------------------
C     CALCUL DES CONTRAINTES INITIALES
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ISIGSH/=0 .OR. ITHKSHEL == 2) THEN
C to be checked for IGTYP = 51 : ok
        IF (MPT > 0) THEN
           CALL LAYINI1(
     .        ELBUF_STR  ,LFT        ,LLT        ,GEO        ,IGEO      ,
     .        MAT        ,PID        ,MATLY      ,POSLY      ,IGTYP     ,
     .        NLAY       ,MPT        ,ISUBSTACK  ,STACK      ,DRAPE     ,
     .        NFT        ,GBUF%THK   ,NEL        ,IDRAPE     ,SCDRAPE   ,    
     .        DRAPEG%INDX)
           CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,LFT    ,LLT    ,
     .             NLAY     ,IREP    ,NEL     ,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z , 
     .             IDRAPE, IGTYP )
        END IF
        CALL CSIGINI(ELBUF_STR ,
     1       LFT     ,LLT      ,NFT      ,MPT       ,ISTRAIN   ,
     2       GBUF%THK,GBUF%EINT,GBUF%STRA,GBUF%HOURG,GBUF%PLA  ,
     3       GBUF%FOR,GBUF%MOM ,SIGSH    ,NLAY      ,GBUF%G_HOURG,
     4       NUMELC  ,IXC      ,NIXC     ,NSIGSH    ,NUMSHEL   ,
     5       PTSHEL  ,IGEO     ,THK      ,NEL       ,E1X       , 
     6       E2X     ,E3X      ,E1Y      ,E2Y       ,E3Y       ,
     7       E1Z     ,E2Z      ,E3Z      ,ISIGSH    ,DIR_A     ,
     8       DIR_B   ,POSLY    ,IGTYP    )    
      ELSEIF ( ITHKSHEL == 1 ) THEN
        CALL THICKINI(LFT     ,LLT   ,NFT    ,PTSHEL,NUMELC,
     2                GBUF%THK,THK   ,IXC    ,NIXC  ,NSIGSH,
     3                SIGSH   )
      ENDIF
C-----------------------------------------------------------------------
C     CALCUL DES DEFORMATIONS INITIALES (MEMBRANE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ISTRAIN == 1 .AND. NXREF > 0) THEN
        UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR
        CALL CEPSINI(ELBUF_STR  ,
     .       LFT      ,LLT      ,ISMSTR      ,MTN          ,ITHK    ,
     .       PM       ,GEO      ,IXC(1,NFT+1),X    ,XREFC(1,1,NFT+1),
     .       GBUF%FOR ,GBUF%THK ,GBUF%EINT   ,GBUF%STRA   ,NLAY     ,
     .       PX1G     ,PX2G     ,PY1G        ,PY2G        ,X2S      ,
     .       Y2S      ,X3S      ,Y3S         ,X4S         ,Y4S      ,
     .       UVAR     ,BUFMAT   ,IPM         ,IGEO        ,IMAT     ,
     .       SKEW     ,NEL      ,DIR_A       ,DIR_B       ,GBUF%SIGI,
     .       NPF      ,TF       ,IREP        )
c
        CALL CEPSCHK(LFT, LLT,NFT, PM, GEO,IXC(1,NFT+1),GBUF%STRA,THK,
     .       NEL,CPT_ELTENS)
        IF (ISMSTR == 1 .AND. MTN==19) IPARG(9)=11
c
      ELSEIF (ISMSTR == 11 .OR.(ISMSTR==1 .AND. MTN==19)) THEN
C       to be checked for IGTYP = 51
        CALL CSMS11_INI(LFT   ,LLT   ,IXC(1,NFT+1),X     ,
     .                  X2S   ,Y2S   ,X3S   ,Y3S  ,X4S   ,Y4S   )
      ENDIF
c----------------------------
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
      ELSEIF (ISMSTR == 11 .OR.(ISMSTR==1 .AND. MTN==19)) THEN                  
        DO I=LFT,LLT                          
          ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I)
          ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = Y2S(I)
          ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X3S(I)
          ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = Y3S(I)
          ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X4S(I)
          ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = Y4S(I)
        ENDDO                                 
      ENDIF                                   
C-----
      IF (IUSER == 1 .AND. MTN > 28) THEN
C to be checked for IGTYP = 51
        CALL CUSERINI(ELBUF_STR,
     1                LFT    ,LLT    ,NFT      ,NEL    ,NPT    ,
     2                ISTRAIN,SIGSH  ,NUMELC   ,IXC    ,NIXC   ,
     3                NSIGSH ,NUMSHEL,PTSHEL   ,IUN    ,IUN    ,
     4                NLAY   )
      ENDIF

      IF (IYLDINI == 1 .AND. (MTN== 36.OR. MTN==87))THEN
        CALL CMATINI(ELBUF_STR,
     1                LFT    ,LLT    ,NFT      ,NEL    ,NPT    ,
     2                ISTRAIN,SIGSH  ,NUMELC   ,IXC    ,NIXC   ,
     3                NSIGSH ,NUMSHEL,PTSHEL   ,IUN    ,IUN    ,
     4                NLAY   )
      ENDIF

C----------------------------------------
c Failure model initialisation
C----------------------------------------
c     tag edge elements in local UVAR for /FAIL/ALTER (XFEM)
      IF (IFAIL > 0) THEN
        CALL FAIL_WINDSHIELD_INIT(
     .       ELBUF_STR,NEL      ,NFT      ,ITY      ,NGL      ,
     .       IPM      ,BUFMAT   ,IGRSH4N  ,IGRSH3N  )
      ENDIF
C to be checked for IGTYP = 51
      CALL CFAILINI(ELBUF_STR,IXC     ,NIXC    ,NPTT    ,NLAY    ,
     .              IPM      ,SIGSH   ,NSIGSH  ,NUMELC  ,PTSHEL  ,
     .              RNOISE   ,PERTURB ,BUFMAT  ,ALDT    ,THK     )
C-------------------------------------------
C    CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
c        IGTYP=GEO(12,IXC(6,I+NFT))
        IF (IGTYP /= 0  .AND. IGTYP /= 1  .AND.
     .      IGTYP /= 9  .AND. IGTYP /= 10 .AND.
     .      IGTYP /= 11 .AND. IGTYP /= 16 .AND. 
     .      IGTYP /= 17 .AND. IGTYP /= 51 .AND. 
     .      IGTYP /= 52) THEN
          CALL ANCMSG(MSGID=25,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IPROP)
        ENDIF
      NDEPAR=NUMELS+NFT
      DO I=LFT,LLT
        DTELEM(NDEPAR+I)=DT(I)
      ENDDO
C---
      IF (IXFEM > 0) THEN
        CALL CBUFXFE(ELBUF_STR,XFEM_STR,ISUBSTACK,STACK   ,
     .               IGEO     ,GEO ,LFT  ,LLT ,MAT,
     .               PID      ,NPT ,NPTT ,NLAY,IR ,
     .               IS       ,IXFEM,MTN ,NG)
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
C---
      IF (ALLOCATED(DIR_B))    DEALLOCATE(DIR_B)                                                          
      IF (ALLOCATED(DIR_A))    DEALLOCATE(DIR_A)  
      DEALLOCATE(PHI1,PHI2,COOR1,COOR2,COOR3,COOR4)  
C-----------------------------
      RETURN
      END SUBROUTINE CINIT3      
Chd|====================================================================
Chd|  CSMS11_INI                    source/elements/shell/coque/cinit3.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CSMS11_INI(
     1           JFT    ,JLT   ,IXC   ,X      ,X2S     ,
     2           Y2S    ,X3S   ,Y3S   ,X4S    ,Y4S     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      INTEGER IXC(NIXC,*)
      my_real
     . X(3,*), X2S(*), Y2S(*), X3S(*), Y3S(*), X4S(*), Y4S(*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NIT, NT, I1, I2, I3, I4
      my_real
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) , X4(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) , Y4(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) , Z4(MVSIZ)
      my_real
     .   SUMA,S1,S2

      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .   X42(MVSIZ), Y42(MVSIZ), Z42(MVSIZ),
     .   X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ)
C---------------------------------------------------------------
       DO I=JFT,JLT
         X1(I)=ZERO
         Y1(I)=ZERO
         Z1(I)=ZERO
         X2(I)=X(1,IXC(3,I))-X(1,IXC(2,I))
         Y2(I)=X(2,IXC(3,I))-X(2,IXC(2,I))
         Z2(I)=X(3,IXC(3,I))-X(3,IXC(2,I))
         X3(I)=X(1,IXC(4,I))-X(1,IXC(2,I))
         Y3(I)=X(2,IXC(4,I))-X(2,IXC(2,I))
         Z3(I)=X(3,IXC(4,I))-X(3,IXC(2,I))
         X4(I)=X(1,IXC(5,I))-X(1,IXC(2,I))
         Y4(I)=X(2,IXC(5,I))-X(2,IXC(2,I))
         Z4(I)=X(3,IXC(5,I))-X(3,IXC(2,I))
       ENDDO
        DO I=JFT,JLT
          E1X(I) = X2(I)+X3(I)-X1(I)-X4(I)
          E1Y(I) = Y2(I)+Y3(I)-Y1(I)-Y4(I)
          E1Z(I) = Z2(I)+Z3(I)-Z1(I)-Z4(I)
C
          E2X(I) = X3(I)+X4(I)-X1(I)-X2(I)
          E2Y(I) = Y3(I)+Y4(I)-Y1(I)-Y2(I)
          E2Z(I) = Z3(I)+Z4(I)-Z1(I)-Z2(I)
C
          E3X(I) = E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
          E3Y(I) = E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
          E3Z(I) = E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
        ENDDO
C---
        DO I=JFT,JLT                         
          SUMA   = E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)  
          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
          E3X(I) = E3X(I)*SUMA                              
          E3Y(I) = E3Y(I)*SUMA                              
          E3Z(I) = E3Z(I)*SUMA                              
C
          S1     = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I) 
          S2     = E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I) 
          SUMA   = SQRT(S1/S2)                
          E1X(I) = E1X(I) + (E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I))*SUMA
          E1Y(I) = E1Y(I) + (E2Z(I)*E3X(I)-E2X(I)*E3Z(I))*SUMA
          E1Z(I) = E1Z(I) + (E2X(I)*E3Y(I)-E2Y(I)*E3X(I))*SUMA
C
          SUMA   = E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
          SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
          E1X(I) = E1X(I)*SUMA                              
          E1Y(I) = E1Y(I)*SUMA                              
          E1Z(I) = E1Z(I)*SUMA                              
C
          E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
          E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
          E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
        ENDDO
      DO I=JFT,JLT
        X2S(I) = E1X(I)*X2(I) + E1Y(I)*Y2(I) + E1Z(I)*Z2(I)
        Y2S(I) = E2X(I)*X2(I) + E2Y(I)*Y2(I) + E2Z(I)*Z2(I)
        X3S(I) = E1X(I)*X3(I) + E1Y(I)*Y3(I) + E1Z(I)*Z3(I)
        Y3S(I) = E2X(I)*X3(I) + E2Y(I)*Y3(I) + E2Z(I)*Z3(I)
        X4S(I) = E1X(I)*X4(I) + E1Y(I)*Y4(I) + E1Z(I)*Z4(I)
        Y4S(I) = E2X(I)*X4(I) + E2Y(I)*Y4(I) + E2Z(I)*Z4(I)
      ENDDO
C-----------
      RETURN
      END
