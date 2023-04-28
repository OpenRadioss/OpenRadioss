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
Chd|  CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C1BUF3                        source/elements/shell/coque/c1buf3.F
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        C3EPSCHK                      source/elements/sh3n/coque3n/c3init3.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        CDKDERII                      source/elements/sh3n/coquedk/cdkderii.F
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|        CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|        CFAILINI4                     source/elements/shell/coque/cfailini.F
Chd|        CM35IN3                       source/materials/mat/mat035/cm35in3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|        CMATINI4                      source/materials/mat_share/cmatini4.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|        CUSERINI4                     source/elements/shell/coqueba/cuserini4.F
Chd|        LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|        THICKINI                      source/elements/shell/coqueba/thickini.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CDKINIT3(ELBUF_STR,GROUP_PARAM,
     1                   IXTG  ,PM    ,X     ,GEO   ,
     2                   XMAS  ,IN    ,NVC   ,DTELEM,
     3                   XREFTG,OFFSET,NEL   ,ITHK  ,THK    ,
     4                   ISIGSH,SIGSH ,STIFN ,STIFR,PARTSAV ,
     5                   V     ,IPART ,MSTG  ,INTG ,PTG,
     8                   SKEW  ,ISH3N ,NSIGSH  ,IGEO   ,IPM   ,
     9                   IUSER ,ETNOD,NSHNOD  ,STTG   ,PTSH3N,
     A                   BUFMAT,SH3TREE ,MCP  ,MCPS  , TEMP  ,
     B                   IPARG,CPT_ELTENS,PART_AREA ,NPF, TF   ,
     C                   SH3TRIM,ISUBSTACK,STACK,RNOISE ,DRAPE,
     D                   SH3ANG,GEO_STACK,IGEO_STACK,STRTG,
     E                   PERTURB,IYLDINI,ELE_AREA,NLOC_DMG,
     G                   IDRAPE ,DRAPEG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD            
      USE STACK_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "vect01_c.inc"
#include      "scry_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   IXTG(NIXTG,*),IPART(*), OFFSET, NEL, ITHK, ISIGSH,
     .   ISH3N,NSIGSH,NLAY,NPTR,NPTS,NPTT,IL,IR,IS,IT,IYLDINI,
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),IUSER, NSHNOD(*),NPF(*),
     .   PTSH3N(*), SH3TREE(*),IPARG(*),CPT_ELTENS,SH3TRIM(*),
     .   ISUBSTACK,IGEO_STACK(*),PERTURB(NPERTURB),IDRAPE
      my_real
     .   PM(NPROPM,*),X(*),GEO(NPROPG,*),XMAS(*),
     .   IN(*),DTELEM(*), XREFTG(3,3,*),THK(*),SIGSH(NSIGSH,*),
     .   STIFN(*),STIFR(*),PARTSAV(20,*), V(*), SKEW(LSKEW,*),
     .   MSTG(*),INTG(*),PTG(3,*),ETNOD(*), STTG(*),BUFMAT(*),
     .   MCP(*),MCPS(*),TEMP(*),PART_AREA(*),TF(*),
     .   RNOISE(*),SH3ANG(*),GEO_STACK(*),STRTG(*),ELE_AREA(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (GROUP_PARAM_)  :: GROUP_PARAM
      TYPE (NLOCAL_STR_) :: NLOC_DMG
      TYPE (DRAPE_)  :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,NDEPAR,IGTYP,IMAT,IPROP,IGMAT,NVC,IHBE,NPG,MPT,
     .   PTM,PTF,PTS,NUVAR,NUVARR,ID,LENF,LENM,LENS,IREP,IPG
      INTEGER JJ(5),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .   MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ)
      INTEGER  LAYNPT_MAX,LAY_MAX,NPT_ALL
      my_real,
     .   DIMENSION(MVSIZ) :: PX2,PY2,PX3,PY3,X2S,Y2S,X3S,Y3S,
     .                       AREA,ALDT,IORTHLOC,DT
      my_real X1(MVSIZ), X2(MVSIZ), X3(MVSIZ) ,X4(MVSIZ),
     .        Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),Y4(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),Z4(MVSIZ),
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ)
      my_real, ALLOCATABLE, DIMENSION(:) :: DIR_A,DIR_B
      
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDX
      my_real,
     .   DIMENSION(:) ,POINTER  :: UVAR
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX)
      my_real
     .   POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX)
C-----------------------------------------------
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IORTHLOC = 0
      IMAT  = IXTG(1,1+NFT)          ! mat N
      IPROP = IXTG(NIXTG-1,1+NFT)    ! property N
C      IGTYP = GEO(12,IPROP)
      IGTYP = IGEO(11,IPROP)
      IGMAT = IGEO(98,IPROP)
      ID    = IGEO(1,IPROP)
      IREP  = IPARG(35)
C           
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      NPG  = NPTR*NPTS
      IF (NPT /= 0) NPT  = NPTT*NLAY
      LENF = NEL*GBUF%G_FORPG/NPG
      LENM = NEL*GBUF%G_MOMPG/NPG
      LENS = NEL*GBUF%G_STRPG/NPG
!
      DO I=1,5
        JJ(I) = NEL*(I-1)
      ENDDO
C
      DO I=LFT,LLT
        MAT(I) = IMAT
        PID(I) = IPROP
      ENDDO
C-----------------------------------------------
      CALL C3COORI(X,XREFTG(1,1,NFT+1),IXTG(1,NFT+1),NGL,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
      CALL C3VEOK3(NVC ,IX1 ,IX2 ,IX3 )
      CALL C3EVEC3(LFT ,LLT ,AREA,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .             E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             X31, Y31, Z31 ,X2L ,X3L ,Y3L )
      IF(IDRAPE > 0 ) THEN
        ALLOCATE(INDX(NUMELTG))
        INDX = 0
        INDX(1:NUMELTG) = DRAPEG%INDX(NUMELC + 1 : NUMELC + NUMELTG)
      ELSE
        ALLOCATE(INDX(0))
      ENDIF        
      CALL C3INMAS(X,XREFTG(1,1,NFT+1),IXTG,GEO,PM,XMAS,IN,THK,
     .             PARTSAV,V,IPART(NFT+1),MSTG(NFT+1),INTG(NFT+1),
     .             PTG(1,NFT+1),IGEO    ,IMAT  ,IPROP   ,AREA    ,
     .             ETNOD,NSHNOD,STTG(NFT+1),SH3TREE,MCP   ,
     .             MCPS(NFT+1) , TEMP,SH3TRIM,ISUBSTACK,NLAY,
     .             ELBUF_STR,STACK,GBUF%THK_I,RNOISE,DRAPE ,
     .             PERTURB,IX1   ,IX2      ,IX3    ,
     .             X2L    ,X3L    ,Y3L   ,IDRAPE , INDX)
C-----------------------------------------------
      IF (MTN == 1 .OR. MTN == 3 .OR. MTN == 23) NPT = 0
      NPT_ALL = 0
      DO IL=1,NLAY
        NPT_ALL = NPT_ALL + ELBUF_STR%BUFLY(IL)%NPTT
      ENDDO
      MPT  = MAX(1,NPT_ALL)
      IF (IPARG(6) == 0.OR.NPT==0) MPT=0
C
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
      NUVAR  = 0
      NUVARR = 0
      IF (MTN>=29) THEN
        DO I=LFT,LLT
          IMAT = IXTG(1,I+NFT)
          NUVAR  = MAX(NUVAR,IPM(8,IMAT))
          NUVARR = MAX(NUVARR,IPM(221,IMAT))
        ENDDO
      ENDIF
C---------------------------
      CALL CDKEVEC3(LFT ,LLT ,AREA,
     .              X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .              Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .              E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
C------------
C Tags total area of the part (needed in /ADMAS for shells)
C------------
      IF ((IMASADD > 0).OR.(NLOC_DMG%IMOD > 0)) THEN
        DO I=LFT,LLT
          J = IPART(I+NFT)
C         PART_AREA(J) = PART_AREA(J) + AREA(I)
          ELE_AREA(NUMELC+I+NFT) = AREA(I)
          IF (GBUF%G_AREA > 0) GBUF%AREA(I) = AREA(I)
        ENDDO
      ENDIF
C------------
      CALL CDKDERII(LFT,LLT,PM,GEO,PX2,PY2,PX3,PY3,
     .             STIFN   ,STIFR   ,IXTG(1,NFT+1),THK, SH3TREE,
     .             ALDT    ,BUFMAT  ,IPM ,IGEO,STACK%PM, 
     .             ISUBSTACK,STRTG(NFT+1),GROUP_PARAM, 
     .             IMAT ,IPROP,AREA,   DT  ,
     .             X1   ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1   ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .             E1Y  ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
C
      CALL C1BUF3(GEO,GBUF%THK,GBUF%OFF,THK,KSH3TREE,SH3TREE)
c---------------------------
c---- Init UVAR
      IF (MTN == 35) THEN
        CALL CM35IN3(ELBUF_STR,THK,AREA,NEL,NLAY,
     .               NPTR,NPTS,NPTT,IGTYP)
      ENDIF
C      
       IF (( ISIGSH/=0 .OR. ITHKSHEL == 2) .and. MPT>0) THEN
           CALL LAYINI1(
     .        ELBUF_STR  ,LFT        ,LLT        ,GEO        ,IGEO      ,
     .        MAT        ,PID        ,MATLY      ,POSLY      ,IGTYP     ,
     .        NLAY       ,MPT        ,ISUBSTACK  ,STACK      ,DRAPE     ,
     .        NFT        ,GBUF%THK   ,NEL        ,IDRAPE     ,STDRAPE   ,
     .        INDX)
        END IF
C------------
      IS = 1
      DO IR =1,NPG
        IPG = IR
        PTF = (IR-1)*LENF
        PTM = (IR-1)*LENM
        PTS = (IR-1)*LENS
c
        CALL CMAINI3(ELBUF_STR,PM       ,GEO      ,NEL       ,NLAY     ,
     .               SKEW     ,IGEO ,IXTG(1,NFT+1),NIXTG     ,NUMELTG  ,
     .               NSIGSH   ,SIGSH    ,PTSH3N   ,IGTYP     ,IORTHLOC ,
     .               IPM      ,ID       ,ALDT     ,BUFMAT    ,
     .               IR       ,IS       ,ISUBSTACK,STACK     ,IREP     ,
     .               DRAPE    ,SH3ANG(NFT+1),GEO_STACK,IGEO_STACK,
     .               IGMAT    ,IMAT     ,IPROP   ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,X ,
     .               NPT_ALL  ,IDRAPE   ,STDRAPE ,INDX)
C-----------------------------------------------------------------------
C       CALCUL DES CONTRAINTES INITIALES
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        IF ((ISIGSH /= 0 .OR. ITHKSHEL == 2).AND. ISH3N == 30 ) THEN
           IF (MPT>0) 
     .       CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,LFT    ,LLT    ,
     .             NLAY     ,IREP    ,NEL     ,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .            IDRAPE, IGTYP)
          IHBE = 11
          CALL CSIGINI4(ELBUF_STR,IHBE   ,
     1               LFT     ,LLT    ,NFT      ,MPT      ,ISTRAIN,
     2               GBUF%THK ,GBUF%EINT,GBUF%STRPG(PTS+1),GBUF%HOURG,
     3               GBUF%FORPG(PTF+1),GBUF%MOMPG(PTM+1),SIGSH   ,NSIGSH ,NUMELTG  ,
     4               IXTG    ,NIXTG   ,NUMSH3N ,PTSH3N  ,IGEO ,
     5               IR      ,IS      ,IR      ,NPG     ,GBUF%G_PLA,
     6               GBUF%PLA,THK     ,IGTYP   ,NEL     ,ISIGSH ,
     7               E1X   ,E2X      ,E3X      ,E1Y   ,E2Y  ,E3Y,
     8               E1Z   ,E2Z      ,E3Z      ,DIR_A  ,DIR_B,POSLY )
          ELSEIF ( ITHKSHEL == 1 .AND. ISH3N == 30 ) THEN
           CALL THICKINI(LFT     ,LLT   ,NFT    ,PTSH3N,NUMELTG,
     2                   GBUF%THK,THK   ,IXTG   ,NIXTG ,NSIGSH ,
     3                   SIGSH   )
        ENDIF
C
        IF (IUSER == 1.AND.MTN>=28) THEN
          CALL CUSERINI4(ELBUF_STR,
     1                 LFT     ,LLT    ,NFT     ,NEL     ,ISTRAIN  ,
     2                 SIGSH   ,NSIGSH ,NUMELC  ,IXTG    ,NIXTG    ,
     3                 NUMSH3N ,PTSH3N ,IR      ,IS      ,NPT      ,
     4                 IGTYP   ,IGEO   ,NLAY    ,NPG     ,IPG      )
        ENDIF
C-----------------------------------------------------------------------
         IF (IYLDINI == 1 .AND. (MTN== 36.OR. MTN==87)) THEN
          CALL CMATINI4(ELBUF_STR,
     1                 LFT     ,LLT    ,NFT     ,NEL     ,ISTRAIN  ,
     2                 SIGSH   ,NSIGSH ,NUMELC  ,IXTG    ,NIXTG    ,
     3                 NUMSH3N ,PTSH3N ,IR      ,IS      ,NPT      ,
     4                 IGTYP   ,IGEO   ,NLAY    ,NPG     ,IPG      )
        ENDIF
       ENDDO
C----------------------------------------
c Failure model initialisation
C----------------------------------------
c	     IF ( NPERTURB /= 0 .OR. NVSHELL1 /= 0) THEN
	       CALL CFAILINI4(ELBUF_STR,IXTG    ,NIXTG   ,NPTR    ,NPTS    ,
     .                 NPTT     ,NLAY    ,IPM     ,SIGSH   ,NSIGSH  ,
     .                 NUMELTG  ,PTSH3N  ,RNOISE  ,PERTURB ,BUFMAT  ,
     .                 ALDT     ,THK     )
c	     ENDIF
C-----------------------------------------------------------------------
C     CALCUL DES DEFORMATIONS INITIALES (MEMBRANE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ISTRAIN == 1 .AND. NXREF > 0) THEN
        UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR  ! 
        CALL CDKEPSINI(ELBUF_STR,
     .       LFT     ,LLT     ,ISMSTR    ,MTN      ,ITHK     ,
     .       PM      ,GEO     ,IXTG(1,NFT+1),X ,XREFTG(1,1,NFT+1),
     .       GBUF%FOR,GBUF%THK,GBUF%EINT,GBUF%STRA,
     .       PX2     ,PY2     ,PX3      ,PY3       ,X2S      ,
     .       Y2S     ,X3S     ,Y3S      ,GBUF%OFF  ,IMAT     ,
     .       UVAR    ,BUFMAT  ,IPM      ,IGEO      ,NEL      ,
     .       NLAY    ,DIR_A   ,DIR_B    ,GBUF%SIGI ,NPF      ,
     .       TF      ,IREP    )
c
        CALL C3EPSCHK(LFT, LLT,NFT, PM, GEO,IXTG(1,NFT+1),GBUF%STRA,THK,
     .       NEL,CPT_ELTENS)
c
        IF (ISMSTR == 1) IPARG(9)=11
c
        IF (ISMSTR == 11 .OR.(ISMSTR==1 .AND. MTN==19)) THEN
          DO I=LFT,LLT
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = Y2S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X3S(I)
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = Y3S(I)
          ENDDO
        ENDIF
C
        DO IR =1,NPG
          PTF = (IR-1)*LENF
          PTM = (IR-1)*LENM
          PTS = (IR-1)*LENS
          DO I=LFT,LLT
            GBUF%FORPG(PTF+JJ(1)+I) = GBUF%FOR(JJ(1)+I)
            GBUF%FORPG(PTF+JJ(2)+I) = GBUF%FOR(JJ(2)+I)
            GBUF%FORPG(PTF+JJ(3)+I) = GBUF%FOR(JJ(3)+I)
!
            GBUF%MOMPG(PTM+JJ(1)+I) = GBUF%MOM(JJ(1)+I)
            GBUF%MOMPG(PTM+JJ(2)+I) = GBUF%MOM(JJ(2)+I)
            GBUF%MOMPG(PTM+JJ(3)+I) = GBUF%MOM(JJ(3)+I)
          ENDDO
         IF (MTN == 58 .and. IR > 1) THEN
           UVAR => ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VAR ! law58 => NLAY=1     
           NUVAR = ELBUF_STR%BUFLY(1)%NVAR_MAT                               
           DO I=1,NEL*NUVAR                                                  
             UVAR(I) = ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR(I)                  
           ENDDO                                                             
         END IF
        ENDDO
      ENDIF
C-------------------------------------------
C     CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
c        IGTYP=GEO(12,IXTG(5,I+NFT))
         IF (IGTYP /= 0  .AND. IGTYP /= 1  .AND.
     .       IGTYP /= 9  .AND. IGTYP /= 10 .AND.
     .       IGTYP /= 11 .AND. IGTYP /= 16 .AND. 
     .       IGTYP /= 17 .AND. IGTYP /= 51 .AND.
     .       IGTYP /= 52 ) THEN
          CALL ANCMSG(MSGID=25,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=IGEO(1,IPROP))
        ENDIF
      NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NFT
      DO I=LFT,LLT
        DTELEM(NDEPAR+I) = DT(I)
      ENDDO
C------------
      ! Compute the initial volume
      DO I=LFT,LLT
        IF (GBUF%G_VOL > 0) GBUF%VOL(I) = AREA(I)*GBUF%THK(I)
      ENDDO
C      
      DEALLOCATE(DIR_A)
      DEALLOCATE(DIR_B)
      IF(ALLOCATED(INDX))     DEALLOCATE(INDX)
C---
      RETURN
      END
Chd|====================================================================
Chd|  CDKPXPYI                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- called by -----------
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKPXPYI(JFT ,JLT ,ISMSTR   ,
     .                    PX2G,PY2G,PX3G,PY3G,
     .                    PX2 ,PY2 ,PX3 ,PY3 ,
     .                    X2  , Y2 ,X3  , Y3 ,
     .                    X1G  ,X2G  ,X3G  ,Y1G  ,Y2G  ,
     .                    Y3G  ,Z1G  ,Z2G  ,Z3G  ,
     .                    E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .                    E3Y  ,E1Z  ,E2Z  ,E3Z  ,AREA )
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
      INTEGER JFT, JLT, ISMSTR
C     REAL
      my_real
     .     PX2(*),  PY2(*),  PX3(*),PY3(*),
     .     PX2G(*), PY2G(*), PX3G(*),PY3G(*),
     .     X2(*),   Y2(*),   X3(*), Y3(*)
      my_real
     .     X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ),
     .     Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ),
     .     Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),AREA(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real AREAI
      my_real X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .        X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ)
C=======================================================================
C
      IF (ISMSTR/=3)THEN
        DO I=JFT,JLT
          X21G(I)=X2G(I)-X1G(I)
          Y21G(I)=Y2G(I)-Y1G(I)
          Z21G(I)=Z2G(I)-Z1G(I)
          X31G(I)=X3G(I)-X1G(I)
          Y31G(I)=Y3G(I)-Y1G(I)
          Z31G(I)=Z3G(I)-Z1G(I)
        ENDDO
C
        DO I=JFT,JLT
          Y3(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
          X3(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
          X2(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
          Y2(I)=E2X(I)*X21G(I)+E2Y(I)*Y21G(I)+E2Z(I)*Z21G(I)
        ENDDO
C
        DO I=JFT,JLT
         AREAI = HALF/AREA(I)
         PX2(I)=Y3(I)*AREAI
         PY2(I)=-X3(I)*AREAI
         PX3(I)=-Y2(I)*AREAI
         PY3(I)=X2(I)*AREAI
        ENDDO
C
      ELSE
C
        DO I=JFT,JLT
          PX2(I) = PX2G(I)
          PY2(I) = PY2G(I)
          PX3(I) = PX3G(I)
          PY3(I) = PY3G(I)
        ENDDO
C
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CDKDEFOI                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- called by -----------
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKDEFOI(JFT  ,JLT  ,NEL  ,VL   ,GSTR ,
     .                    PX2  ,PY2  ,PX3  ,PY3  ,
     .                    E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .                    E3Y  ,E1Z  ,E2Z  ,E3Z  ,
     .                    EXX  ,EYY  ,EXY  ,EYZ  ,EZX  )
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
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   VL(3,3,*),GSTR(NEL,8),PX2(*),PY2(*),PX3(*),PY3(*)
      my_real
     .    EXX(MVSIZ) , EYY(MVSIZ) , EXY(MVSIZ) ,
     .    EZX(MVSIZ) , EYZ(MVSIZ) ,
     .    E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) ,
     .    E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) ,
     .    E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real VX1(MVSIZ) , VX2(MVSIZ) , VX3(MVSIZ) ,
     .        VY1(MVSIZ) , VY2(MVSIZ) , VY3(MVSIZ) ,
     .        VZ1(MVSIZ) , VZ2(MVSIZ) , VZ3(MVSIZ) ,
     .        VX12(MVSIZ), VX13(MVSIZ), VX23(MVSIZ),
     .        VY12(MVSIZ), VY13(MVSIZ), VY23(MVSIZ),
     .        VZ12(MVSIZ), VZ13(MVSIZ), VZ23(MVSIZ)
      my_real TMP1, TMP2, FAC1
C=======================================================================
      DO I=JFT,JLT
        VX1(I)=E1X(I)*VL(1,1,I)+E1Y(I)*VL(2,1,I)+E1Z(I)*VL(3,1,I)
        VX2(I)=E1X(I)*VL(1,2,I)+E1Y(I)*VL(2,2,I)+E1Z(I)*VL(3,2,I)
        VX3(I)=E1X(I)*VL(1,3,I)+E1Y(I)*VL(2,3,I)+E1Z(I)*VL(3,3,I)
C
        VY3(I)=E2X(I)*VL(1,3,I)+E2Y(I)*VL(2,3,I)+E2Z(I)*VL(3,3,I)
        VY2(I)=E2X(I)*VL(1,2,I)+E2Y(I)*VL(2,2,I)+E2Z(I)*VL(3,2,I)
        VY1(I)=E2X(I)*VL(1,1,I)+E2Y(I)*VL(2,1,I)+E2Z(I)*VL(3,1,I)
C
        VZ1(I)=E3X(I)*VL(1,1,I)+E3Y(I)*VL(2,1,I)+E3Z(I)*VL(3,1,I)
        VZ2(I)=E3X(I)*VL(1,2,I)+E3Y(I)*VL(2,2,I)+E3Z(I)*VL(3,2,I)
        VZ3(I)=E3X(I)*VL(1,3,I)+E3Y(I)*VL(2,3,I)+E3Z(I)*VL(3,3,I)
      ENDDO
C
      DO I=JFT,JLT
C
        VX12(I)=-VX1(I) + VX2(I)
        VY12(I)=-VY1(I) + VY2(I)
        VX13(I)=-VX1(I) + VX3(I)
        VY13(I)=-VY1(I) + VY3(I)
C
        EXX(I)=PX2(I)*VX12(I) + PX3(I)*VX13(I)
        EYY(I)=PY2(I)*VY12(I) + PY3(I)*VY13(I)
C
        EXY(I)=PY2(I)*VX12(I) + PY3(I)*VX13(I)
     .       + PX2(I)*VY12(I) + PX3(I)*VY13(I)
        EYZ(I)=ZERO
        EZX(I)=ZERO
      ENDDO
C
      DO I=JFT,JLT
        GSTR(I,1)=GSTR(I,1)+EXX(I)
        GSTR(I,2)=GSTR(I,2)+EYY(I)
        GSTR(I,3)=GSTR(I,3)+EXY(I)
      ENDDO
C
      RETURN
      END
