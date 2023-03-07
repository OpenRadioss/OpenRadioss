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
Chd|  C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C1BUF3                        source/elements/shell/coque/c1buf3.F
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        C3DERII                       source/elements/sh3n/coque3n/c3derii.F
Chd|        C3EPS11_INI                   source/elements/sh3n/coque3n/c3init3.F
Chd|        C3EPSCHK                      source/elements/sh3n/coque3n/c3init3.F
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|        CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|        CFAILINI                      source/elements/shell/coque/cfailini.F
Chd|        CM27IN3                       source/materials/mat/mat027/cm27in3.F
Chd|        CM35IN3                       source/materials/mat/mat035/cm35in3.F
Chd|        CM58IN3                       source/materials/mat/mat058/cm58in3.F
Chd|        CMATINI                       source/materials/mat_share/cmatini.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        CORTHDIR                      source/elements/shell/coque/corthdir.F
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CUSERINI                      source/elements/shell/coque/cuserini.F
Chd|        FAIL_WINDSHIELD_INIT          source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|        THICKINI                      source/elements/shell/coqueba/thickini.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE C3INIT3(ELBUF_STR,
     1                   IXTG   ,PM     ,X      ,GEO     ,IGRSH4N ,
     2                   XMAS    ,IN     ,NVC    ,DTELEM ,IGRSH3N ,
     3                   XREFTG  ,OFFSET ,NEL    ,ITHK   ,THK     ,
     4                   ISIGSH  ,SIGSH  ,STIFN  ,STIFR  ,PARTSAV ,
     5                   V       ,IPART  ,MSTG   ,INTG   ,PTG     ,
     8                   SKEW    ,IPARG  ,NSIGSH ,IGEO   ,IUSER   ,
     9                   ETNOD   ,NSHNOD ,STTG   ,PTSH3N ,IPM     ,
     A                   BUFMAT  ,SH3TREE,MCP     ,MCPTG  ,TEMP   ,
     B                   CPT_ELTENS,PART_AREA,ITAGE,ITAGN,IXFEM   ,
     C                   NPF    , TF    ,SH3TRIM ,XFEM_STR,ISUBSTACK,
     D                   STACK   ,RNOISE ,DRAPE ,SH3ANG  ,
     E                   GEO_STACK,IGEO_STACK,STRTG,PERTURB ,ISH3N,
     F                   IYLDINI ,ELE_AREA,NLOC_DMG,NG,GROUP_PARAM,
     G                   IDRAPE , DRAPEG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD          
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD    
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
#include      "scry_c.inc"
C-----------------------------------------------
C      V a r i a b l e s
C-----------------------------------------------
      INTEGER NDDIM,OFFSET,NEL,ITHK,ISIGSH,NSIGSH,IUSER,
     .  CPT_ELTENS,ISUBSTACK,IYLDINI,ISH3N,NG,IDRAPE
      INTEGER IXTG(NIXTG,*),IPART(*),IGEO(NPROPGI,*),IPM(NPROPMI,*),
     .  IPARG(*),NSHNOD(*), PTSH3N(*),NPF(*),
     .  SH3TREE(*),ITAGE(*),ITAGN(*),IXFEM,IAD0,SH3TRIM(*),
     .  IGEO_STACK(*),PERTURB(NPERTURB)
C     REAL
      my_real
     .   PM(*),X(3,*),GEO(NPROPG,*),XMAS(*),XREFTG(3,3,*),
     .   IN(*),DTELEM(*),THK(*),SIGSH(NSIGSH,*),
     .   STIFN(*),STIFR(*),PARTSAV(20,*), V(*), SKEW(LSKEW,*),
     .   MSTG(*),INTG(*),PTG(3,*),
     .   ETNOD(*), STTG(*),BUFMAT(*),MCP(*),MCPTG(*),TEMP(*),
     .   PART_AREA(*),TF(*),RNOISE(*),SH3ANG(*),
     .   GEO_STACK(*),STRTG(*),ELE_AREA(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,*):: XFEM_STR
      !   when XFEM is ON, XFEM_STR's dimension = NGROUP,NXEL
      TYPE (STACK_PLY) :: STACK
      TYPE (NLOCAL_STR_) :: NLOC_DMG
      TYPE (GRP_PARAM_STRUCT_)  :: GROUP_PARAM
      TYPE (DRAPE_) :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NDEPAR,IGTYP,NVC,NUVAR,NLAY,IR,IS,IL,IFAIL,
     .        NPTR,NPTS,NPTT,IXEL,II,IT,ILAW,IMAT,IPROP,IREP,ITG,IGMAT,
     .        IFRAM_OLD,NPT_ALL,MPT,LAYNPT_MAX,LAY_MAX
      INTEGER IORTHLOC(MVSIZ),MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),JJ(6),
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ)
      my_real
     .   VX(MVSIZ),VY(MVSIZ),VZ(MVSIZ),PHI1(NPT,MVSIZ),PHI2(NPT,MVSIZ),
     .   COOR1(NPT,MVSIZ),COOR2(NPT,MVSIZ),COOR3(NPT,MVSIZ),
     .   COOR4(NPT,MVSIZ),ALDT(MVSIZ),AREA(MVSIZ)
      my_real,
     .   DIMENSION(MVSIZ) :: PX1G,PY1G,PY2G,X2S,X3S,Y3S,DT
      my_real X1(MVSIZ), X2(MVSIZ), X3(MVSIZ) ,X4(MVSIZ),
     .        Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),Y4(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),Z4(MVSIZ),
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      my_real, 
     .    ALLOCATABLE, DIMENSION(:) :: DIR_A,DIR_B
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDX     
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX)
      my_real
     .   POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX)
C-----------------------------------------------
      my_real,
     .  DIMENSION(:) ,POINTER :: UVAR,DIR1,DIR2
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_LAY_) ,POINTER :: BUFLY 
      TYPE(L_BUFEL_DIR_), POINTER :: LBUF_DIR
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      BUFLY => ELBUF_STR%BUFLY(1)
c
      IMAT  = IXTG(1,1+NFT)          ! mat N   
      IPROP = IXTG(NIXTG-1,1+NFT)    ! property N   
      IGTYP = NINT(GEO(12,IPROP))
      ID    = IGEO(1,IPROP)
      IGMAT = IGEO(98,IPROP)
      IREP  = IPARG(35)
      IFAIL = IPARG(43)
c
      IF (ISH3N==3.AND.ISH3NFRAM==0) THEN
       IFRAM_OLD =0
      ELSE
       IFRAM_OLD =1
      END IF
C      
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
      NUVAR = IPM(8,IXTG(1,1+NFT))
      VX   = ZERO
      VY   = ZERO
      VZ   = ZERO
      PHI1 = ZERO
      PHI2 = ZERO
      COOR1 = ZERO
      COOR2 = ZERO
      COOR3 = ZERO
      COOR4 = ZERO
      IORTHLOC = 0
      ITG = 1+NUMELC
C---
      IR = 1
      IS = 1
      NLAY = ELBUF_STR%NLAY
      NXEL = ELBUF_STR%NXEL
      NPTT = ELBUF_STR%NPTT
C      
      NPT_ALL = 0
      DO IL=1,NLAY
        NPT_ALL = NPT_ALL + ELBUF_STR%BUFLY(IL)%NPTT
      ENDDO
      MPT  = MAX(1,NPT_ALL)
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
C
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
C
      DO I=LFT,LLT
        MAT(I) = IMAT
        PID(I) = IPROP
      ENDDO
C
      IF (IXFEM > 0) THEN
        DO  I=LFT,LLT
          ITAGN(IXTG(2,I+NFT)) =1
          ITAGN(IXTG(3,I+NFT)) =1
          ITAGN(IXTG(4,I+NFT)) =1
          ITAGE(I+NFT) = 1
        ENDDO
      ENDIF
C
      CALL C3COORI(X,XREFTG(1,1,NFT+1),IXTG(1,NFT+1),NGL,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
      CALL C3VEOK3(NVC ,IX1 ,IX2 ,IX3 )
      CALL C3EVEC3(LFT ,LLT ,AREA,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .             E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             X31, Y31, Z31 ,X2L ,X3L ,Y3L )
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
     .             ETNOD,NSHNOD,STTG(NFT+1)  ,SH3TREE ,MCP   ,
     .             MCPTG(NFT+1),TEMP  ,SH3TRIM,ISUBSTACK,NLAY , 
     .             ELBUF_STR   ,STACK ,GBUF%THK_I,RNOISE,DRAPE,
     .             PERTURB,IX1   ,IX2      ,IX3    ,
     .             X2L      ,X3L    ,Y3L   ,IDRAPE,INDX)
C-----------------------------------------------
      CALL C3DERII(LFT,LLT,PM,GEO,PX1G,PY1G,PY2G,
     .             STIFN   ,STIFR   ,IXTG(1,NFT+1),
     .             THK,SH3TREE,ALDT ,BUFMAT , IPM     ,IGEO,
     .             STACK%PM,ISUBSTACK,STRTG(NFT+1),IMAT,IPROP,
     .             AREA ,DT  ,X31 ,Y31 ,Z31 ,
     .             E1X  ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,
     .             E1Z  ,E2Z ,E3Z ,X2L ,X3L ,Y3L ,
     .             GROUP_PARAM)
C
      CALL C1BUF3(GEO,GBUF%THK,GBUF%OFF,THK,KSH3TREE,SH3TREE)
C
      IF (IXFEM > 0) THEN
        DO IXEL=1,NXEL
          DO I=LFT,LLT
            XFEM_STR(NG,IXEL)%GBUF%THK(I) = THK(I)
            XFEM_STR(NG,IXEL)%GBUF%OFF(I) = -ONE
          END DO
        ENDDO
      ENDIF
C-------new local system for ortho or aniso init     
      IF (IFRAM_OLD ==0 ) 
     .   CALL CDKEVEC3(LFT ,LLT ,AREA,
     .                 X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                 Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                 E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
C----
C     PHI, COOR used only with dimension(NLAY,MVSIZ)
c
      CALL CORTHINI(
     .   LFT       ,LLT       ,NFT       ,NLAY      ,NUMELTG   ,
     .   NSIGSH    ,NIXTG     ,IXTG(1,NFT+1),IGEO   ,GEO       ,
     .   SKEW      ,SIGSH     ,PTSH3N    ,PHI1      ,PHI2      ,
     .   VX        ,VY        ,VZ        ,COOR1     ,COOR2     ,
     .   COOR3     ,COOR4     ,IORTHLOC  ,ISUBSTACK ,STACK     ,
     .   IREP      ,ELBUF_STR ,DRAPE    ,SH3ANG(NFT+1),X       ,
     .   GEO_STACK ,IGEO_STACK,E3X       ,E3Y       ,E3Z       ,
     .   GBUF%BETAORTH,X1     ,X2        ,Y1        ,Y2        ,
     .   Z1         ,Z2       ,NEL       ,GBUF%G_ADD_NODE,GBUF%ADD_NODE,
     .   NPT_ALL    ,IDRAPE   ,INDX)
c---     
      IF(IGTYP == 51 .OR. IGTYP == 52 .OR. IGMAT > 0) THEN   
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
     .                NPT_ALL    ,IDRAPE)
C            
      ELSEIF (MTN == 27) THEN
        CALL CM27IN3(ELBUF_STR,
     .               GEO ,IGEO ,PM ,IPM ,IXTG(1,1+NFT) ,NIXTG,
     .               NLAY,IR   ,IS ,IMAT )
      ELSEIF (MTN == 35) THEN
        NPTR = ELBUF_STR%NPTR
        NPTS = ELBUF_STR%NPTS
        NPTT = ELBUF_STR%NPTT
        CALL CM35IN3(ELBUF_STR,THK,AREA,NEL,NLAY,
     .               NPTR,NPTS,NPTT,IGTYP)
      ELSEIF (MTN == 15 .or. MTN == 19 .or. MTN == 25 .or. MTN >= 28)THEN
        IF (MTN == 19 .AND. IGTYP /= 9) THEN
          CALL ANCMSG(MSGID=5,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=IGEO(1,IXTG(NIXTG-1,NFT+1)))
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
     .                E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                NPT_ALL    ,IDRAPE)
      ENDIF
c
      IF (MTN == 58 .AND. IGTYP /= 16 .AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN  
          CALL ANCMSG(MSGID=658,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=MTN,
     .                I3=IGTYP)
      ELSEIF (MTN == 58 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
        IF(IDRAPE == 0 ) THEN
           DO IL = 1,NLAY
             NPTT =  ELBUF_STR%BUFLY(IL)%NPTT
             IMAT =  ELBUF_STR%BUFLY(IL)%IMAT
             ILAW =  ELBUF_STR%BUFLY(IL)%ILAW
             IF (ILAW == 58) THEN
               DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
               DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
               DO IT=1,NPTT
                 LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                 UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR  
                 NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                 CALL CM58IN3(IREP     ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .                        BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .                        X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                        Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                        E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
               ENDDO
             ENDIF
           ENDDO ! DO IL = 1,NLAY
        ELSE
           DO IL = 1,NLAY
             NPTT =  ELBUF_STR%BUFLY(IL)%NPTT
             IMAT =  ELBUF_STR%BUFLY(IL)%IMAT
             ILAW =  ELBUF_STR%BUFLY(IL)%ILAW
             IF (ILAW == 58) THEN
               DO IT=1,NPTT
                 LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                 UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR  
                 NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
                 LBUF_DIR =>ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)    
                 DIR1 => LBUF_DIR%DIRA
                 DIR2 => LBUF_DIR%DIRB
                 CALL CM58IN3(IREP     ,IMAT     ,IPM     ,DIR1    ,DIR2    ,
     .                        BUFMAT   ,UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .                        X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                        Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                        E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
               ENDDO
             ENDIF
           ENDDO ! DO IL = 1,NLAY
        ENDIF   
      ENDIF ! IF (MTN == 58) THEN
C-----------------------------------------------------------------------
C     CALCUL DES CONTRAINTES INITIALES
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ISIGSH /= 0 .OR. ITHKSHEL == 2) THEN
C to be checked for IGTYP = 51
        IF (MPT>0) THEN
           CALL LAYINI1(
     .        ELBUF_STR  ,LFT        ,LLT        ,GEO        ,IGEO      ,
     .        MAT        ,PID        ,MATLY      ,POSLY      ,IGTYP     ,
     .        NLAY       ,MPT        ,ISUBSTACK  ,STACK      ,DRAPE     ,
     .        NFT        ,GBUF%THK   ,NEL        ,IDRAPE    , STDRAPE   ,
     .        INDX)
           CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,LFT    ,LLT    ,
     .             NLAY     ,IREP    ,NEL     ,
     .             X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .             Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .             E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z , 
     .             IDRAPE   , IGTYP)
        END IF
        CALL CSIGINI(ELBUF_STR,
     1       LFT     ,LLT      ,NFT      ,MPT       ,ISTRAIN   ,            
     2       GBUF%THK,GBUF%EINT,GBUF%STRA,GBUF%HOURG,GBUF%PLA  ,            
     3       GBUF%FOR,GBUF%MOM ,SIGSH    ,NLAY      ,GBUF%G_HOURG,        
     4       NUMELTG ,IXTG     ,NIXTG    ,NSIGSH    ,NUMSH3N   ,             
     5       PTSH3N  ,IGEO     ,THK      ,NEL       ,E1X       ,                                         
     6       E2X     ,E3X      ,E1Y      ,E2Y       ,E3Y       ,
     7       E1Z     ,E2Z      ,E3Z      ,ISIGSH    ,DIR_A     ,
     8       DIR_B   ,POSLY    ,IGTYP    )
      ELSEIF ( ITHKSHEL == 1 ) THEN
        CALL THICKINI(LFT     ,LLT   ,NFT    ,PTSH3N,NUMELTG,
     2                GBUF%THK,THK   ,IXTG   ,NIXTG ,NSIGSH ,
     3                SIGSH   )
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
c
C to be checked for IGTYP = 51
c      IF(NPERTURB /= 0 .OR. NVSHELL1 /= 0) THEN
        CALL CFAILINI(ELBUF_STR,IXTG    ,NIXTG  ,NPTT    ,NLAY     ,
     .                IPM      ,SIGSH   ,NSIGSH ,NUMELTG ,PTSH3N   ,
     .                RNOISE   ,PERTURB ,BUFMAT ,ALDT    ,THK      )
c      ENDIF
C-----------------------------------------------------------------------
C     CALCUL DES DEFORMATIONS INITIALES (MEMBRANE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ISTRAIN == 1 .AND. NXREF > 0) THEN
        UVAR => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR
C to be checked for IGTYP = 51
        CALL C3EPSINI(ELBUF_STR,
     .       LFT     ,LLT      ,ISMSTR     ,MTN      ,ITHK     ,
     .       PM      ,GEO      ,IXTG(1,NFT+1),X      ,XREFTG(1,1,NFT+1),
     .       GBUF%FOR,GBUF%THK ,GBUF%EINT  ,GBUF%STRA,NLAY     ,
     .       PX1G    ,PY1G     ,PY2G       ,X2S      ,X3S      ,
     .       Y3S     ,UVAR     ,BUFMAT     ,IPM      ,IGEO     ,
     .       NEL     ,DIR_A    ,DIR_B      ,GBUF%SIGI,NPF      ,
     .       TF      ,IREP    ,IFRAM_OLD   ,IMAT     )
c
        CALL C3EPSCHK(LFT, LLT,NFT, PM, GEO,IXTG(1,NFT+1), GBUF%STRA,THK,
     .       NEL,CPT_ELTENS)
c
        IF (ISMSTR == 1 .AND. MTN==19) IPARG(9) = 11
c
      ELSEIF (ISMSTR == 11 .OR. (ISMSTR==1 .AND. MTN==19)) THEN
C to be checked for IGTYP = 51
        CALL C3EPS11_INI(
     .                 LFT     ,LLT     ,IXTG(1,NFT+1),X     ,X2S     ,    
     .                 X3S     ,Y3S     )
      ENDIF
c
      IF (ISMSTR == 10) THEN                  
          DO I=LFT,LLT                          
            II = NFT + I            
            ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X(1,IXTG(3,II))-X(1,IXTG(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = X(2,IXTG(3,II))-X(2,IXTG(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = X(3,IXTG(3,II))-X(3,IXTG(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(4)+I) = X(1,IXTG(4,II))-X(1,IXTG(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(5)+I) = X(2,IXTG(4,II))-X(2,IXTG(2,II))  
            ELBUF_STR%GBUF%SMSTR(JJ(6)+I) = X(3,IXTG(4,II))-X(3,IXTG(2,II))  
          ENDDO                                 
      ELSEIF (ISMSTR == 11 .OR.(ISMSTR==1 .AND. MTN==19)) THEN                  
        DO I=LFT,LLT                          
          ELBUF_STR%GBUF%SMSTR(JJ(1)+I) = X2S(I)  
          ELBUF_STR%GBUF%SMSTR(JJ(2)+I) = X3S(I)  
          ELBUF_STR%GBUF%SMSTR(JJ(3)+I) = Y3S(I)  
        ENDDO                                 
      ENDIF                                   
C
      IF (IUSER == 1 .and. MTN > 28) THEN
C to be checked for IGTYP = 51
        CALL CUSERINI(ELBUF_STR,
     1                LFT    ,LLT    ,NFT      ,NEL     ,NPT     ,
     2                ISTRAIN,SIGSH  ,NUMELTG  ,IXTG    ,NIXTG   ,
     3                NSIGSH ,NUMSH3N,PTSH3N   ,IR      ,IS      ,
     4                NLAY   )
      ENDIF
C-------------------------------------------
       IF (IYLDINI == 1 .AND. (MTN== 36.OR. MTN==87))THEN
        CALL CMATINI(ELBUF_STR,
     1                LFT    ,LLT    ,NFT      ,NEL     ,NPT     ,
     2                ISTRAIN,SIGSH  ,NUMELTG  ,IXTG    ,NIXTG   ,
     3                NSIGSH ,NUMSH3N,PTSH3N   ,IR      ,IS      ,
     4                NLAY   )
       ENDIF
C-------------------------------------------
C     CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
c         IGTYP=GEO(12,IXTG(5,I+NFT))
         IF (IGTYP /= 0  .AND. IGTYP /= 1  .AND.
     .       IGTYP /= 9  .AND. IGTYP /= 10 .AND.
     .       IGTYP /= 11 .AND. IGTYP /= 16 .AND. 
     .       IGTYP /= 17 .AND. IGTYP /= 51 .AND.
     .       IGTYP /= 52 ) THEN
           CALL ANCMSG(MSGID=25,
     .                 ANMODE=ANINFO,
     .                 MSGTYPE=MSGERROR,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IPROP)
         ENDIF
       NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NFT
       DO I=LFT,LLT
         DTELEM(NDEPAR+I) = DT(I)
       END DO
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
C---
      IF (ALLOCATED(DIR_B))    DEALLOCATE(DIR_B)
      IF (ALLOCATED(DIR_A))    DEALLOCATE(DIR_A)
      IF (ALLOCATED(INDX))     DEALLOCATE(INDX)
C------------
      RETURN
      END
C
Chd|====================================================================
Chd|  C3EPSCHK                      source/elements/sh3n/coque3n/c3init3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE C3EPSCHK(JFT,JLT,NFT,PM,GEO,IXTG,GSTR,THK,
     .            NEL,CPT_ELTENS)
      USE MESSAGE_MOD
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
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NFT ,NEL ,IXTG(NIXTG,*),CPT_ELTENS
      my_real
     .   PM(NPROPM,*), GSTR(NEL,8),THK(*), GEO(NPROPG,*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C------------------------------------------------------
      INTEGER I
      my_real
     .   DELT(MVSIZ),X1(MVSIZ),X2(MVSIZ)
C-------------------------------------------------------------
      DO I=JFT,JLT
        DELT(I)=((GSTR(I,1)+GSTR(I,2))*(GSTR(I,1)+GSTR(I,2)))-FOUR*
     .   (GSTR(I,1)*GSTR(I,2)-FOURTH*GSTR(I,3)*GSTR(I,3))
       IF(DELT(I)>=ZERO)THEN
         X1(I) = (GSTR(I,1)+GSTR(I,2)-SQRT(DELT(I)))/TWO
         X2(I) = (GSTR(I,1)+GSTR(I,2)+SQRT(DELT(I)))/TWO
         IF((X1(I)>ZERO.AND.(X1(I)>EM10)).OR.(X2(I)>ZERO
     .    .AND.(X2(I)>EM10)))THEN
           IF(IPRI == 5) THEN
             CALL ANCMSG(MSGID=607,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   R1=MIN(X1(I),X2(I)),
     .                   R2=MAX(X1(I),X2(I)),
     .                   I1=IXTG(6,I))
           ELSE
             CPT_ELTENS = CPT_ELTENS + 1
           ENDIF
         ENDIF
       ENDIF
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3EPS11_INI                   source/elements/sh3n/coque3n/c3init3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3EPS11_INI(
     1           JFT    ,JLT   ,IXTG  ,X      ,X2S     ,
     2           X3S    ,Y3S   )
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
      INTEGER IXTG(NIXTG,*)
      my_real X(3,*),X2S(*), X3S(*), Y3S(*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NIT, NT, I1, I2, I3, I4
      my_real
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) , 
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) , 
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) 
      my_real SUM,S1,S2
      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .   X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .   X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ)
C---------------------------------------------------------------
       DO I=JFT,JLT
         X1(I)=ZERO
         Y1(I)=ZERO
         Z1(I)=ZERO
         X2(I)=X(1,IXTG(3,I))-X(1,IXTG(2,I))
         Y2(I)=X(2,IXTG(3,I))-X(2,IXTG(2,I))
         Z2(I)=X(3,IXTG(3,I))-X(3,IXTG(2,I))
         X3(I)=X(1,IXTG(4,I))-X(1,IXTG(2,I))
         Y3(I)=X(2,IXTG(4,I))-X(2,IXTG(2,I))
         Z3(I)=X(3,IXTG(4,I))-X(3,IXTG(2,I))
       ENDDO
C
       DO I=JFT,JLT
        X21(I)=X2(I)
        Y21(I)=Y2(I)
        Z21(I)=Z2(I)
        X31(I)=X3(I)
        Y31(I)=Y3(I)
        Z31(I)=Z3(I)
        X32(I)=X3(I)-X2(I)
        Y32(I)=Y3(I)-Y2(I)
        Z32(I)=Z3(I)-Z2(I)
       ENDDO
       DO I=JFT,JLT
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        SUM = ONE/SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)* SUM
        E1Y(I)=E1Y(I)* SUM
        E1Z(I)=E1Z(I)* SUM
       ENDDO
C
       DO I=JFT,JLT
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM = ONE/SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)* SUM
        E3Y(I)=E3Y(I)* SUM
        E3Z(I)=E3Z(I)* SUM
       ENDDO
C
       DO I=JFT,JLT
         E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
         E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
         E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
         SUM = ONE/SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
         E2X(I)=E2X(I)*SUM
         E2Y(I)=E2Y(I)*SUM
         E2Z(I)=E2Z(I)*SUM
       ENDDO
C       
      DO I=JFT,JLT
        Y3S(I)=E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X3S(I)=E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
        X2S(I)=E1X(I)*X21(I)+E1Y(I)*Y21(I)+E1Z(I)*Z21(I)
      ENDDO
C-----------
      RETURN
      END
