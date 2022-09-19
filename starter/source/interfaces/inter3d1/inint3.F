Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|        I11DST3                       source/interfaces/inter3d1/i11dst3.F
Chd|        I11NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I11PWR3                       source/interfaces/inter3d1/i11pwr3.F
Chd|        I11REMLINE                    source/interfaces/inter3d1/i11remlin.F
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I12CHK3                       source/interfaces/inter3d1/i12chk3.F
Chd|        I12TID3                       source/interfaces/inter3d1/i12tid3.F
Chd|        I17STI3                       source/interfaces/inter3d1/i17sti3.F
Chd|        I18PWR3                       source/interfaces/inter3d1/i18pwr3.F
Chd|        I1CHK3                        source/interfaces/inter3d1/i1chk3.F
Chd|        I1TID3                        source/interfaces/inter3d1/i1tid3.F
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|        I22ERR3                       source/interfaces/inter3d1/i22err3.F
Chd|        I22STI3                       source/interfaces/inter3d1/i22sti3.F
Chd|        I22TZINF                      source/interfaces/inter3d1/i22tzinf.F
Chd|        I23BUC1                       source/interfaces/inter3d1/i23buc3.F
Chd|        I23DST3                       source/interfaces/inter3d1/i23dst3.F
Chd|        I23GAP3                       source/interfaces/inter3d1/i23gap3.F
Chd|        I23PWR3                       source/interfaces/inter3d1/i23pwr3.F
Chd|        I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|        I24CAND                       source/interfaces/inter3d1/i24pen3.F
Chd|        I24COR3                       source/interfaces/inter3d1/i24cor3.F
Chd|        I24FICI_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|        I24FICS_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|        I24FICV_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|        I24INISUR_NEI                 source/interfaces/inter3d1/i24inisu_nei.F
Chd|        I24INI_GAP_N                  source/interfaces/inter3d1/i24inisu_nei.F
Chd|        I24ISEGPT_INI                 source/interfaces/inter3d1/i24surfi.F
Chd|        I24LL_KG                      source/interfaces/inter3d1/i24sti3.F
Chd|        I24NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|        I24PWR3                       source/interfaces/inter3d1/i24pwr3.F
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25BUCE_EDG                   source/interfaces/inter3d1/i25buce_edg.F
Chd|        I25BUC_VOX1                   source/interfaces/inter3d1/i25buc_vox1.F
Chd|        I25CAND                       source/interfaces/inter3d1/i25pwr3.F
Chd|        I25COR3                       source/interfaces/inter3d1/i25cor3.F
Chd|        I25COR3E                      source/interfaces/inter3d1/i25cor3e.F
Chd|        I25COR3_E2S                   source/interfaces/inter3d1/i25cor3_e2s.F
Chd|        I25DST3_E2S                   source/interfaces/inter3d1/i25dst3_e2s.F
Chd|        I25INI_GAP_N                  source/interfaces/inter3d1/i25neigh.F
Chd|        I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|        I25NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I25NORM                       source/interfaces/inter3d1/i25norm3.F
Chd|        I25PEN3                       source/interfaces/inter3d1/i25pen3.F
Chd|        I25PEN3E                      source/interfaces/inter3d1/i25pen3e.F
Chd|        I25PWR3                       source/interfaces/inter3d1/i25pwr3.F
Chd|        I25PWR3E                      source/interfaces/inter3d1/i25pwr3e.F
Chd|        I25PWR3_E2S                   source/interfaces/inter3d1/i25pwr3_e2s.F
Chd|        I25REMLINE                    source/interfaces/int25/i25remlin.F
Chd|        I25SORS                       source/interfaces/inter3d1/i25sors.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25STI_EDG                    source/interfaces/inter3d1/i25sti_edg.F
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|        I2MAIN                        source/interfaces/interf1/i2master.F
Chd|        I2NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|        I2SURFS                       source/interfaces/inter3d1/i2surfs.F
Chd|        I2TID3                        source/interfaces/inter3d1/i2tid3.F
Chd|        I2WCONTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I3PEN3                        source/interfaces/inter3d1/i3pen3.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I5PWR3                        source/interfaces/inter3d1/i3pen3.F
Chd|        I7BUC1                        source/interfaces/inter3d1/i7buc1.F
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7ERR3                        source/interfaces/inter3d1/i7err3.F
Chd|        I7NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        I7PWR3                        source/interfaces/inter3d1/i7pwr3.F
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|        I9STI3                        source/interfaces/int09/i9sti3.F
Chd|        IN12R                         source/interfaces/inter3d1/in12r.F
Chd|        ININT0                        source/interfaces/interf1/inint0.F
Chd|        ININT0_8                      source/interfaces/interf1/inint0_8.F
Chd|        INVOI3                        source/interfaces/inter3d1/invoi3.F
Chd|        ISPT2_INI                     source/interfaces/inter3d1/i24surfi.F
Chd|        IWCONTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|        UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_REMNODE               source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG           source/interfaces/interf1/upgrade_remnode.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        IXINTMOD                      share/modules1/restart_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININT3(INSCR     ,X            ,IXS        ,IXC       ,PM      ,
     1                  GEO       ,IPARI        ,NUMINT     ,ITAB      ,MS      ,
     2                  MWA       ,RWA          ,IXTG       ,IWRN      ,IKINE   ,
     3                  IXT       ,IXP          ,IXR        ,NELEMINT  ,IDDLEVEL,
     4                  IFIEND    ,ALE_CONNECTIVITY,NSNET      ,NMNET     ,IGRBRIC ,
     5                  IWCONT    ,NSNT         ,NMNT       ,NSN2T     ,NMN2T   ,
     6                  IWCIN2    ,KNOD2ELS     ,KNOD2ELC   ,KNOD2ELTG ,NOD2ELS ,
     7                  NOD2ELC    ,NOD2ELTG    ,IGRSURF    ,IKINE1    ,IELEM21 ,
     8                  SH4TREE    ,SH3TREE     ,IPART      ,IPARTC    ,IPARTTG ,
     9                  THK        ,THK_PART    ,NOD2EL1D   ,KNOD2EL1D ,IXS10   ,
     A                  I_MEM      ,RESORT      ,SIXINT     ,IXS16     ,IXS20   ,
     B                  ID         ,TITR        ,IREMNODE   ,NREMNODE  ,IPARTS  ,
     C                  KXX        ,IXX         ,IGEO       ,INTERCEP  ,LELX    ,
     D                  INTBUF_TAB ,FILLSOL     ,PM_STACK   ,IWORKSH   ,KXIG3D  ,
     E                  IXIG3D     ,TAGPRT_FRIC ,INTBUF_FRIC_TAB,IPARTT,IPARTP  ,
     F                  IPARTX     ,IPARTR      ,NSN_MULTI_CONNEC,T2_ADD_CONNEC,T2_NB_CONNEC,
     G                  T2_CONNEC  ,NOM_OPT     ,ICODE     ,ISKEW      ,IREMNODE_EDG)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE IXINTMOD     !IXINT
      USE FRONT_MOD
      USE INTBUFDEF_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMINT, IWRN, NSNT, NMNT, NSN2T, NMN2T,
     .   NSNET  ,NMNET ,RESORT,SIXINT
      INTEGER INSCR(*), IXS(NIXS,*), IXC(NIXC,*),
     .   IPARI(*), IXT(NIXT,*) ,IXP(NIXP,*) ,IXR(NIXR,*),
     .   ITAB(*), MWA(*), IXTG(NIXTG,*), IKINE(*),
     .   NELEMINT,  IDDLEVEL,IFIEND,
     .   IWCONT(*), IWCIN2(*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*),
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*), IPART(*),
     .   IPARTC(*), IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*),
     .   IXS10(6,*), IXS16(8,*), IXS20(12,*), IPARTS(*),
     .   KXIG3D(NIXIG3D,*),IXIG3D(*),TAGPRT_FRIC(*),
     .   IPARTT(*)  ,IPARTP(*)  ,IPARTX(*) ,IPARTR(*),
     .   ICODE(*), ISKEW(*)
      INTEGER IKINE1(*), IELEM21(*),I_MEM,ID,IREMNODE,IREMNODE_EDG,
     .   NREMNODE,KXX(*),IXX(*),IGEO(*),IWORKSH(*),NSN_MULTI_CONNEC,T2_ADD_CONNEC(*),
     .   T2_NB_CONNEC(*),T2_CONNEC(*)
      INTEGER NOM_OPT(LNOPT1,*)
      my_real
     .   PM(*), GEO(*), MS(*),RWA(6,*),
     .   THK(*),THK_PART(*), LELX(*), FILLSOL(*),
     .   PM_STACK(*)
      my_real, TARGET ::  X(3*NUMNOD)
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC)  :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NMN0, NTY, NST, NMT, IBUC, NOINT,
     .   NSNE, NMNE,NLINS,NLINM,NLN,L16,L17,L20,L21,L22,L23,J31P,J36P,
     .   I, I_STOK,IRS,IRM,ILEV,IDEL2,IPROJ,
     .   NSEG, NGROUS, NG, INACTI,IWPENE,ISTOK,
     .   JLT_NEW,IGAP,MULTIMP,INPENE,ISEARCH,ITIED,
     .   IGN,IGE,NME,NMES,NAD,EAD,ISU1,ISU2,L30,
     .   INTTH, IBIDON,NLINSA,NLINMA,ISS2,IFS2,ISYM,IGNORE,NCONT,ICURV,
     .   DIMFLAG,IPEN0,INTKG,INTPLY,NRTSE,NSN0,ITH, NADMSR, NEDGE, IERROR, IT19,INT_TYP,
     .   NRTM_FE, NRTM_IGE,NMN_IGE,NMN_FE,NRTS_FE, NRTS_IGE,NSN_IGE,NSN_FE,L,IEDG4,IS1,
     .   IVIS2, ISHARP, IEDGE, INACTBID, ITHK25, IGAP0
      INTEGER
     .   N1(MVSIZ),N2(MVSIZ),M1(4,MVSIZ),M2(4,MVSIZ),NRTM0,NRTM_SH,IWPENE0,IAD,NBRIC,II,IBRIC,IAD_IXINT,
     .   NODEID, INOD, MVOISN(MVSIZ,4),IBOUND(4,MVSIZ)
      INTEGER ITASK, NEDGE_T, ESHIFT, NRTM_T, SSHIFT, MULNSNE, MULNSNS, MULTIMPE, MULTIMPS, NCONTE,
     .        CAND_E_OLD(2), I_MEME(2),
     .        I_START, I_MEM_REM, NEW_SIZE
      my_real
     .        MARGE, GAP, VMAXDT, BMINMA(6), GAPM_MX, GAPS_MX, GAPM_L_MX,GAPS_L_MX
      my_real
     .   MAXBOX,MINBOX,GAP0,BID,TZINF,GAPINF,BIDON,FPENMAX,DRAD,PENMN,GAPSCALE,BGAPEMX_L
      my_real
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),GAPV(MVSIZ)
      my_real
     .   RDUM(1)  ,PENE_MAX,PENMAX,FACF
      my_real, DIMENSION(:),ALLOCATABLE :: PENMIN
      INTEGER, DIMENSION(:),ALLOCATABLE :: IPARTNS,INOD2LIN,TAGSECND,NOD2LIN
      my_real, DIMENSION(:),ALLOCATABLE, TARGET :: XE
      my_real, DIMENSION(:),POINTER :: PTR_X
      INTEGER, POINTER :: pIXINT
      INTEGER, DIMENSION(:),ALLOCATABLE :: IPARTSM
C-----------------------------------------------
!     Local variables for equivalence removal
      INTEGER, DIMENSION(MVSIZ) ::PROV_N,PROV_E
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,STIF
      my_real, DIMENSION(MVSIZ) :: N11,N21,N31
      my_real, DIMENSION(MVSIZ) :: XN1,YN1,ZN1
      my_real, DIMENSION(MVSIZ) :: XN2,YN2,ZN2
      my_real, DIMENSION(MVSIZ) :: XN3,YN3,ZN3
      my_real, DIMENSION(MVSIZ) :: XN4,YN4,ZN4
      my_real, DIMENSION(MVSIZ) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ) :: S,T
      my_real
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5), PENE(4,MVSIZ), ! Dimension 4 is needed for i25dst3_e2s
     .   GAPS(MVSIZ), GAPM(MVSIZ), GAP_NM(4,MVSIZ), GAPMXL(MVSIZ),
     .   XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ), XZS1(MVSIZ), XZS2(MVSIZ),
     .   XXM1(4,MVSIZ), XXM2(4,MVSIZ), XYM1(4,MVSIZ), XYM2(4,MVSIZ), XZM1(4,MVSIZ), XZM2(4,MVSIZ),
     .   GAPVE(MVSIZ),
     .   EX(4,MVSIZ), EY(4,MVSIZ), EZ(4,MVSIZ), FX(MVSIZ), FY(MVSIZ), FZ(MVSIZ)
      LOGICAL PRINT_WARNING,TYPE18
      CHARACTER FILNAM*100
C=======================================================================
      INPENE   = 0
      IWPENE   = 0
      ISTOK    = 0
      NRTS     = IPARI(3)
      NRTM     = IPARI(4)
      NSN      = IPARI(5)
      NMN      = IPARI(6)
      NMN0     = NMN
      NTY      = IPARI(7)
      NST      = IPARI(8)
      NMT      = IPARI(9)
      IBUC     = IPARI(12)
      ISEARCH  = IPARI(12)
      NOINT    = IPARI(15)
      IGAP     = IPARI(21)
      INACTI   = IPARI(22)
      MULTIMP  = IPARI(23)
      IRM      = IPARI(24)
      IRS      = IPARI(25)
      IDEL2    = IPARI(17)
      ILEV     = IPARI(20)
      ITIED    = 0
      ISU1     = IPARI(45)
      ISU2     = IPARI(46)
      INTTH    = IPARI(47)
      NRTM_SH  = IPARI(42)
      NCONT    = IPARI(18)
      ICURV    = IPARI(39)
      INTKG    = IPARI(65)
      INTPLY   = IPARI(66)
      IT19     = IPARI(71)
      DRAD     = ZERO
      RDUM(1)  = 0
      NRTM_IGE = IPARI(73)
      NRTM_FE  = IPARI(74)
      NRTS_IGE = IPARI(75)
      NRTS_FE  = IPARI(76)
      NSN_IGE  = IPARI(77)
      NSN_FE   = IPARI(78)
      NMN_IGE  = IPARI(79)
      NMN_FE   = IPARI(80)
      IGAP0    = IPARI(53)
      IEDGE    = IPARI(58)
      NCONTE   = IPARI(88)
      IS1 = IPARI(13) / 10

      TYPE18=.FALSE.
      IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

      IF (IT19<=0) THEN
      INT_TYP = MAX(NTY,ABS(IT19)*19)
      IF (RESORT == 0)THEN
       IF((NTY==7.AND.INACTI==7).AND.(NTY/=22))THEN
         WRITE(IOUT,2181)NOINT
       ELSE
         WRITE(IOUT,2001)NOINT,INT_TYP
       END IF
      END IF
      ENDIF
C-----------------------------------------------------------------------
      IF(NTY==1) THEN
C-----------------------------------------------------------------------
       L17=1
       L20=L17+NMN
       L22=L20+1+NSN

      !must be flushed to 0 (in old code INBUF and BUFIN
      !flushed between 2 domain decomposition
      INTBUF_TAB%NRT(1:NMT) = 0
C
       CALL ININT0(
     1    X                ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSEGM ,INTBUF_TAB%NRT ,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV   ,INTBUF_TAB%ILOCS   ,NSN              ,NMN            ,NRTM   )
       CALL I1CHK3(
     1    X                ,INTBUF_TAB%IRECTS  ,IXS              ,NRTS           ,IXC           ,
     2    NUMINT           ,NSN                ,INTBUF_TAB%NSV   ,NOINT          ,IXTG          ,
     3    GEO              ,PM                 ,KNOD2ELS         ,KNOD2ELC       ,KNOD2ELTG     ,
     4    NOD2ELS          , NOD2ELC           ,NOD2ELTG         ,IGRSURF(ISU1)  ,
     5    THK              ,NTY                ,IXS10            ,IXS16          ,IXS20         ,
     6    ID               ,TITR               ,IGEO             ,PM_STACK        ,IWORKSH )
       CALL I1CHK3(
     1    X                ,INTBUF_TAB%IRECTM  ,IXS              ,NRTM           ,IXC ,
     2    -NUMINT          ,NMN                ,INTBUF_TAB%MSR   ,NOINT          ,IXTG,
     3    GEO              ,PM                 ,KNOD2ELS         ,KNOD2ELC       ,KNOD2ELTG     ,
     4    NOD2ELS          , NOD2ELC           ,NOD2ELTG         ,IGRSURF(ISU2)  ,
     5    THK              ,NTY                ,IXS10            ,IXS16          ,IXS20         ,
     6    ID               ,TITR               , IGEO            ,PM_STACK        ,IWORKSH)
       CALL INVOI3(
     1    X                ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NRT   ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS ,INTBUF_TAB%IRTLM   ,INTBUF_TAB%NSEGM ,NSN            ,NMN ,
     3    ITAB             ,ID                 ,TITR             ,NRTM)
       WRITE(IOUT,2002)
C
       CALL I1TID3(
     1    X                ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%CSTS  ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS ,INTBUF_TAB%IRTLM   ,NSN              ,ITAB           ,IKINE         ,
     3    IKINE1           ,ID                 ,TITR             ,ILEV           ,NTY           ,
     4    INTBUF_TAB%CSTS_BIS)
C-----------------------------------------------------------------------
      ELSEIF(NTY==2.AND.ISEARCH==1) THEN
C-----------------------------------------------------------------------
       L16 = 1
       L17 = L16+NSN
       L20 = L17+NMN
       L21 = L20+1+NSN
       L22 = L21+1+NMN
       L23 = L22+NST
       CALL ININT0(
     1    X                ,INTBUF_TAB%IRECTM  ,INSCR(L21)       ,INSCR(L23)     ,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV   ,INSCR(L16)         ,NSN              ,NMN            ,NRTM          )

       CALL I1CHK3(
     1    X               ,INTBUF_TAB%IRECTM   ,IXS              ,NRTM           ,IXC           ,
     2    -NUMINT         ,NMN                 ,INTBUF_TAB%MSR   ,NOINT          ,IXTG          ,
     3    GEO             ,PM                  ,KNOD2ELS         ,KNOD2ELC       ,KNOD2ELTG     ,
     4    NOD2ELS         ,NOD2ELC             ,NOD2ELTG         ,IGRSURF(ISU2)  ,
     5    THK             ,NTY                 ,IXS10            ,IXS16          ,IXS20         ,
     6    ID              ,TITR                ,IGEO             ,PM_STACK       , IWORKSH      )
       CALL INVOI3(
     1    X               ,INTBUF_TAB%IRECTM   ,INSCR(L23)       ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV,
     2    INSCR(L16)      ,INTBUF_TAB%IRTLM    ,INSCR(L21)       ,NSN            ,NMN           ,
     3    ITAB            ,ID                  ,TITR             ,NRTM)
       WRITE(IOUT,2002)
       CALL I1TID3(
     1    X               ,INTBUF_TAB%IRECTM   ,INTBUF_TAB%CSTS  ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV,
     2    INSCR(L16)      ,INTBUF_TAB%IRTLM    ,NSN              ,ITAB           ,IKINE         ,
     3    IKINE1          ,ID                  ,TITR             ,ILEV           ,NTY           ,
     4    INTBUF_TAB%CSTS_BIS)
C selection main nodes utiles et recompactage buffer interface
C      Attention NMN et K14 modifie par I2MAIN
       CALL I2MAIN(INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRECTM,IPARI,
     .               MWA,MWA(NUMNOD+1),INTBUF_TAB)
C
       NMN = IPARI(6)
       INTTH = IPARI(47)
       IF ((ILEV==20 .OR. ILEV==21 .OR. ILEV==22) .OR. INTTH > 0 ) THEN
           J31P =1+2*(NMN0-NMN)
           J36P =1+2*(NMN0-NMN)
          DO I = 1,NSN
            INTBUF_TAB%AREAS2(I) =  INTBUF_TAB%AREAS2(J31P+I-1)
          ENDDO
         CALL I2SURFS(
     .        X   ,INTBUF_TAB%NSV  ,INTBUF_TAB%AREAS2,NSN ,ITAB      ,
     .        IXC       ,IXTG      ,IXS       ,KNOD2ELS  ,NOD2ELS   ,
     .        KNOD2ELC  ,NOD2ELC   ,KNOD2ELTG ,NOD2ELTG  ,ILEV      ,
     .        ID        ,TITR      ,INTTH    )
          ENDIF
       IF ((ILEV >=10 .AND. ILEV < 23) ) THEN
         IF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN
           J31P =1+2*(NMN0-NMN)
           J36P =1+2*(NMN0-NMN)
           INTBUF_TAB%RUPT(1) = INTBUF_TAB%IRECTM(J36P)
           INTBUF_TAB%RUPT(2) = INTBUF_TAB%IRECTM(J36P+1)
           INTBUF_TAB%RUPT(3) = INTBUF_TAB%IRECTM(J36P+2)
           INTBUF_TAB%RUPT(4) = INTBUF_TAB%IRECTM(J36P+3)
           INTBUF_TAB%RUPT(5) = INTBUF_TAB%IRECTM(J36P+4)
           INTBUF_TAB%RUPT(6) = INTBUF_TAB%IRECTM(J36P+5)
         ENDIF

       ENDIF
C
       IF (IDDLEVEL==0.OR.NDSOLV==1)THEN
         IF ( ((NELEMINT+NSN))*6 > SIXINT)
     .     CALL UPGRADE_IXINT(SIXINT,NELEMINT,NSN)
           NULLIFY(pIXINT)
           IF(SIXINT>0)THEN
             IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
             pIXINT => IXINT(IAD_IXINT)
           ENDIF
           CALL I2NINTDD(NELEMINT       ,pIXINT           ,INTBUF_TAB%IRECTM,
     .                   INTBUF_TAB%NSV ,INTBUF_TAB%IRTLM ,NSN              ,IFIEND )
       ENDIF
       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     +    (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL I2WCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCIN2,
     2                   NSN2T     ,NMN2T     )
       END IF
C
C-----------------------------------------------------------------------
      ELSEIF(NTY==2.AND.ISEARCH==2) THEN
C-----------------------------------------------------------------------
       TZINF=INTBUF_TAB%VARIABLES(4)
C j21 utilise en tmp dans i2chk3, i2buc3 et i2tied3
C
       CALL I2CHK3(
     1   X                 ,INTBUF_TAB%IRECTM ,IXS                ,NRTM                   ,IXC             ,
     2   -NUMINT           ,NSN               ,INTBUF_TAB%MSR     ,NOINT                  ,
     3   IXTG              ,INTBUF_TAB%IRTLM  ,INTBUF_TAB%CSTS    ,INTBUF_TAB%DPARA       ,GEO             ,
     4   PM                ,KNOD2ELS          ,KNOD2ELC           ,KNOD2ELTG              ,NOD2ELS         ,
     5   NOD2ELC           ,NOD2ELTG          ,IGRSURF(ISU2)      ,THK                    ,
     6   NTY               ,IXS10             ,IXS16              ,IXS20                  ,IGEO            ,
     7   PM_STACK          ,IWORKSH )
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       IGNORE = IPARI(34)
       IPROJ  = IPARI(57)
       CALL I2BUC1(
     1   X                 ,INTBUF_TAB%IRECTM ,INTBUF_TAB%NSV    ,NSEG                    ,INTBUF_TAB%IRTLM,
     2   NMN               ,NRTM              ,MWA               ,NSN                     ,RWA             ,
     3   NOINT             ,INTBUF_TAB%MSR    ,INTBUF_TAB%CSTS   ,INTBUF_TAB%DPARA        ,TZINF           ,
     4   IGNORE            ,THK               ,KNOD2ELS          ,KNOD2ELC                ,KNOD2ELTG       ,
     5   NOD2ELS           ,NOD2ELC           ,NOD2ELTG          ,
     6   -NUMINT           ,IXC               ,IXTG              ,THK_PART                ,IPARTC          ,
     7   GEO               ,IXS               ,IXS10             ,PM                      ,IXS16           ,
     8   IXS20             ,IPARTTG           ,ID,TITR           ,IGEO                    ,PM_STACK        ,
     9   IWORKSH ,
     1   IX1    ,IX2    ,IX3,IX4 ,NSVG      ,
     2   PROV_N ,PROV_E ,N11,N21 ,N31       ,
     3   X1     ,X2     ,X3 ,X4  ,STIF      ,
     4   Y1     ,Y2     ,Y3 ,Y4  ,Z1        ,
     5   Z2     ,Z3     ,Z4 ,XI  ,YI        ,
     6   ZI     ,X0     ,Y0 ,Z0  ,XN1       ,
     7   YN1    ,ZN1    ,XN2,YN2 ,ZN2       ,
     8   XN3    ,YN3    ,ZN3,XN4 ,YN4       ,
     9   ZN4    ,P1     ,P2 ,P3  ,P4        ,
     1   LB1    ,LB2    ,LB3,LB4 ,LC1       ,
     2   LC2    ,LC3    ,LC4,S   ,T         ,
     3   ILEV)
C
C      Attention NMN, NSN, K13, K14 et K15 modifie par I2TID3
       CALL I2TID3(
     1    X                ,INTBUF_TAB%IRECTM ,INTBUF_TAB%CSTS   ,INTBUF_TAB%MSR          ,INTBUF_TAB%NSV  ,
     2    INTBUF_TAB%IRTLM ,ITAB              ,IKINE             ,IKINE1                  ,INTBUF_TAB%DPARA,
     3    IPARI            ,TZINF             ,IDDLEVEL          ,
     4    ID               ,TITR              ,INTBUF_TAB        ,INTBUF_TAB%VARIABLES(4) , IPROJ,
     5    IXS              ,IXC               ,IXS10             ,IXS16                   ,IXS20,INTBUF_TAB%CSTS_BIS,
     6    NSN_MULTI_CONNEC ,T2_ADD_CONNEC     ,T2_NB_CONNEC      ,T2_CONNEC               ,IXTG)
C    
       NSN = IPARI(5)
       NMN = IPARI(6)
       INTTH = IPARI(47)
C
c       IF ((ILEV >=10 .AND. ILEV < 23) ) THEN
       IF ((ILEV >=10 .AND. ILEV < 23) .OR. INTTH > 0) THEN
         CALL I2SURFS(
     .    X                ,INTBUF_TAB%NSV    ,INTBUF_TAB%AREAS2 ,NSN                     ,ITAB,
     .    IXC              ,IXTG              ,IXS               ,KNOD2ELS                ,NOD2ELS   ,
     .    KNOD2ELC         ,NOD2ELC           ,KNOD2ELTG         ,NOD2ELTG                ,ILEV      ,
     .    ID               ,TITR              ,INTTH    )
       ENDIF
C
       IF (IDDLEVEL==0.OR.NDSOLV==1)THEN
         IF ( (NELEMINT+NSN)*6 >  SIXINT)CALL UPGRADE_IXINT(SIXINT,NELEMINT,NSN)
         NULLIFY(pIXINT)
         IF(SIXINT>0)THEN
           IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
           pIXINT => IXINT(IAD_IXINT)
         ENDIF
         CALL I2NINTDD(NELEMINT       ,pIXINT           ,INTBUF_TAB%IRECTM,
     .                 INTBUF_TAB%NSV ,INTBUF_TAB%IRTLM ,NSN              ,IFIEND )
       ENDIF
C
       IF(IDDLEVEL==0.OR.(NDSOLV==1).AND.
     +   (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL I2WCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCIN2,
     +                 NSN2T        ,NMN2T        )
       END IF
C
C-----------------------------------------------------------------------
      ELSEIF(NTY==3) THEN
C-----------------------------------------------------------------------
      !must be flushed to 0 (in old code INBUF and BUFIN
      !flushed between 2 domain decomposition
      INTBUF_TAB%LNSV(1:NST)  = 0
      INTBUF_TAB%LMSR(1:NMT)  = 0
      INTBUF_TAB%STFNS(1:NSN) = 0
      INTBUF_TAB%STFNM(1:NMN) = 0

       CALL ININT0(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     2    INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN ,NSN   ,NRTS   )
       CALL ININT0(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN   ,NRTM   )
       CALL I3STI3(
     1 X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,IXS,PM   ,
     2 GEO       ,NRTS      ,IXC       ,INTBUF_TAB%STFNS,INTBUF_TAB%NSEGS,
     3 INTBUF_TAB%LNSV,NUMINT ,NSN       ,INTBUF_TAB%NSV,INTBUF_TAB%STFAC,
     4 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG      ,IRS   ,
     5 KNOD2ELS  ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS      ,NOD2ELC   ,
     6 NOD2ELTG  ,IGRSURF(ISU1),THK     ,IXS10   ,
     7 IXS16 ,IXS20        ,ID,TITR,BIDON     ,BIDON   ,
     8 BIDON ,BIDON        ,IGEO   ,FILLSOL  ,PM_STACK, IWORKSH )

       CALL I3STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXS       ,PM        ,
     2 GEO       ,NRTM      ,IXC       ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,
     3 INTBUF_TAB%LMSR,-NUMINT ,NMN      ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC,
     4 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG      ,IRM       ,
     5 KNOD2ELS  ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS      ,NOD2ELC   ,
     6 NOD2ELTG  ,IGRSURF(ISU2),THK          ,IXS10     ,
     7 IXS16     ,IXS20        ,ID,TITR,BIDON       ,BIDON     ,
     8 BIDON     ,BIDON        ,IGEO   ,FILLSOL    ,PM_STACK, IWORKSH  )

       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       ,NMN ,
     3    ITAB,ID,TITR,NRTM)

       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     2    INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN       ,NSN ,
     3    ITAB,ID,TITR,NRTS)

       WRITE(IOUT,2002)
       CALL I3PEN3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),NSN,
     2    ITAB      ,IWPENE    ,ID,TITR)

       WRITE(IOUT,2003)
       CALL I3PEN3(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2    INTBUF_TAB%IRTLS,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%VARIABLES(2),NMN,
     3    ITAB      ,IWPENE    ,ID,TITR)

C

C-----------------------------------------------------------------------
      ELSEIF(NTY==5) THEN
C-----------------------------------------------------------------------
      !must be flushed to 0 (in old code INBUF and BUFIN
      !flushed between 2 domain decomposition
      INTBUF_TAB%LNSV(1:NST) = 0
      INTBUF_TAB%LMSR(1:NMT) = 0
      INTBUF_TAB%STFNS(1:NSN) = 0
      INTBUF_TAB%STFNM(1:NMN) = 0

       CALL ININT0(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN ,NMN   ,NRTM   )
       CALL I3STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXS       ,PM          ,
     2 GEO       ,NRTM      ,IXC       ,INTBUF_TAB%STFNM, INTBUF_TAB%NSEGM  ,
     3 INTBUF_TAB%LMSR,-NUMINT   ,NMN       ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC(1),
     4 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG         ,IRM       ,
     5 KNOD2ELS  ,KNOD2ELC  ,KNOD2ELTG    ,NOD2ELS      ,NOD2ELC   ,
     6 NOD2ELTG  ,IGRSURF(ISU2),THK             ,IXS10     ,
     7 IXS16     ,IXS20        ,ID,TITR,BIDON           ,BIDON     ,
     8 BIDON     ,BIDON        ,IGEO   ,FILLSOL         ,PM_STACK, IWORKSH)
       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       ,NMN ,
     3    ITAB,ID,TITR,NRTM)
       WRITE(IOUT,2002)
       CALL I3PEN3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),
     3    NSN,  ITAB      ,IWPENE    ,ID,TITR)
       IF(INACTI==3 .OR. INACTI==4)THEN
         CALL I5PWR3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),
     3    NSN, ITAB      ,INACTI    )
       END IF
C

C-----------------------------------------------------------------------
      ELSEIF(NTY==7) THEN
C-----------------------------------------------------------------------
C       I_STOK = 0
       DRAD = INTBUF_TAB%VARIABLES(32)

       IF(INTBUF_TAB%S_NIGE/=0) THEN
         ALLOCATE(XE(3*(NUMNOD+INTBUF_TAB%S_NIGE)))
         XE(1:3*NUMNOD) = X(1:3*NUMNOD)
         XE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB%S_NIGE)) = INTBUF_TAB%XIGE(1:3*INTBUF_TAB%S_NIGE)
         PTR_X => XE
       ELSE
         PTR_X => X
       ENDIF

       CALL I7ERR3(
     1    PTR_X     ,NRTM     ,INTBUF_TAB%IRECTM , NOINT, ITAB,ID,TITR,
     2  IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3  X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4  Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5  Z4       ,N11    ,N21    ,N31     ,X0    ,
     6  Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     7  XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     8  ZN3      ,XN4    ,YN4    ,ZN4      )
C----------------
C      CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C----------------
       CALL I7STI3(
     1    PTR_X            ,INTBUF_TAB%IRECTM       , INTBUF_TAB%STFM ,    IXS          , PM              ,
     2    GEO              ,NRTM_FE                 , IXC             ,-NUMINT          , INTBUF_TAB%STFAC,
     3    NTY              ,INTBUF_TAB%VARIABLES(2) , NOINT           ,INTBUF_TAB%STFNS , NSN             ,
     4    MS               ,INTBUF_TAB%NSV          , IXTG            ,IGAP             , RWA             ,
     5    INTBUF_TAB%GAP_S ,INTBUF_TAB%GAP_M        ,INTBUF_TAB%VARIABLES(13),IXT       , IXP             ,
     6    INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(16),INACTI     ,KNOD2ELS         , KNOD2ELC        ,
     7    KNOD2ELTG        ,NOD2ELS                 ,NOD2ELC          ,NOD2ELTG         , IGRSURF(ISU2)   ,
     8    IPARI(47)        ,INTBUF_TAB%IELES        ,INTBUF_TAB%IELEC ,INTBUF_TAB%AREAS , SH4TREE         ,
     9    SH3TREE          ,IPART                   ,IPARTC           ,IPARTTG          , THK             ,
     C    THK_PART         ,INTBUF_TAB%VARIABLES(28),INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,NOD2EL1D         ,
     D    KNOD2EL1D        ,IXR                     ,ITAB             ,INTBUF_TAB%VARIABLES(7),IXS10      ,
     E    IXS16            ,IXS20                   ,ID               ,TITR             , IDDLEVEL        ,
     F    DRAD             ,IGEO                    ,FILLSOL          ,PM_STACK         , IWORKSH         ,
     G    IT19             ,KXIG3D                  ,IXIG3D           ,IPARI(72)        , IPARTS          ,
     H    TAGPRT_FRIC      ,INTBUF_TAB%IPARTFRICS   ,INTBUF_TAB%IPARTFRICM ,INTBUF_FRIC_TAB,NRTM_IGE      ,
     I    IPARI(63)        ,GAPM_MX                 ,GAPS_MX          ,GAPM_L_MX        ,GAPS_L_MX)
       IPARI(21) = IGAP
C
C----------------
C      IREM GAP
C----------------
       IF (IREMNODE == 1) THEN ! IDDLEVEL == 1 !
C
         CALL I7REMNODE(IREMNODE,NOINT,TITR,INTBUF_TAB,NUMNOD+NUMFAKENODIGEO ,
     1   X,NRTM     ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,NSN,
     2 ITAB ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     3 INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,IGAP,INTBUF_TAB%VARIABLES(2),DRAD  ,
     4 IPARI(62) ,NTY       ,IPARI     ,I_MEM_REM ,GAPM_MX            ,
     5 GAPS_MX   ,GAPM_L_MX ,GAPS_L_MX ,ILEV      ,INTBUF_TAB%NBINFLG ,
     6 INTBUF_TAB%MBINFLG,INTBUF_TAB%VARIABLES(46))
C
       END IF
C----------------
C      VOXEL SORT
C----------------
C     I_STOK=INTBUF_TAB%I_STOK(1)
      CALL I7BUC_VOX1(
     1 PTR_X    ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG  ,
     2 NMN      ,NRTM      ,MWA     ,NSN        ,INTBUF_TAB%CAND_E  ,
     3 INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),RWA  ,NOINT    ,INTBUF_TAB%I_STOK(1) ,
     4 INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8), INTBUF_TAB%MSR ,
     5 INTBUF_TAB%STFM ,INTBUF_TAB%STFNS  ,MULTIMP   ,1 ,IDDLEVEL    ,
     6 ITAB      ,INTBUF_TAB%GAP_S  ,INTBUF_TAB%GAP_M, IGAP,INTBUF_TAB%VARIABLES(13),
     7 INTBUF_TAB%VARIABLES(16) ,INACTI  ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML ,I_MEM ,
     8 NCONT     ,ICURV ,INTBUF_TAB%VARIABLES(7), ID           ,  TITR    ,
     9 DRAD      ,INTERCEP, NUMINT,PROV_N,PROV_E,
     1 IX1   ,IX2     ,IX3   ,IX4   ,NSVG   ,
     2 X1    ,X2      ,X3    ,X4    ,Y1    ,
     3 Y2    ,Y3      ,Y4    ,Z1    ,Z2    ,
     4 Z3    ,Z4      ,XI    ,YI    ,ZI    ,
     5 X0    ,Y0      ,Z0    ,XN1   ,YN1   ,
     6 ZN1   ,XN2     ,YN2   ,ZN2   ,XN3   ,
     7 YN3   ,ZN3     ,XN4   ,YN4   ,ZN4   ,
     8 P1    ,P2      ,P3    ,P4    ,LB1   ,
     9 LB2   ,LB3     ,LB4   ,LC1   ,LC2   ,
     1 LC3   ,LC4     ,N11   ,N21   ,N31   ,
     2 STIF  ,PENE    ,IREMNODE  ,IPARI(63),
     3 INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE,INTBUF_TAB%VARIABLES(46))
C I_STOK sauvegarde a sa place pour optimisation tri
         I_STOK=INTBUF_TAB%I_STOK(1)

       IF (I_MEM == 2 ) RETURN
C
C----------------
C
      IF (IDDLEVEL==0)THEN
       IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
         CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
       ENDIF
       NULLIFY(pIXINT)
       IF(SIXINT>0)THEN
         IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
         pIXINT => IXINT(IAD_IXINT)
       ENDIF
       CALL I7NINTDD(
     1   NELEMINT         ,pIXINT                 ,INTBUF_TAB%IRECTM ,
     2   INTBUF_TAB%NSV   ,INTBUF_TAB%CAND_E      ,INTBUF_TAB%CAND_N ,I_STOK ,IFIEND,
     3   PTR_X             ,INTBUF_TAB%VARIABLES(2),DRAD, IGAP,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     4   INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML      ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     .                                                                      INTBUF_TAB%VARIABLES(46))
      ENDIF

      IF((IDDLEVEL==0.OR.NDSOLV==1).AND.(DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
          CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN_FE,NMN_FE,
     .         IWCONT,NSNT,NMNT)

      END IF
cc     ENDIF
C
C     PUIS CALCULER LES PENETRATIONS INITIALES AVEC ONE I7PEN3
C
      IF (IDDLEVEL>0)THEN      ! opti calcul pene initiale uniquement au second passage pour ne pas modifier les candidats
        NGROUS=1+(I_STOK-1)/NVSIZ
C
       DO NG=1,NGROUS
        IF(IPRI>=1) WRITE(IOUT,2007)
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )

        CALL I7COR3(
     1  PTR_X   ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT) ,
     2  INTBUF_TAB%STFM,INTBUF_TAB%STFNS,GAPV    ,IGAP    ,INTBUF_TAB%VARIABLES(2) ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,1,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     4  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,ZERO,IX1     ,IX2   ,
     5  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8  XI     ,YI     ,ZI  ,STIF    ,ZERO  )
         CALL I7DST3(IX3,IX4,X1 ,X2 ,X3 ,
     1               X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2               Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3               YI ,ZI ,X0 ,Y0 ,Z0 ,
     4               XN1,YN1,ZN1,XN2,YN2,
     5               ZN2,XN3,YN3,ZN3,XN4,
     6               YN4,ZN4,P1 ,P2 ,P3 ,
     7               P4 ,LB1,LB2,LB3,LB4,
     8               LC1,LC2,LC3,LC4)
         CALL I7PEN3(ZERO,GAPV,N11,N21,N31 ,
     1                  PENE ,XN1 ,YN1,ZN1,XN2,
     2                  YN2  ,ZN2 ,XN3,YN3,ZN3,
     3                  XN4  ,YN4 ,ZN4,P1 ,P2 ,
     4                  P3   ,P4)
         IF(INACTI==7)THEN
          CALL I18PWR3(ITAB  ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%STFNS,
     1      INTBUF_TAB%STFM,PTR_X ,INTBUF_TAB%NSV,IWPENE    ,INTBUF_TAB%CAND_P,
     2      INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,MWA   ,NOINT     ,GAPV     ,
     3      IX1,IX2,IX3,IX4,NSVG,
     4      PENE)
         ELSE
          FPENMAX   = INTBUF_TAB%VARIABLES(27)
          CALL I7PWR3(ITAB  ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%STFNS,
     1      INTBUF_TAB%STFM,PTR_X  ,INTBUF_TAB%NSV,IWPENE    ,INTBUF_TAB%CAND_P,
     2      INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,MWA      , NOINT     ,GAPV         ,
     3      NTY          ,ITIED    ,FPENMAX   ,ID        ,TITR     ,
     4      IDDLEVEL     ,IREMNODE ,INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE,ISTOK,
     5      IX1 ,IX2    ,IX3    ,IX4    ,NSVG   ,
     6      X1  ,X2     ,X3     ,X4     ,Y1     ,
     7      Y2  ,Y3     ,Y4     ,Z1     ,Z2     ,
     8      Z3  ,Z4     ,XI     ,YI     ,ZI     ,
     9      N11 ,N21    ,N31    ,PENE   )
         ENDIF
         IF(IWPENE/=0.AND.INACTI==3.OR.INACTI==4) IWRN = 1
        ENDDO
        CALL ANCMSG(MSGID=1164,
     .          MSGTYPE=MSGWARNING,
     .          ANMODE=ANINFO_BLIND_1,
     .          I1=ID,
     .          C1=TITR,
     .          PRMOD=MSG_PRINT)
        DO I=1,NUMNOD+NUMFAKENODIGEO
         INPENE=INPENE+MIN(MWA(I),1)
        ENDDO
        IF(ISTOK==0.AND.(INACTI==5.OR.INACTI==6))
     .   IPARI(22) = -INACTI ! passage en negatif pour dimensionnenment
        INTBUF_TAB%I_STOK(1)=ISTOK
       ENDIF
C
      ELSEIF(NTY==8) THEN
       CALL ININT0_8(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,
     .               INTBUF_TAB%MSR,
     1    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN ,NMN       ,NRTM   ,
     2    NUMNOD)
       CALL I3STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXS ,PM       ,
     2 GEO       ,NRTM      ,IXC       ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,
     3 INTBUF_TAB%LMSR,-NUMINT   ,NMN  ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC(1),
     4 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG      ,IRS   ,
     5 KNOD2ELS  ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS   ,NOD2ELC   ,
     6 NOD2ELTG  ,IGRSURF(ISU2),THK       ,IXS10     ,
     7 IXS16     ,IXS20        ,ID,TITR,INTBUF_TAB%GAPN,INTBUF_TAB%STF8  ,
     8 INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(4), IGEO ,FILLSOL    ,
     9 PM_STACK , IWORKSH)
       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       ,NMN ,
     3    ITAB,ID,TITR,NRTM)
       WRITE(IOUT,2002)
       CALL I3PEN3(
     1   X     ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2   INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),NSN ,
     3   ITAB            ,IWPENE    ,ID,TITR)
C-----------------------------------------------------------------------
      ELSEIF(NTY==9) THEN
C-----------------------------------------------------------------------

      !must be flushed to 0 (in old code INBUF and BUFIN
      !flushed between 2 domain decomposition
      INTBUF_TAB%LNSV(1:NST) = 0
      INTBUF_TAB%LMSR(1:NMT) = 0
      INTBUF_TAB%STFNS(1:NSN) = 0
      INTBUF_TAB%STFNM(1:NMN) = 0

       CALL ININT0(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     2    INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN ,NSN            ,NRTS   )
       CALL ININT0(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN ,NMN            ,NRTM   )

       CALL I9STI3(
     1   X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,IXS ,PM ,
     2   GEO       ,NRTS             ,IXC       ,INTBUF_TAB%STFNS,INTBUF_TAB%NSEGS,
     3   INTBUF_TAB%LNSV,NUMINT      ,NSN       ,INTBUF_TAB%NSV,INTBUF_TAB%STFAC(1),
     4   NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG      ,INTBUF_TAB%IELES,
     5   KNOD2ELS  ,NOD2ELS   ,IGRSURF   ,ISU1      ,IXS10     ,
     6   IXS16     ,IXS20     ,ID        ,TITR)
       CALL I9STI3(
     1   X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXS       ,PM        ,
     2   GEO       ,NRTM      ,IXC       ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,
     3   INTBUF_TAB%LMSR,-NUMINT   ,NMN    ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC(1),
     4   NTY       ,INTBUF_TAB%VARIABLES(2),NOINT   ,IXTG      ,INTBUF_TAB%IELEM,
     5   KNOD2ELS  ,NOD2ELS   ,IGRSURF     ,ISU2    ,IXS10     ,
     6   IXS16     ,IXS20     ,ID       ,TITR)
       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       ,NMN ,
     3    ITAB,ID,TITR,NRTM)
       CALL INVOI3(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     2    INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN       ,NSN ,
     3    ITAB,ID,TITR,NRTS)
       IF(NMN>0)THEN
         WRITE(IOUT,2002)
         CALL I3PEN3(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),NSN,
     3    ITAB      ,IWPENE    ,ID,TITR)
         WRITE(IOUT,2003)
         CALL I3PEN3(
     1    X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2    INTBUF_TAB%IRTLS,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%VARIABLES(2),NMN,
     3    ITAB      ,IWPENE    ,ID,TITR)
C
       ENDIF
C-----------------------------------------------------------------------
      ELSEIF(NTY==10) THEN
C-----------------------------------------------------------------------
C      K14 = ELEMS CANDIDATS A L'IMPACT...
C      K15 = NOEUDS CANDIDATS A L'IMPACT + ADRESSE ELEMS CORRES.
       ITIED=NINT(INTBUF_TAB%VARIABLES(1))
C
C      CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C
        IBIDON = 0
        BIDON = ZERO

       CALL I7STI3(
     1             X         ,INTBUF_TAB%IRECTM       ,INTBUF_TAB%STFM  ,IXS     ,PM              ,
     2             GEO       ,NRTM                    ,IXC              ,-NUMINT ,INTBUF_TAB%STFAC,
     3             NTY       ,INTBUF_TAB%VARIABLES(2) ,NOINT            ,INTBUF_TAB%STFNS, NSN    ,
     4             MS        ,INTBUF_TAB%NSV          ,IXTG             ,IGAP            , RWA    ,
     5      INTBUF_TAB%GAP_S ,INTBUF_TAB%GAP_M        ,INTBUF_TAB%VARIABLES(13),IXT      ,IXP     ,
     6      INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(16),INACTI     ,KNOD2ELS        ,KNOD2ELC,
     9            KNOD2ELTG  ,NOD2ELS                 ,NOD2ELC          ,NOD2ELTG        ,IGRSURF(ISU2),
     A            IBIDON     ,IBIDON                  ,IBIDON           ,BIDON           ,SH4TREE ,
     B            SH3TREE    ,IPART                   ,IPARTC           ,IPARTTG         ,THK     ,
     C            THK_PART   ,INTBUF_TAB%VARIABLES(27),RDUM             ,RDUM            ,NOD2EL1D,
     D            KNOD2EL1D  ,IXR                     ,ITAB     ,INTBUF_TAB%VARIABLES(7) ,IXS10   ,
     E            IXS16      ,IXS20                   ,ID               ,TITR            ,IDDLEVEL,
     F            DRAD       ,IGEO                    ,FILLSOL          ,PM_STACK        ,IWORKSH ,
     G            IT19       ,BID                     ,BID              ,IBIDON          ,IPARTS  ,
     H            IBIDON     ,IBIDON                  ,IBIDON           ,IBIDON          ,IBIDON  ,
     I            IPARI(63)  ,GAPM_MX                 ,GAPS_MX          ,GAPM_L_MX        ,GAPS_L_MX)
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       CALL I7BUC1(
     1   X          ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG,
     2   NMN        ,NRTM        ,MWA       ,NSN         ,INTBUF_TAB%CAND_E,
     3   INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),RWA    ,NOINT  ,I_STOK        ,
     4   INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX ,MINBOX ,INTBUF_TAB%MSR ,
     5   INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS  ,MULTIMP   ,1     ,IDDLEVEL   ,
     6   ITAB      ,INTBUF_TAB%GAP_S ,INTBUF_TAB%GAP_M,IGAP ,INTBUF_TAB%VARIABLES(13),
     7   INTBUF_TAB%VARIABLES(16) ,INACTI ,RDUM,RDUM ,I_MEM ,
     8   ID,TITR,IT19,PROV_N,PROV_E,
     9   NSVG,IX1 ,IX2 ,IX3   ,IX4   ,
     1   N11 ,N21 ,N31 ,PENE  ,X1    ,
     2   X2  ,X3  ,X4  ,Y1    ,Y2    ,
     3   Y3  ,Y4  ,Z1  ,Z2    ,Z3    ,
     4   Z4  ,XI  ,YI  ,ZI    ,X0    ,
     5   Y0  ,Z0  ,XN1 ,YN1   ,ZN1   ,
     6   XN2 ,YN2 ,ZN2 ,XN3   ,YN3   ,
     7   ZN3 ,XN4 ,YN4 ,ZN4   ,P1    ,
     8   P2  ,P3  ,P4  ,LB1   ,LB2   ,
     9   LB3 ,LB4 ,LC1 ,LC2   ,LC3   ,
     1   LC4,STIF)
       IF (I_MEM == 2) RETURN
       INTBUF_TAB%VARIABLES(9)  = MAXBOX
       INTBUF_TAB%VARIABLES(12) = MINBOX
C
C      PUIS CALCULER LES PENETRATIONS INITIALES AVEC ONE I7PEN3
C
       NGROUS=1+(I_STOK-1)/NVSIZ
C
       DO NG=1,NGROUS
        IF(IPRI>=1) WRITE(IOUT,2007)
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )
        CALL I7COR3(
     1  X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT) ,
     2  INTBUF_TAB%STFM,INTBUF_TAB%STFNS,GAPV    ,IGAP    ,INTBUF_TAB%VARIABLES(2) ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,1,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     4  RDUM,RDUM,DRAD,IX1     ,IX2   ,
     5  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8  XI     ,YI     ,ZI  ,STIF    ,INTBUF_TAB%VARIABLES(46))
        CALL I7DST3(IX3,IX4,X1 ,X2 ,X3 ,
     1               X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2               Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3               YI ,ZI ,X0 ,Y0 ,Z0 ,
     4               XN1,YN1,ZN1,XN2,YN2,
     5               ZN2,XN3,YN3,ZN3,XN4,
     6               YN4,ZN4,P1 ,P2 ,P3 ,
     7               P4 ,LB1,LB2,LB3,LB4,
     8               LC1,LC2,LC3,LC4)
        CALL I7PEN3(ZERO,GAPV,N11,N21,N31,
     1                  PENE ,XN1 ,YN1,ZN1,XN2,
     2                  YN2  ,ZN2 ,XN3,YN3,ZN3,
     3                  XN4  ,YN4 ,ZN4,P1 ,P2 ,
     4                  P3   ,P4)
        INACTI = 0
        FPENMAX   = INTBUF_TAB%VARIABLES(27)
          CALL I7PWR3(ITAB  ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%STFNS,
     1      INTBUF_TAB%STFM,X     ,INTBUF_TAB%NSV,IWPENE    ,INTBUF_TAB%CAND_P,
     2      INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,MWA      , NOINT     ,GAPV         ,
     3      NTY          ,ITIED    ,FPENMAX   ,ID        ,TITR     ,
     4      IDDLEVEL     ,IREMNODE ,IBIDON    ,IBIDON    ,ISTOK    ,
     5      IX1,IX2,IX3,IX4,NSVG,
     6      X1 ,X2 ,X3 ,X4 ,Y1  ,
     7      Y2 ,Y3 ,Y4 ,Z1 ,Z2  ,
     8      Z3 ,Z4 ,XI ,YI ,ZI  ,
     9      N11,N21,N31,PENE)
        IF(IWPENE/=0.AND.INACTI==3.OR.INACTI==4) IWRN = 1
       ENDDO
       CALL ANCMSG(MSGID=1164,
     .          MSGTYPE=MSGWARNING,
     .          ANMODE=ANINFO_BLIND_1,
     .          I1=ID,
     .          C1=TITR,
     .          PRMOD=MSG_PRINT)

       DO I=1,NUMNOD
         INPENE=INPENE+MIN(MWA(I),1)
       ENDDO
       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     +    (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                   NSNT,NMNT)
       END IF
C-----------------------------------------------------------------------
      ELSEIF(NTY==11) THEN
C-----------------------------------------------------------------------
C      CALCUL DES RIGIDITES ELEMENTTAIRES 
C
       GAP0 =INTBUF_TAB%VARIABLES(2)
       GAPINF = EP30
C        cette initialisation est necessaire seulement apres l appel de i11edge
C        dans lecins 
C        qui compacte sur deux emplacements les segments 
C        ils sont sur quatre emplacements a la lecture : 
C           [2 cases remplies puis deux cases vides]
C        cette initialisation specifique est donc faite dans inint3 
C        en dehors de bufinti
C        seul endroit ou BUFINTI est appele apres l appel de i11edge
C        => pour l'engine tout est sur deux emplacement

C      INTBUF_TAB%VARIABLES(1) EST MODIFIE PAR I11STI3; GAP0 NON

       CALL I11STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM  ,IXS       ,PM        ,
     2 GEO       ,NRTM         ,IXC         ,NUMINT    ,INTBUF_TAB%STFAC,
     3 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT        ,INTBUF_TAB%GAP_M,
     4 MS        ,IXTG         ,IXT         ,IXP       ,IXR       ,
     5 IGAP      ,INTBUF_TAB%VARIABLES(13),GAP0     ,GAPINF    ,IPARTC       ,
     6 IPARTTG   ,THK      ,THK_PART  ,INTBUF_TAB%VARIABLES(28),INTBUF_TAB%GAP_ML,
     7 NOD2EL1D  ,KNOD2EL1D     ,ITAB       ,IXS10     ,ID,TITR      ,
     8 KXX       ,IXX           ,IGEO       ,KNOD2ELS  ,KNOD2ELC     ,
     9 KNOD2ELTG ,NOD2ELS       ,NOD2ELC    ,NOD2ELTG  ,LELX         ,
     A FILLSOL   ,INTTH         ,DRAD       ,INTBUF_TAB%AREAM ,INTBUF_TAB%IELES ,
     B PM_STACK  ,IWORKSH       ,IT19 ,INTBUF_TAB%VARIABLES(7),IPARI(72)  ,
     C IPARTS    ,TAGPRT_FRIC   ,INTBUF_TAB%IPARTFRICM,INTBUF_FRIC_TAB,
     D IPARTT    ,IPARTP        ,IPARTX     ,IPARTR, IPARI(63))
C 
      DRAD = INTBUF_TAB%VARIABLES(24)
       CALL I11STI3(
     1 X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS  ,IXS      ,PM        ,
     2 GEO       ,NRTS         ,IXC         ,-NUMINT,INTBUF_TAB%STFAC(1),
     3 NTY       ,INTBUF_TAB%VARIABLES(2),NOINT     ,INTBUF_TAB%GAP_S,
     4 MS        ,IXTG         ,IXT         ,IXP    ,IXR       ,
     5 IGAP      ,INTBUF_TAB%VARIABLES(13),GAP0     ,GAPINF    ,IPARTC        ,
     6 IPARTTG   ,THK      ,THK_PART  ,INTBUF_TAB%VARIABLES(28),INTBUF_TAB%GAP_SL,
     7 NOD2EL1D  ,KNOD2EL1D     ,ITAB       ,IXS10    ,ID,TITR      ,
     8 KXX       ,IXX           ,IGEO       ,KNOD2ELS ,KNOD2ELC     ,
     9 KNOD2ELTG ,NOD2ELS       ,NOD2ELC    ,NOD2ELTG  ,LELX         ,
     A FILLSOL   ,INTTH         , DRAD      ,INTBUF_TAB%AREAS,INTBUF_TAB%IELEC,
     B PM_STACK  ,IWORKSH       ,IT19       ,INTBUF_TAB%VARIABLES(7),IPARI(72)  ,
     C IPARTS    ,TAGPRT_FRIC,INTBUF_TAB%IPARTFRICS,INTBUF_FRIC_TAB,
     D IPARTT    ,IPARTP        ,IPARTX     ,IPARTR, IPARI(63))
C
       INTBUF_TAB%VARIABLES(6)=MAX(GAPINF,INTBUF_TAB%VARIABLES(13))
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
C
       IF (IREMNODE == 1) THEN
C
          ALLOCATE(INOD2LIN(NUMNOD+1),TAGSECND(NUMNOD),NOD2LIN(2*NRTM))
          I_START = 1
          I_MEM_REM = 0
          DO WHILE (I_START < NRTM)
C
            CALL I11REMLINE(
     1      X,NRTM,INTBUF_TAB%IRECTM,NRTS,INTBUF_TAB%IRECTS,
     2      NUMNOD,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),IGAP,
     3      INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE, INTBUF_TAB%VARIABLES(2), DRAD,IPARI(62),
     4      I_START,I_MEM_REM,INOD2LIN,TAGSECND,NOD2LIN,INTBUF_TAB%VARIABLES(46))
C
C           Reallocation of REMNODE arrays if necessary
C
            IF (I_MEM_REM == 1) THEN
              NEW_SIZE = IPARI(62) + 5*NRTM
              CALL UPGRADE_REMNODE(IPARI(1),NEW_SIZE,INTBUF_TAB,NTY)
              I_MEM_REM = 0
            ENDIF
C
          ENDDO
C
          DEALLOCATE(INOD2LIN,TAGSECND,NOD2LIN)
          IREMNODE = 0
C
        ENDIF

c      CALL I11BUC1(
c    1   X         ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%IRECTS,INTBUF_TAB%VARIABLES(4),NRTS,
c    2   NMN       ,NRTM        ,MWA       ,NSN         ,INTBUF_TAB%CAND_E,
c    3   INTBUF_TAB%CAND_N,INTBUF_TAB%VARIABLES(2),RWA       ,NOINT    ,I_STOK    ,
c    4   INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX  ,MINBOX ,INTBUF_TAB%MSR,
c    5   INTBUF_TAB%NSV,MULTIMP     ,INTBUF_TAB%ADCCM,INTBUF_TAB%CHAIN,
c    6   I_MEM     ,ID,TITR     ,IDDLEVEL  ,DRAD, IT19)

       CALL I11BUC_VOX1(
     1   X         ,INTBUF_TAB%IRECTS  ,INTBUF_TAB%IRECTM       ,NRTS               ,NMN,
     2   NRTM      ,NSN                ,INTBUF_TAB%CAND_E       ,INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),
     3   NOINT    ,I_STOK              ,INTBUF_TAB%VARIABLES(8) ,MAXBOX             ,MINBOX,
     4   NCONT    ,MULTIMP,                 INTBUF_TAB%MSR,
     5   INTBUF_TAB%ADCCM,INTBUF_TAB%CHAIN,ITAB,INTBUF_TAB%NSV,
     6   0, I_MEM     ,ID,TITR     ,IDDLEVEL,INTBUF_TAB%VARIABLES(4),
     7   DRAD, INTERCEP,IGAP,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     8   INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML ,INTBUF_TAB%VARIABLES(13),IPARI(63),INTBUF_TAB%KREMNODE,
     3   INTBUF_TAB%REMNODE,INTBUF_TAB%VARIABLES(46))
C
       INTBUF_TAB%VARIABLES(5) = 0
C
       IF (I_MEM == 2 ) RETURN
       INTBUF_TAB%VARIABLES(9)  = MAXBOX
       INTBUF_TAB%VARIABLES(12) = MINBOX
C
       IF (IDDLEVEL==0)THEN
         IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
           CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
         ENDIF
         NULLIFY(pIXINT)
         IF(SIXINT>0)THEN
           IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
           pIXINT => IXINT(IAD_IXINT)
         ENDIF
         CALL I11NINTDD(
     .     NELEMINT          ,pIXINT                  ,INTBUF_TAB%IRECTS ,
     .     INTBUF_TAB%IRECTM ,INTBUF_TAB%CAND_N       ,INTBUF_TAB%CAND_E , I_STOK ,IFIEND,
     .     X                 ,INTBUF_TAB%VARIABLES(1) ,DRAD              ,INTBUF_TAB%VARIABLES(46))
       ENDIF

       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     +    (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                 NSNT,NMNT)
       END IF
C
C      PUIS CALCULER LES PENETRATIONS INITIALES AVEC ONE I11PEN3
C
       NGROUS=1+(I_STOK-1)/NVSIZ
C
       IF(IPRI>=1) WRITE(IOUT,2011)
C

       PRINT_WARNING = .TRUE.
       DO NG=1,NGROUS
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )
        JLT_NEW = 0
        CALL I11DST3(
     1   LLT    ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%IRECTS,
     2   INTBUF_TAB%IRECTM,NX          ,NY         ,NZ        ,
     4   N1        ,N2          ,M1         ,M2        ,JLT_NEW   ,
     5   X         ,IGAP        ,INTBUF_TAB%GAP_S ,INTBUF_TAB%GAP_M,GAPV,
     6   INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,DRAD ,INTBUF_TAB%VARIABLES(46))
        LLT = JLT_NEW
C
        CALL I11PWR3(ITAB   ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),
     2    INTBUF_TAB%STFS,INTBUF_TAB%STFM,X      ,INTBUF_TAB%NSV,IWPENE ,
     3    N1        ,N2        ,M1     ,M2        ,NX        ,
     4    NY        ,NZ        ,GAPV   ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     5    INTBUF_TAB%PENIS,INTBUF_TAB%PENIM,IGAP,PRINT_WARNING)
        IF(IWPENE/=0.AND.INACTI==3.OR.INACTI==4) IWRN = 1

       ENDDO
       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     +    (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                 NSNT,NMNT)
       END IF
C-----------------------------------------------------------------------
      ELSEIF(NTY==12) THEN
C-----------------------------------------------------------------------
        ITIED=INT(INTBUF_TAB%VARIABLES(1))
        IF(INT(INTBUF_TAB%VARIABLES(1))==2)
     .  CALL IN12R(X,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,NSN,1)

        !flushed between 2 domain decomposition
        INTBUF_TAB%LMSR(1:NMT) = 0
        CALL ININT0(
     1    X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR ,INTBUF_TAB%MSR,
     2    INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN       ,NMN        ,NRTM   )
       IPARI(30)=-1
      CALL I12CHK3(X,INTBUF_TAB%IRECTM,IXS       ,NRTM     ,IXC        ,
     1          -NUMINT,NMN    ,INTBUF_TAB%MSR,NOINT     ,MWA        ,
     2          IXTG,GEO,PM,INTBUF_TAB%IELEM, ALE_CONNECTIVITY,
     3          IPARI(30),INTBUF_TAB%FCOUNT,ITIED, ITAB,KNOD2ELS,
     4          NOD2ELS,NTY,ID,TITR)
       CALL I12CHK3(X,INTBUF_TAB%IRECTS,IXS       ,NRTS     ,IXC,
     1          NUMINT,NSN      ,INTBUF_TAB%NSV,NOINT     ,MWA,
     2          IXTG,GEO,PM,INTBUF_TAB%IELES, ALE_CONNECTIVITY,
     3          IPARI(30),INTBUF_TAB%FCOUNT,ITIED, ITAB,KNOD2ELS,
     4          NOD2ELS,NTY,ID,TITR)
       IF(IPARI(30)==0)THEN
          CALL ANCMSG(MSGID=1250,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)

       ELSEIF(ITIED==3)THEN
         IPARI(30)=0
       ENDIF
        CALL INVOI3(
     1     X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2     INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN       ,NMN ,
     3     ITAB,ID,TITR,NRTM)
        WRITE(IOUT,2002)
        CALL I12TID3(
     1     X,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV  ,
     2     INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,NSN    ,ITAB   ,INTBUF_TAB%VARIABLES(2),
     3     ID,TITR)
       IF(INT(INTBUF_TAB%VARIABLES(1))==2)
     .  CALL IN12R(X,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,NSN,2)
C
C-----------------------------------------------------------------------
      ELSEIF(NTY==17.AND.IPARI(33)==0) THEN
C-----------------------------------------------------------------------
       IGN   = IPARI(36)
       IGE   = IPARI(34)
       NMES  = IGRBRIC(IGN)%NENTITY
       NME   = IGRBRIC(IGE)%NENTITY
C
C      CALCUL DES RIGIDITES ELEMENTTAIRES
C
       CALL I17STI3(
     1 IXS     ,PM        ,NME    ,NMES  ,IGRBRIC(IGE)%ENTITY,IGRBRIC(IGN)%ENTITY,
     2 INTBUF_TAB%KM,INTBUF_TAB%KS)
C-----------------------------------------------------------------------
      ELSEIF(NTY==20) THEN
C-----------------------------------------------------------------------
       CALL I20INI3(X       ,IXS    ,IXC     ,
     2              PM      ,GEO     ,IPARI   ,NUMINT ,ITAB    ,
     3              MS      ,MWA     ,RWA     ,IXTG   ,IWRN    ,
     4              IKINE   ,IXT     ,IXP     ,IXR    ,NELEMINT,
     5              IDDLEVEL,IFIEND  ,NSNET   ,
     6              NMNET   ,IWCONT ,NSNT    ,
     7              NMNT    ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,
     8              NOD2ELC ,NOD2ELTG,IGRSURF ,IKINE1 ,IPART   ,
     9              IPARTC  ,IPARTTG ,THK     ,THK_PART,INPENE ,
     A              IWPENE  ,IXS10,I_MEM     ,
     B              SIXINT  ,IXS16   ,IXS20   ,ID   ,TITR      ,
     C              KXX     ,IXX     ,IGEO  ,NOD2EL1D,KNOD2EL1D,
     D              LELX    ,INTBUF_TAB , PM_STACK , IWORKSH)
       IF (I_MEM ==2) RETURN
C-----------------------------------------------------------------------
      ELSEIF(NTY==21) THEN
C-----------------------------------------------------------------------
       !must be flushed to 0 (in old code INBUF and BUFIN
       !flushed between 2 domain decomposition)
       INTBUF_TAB%AREAS(1:NSN) = 0
       IF(INTTH/=0)THEN
         INTBUF_TAB%AS(1:NSN) = 0
         INTBUF_TAB%BS(1:NSN) = 0
       ENDIF

C      detecte les segments de surface nulle.
       CALL I7ERR3(
     1  X         ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT     ,ITAB,ID,TITR,
     2  IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3  X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4  Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5  Z4       ,N11    ,N21    ,N31     ,X0    ,
     6  Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     7  XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     8  ZN3      ,XN4    ,YN4    ,ZN4      )
C
C      RECHERCHE DES ELTS SECND et CHECK DES ELTS main  
C      + Preparation Thermique.
C
       CALL I21ELS3(
     1 X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM  ,NRTS      ,NRTM      ,
     2 GEO     ,IXS        ,PM        ,IXC        ,IXTG      ,
     3 -NUMINT   ,NTY        ,NOINT     ,NSN      ,INTBUF_TAB%NSV  ,
     4 INTBUF_TAB%IELES,IPARI(47) ,INTBUF_TAB%AREAS,NMN   ,INTBUF_TAB%MSR,
     5 KNOD2ELS   ,KNOD2ELC    ,KNOD2ELTG ,NOD2ELS  ,NOD2ELC      ,
     6 NOD2ELTG  ,IGRSURF(ISU1),IGRSURF(ISU2),IELEM21   ,
     7 THK     ,INTBUF_TAB%AS,INTBUF_TAB%BS,IXS10    ,IXS16      ,
     8 IXS20     ,ID,TITR,IGEO,SH4TREE   ,
     9 SH3TREE   ,IPART        ,IPARTC       ,IPARTTG  , PM_STACK ,
     A IWORKSH )
C
C   ...CALCUL DU GAP et des PENETRATIONS INITIALES, apres INITIA (lecture des epaisseurs). 
C
C-----------------------------------------------------------------------
      ELSEIF(NTY==22) THEN
C-----------------------------------------------------------------------
C     #########################################
C     #     FAIRE UNE VERIFIACTION ICI        #
C     #     Verifier si loi 51 ou 37 utilise.#
C     #########################################
      IF(ISU1>0)THEN
        CALL I22ERR3(
     1    X         ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT                   ,ITAB      ,
     2    PM        ,IXS      ,IGRBRIC(ISU1)%NENTITY ,IGRBRIC(ISU1)%ENTITY   ,ID        ,
     3    TITR,
     4    IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     5    X2       ,X3     ,X4     ,Y1     ,Y2     ,
     6    Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     7    Z4       ,N11    ,N21    ,N31     ,X0    ,
     8    Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     9    XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     1    ZN3      ,XN4    ,YN4    ,ZN4      )
      ENDIF

C
C      CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C
       CALL I22STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM  ,IXS       ,PM        ,
     2 GEO       ,NRTM      ,IXC         ,-NUMINT   ,INTBUF_TAB%STFAC,
     3            NTY       ,INTBUF_TAB%VARIABLES(2),NOINT     ,
     4 INTBUF_TAB%STFNS,NSN       ,MS          ,INTBUF_TAB%NSV,IXTG      ,
     5 IGAP  ,RWA       ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),
     6 IXT   ,IXP       ,INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(16),
     8 INACTI    ,KNOD2ELS  ,KNOD2ELC   ,KNOD2ELTG ,NOD2ELS   ,
     A NOD2ELC   ,NOD2ELTG  ,IPARI(47) ,
     B INTBUF_TAB%IELES,INTBUF_TAB%IELEC, INTBUF_TAB%AREAS  ,SH4TREE   ,SH3TREE,
     C IPART     ,IPARTC    ,IPARTTG   ,THK ,THK_PART,
     D INTBUF_TAB%VARIABLES(28),INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,NOD2EL1D,KNOD2EL1D,
     E IXR       ,ITAB,INTBUF_TAB%VARIABLES(7),IXS10       ,IXS16   ,IXS20    ,
     F ID,TITR   ,IGEO ,PM_STACK, IWORKSH )

       IPARI(21) = IGAP
       IAD       = IPARI(39)
       ISU1      = IPARI(45)
       IF(ISU1>0)THEN
         NBRIC     = IGRBRIC(ISU1)%NENTITY
       ELSE
         NBRIC     = 0
       ENDIF

       IAD=MAX(IAD,1) !in case of ISU1=0 (empty group => normal termination with error message)

       CALL I22TZINF(
     1   X               ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG,
     2   NMN             ,NRTM           ,MWA          ,NSN            ,INTBUF_TAB%IRTLM   ,
     3   INTBUF_TAB%CAND_N   ,INTBUF_TAB%VARIABLES(2),RWA          ,NOINT          ,I_STOK          ,
     4   INTBUF_TAB%VARIABLES(5) ,INTBUF_TAB%VARIABLES(8),MAXBOX   ,MINBOX     ,INTBUF_TAB%MSR ,
     5   INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS  ,MULTIMP      ,1              ,IDDLEVEL        ,
     6   ITAB            ,INTBUF_TAB%GAP_S  ,INTBUF_TAB%GAP_M,IGAP           ,INTBUF_TAB%VARIABLES(13),
     7   INTBUF_TAB%VARIABLES(16),INACTI         ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML  ,I_MEM     ,
     8   ID              ,TITR           ,IGRBRIC(ISU1)%ENTITY ,NBRIC          ,IXS              )
C-----------------------------------------------------------------------
      ELSEIF(NTY==23) THEN
C-----------------------------------------------------------
C      detecte les segments de surface nulle.
       CALL I7ERR3(
     1  X      ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT     ,ITAB,ID,TITR,
     2  IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3  X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4  Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5  Z4       ,N11    ,N21    ,N31     ,X0    ,
     6  Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     7  XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     8  ZN3      ,XN4    ,YN4    ,ZN4      )
C
C      CALCUL DU GAP / RECHERCHE DES ELTS SECND et CHECK DES ELTS main  
C
       CALL I23GAP3(
     1 X     ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM  ,NRTS      ,NRTM      ,
     2 GEO     ,IXS        ,PM        ,IXC        ,IXTG      ,
     3 -NUMINT   ,NTY        ,NOINT     ,NSN      ,INTBUF_TAB%NSV  ,
     4 IPARI(47) ,NMN   ,INTBUF_TAB%MSR,RWA        ,
     5 KNOD2ELS   ,KNOD2ELC    ,KNOD2ELTG ,NOD2ELS  ,NOD2ELC      ,
     6 NOD2ELTG   ,THK         ,IXS10      ,IXS16    ,IXS20      ,
     7 IPARTC      ,IPARTTG     ,INTBUF_TAB%VARIABLES(2),IGAP,INTBUF_TAB%GAP_S,
     8 INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(16),
     .       INTBUF_TAB%VARIABLES(19),INTBUF_TAB%VARIABLES(7),
     9 INTBUF_TAB%STFNS,INTBUF_TAB%STFM,ID,TITR,INTBUF_TAB%GAP_M, IGEO  ,
     A  PM_STACK , IWORKSH)
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       CALL I23BUC1(
     1   X  ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG ,
     2   NMN         ,NRTM      ,MWA       ,NSN ,INTBUF_TAB%CAND_E ,
     3   INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),RWA ,NOINT ,I_STOK        ,
     4   INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX ,MINBOX ,INTBUF_TAB%MSR ,
     5   INTBUF_TAB%STFM  ,MULTIMP   ,ITAB        ,INTBUF_TAB%GAP_S  ,IGAP  ,
     6   INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16) ,INACTI ,NRTS ,INTBUF_TAB%IRECTS,
     7   I_MEM ,IDDLEVEL,ID,TITR,INTBUF_TAB%GAP_M,
     8   PROV_N,PROV_E ,IX1,IX2,
     9   IX3   ,IX4   ,NSVG   ,X1 ,X2 ,
     1   X3    ,X4    ,Y1     ,Y2 ,Y3 ,
     2   Y4    ,Z1    ,Z2     ,Z3 ,Z4 ,
     3   XI    ,YI    ,ZI     ,X0 ,Y0 ,
     4   Z0    ,XN1   ,YN1    ,ZN1,XN2,
     5   YN2   ,ZN2   ,XN3    ,YN3,ZN3,
     6   XN4   ,YN4   ,ZN4    ,P1 ,P2 ,
     7   P3    ,P4    ,LB1    ,LB2,LB3,
     8   LB4   ,LC1   ,LC2    ,LC3,LC4,
     9   N11   ,N21   ,N31    ,PENE    )
       IF (I_MEM == 2 ) RETURN
       INTBUF_TAB%VARIABLES(9) = MAXBOX
       INTBUF_TAB%VARIABLES(12) = MINBOX
       IF (IDDLEVEL==0)THEN
           IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
             CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
           ENDIF
           NULLIFY(pIXINT)
           IF(SIXINT>0)THEN
             IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
             pIXINT => IXINT(IAD_IXINT)
           ENDIF
       CALL I7NINTDD(NELEMINT ,pIXINT               ,INTBUF_TAB%IRECTM  ,
     2     INTBUF_TAB%NSV   ,INTBUF_TAB%CAND_E      ,INTBUF_TAB%CAND_N  ,I_STOK  ,IFIEND,
     3     X                ,INTBUF_TAB%VARIABLES(2),DRAD, IGAP,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     4     INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML      ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     .                                                                        INTBUF_TAB%VARIABLES(46))
       ENDIF

       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     .  (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
      CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .       NSNT,NMNT)
       END IF
C
C      PUIS CALCULER LES PENETRATIONS INITIALES AVEC ONE I23DST3
C
       NGROUS=1+(I_STOK-1)/NVSIZ
C
       DO 723 NG=1,NGROUS

        IF(IPRI>=1) WRITE(IOUT,2007)
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )
        CALL I23DST3(
     1    LLT ,INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%IRECTM,
     .    INTBUF_TAB%NSV  ,
     2    INTBUF_TAB%GAP_S,X     ,INTBUF_TAB%MSR,PENE,INTBUF_TAB%IFPEN(1+NFT) ,
     3    IGAP ,INTBUF_TAB%VARIABLES(2), INTBUF_TAB%VARIABLES(16),
     .       INTBUF_TAB%VARIABLES(13), GAPV,
     4    INTBUF_TAB%GAP_M)

        CALL I23PWR3(
     .   ITAB   ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),
     .                                        INTBUF_TAB%STFNS,
     1   X      ,LLT   ,INTBUF_TAB%NSV,IWPENE ,PENE         ,
     2   NOINT  ,NTY   ,INTBUF_TAB%GAP_S,INTBUF_TAB%MSR,
     3   INTBUF_TAB%IRECTM ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     .                                            INTBUF_TAB%VARIABLES(27),
     4   NSN    ,MWA   ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     5   INTBUF_TAB%CAND_P,INTBUF_TAB%STFM,INTBUF_TAB%IFPEN(1+NFT),INTBUF_TAB%IFPEN,GAPV  )

 723   CONTINUE

C
       IF(IWPENE/=0) THEN
          CALL ANCMSG(MSGID=499,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IWPENE)
       ENDIF
C
       IF(IWPENE==0.AND.(INACTI==5.OR.INACTI==6))
     .   IPARI(22) = -INACTI     ! passage en negatif pour dimensionnenment
       INTBUF_TAB%I_STOK(1)=IWPENE
C
C   ...CALCUL DU GAP et des PENETRATIONS INITIALES, apres INITIA (lecture des epaisseurs). 
C
C-----------------------------------------------------------------------
      ELSEIF(NTY==24) THEN
C-----------------------------------------------------------------------
       IPEN0 = IPARI(54)
       NRTSE = IPARI(52)
       NSNE  = IPARI(55)
       NRTM0=NRTM-NRTM_SH
       NSN0=NSN-NSNE
       INTBUF_TAB%MVOISIN(1:4*NRTM)=0
       CALL I7ERR3(
     1  X         ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT     ,ITAB,ID,TITR,
     2  IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3  X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4  Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5  Z4       ,N11    ,N21    ,N31     ,X0    ,
     6  Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     7  XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     8  ZN3      ,XN4    ,YN4    ,ZN4      )
C
C      CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C----  INTBUF_TAB%CRIT V/A is used temporaly for sorting and Pen-max
       ALLOCATE(IPARTNS(NSN))
       IPARTNS(1:NSN)=0
       CALL I24STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM  ,IXS       ,PM ,
     2 GEO       ,NRTM0     ,IXC         ,-NUMINT   ,INTBUF_TAB%STFAC(1),
     3            NTY       ,INTBUF_TAB%VARIABLES(2),NOINT     ,
     4 INTBUF_TAB%STFNS,NSN0      ,MS          ,INTBUF_TAB%NSV,IXTG  ,
     5 IGAP      ,RWA   ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),
     6 IXT       ,IXP       ,INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(35),
     9 INACTI    ,KNOD2ELS  ,KNOD2ELC   ,KNOD2ELTG ,NOD2ELS   ,
     A NOD2ELC   ,NOD2ELTG  ,IGRSURF(MAX(1,ISU2)),IPARI(47) ,
     B INTBUF_TAB%IELES,INTBUF_TAB%IELEC, INTBUF_TAB%AREAS ,SH4TREE   ,SH3TREE,
     C IPART     ,IPARTC    ,IPARTTG   ,THK ,THK_PART,
     D IXR       ,ITAB      ,INTBUF_TAB%VARIABLES(7),IXS10 ,INTBUF_TAB%MSEGTYP24   ,
     E NRTM_SH    ,IXS16    ,IXS20    ,INTBUF_TAB%GAP_NM,INTBUF_TAB%MVOISIN    ,
     F ILEV      ,IGRSURF(MAX(1,ISU1)),INTBUF_TAB%VARIABLES(36),ID,TITR ,IPARI(53) ,
     G INTBUF_TAB%PENE_OLD  ,IPARTNS     ,IPARTS      , IGEO      ,FILLSOL   ,
     H PM_STACK  ,IWORKSH    ,IPARI(72)  ,TAGPRT_FRIC   , INTBUF_TAB%IPARTFRICS,
     G INTBUF_TAB%IPARTFRICM,INTBUF_FRIC_TAB,IPARI(86) , NRTS   ,INTBUF_TAB%IRECTS ,
     I INTBUF_TAB%IELNRTS,INTBUF_TAB%ADRECTS,INTBUF_TAB%FACNRTS,NMN,INTBUF_TAB%MSR   )
       IPARI(21) = IGAP
C---------GAP_S for fictive nodes       
        IF (NSNE >0) THEN
          CALL I24FICS_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                     INTBUF_TAB%IS2PT ,NSN     ,INTBUF_TAB%GAP_S   )
          CALL I24FICS_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                     INTBUF_TAB%IS2PT ,NSN     ,INTBUF_TAB%STFNS   )
          CALL I24FICI_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                     INTBUF_TAB%IS2PT ,NSN     ,IPARTNS   )
          CALL I24ISEGPT_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                 INTBUF_TAB%IS2PT ,NSN ,INTBUF_TAB%ISEGPT,3 ,INTBUF_TAB%ISPT2)
C---------secnd normal (nodal)------------------     
          CALL I24FICV_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                     INTBUF_TAB%IS2PT ,NSN     ,INTBUF_TAB%PENE_OLD,3   )
          CALL I24FICI_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                     INTBUF_TAB%IS2PT ,NSN     ,INTBUF_TAB%NBINFLG )
        END IF

C      INITIALIZE NODAL SECND,MAIN LENGHT FOR [K]geo(implicit) 
C------------------
       IF(INTKG > 0 ) THEN
        CALL I24LL_KG(
     1 X         ,INTBUF_TAB%IRECTM,IXS       ,PM        ,RWA      ,
     2 GEO       ,NRTM0     ,IXC         ,-NUMINT    ,NTY          ,
     3 NOINT     ,NSN0      ,INTBUF_TAB%NSV,IXTG      ,IXT         ,
     4 IXP       ,IPART     ,IPARTC      ,IPARTTG    ,THK          ,
     D THK_PART  ,IXR       ,ITAB        ,IXS10      ,IXS16        ,
     E IXS20     ,NMN       ,INTBUF_TAB%MSR,INTBUF_TAB%NOD_2RY_LGTH,
     F INTBUF_TAB%NOD_MAS_LGTH)
        CALL I24FICS_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,INTBUF_TAB%NSV    ,
     1                  INTBUF_TAB%IS2PT ,NSN     ,INTBUF_TAB%NOD_2RY_LGTH   )
       END IF
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       PENMAX = INTBUF_TAB%VARIABLES(37)


C ====================================================================
C      The following commented code flushes input for the hypmersh library
C      It is used to test the library
C      FILNAM='input24.dat'   
C      OPEN(UNIT=67,FILE=FILNAM,ACCESS='SEQUENTIAL',
C     .                  FORM='FORMATTED',STATUS='UNKNOWN')
C      IF(IDDLEVEL==1) CALL IO_TYPE24(0,67,
C     .                     IGAP, 
C     .                     IPEN0, 
C     .                     I_STOK, 
C     .                     INACTI,
C     .                     MULTIMP, 
C     .                     NMN, 
C     .                     NRTM, 
C     .                     NRTM0,
C     .                     NRTM_SH,
C     .                     NRTSE,
C     .                     NSN,
C     .                     NSN0,
C     .                     NSNE,
C     .                     NUMELS,
C     .                     NUMELS8,
C     .                     NUMELS10,
C     .                     NUMELS16,
C     .                     NUMELS20,
C     .                     NUMNOD ,
C     .                     INTBUF_TAB%I_STOK,
C     .                     INTBUF_TAB%ICONT_I,
C     .                     IPARTNS,
C     .                     INTBUF_TAB%IRECTM,
C     .                     INTBUF_TAB%IRTLM,
C     .                     INTBUF_TAB%IRTSE,
C     .                     INTBUF_TAB%IS2SE,
C     .                     INTBUF_TAB%IS2PT,
C     .                     ITAB ,
C     .                     INTBUF_TAB%MBINFLG,
C     .                     INTBUF_TAB%MSEGTYP24,
C     .                     INTBUF_TAB%MSR,
C     .                     MWA,
C     .                     INTBUF_TAB%NBINFLG,
C     .                     NOD2ELS,
C     .                     KNOD2ELS,
C     .                     NSEG,
C     .                     INTBUF_TAB%NSV,
C     .                     IXS,
C     .                     IXS10,
C     .                     IXS16,
C     .                     IXS20 ,
C     .                     INTBUF_TAB%MVOISIN,
C     .                     INTBUF_TAB%VARIABLES,
C     .                     X,
C     .                     INTBUF_TAB%CAND_E,
C     .                     INTBUF_TAB%CAND_N,
C     .                     INTBUF_TAB%GAP_M,
C     .                     INTBUF_TAB%GAP_NM,
C     .                     INTBUF_TAB%GAP_S,
C     .                     INTBUF_TAB%PENE_OLD,
C     .                     PENMIN,
C     .                     INTBUF_TAB%STFM ,
C     .                     INTBUF_TAB%STFNS,
C     .                     INTBUF_TAB%STIF_OLD,
C     .                     INTBUF_TAB%XFIC,
C     .                     RWA)
C      CLOSE(67)

       CALL I24BUC1(
     1   X        ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG  ,
     2   NMN      ,NRTM              ,MWA     ,NSN        ,INTBUF_TAB%CAND_E  ,
     3   INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),RWA     ,NOINT      ,I_STOK      ,
     4   INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX  ,MINBOX,INTBUF_TAB%MSR,
     5   INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS  ,MULTIMP   ,1       ,IDDLEVEL    ,
     6   ITAB     ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP,INTBUF_TAB%VARIABLES(13),
     7   INTBUF_TAB%VARIABLES(16),INACTI   ,INTBUF_TAB%STIF_OLD,INTBUF_TAB%PENE_OLD,I_MEM  ,
     8   INTBUF_TAB%VARIABLES(25),ID  ,TITR ,INTBUF_TAB%NBINFLG ,INTBUF_TAB%MBINFLG,
     9   ILEV ,INTBUF_TAB%MSEGTYP24,INTBUF_TAB%GAP_NM,INTBUF_TAB%MVOISIN,IXS   ,IXS10 ,
     A   IXS16  ,IXS20   ,IPARTNS ,IPEN0,PENMAX,INTBUF_TAB%IRTSE,
     B   INTBUF_TAB%IS2SE,INTBUF_TAB%IS2PT,INTBUF_TAB%XFIC,NRTSE ,NSNE ,PROV_N ,PROV_E,NSVG   ,
     1   IX1    ,IX2    ,IX3    ,IX4    ,X1     ,
     2   X2     ,X3     ,X4     ,Y1     ,Y2     ,
     3   Y3     ,Y4     ,Z1     ,Z2     ,Z3     ,
     4   Z4     ,XI     ,YI     ,ZI     ,X0     ,
     5   Y0     ,Z0     ,STIF   ,PENE   ,XN1    ,
     6   YN1    ,ZN1    ,XN2    ,YN2    ,ZN2    ,
     7   XN3    ,YN3    ,ZN3    ,XN4    ,YN4    ,
     8   ZN4    ,P1     ,P2     ,P3     ,P4     ,
     9   LB1    ,LB2    ,LB3    ,LB4    ,LC1    ,
     1   LC2    ,LC3    ,LC4    ,N11    ,N21    ,
     2   N31    ,INTBUF_TAB%VARIABLES(46))
         IF (I_MEM == 2) RETURN
       INTBUF_TAB%VARIABLES(9)  = MAXBOX
       INTBUF_TAB%VARIABLES(12) = MINBOX
       IF (IDDLEVEL==0)THEN
         IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
           CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
         ENDIF
         NULLIFY(pIXINT)
         IF(SIXINT>0)THEN
           IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
           pIXINT => IXINT(IAD_IXINT)
         ENDIF
         CALL I24NINTDD(
     .         NELEMINT       ,pIXINT                  ,INTBUF_TAB%IRECTM,
     .         INTBUF_TAB%NSV ,INTBUF_TAB%CAND_E       ,INTBUF_TAB%CAND_N ,I_STOK ,IFIEND,
     .         X              ,INTBUF_TAB%VARIABLES(2) ,DRAD,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     .         INTBUF_TAB%VARIABLES(46))
       ENDIF
       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     .  (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN0,NMN,IWCONT,
     .                 NSNT,NMNT)
       END IF
C
C-------PUIS CALCULER LES PENETRATIONS INITIALES as type5 for solid
C
       NGROUS=1+(I_STOK-1)/NVSIZ
       IF (NGROUS>0) THEN
C initialise IRTLM(2,NSN)=0 and after in I24PEN3, 
       DO I=1,2*NSN
        INTBUF_TAB%IRTLM(I)=0
       END DO
C---------PENE_OLD(5,NI)=ZERO       
       DO I=1,NSN
        INTBUF_TAB%PENE_OLD(1+5*(I-1)+4)=ZERO
       END DO
       ALLOCATE(PENMIN(NSN))
       PENMIN=EP10
       IWPENE0 = 0
       DO NG=1,NGROUS
        IF(IPRI>=5) WRITE(IOUT,2007)
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )
        CALL I24COR3(
     1    X     ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT) ,
     2    INTBUF_TAB%STFM,INTBUF_TAB%STFNS,GAPV   ,IGAP    ,INTBUF_TAB%VARIABLES(2) ,
     3    INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M)
        CALL I24PEN3(X    ,INTBUF_TAB%IRECTM ,GAPV        ,INTBUF_TAB%CAND_E(1+NFT),
     2              INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%NSV,INACTI,ITAB,MWA,IWPENE ,
     3              NSN   ,INTBUF_TAB%IRTLM,INTBUF_TAB%MSEGTYP24,IWPENE0,PENMIN,
     4              INTBUF_TAB%GAP_NM,INTBUF_TAB%MVOISIN,IXS   ,IXS10  ,IXS16 ,
     5              IXS20,PENMAX,INTBUF_TAB%VARIABLES(38),ID,TITR ,
     6              ILEV ,INTBUF_TAB%PENE_OLD,KNOD2ELS,NOD2ELS,IPARTNS,IPEN0 ,
     7              INTBUF_TAB%ICONT_I,INTBUF_TAB%XFIC,NRTM,INTBUF_TAB%IRTSE   ,
     8              INTBUF_TAB%IS2SE)
C INTBUF_TAB%I_STOK(1)==II_STOK
        INTBUF_TAB%I_STOK(1)=IWPENE
       ENDDO
       IWPENE0 = 0
       IWPENE = 0
       IF (IDDLEVEL == 1 ) THEN
        CALL I24PWR3(
     1    ITAB  ,INACTI,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,PENMIN ,
     1    INTBUF_TAB%VARIABLES(38),I_STOK,INTBUF_TAB%NSV,IWPENE ,INTBUF_TAB%PENE_OLD,
     2    NOINT  ,NTY   ,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRECTM ,
     4    NSN    ,ID    ,TITR   ,INTBUF_TAB%ICONT_I,IWPENE0 )
C-----cancel pressfit if no initial pene     
        IF (INACTI==-1.AND.IWPENE == 0) THEN
         IPARI(40) = 0
         CALL ANCMSG(MSGID=1566,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
        END IF
       END IF !(IDDLEVEL == 1 ) THEN
C------not to repeat the same mess       
       INPENE = 0

C      The following commented code flushes input for the hypmersh library
C      It is used to test the library
c      FILNAM='output24.dat'
c      OPEN(UNIT=68,FILE=FILNAM,ACCESS='SEQUENTIAL',
c    .                  FORM='FORMATTED',STATUS='UNKNOWN')

c     IF(IDDLEVEL==1)  CALL IO_TYPE24(0,68,
c    .                     IGAP,
c    .                     IPEN0,
c    .                     I_STOK,
c    .                     INACTI,
c    .                     MULTIMP,
c    .                     NMN,
c    .                     NRTM,
c    .                     NRTM0,
c    .                     NRTM_SH,
c    .                     NRTSE,
c    .                     NSN,
c    .                     NSN0,
c    .                     NSNE,
c    .                     NUMELS,
C    .                     NUMELS8,
c    .                     NUMELS10,
c    .                     NUMELS16,
c    .                     NUMELS20,
c    .                     NUMNOD ,
c    .                     INTBUF_TAB%I_STOK,
c    .                     INTBUF_TAB%ICONT_I,
c    .                     IPARTNS,
c    .                     INTBUF_TAB%IRECTM,
c    .                     INTBUF_TAB%IRTLM,
c    .                     INTBUF_TAB%IRTSE,
c    .                     INTBUF_TAB%IS2SE,
c    .                     INTBUF_TAB%IS2PT,
c    .                     ITAB ,
c    .                     INTBUF_TAB%MBINFLG,
c    .                     INTBUF_TAB%MSEGTYP24,
c    .                     INTBUF_TAB%MSR,
c    .                     MWA,
c    .                     INTBUF_TAB%NBINFLG,
c    .                     NOD2ELS,
c    .                     KNOD2ELS,
c    .                     NSEG,
c    .                     INTBUF_TAB%NSV,
c    .                     IXS,
c    .                     IXS10,
c    .                     IXS16,
c    .                     IXS20 ,
c    .                     INTBUF_TAB%MVOISIN,
c    .                     INTBUF_TAB%VARIABLES,
c    .                     X,
c    .                     INTBUF_TAB%CAND_E,
c    .                     INTBUF_TAB%CAND_N,
c    .                     INTBUF_TAB%GAP_M,
c    .                     INTBUF_TAB%GAP_NM,
c    .                     INTBUF_TAB%GAP_S,
c    .                     INTBUF_TAB%PENE_OLD,
c    .                     PENMIN,
c    .                     INTBUF_TAB%STFM ,
c    .                     INTBUF_TAB%STFNS,
c    .                     INTBUF_TAB%STIF_OLD,
c    .                     INTBUF_TAB%XFIC,
c    .                     RWA)
c      CLOSE(67)

        DEALLOCATE(PENMIN,IPARTNS)
       END IF !(NGROUS>0) THEN
       IF (INACTI==5.OR.INACTI==-1) THEN
C---       PEN_OLD(1:3,1:NSN)=ZERO
        PENE_MAX = ZERO
        DO I=1,NSN
         INTBUF_TAB%PENE_OLD(5*(I-1)+1)=ZERO
         INTBUF_TAB%PENE_OLD(5*(I-1)+2)=ZERO
         INTBUF_TAB%PENE_OLD(5*(I-1)+3)=INTBUF_TAB%PENE_OLD(5*(I-1)+5)
         PENE_MAX = MAX(PENE_MAX,INTBUF_TAB%PENE_OLD(5*(I-1)+5))
        END DO
        INTBUF_TAB%VARIABLES(23) = PENE_MAX
       END IF
       IF (NSNE>0 .AND. IDDLEVEL == 1 ) THEN
        FACF = ONE*NSNE/NSN
        WRITE(IOUT,2500)NSNE,FACF
       END IF
C-----------------------------------------------------------
C      ELEMENT NEIGHBROUR 
C--------------------------------------------------------
       CALL I24INISUR_NEI(
     1  NRTM         ,NSN    ,INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM,INTBUF_TAB%MVOISIN,
     2  INTBUF_TAB%NVOISIN,INTBUF_TAB%MSEGLO,INTBUF_TAB%MSEGTYP24,ITAB ,X   ,
     3  ID,TITR,IGEO )

       CALL I24INI_GAP_N(
     1 NRTM  ,INTBUF_TAB%IRECTM,IXS   ,GEO   ,IXC       ,IXTG  ,
     2 IXT   ,IXP   ,IPART  ,IPARTC   ,IPARTTG  ,
     3 THK   ,THK_PART,INTBUF_TAB%NVOISIN,INTBUF_TAB%GAP_NM,INTBUF_TAB%GAP_M,
     4 NMN   ,INTBUF_TAB%MSR,INTBUF_TAB%GAPN_M,INTBUF_TAB%GAP_N0,INTPLY,
     5 INTBUF_TAB%VARIABLES(36),IGEO,INTBUF_TAB%MSEGTYP24 )

C int24 :candidats initiaux pour inacti=5 -> tri a TT=0 engine
       IF (INACTI==5.OR.INACTI==-1)THEN
         CALL I24CAND(INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,NSN  ,
     +                INTBUF_TAB%IRTLM ,INTBUF_TAB%I_STOK(1),
     *                INTBUF_TAB%MSEGTYP24)

         IEDG4 = IPARI(59)
         IF(IEDG4 > 0)THEN
C T24 E2E
C Initialize ISPT2 Arrays for cycle 0 in case of INACTI
            CALL ISPT2_INI(INTBUF_TAB%CAND_N, INTBUF_TAB%I_STOK(1), NSN, INTBUF_TAB%IRTLM,
     *                     INTBUF_TAB%ISEGPT, INTBUF_TAB%ISPT2 )

         ENDIF
        ELSE
           INTBUF_TAB%I_STOK(1)=0
        ENDIF
C-----------------------------------------------------------------------
      ELSEIF(NTY==25) THEN
C-----------------------------------------------------------------------
C
C      Sorting NSV (Retrier les structures vs NSN, cf sub-interfaces, etc !)
       CALL I25SORS(NSN, NRTS, ITAB, ILEV, IPARI,
     .              INTBUF_TAB)
C------
C      IPEN0 = IPARI(54)
       IVIS2 = IPARI(14)
       ISHARP= IPARI(84)
       NRTM0=NRTM-NRTM_SH
       IF(INTTH > 0) DRAD = INTBUF_TAB%VARIABLES(32)

       CALL I7ERR3(
     1  X         ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT     ,ITAB,ID,TITR,
     2  IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3  X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4  Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5  Z4       ,N11    ,N21    ,N31     ,X0    ,
     6  Y0       ,Z0     ,XN1    ,YN1     ,ZN1   ,
     7  XN2      ,YN2    ,ZN2    ,XN3     ,YN3   ,
     8  ZN3      ,XN4    ,YN4    ,ZN4      )
C-----------------------------------------------------------
C        CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C-----------------------------------------------------------
       GAPSCALE=INTBUF_TAB%VARIABLES(13)
       ! Warning : VARIABLES(13) is modified after, to store GAPMIN

       IF(IPARI(72) > 0) THEN ! friction model : saving parts id of main segments
          ALLOCATE(IPARTSM(NRTM0))
          IPARTSM(1:NRTM0)=0
       ELSE
          ALLOCATE(IPARTSM(0))
       ENDIF
       CALL I25STI3(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM  ,IXS       ,PM ,
     2 GEO       ,NRTM0     ,IXC         ,-NUMINT   ,INTBUF_TAB%STFAC(1),
     3            NTY       ,INTBUF_TAB%VARIABLES(2),NOINT     ,
     4 INTBUF_TAB%STFNS,NSN      ,MS          ,INTBUF_TAB%NSV,IXTG  ,
     5 IGAP      ,RWA   ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),
     6 GAPSCALE  ,IXT   ,IXP,INTBUF_TAB%VARIABLES(6),INTBUF_TAB%VARIABLES(35),
     9 INACTI    ,KNOD2ELS  ,KNOD2ELC   ,KNOD2ELTG ,NOD2ELS   ,
     A NOD2ELC   ,NOD2ELTG  ,IPARI(47) ,
     B INTBUF_TAB%IELES,INTBUF_TAB%IELEM, INTBUF_TAB%AREAS ,SH4TREE   ,SH3TREE,
     C IPART     ,IPARTC    ,IPARTTG   ,THK ,THK_PART,
     D IXR       ,ITAB      ,INTBUF_TAB%VARIABLES(7),IXS10 ,INTBUF_TAB%MSEGTYP24   ,
     E NRTM_SH    ,IXS16    ,IXS20    ,INTBUF_TAB%GAP_NM,
     F ILEV      ,INTBUF_TAB%VARIABLES(36),ID,TITR ,IGAP0 ,
     G INTBUF_TAB%PENE_OLD ,IPARTS, IGEO   ,FILLSOL ,
     H PM_STACK  ,IWORKSH  ,INTBUF_TAB%VARIABLES(28),INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,
     I KNOD2EL1D,NOD2EL1D  ,IPARI(72)  ,TAGPRT_FRIC  ,INTBUF_TAB%IPARTFRICS     ,
     J INTBUF_TAB%IPARTFRICM,INTBUF_FRIC_TAB,IVIS2   ,GAPM_MX  ,GAPS_MX  ,
     K GAPM_L_MX ,GAPS_L_MX ,IPARTSM     ,DRAD       )
       IPARI(21) = IGAP
C-----------------------------------------------------------
C      ELEMENT NEIGHBROURS & FREE EDGES
C--------------------------------------------------------
C      MVOISIN & NVOISIN sont utilises temporairement pour construire ADMSR
C          MVOISIN(1:4,1:NRTM) neighboring segments along each edge
C          NVOISIN(1:8,1:NRTM) neighboring nodes on the 4 neighbors (on the cross)
C      ADMSR(1:4,1:NRTM)   adresses of node normals
C
       INTBUF_TAB%MVOISIN(1:4*NRTM)=0
       INTBUF_TAB%EVOISIN(1:4*NRTM)=0
       CALL I25NEIGH(
     1  NRTM         ,NSN    ,INTBUF_TAB%NSV,INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM,
     2  INTBUF_TAB%MVOISIN,INTBUF_TAB%EVOISIN,INTBUF_TAB%MSEGLO,INTBUF_TAB%MSEGTYP24,ITAB    ,
     3  X       ,ID     ,TITR    ,IGEO    ,IPARI(67) ,
     4  INTBUF_TAB%ADMSR,INTBUF_TAB%ADSKYN,INTBUF_TAB%IADNOR  ,IPARI(42),IEDGE,
     5  IPARI(68),INTBUF_TAB%LEDGE,INTBUF_TAB%LBOUND,INTBUF_TAB%VARIABLES(26),IPARI(36),
     6  INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBM,INTBUF_TAB%LISUBM,INTBUF_TAB%INFLG_SUBM,
     .                                                                        IPARI(90),
     7  INTBUF_TAB%ADDSUBE,INTBUF_TAB%LISUBE,INTBUF_TAB%INFLG_SUBE,NOINT,NMN,INTBUF_TAB%MSR,
     8  NOM_OPT,ILEV,INTBUF_TAB%MBINFLG,INTBUF_TAB%EBINFLG)

       IF(IEDGE/=0)THEN
         NCONTE   =IPARI(68) ! cf NCONTE=NEDGE cest a voir ...
         IPARI(88)=NCONTE ! cf NCONTE=NEDGE cest a voir ...
       END IF

       ITHK25 = IPARI(91)
       CALL I25INI_GAP_N(
     1 NRTM  ,INTBUF_TAB%IRECTM,IXS   ,GEO   ,IXC       ,IXTG  ,
     2 IXT   ,IXP   ,IPART  ,IPARTC   ,IPARTTG  ,
     3 THK   ,THK_PART,INTBUF_TAB%GAP_NM,INTBUF_TAB%GAP_M,NMN   ,
     4 INTBUF_TAB%MSR,INTBUF_TAB%GAPN_M,
     .                  INTBUF_TAB%VARIABLES(36),GAPSCALE,IGEO  ,
     5 INTBUF_TAB%MSEGTYP24 ,INTBUF_TAB%GAPMSAV, ITHK25)

       NADMSR=IPARI(67)
       NEDGE =IPARI(68)
       CALL I25NORM(NRTM,INTBUF_TAB%IRECTM,NUMNOD,X,INTBUF_TAB%EDGE_BISECTOR,
     .              NSN,INTBUF_TAB%MSR,ITAB,NRTM0,INTBUF_TAB%MSEGTYP24,
     .              INTBUF_TAB%MVOISIN,INTBUF_TAB%EVOISIN,NEDGE,INTBUF_TAB%LEDGE,
     .              INTBUF_TAB%LBOUND,INTBUF_TAB%ADMSR,INTBUF_TAB%VTX_BISECTOR,
     .              INTBUF_TAB%E2S_NOD_NORMAL,NADMSR,IEDGE)
C-----------------------------------------------------------
C      IGAP = 2 (IREM GAP)
C-----------------------------------------------------------
       IF (IREMNODE == 1) THEN ! IDDLEVEL == 1 !
C
         INTBUF_TAB%VARIABLES(13)=ZERO ! fake value
         INTBUF_TAB%VARIABLES(16)=EP30 ! fake value
C
         CALL I7REMNODE(IREMNODE,NOINT,TITR,INTBUF_TAB,NUMNOD+NUMFAKENODIGEO ,
     1   X,NRTM     ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,NSN,
     2 ITAB ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     3 INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,IGAP,INTBUF_TAB%VARIABLES(2),DRAD  ,
     4 IPARI(62) ,NTY       ,IPARI     ,I_MEM_REM ,GAPM_MX            ,
     5 GAPS_MX   ,GAPM_L_MX ,GAPS_L_MX ,ILEV      ,INTBUF_TAB%NBINFLG ,
     6 INTBUF_TAB%MBINFLG,INTBUF_TAB%VARIABLES(46))
C
       END IF
C-----------------------------------------------------------
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       PENMN  = INTBUF_TAB%VARIABLES(38)
       PENMAX = INTBUF_TAB%VARIABLES(37)

       CALL I25BUC_VOX1(
     1 X        ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG  ,
     2 NMN      ,NRTM              ,MWA     ,NSN        ,INTBUF_TAB%CAND_E  ,
     3 INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),NOINT       ,I_STOK ,
     4 INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX  ,MINBOX,INTBUF_TAB%MSR,
     5 INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS  ,MULTIMP   ,1       ,IDDLEVEL    ,
     6 ITAB     ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP,INTBUF_TAB%VARIABLES(13),
     7 INTBUF_TAB%VARIABLES(16),INACTI   ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,I_MEM  ,
     8 INTBUF_TAB%VARIABLES(25),ID  ,TITR ,INTBUF_TAB%NBINFLG ,INTBUF_TAB%MBINFLG,
     9 ILEV ,INTBUF_TAB%MSEGTYP24,INTBUF_TAB%GAP_NM,INTBUF_TAB%VARIABLES(7),
     A IPARTS  ,KNOD2ELS ,NOD2ELS ,
     B IREMNODE,IPARI(63),INTBUF_TAB%KREMNODE,INTBUF_TAB%REMNODE,
     C IXS, IXS10, IXS16, IXS20,ICODE,ISKEW,
     D DRAD,INTBUF_TAB%PENE_OLD,INTBUF_TAB%VARIABLES(46))

       IF (I_MEM == 2) RETURN
C
       IF(IEDGE/=0)THEN

         CALL I25STI_EDG(NEDGE,INTBUF_TAB%LEDGE,INTBUF_TAB%STFE,INTBUF_TAB%STFM,IGAP          ,
     2                   INTBUF_TAB%GAPE,INTBUF_TAB%GAP_E_L,INTBUF_TAB%GAP_M,INTBUF_TAB%GAP_ML,
     .                                                                       INTBUF_TAB%GAP_SL,
     3                   INTBUF_TAB%VARIABLES(40),IPARI(72)  ,INTBUF_TAB%IPARTFRIC_E,
     J                   INTBUF_TAB%IPARTFRICM  ,IPARTSM     ,BGAPEMX_L  ,NSN,INTBUF_TAB%NSV)

         INTBUF_TAB%I_STOK_E(1:2)=0
         CAND_E_OLD(1:2) = INTBUF_TAB%I_STOK_E(1:2)

 2510    continue


C
        IF (IREMNODE_EDG == 1) THEN
C
          ALLOCATE(INOD2LIN(NUMNOD+1),TAGSECND(NUMNOD),NOD2LIN(2*NEDGE))
          I_START = 1
          I_MEM_REM = 0
          DO WHILE (I_START < NEDGE)
C
            CALL I25REMLINE(
     1      X      ,NEDGE  ,INTBUF_TAB%LEDGE,NUMNOD         ,INTBUF_TAB%GAPE     ,INTBUF_TAB%GAP_E_L    ,
     2      IGAP0  ,IGAP   ,DRAD            ,INTBUF_TAB%VARIABLES(40),BGAPEMX_L  ,INTBUF_TAB%KREMNODE_EDG,
     3      INTBUF_TAB%REMNODE_EDG,IPARI(94),I_START        ,I_MEM_REM            ,INOD2LIN              ,
     4      TAGSECND ,NOD2LIN     ,INTBUF_TAB%VARIABLES(46) )
C
C           Reallocation of REMNODE arrays if necessary
C
            IF (I_MEM_REM == 1) THEN
              NEW_SIZE = IPARI(94) + 5*NEDGE
              CALL UPGRADE_REMNODE_EDG(IPARI(1),NEW_SIZE,INTBUF_TAB)
              I_MEM_REM = 0
            ENDIF
C
          ENDDO
C
          DEALLOCATE(INOD2LIN,TAGSECND,NOD2LIN)
          IREMNODE_EDG = 0
C
        ENDIF


         ITASK     = 0
         ESHIFT    = 0
         NEDGE_T   = NEDGE
         SSHIFT    = 0
         NRTM_T    = NRTM

         NCONTE    = IPARI(88)
         MULTIMPE  = IPARI(87)
         MULTIMPS  = IPARI(89)
         MULNSNE   = MULTIMPE*NCONTE
         MULNSNS   = MULTIMPS*NCONTE

         MARGE     = INTBUF_TAB%VARIABLES(25) ! meme marge pour e2e que pour n2s
         GAP       = INTBUF_TAB%VARIABLES(2)
         VMAXDT    = ZERO
         I_MEME(1:2)= 0
         INACTBID  =0 ! nothing to do wrt old impacts in RD Starter
         CALL I25BUCE_EDG(
     1     X      ,INTBUF_TAB%IRECTM,INACTBID,NSN     ,
     2     NMN    ,INTBUF_TAB%CANDM_E2E ,INTBUF_TAB%CANDS_E2E ,
     3     GAP    ,NOINT       ,INTBUF_TAB%I_STOK_E(1) ,MULNSNE    ,BMINMA       ,
     4     MARGE  ,VMAXDT      ,DRAD ,ESHIFT ,NEDGE_T     ,
     5     SSHIFT ,NRTM_T      ,INTBUF_TAB%STFM ,INTBUF_TAB%STFNS,
     6     NCONTE      ,INTBUF_TAB%GAP_M       ,ITASK      ,INTBUF_TAB%VARIABLES(40),
     7     I_MEME ,ITAB   ,INTBUF_TAB%MBINFLG,INTBUF_TAB%EBINFLG,INTBUF_TAB%I_STOK_E(2) ,
     8     MULNSNS,ILEV   ,IBIDON        ,INTBUF_TAB%CAND_P ,IGAP0    ,
     9     IPARI(63),INTBUF_TAB%KREMNODE_EDG(1+2*ESHIFT),INTBUF_TAB%REMNODE_EDG,
     A     IGAP   ,INTBUF_TAB%GAP_ML,IEDGE ,NEDGE ,INTBUF_TAB%MSEGTYP24,
     B     INTBUF_TAB%LEDGE,INTBUF_TAB%ADMSR,INTBUF_TAB%EDGE_BISECTOR,INTBUF_TAB%VTX_BISECTOR,
     C     INTBUF_TAB%CANDM_E2S ,INTBUF_TAB%CANDS_E2S,IBIDON,INTBUF_TAB%CAND_PS,INTBUF_TAB%GAPE,
     D     INTBUF_TAB%GAP_E_L,INTBUF_TAB%VARIABLES(46))

         IF(I_MEME(1) /=0 .OR. I_MEME(2)/=0)THEN
           NCONTE   = IPARI(88)
           IF(I_MEME(1)/=0)THEN ! Main shell edges
             MULTIMPE = MAX(IPARI(87)+8,NINT(IPARI(87)*1.75))
             MULTIMPE = MAX(MULTIMPE,INTBUF_TAB%S_CANDL_MAX / MAX(1,NCONTE))
             INTBUF_TAB%S_CANDL_MAX =
     .               MAX(MULTIMPE*NCONTE,INTBUF_TAB%S_CANDL_MAX)
             CALL UPGRADE_LCAND_EDG(NUMINT,MULTIMPE,INTBUF_TAB)
           END IF
           IF(I_MEME(2)/=0)THEN ! Main Solid edges
             MULTIMPS = MAX(IPARI(89)+8,NINT(IPARI(89)*1.75))
             MULTIMPS = MAX(MULTIMPS,INTBUF_TAB%S_CANDS_MAX / MAX(1,NCONTE))
             INTBUF_TAB%S_CANDS_MAX =
     .               MAX(MULTIMPS*NCONTE,INTBUF_TAB%S_CANDS_MAX)
             CALL UPGRADE_LCAND_E2S(NUMINT,MULTIMPS,INTBUF_TAB)
           END IF
           I_MEME(1:2) = 0
           INTBUF_TAB%I_STOK_E(1:2)=CAND_E_OLD(1:2)
           GOTO 2510 ! on retrie tout
         END IF
       ENDIF
C
       IF (IDDLEVEL==0)THEN
         IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
           CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
         ENDIF
         NULLIFY(pIXINT)
         IF(SIXINT>0)THEN
           IAD_IXINT=MIN(SIXINT, 6*NELEMINT+1)
           pIXINT => IXINT(IAD_IXINT)
         ENDIF
         CALL I25NINTDD(
     .         NELEMINT       ,pIXINT                  ,INTBUF_TAB%IRECTM,
     .         INTBUF_TAB%NSV ,INTBUF_TAB%CAND_E       ,INTBUF_TAB%CAND_N ,I_STOK ,IFIEND,
     .         X              ,INTBUF_TAB%VARIABLES(2) ,DRAD,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     .         INTBUF_TAB%VARIABLES(46))
       ENDIF
       IF((IDDLEVEL==0.OR.NDSOLV==1).AND.
     .  (DECTYP>=3.AND.DECTYP<=6))THEN
C Appel routine poids noeuds interfaces
         CALL IWCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                 NSNT,NMNT)
       END IF

       IF(IDDLEVEL==1)THEN
C-------------------------------------------------------------
C        PUIS CALCULER LES PENETRATIONS INITIALES
C-------------------------------------------------------------
         NGROUS=1+(I_STOK-1)/NVSIZ
         IF (NGROUS>0) THEN
C
           DO I=1,4*NSN
            INTBUF_TAB%IRTLM(I)=0
           END DO
C 
           INTBUF_TAB%PENE_OLD(1:5*NSN)=ZERO
           INTBUF_TAB%TIME_S(1:NSN)=EP20
C
           DO NG=1,NGROUS
            NFT = (NG-1) * NVSIZ
            LFT = 1
            LLT = MIN0( NVSIZ, I_STOK - NFT )

            CALL I25COR3(
     1        LLT  ,IGAP ,X ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     2        INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT) ,XI, YI, ZI,
     4        IX1     ,IX2   ,IX3   ,IX4   ,NSVG   ,
     5        NSN     ,INTBUF_TAB%GAP_S   ,GAPS  ,INTBUF_TAB%ADMSR ,INTBUF_TAB%EDGE_BISECTOR  ,
     7        X1      ,X2     ,X3    ,X4    ,X0          ,
     8        Y1      ,Y2     ,Y3    ,Y4    ,Y0          ,
     9        Z1      ,Z2     ,Z3    ,Z4    ,Z0          ,
     A        NNX     ,NNY    ,NNZ   ,INTBUF_TAB%MVOISIN ,MVOISN  ,
     B        INTBUF_TAB%GAP_M   ,GAPM  ,INTBUF_TAB%GAP_NM, GAP_NM,
     C        INTBUF_TAB%GAP_SL ,INTBUF_TAB%GAP_ML ,GAPMXL,INTBUF_TAB%LBOUND ,IBOUND )

            CALL I25PEN3(LLT    ,INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%CAND_E(1+NFT),PENMN ,PENMAX,
     2                   X1     ,X2     ,X3     ,X4     ,X0     ,
     3                   Y1     ,Y2     ,Y3     ,Y4     ,Y0     ,
     4                   Z1     ,Z2     ,Z3     ,Z4     ,Z0     ,
     5                   XI     ,YI     ,ZI     ,NSN    ,IX1    ,
     6                   IX2    ,IX3    ,IX4    ,NSVG   ,NRTM   ,
     7                   INTBUF_TAB%MSEGLO ,GAPS   ,INTBUF_TAB%IRECTM ,INTBUF_TAB%IRTLM  ,
     8                   INTBUF_TAB%TIME_S ,INTBUF_TAB%PENE_OLD,ITAB ,INTBUF_TAB%MSEGTYP24,ISHARP,
     9                   NNX     ,NNY    ,NNZ   ,GAP_NM  ,MVOISN,
     A                   GAPMXL  ,IVIS2  ,IBOUND,INTBUF_TAB%VTX_BISECTOR )

           ENDDO
C
           CALL I25PWR3(
     .      ITAB   ,INACTI,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,INTBUF_TAB%STFNS,
     1      X      ,I_STOK,INTBUF_TAB%NSV,IWPENE ,INTBUF_TAB%PENE_OLD,
     2      NOINT  ,NTY   ,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,INTBUF_TAB%IRECTM ,
     3      NSN    ,ID    ,TITR    ,INTBUF_TAB%MSEGLO)
C
         END IF !(NGROUS>0) THEN
C-------------------------------------------------------------
         CALL ANCMSG(MSGID=1164,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            I1=ID,
     .            C1=TITR,
     .            PRMOD=MSG_PRINT)
C
         IF(IWPENE /= 0.AND. IDDLEVEL == 1)THEN
           IF(INACTI==0)THEN
             CALL ANCMSG(MSGID=1166,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IWPENE)
           ELSEIF(INACTI==5.AND. IDDLEVEL == 1)THEN
             CALL ANCMSG(MSGID=1167,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IWPENE)
           ELSEIF(INACTI==-1.AND. IDDLEVEL == 1)THEN
             CALL ANCMSG(MSGID=1168,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IWPENE)
           ELSE
             CALL ANCMSG(MSGID=1165,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=INACTI)
           END IF
         END IF
C
         INPENE = IWPENE
         IF (INACTI==5.OR.INACTI==-1) THEN
           DO I=1,NSN
            INTBUF_TAB%PENE_OLD(5*(I-1)+1)=ZERO
            INTBUF_TAB%PENE_OLD(5*(I-1)+2)=ZERO
           END DO
C
           INTBUF_TAB%VARIABLES(23) = ZERO
C
C          INT25 : candidats initiaux pour inacti=5 (garde symetriques) <=> tri a TT=0 engine
           CALL I25CAND(INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,NSN  ,
     +                                    INTBUF_TAB%IRTLM ,INTBUF_TAB%I_STOK(1),
     *                  NRTM ,INTBUF_TAB%MSEGTYP24)
         ELSE
           INTBUF_TAB%I_STOK(1)=0
         END IF
C------------------------------------------------------------
        IF (IEDGE /=0)THEN
          IWPENE=0
          IF(INTBUF_TAB%I_STOK_E(1) > 0) THEN
C 
            ISTOK=0
C 
            NGROUS=1+(INTBUF_TAB%I_STOK_E(1)-1)/NVSIZ
            DO NG=1,NGROUS
              NFT = (NG-1) * NVSIZ
              LFT = 1
              LLT = MIN0( NVSIZ, INTBUF_TAB%I_STOK_E(1) - NFT )

              CALL I25COR3E(
     1         LLT          ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X      ,
     2         INTBUF_TAB%CANDS_E2E(1+NFT) ,INTBUF_TAB%CANDM_E2E(1+NFT) ,
     3         XXS1         ,XXS2       ,XYS1         ,XYS2    ,XZS1        ,
     4         XZS2       ,XXM1         ,XXM2         ,XYM1    ,XYM2        ,
     5         XZM1       ,XZM2         ,EX           ,EY      ,EZ          ,
     7         FX         ,FY           ,FZ           ,
     8         N1           ,N2           ,M1           ,M2           ,NEDGE   ,
     9         INTBUF_TAB%GAPE,GAPVE,
     A         IEDGE       ,INTBUF_TAB%ADMSR,INTBUF_TAB%LBOUND,INTBUF_TAB%EDGE_BISECTOR,
     B         INTBUF_TAB%VTX_BISECTOR       ,ITAB        ,IGAP0    ,IGAP        ,
     C         INTBUF_TAB%GAP_E_L)

              CALL I25PEN3E(
     1         LLT    ,IEDGE ,INTBUF_TAB%CANDS_E2E(1+NFT) ,INTBUF_TAB%CANDM_E2E(1+NFT) ,
     2         N1     ,N2    ,M1    ,M2    ,
     3         XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     4         XZS1  ,XZS2   ,XXM1  ,XXM2  ,XYM1  ,
     5         XYM2  ,XZM1   ,XZM2  ,GAPVE ,PENE  ,
     6         EX    ,EY     ,EZ    ,FX    ,FY    ,
     7         FZ    ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X    ,ITAB )

              CALL I25PWR3E(
     1      ITAB   ,INACTI,INTBUF_TAB%CANDM_E2E(1+NFT),INTBUF_TAB%CANDS_E2E(1+NFT),ISTOK,
     2      LLT    ,PENE  ,IWPENE ,INTBUF_TAB%CAND_P(1+NFT) ,
     3      N1     ,N2    ,M1     ,M2    ,
     4      NOINT  ,NTY   ,INTBUF_TAB%IRECTM ,ID  ,TITR    ,
     5      INTBUF_TAB%CANDM_E2E,INTBUF_TAB%CANDS_E2E,INTBUF_TAB%CAND_P)

            ENDDO
            INTBUF_TAB%I_STOK_E(1)=ISTOK
          END IF
C------------------------------------------------------------
          IF(INTBUF_TAB%I_STOK_E(2) > 0) THEN
C 
            ISTOK=0
C 
            NGROUS=1+(INTBUF_TAB%I_STOK_E(2)-1)/NVSIZ
            DO NG=1,NGROUS
              NFT = (NG-1) * NVSIZ
              LFT = 1
              LLT = MIN0( NVSIZ, INTBUF_TAB%I_STOK_E(2) - NFT )

              CALL I25COR3_E2S(
     1         LLT          ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X      ,
     2         INTBUF_TAB%CANDS_E2S(1+NFT) ,INTBUF_TAB%CANDM_E2S(1+NFT) ,EX ,EY ,EZ ,
     3         XXS1         ,XXS2       ,XYS1         ,XYS2    ,
     4         XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1    ,
     5         XYM2         ,XZM1         ,XZM2         ,
     6         N1           ,N2           ,M1           ,M2           ,NEDGE   ,
     7         INTBUF_TAB%GAPE,GAPVE      ,FX         ,FY             ,FZ      ,
     8         IEDGE        ,INTBUF_TAB%ADMSR,INTBUF_TAB%LBOUND,INTBUF_TAB%EDGE_BISECTOR,
     9         INTBUF_TAB%VTX_BISECTOR,ITAB   )

              CALL I25DST3_E2S(
     1         LLT    ,IEDGE ,INTBUF_TAB%CANDS_E2S(1+NFT) ,INTBUF_TAB%CANDM_E2S(1+NFT) ,
     2         N1     ,N2    ,M1    ,M2   ,
     3         XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     4         XZS1  ,XZS2   ,XXM1  ,XXM2  ,XYM1  ,
     5         XYM2  ,XZM1   ,XZM2  ,GAPVE ,PENE  ,
     6         EX    ,EY     ,EZ    ,FX    ,FY     ,
     7         FZ    ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X    ,ITAB ,INTBUF_TAB%E2S_NOD_NORMAL,
     8         INTBUF_TAB%ADMSR)

              CALL I25PWR3_E2S(
     1         ITAB   ,INACTI,INTBUF_TAB%CANDM_E2S(1+NFT),INTBUF_TAB%CANDS_E2S(1+NFT),ISTOK,
     2         LLT    ,PENE  ,IWPENE ,INTBUF_TAB%CAND_PS(1+4*NFT) ,
     3         N1     ,N2    ,M1     ,M2    ,
     4         NOINT  ,NTY   ,INTBUF_TAB%IRECTM ,ID  ,TITR    ,
     5         INTBUF_TAB%CANDM_E2S,INTBUF_TAB%CANDS_E2S,INTBUF_TAB%CAND_PS)

            ENDDO
            INTBUF_TAB%I_STOK_E(2)=ISTOK
          END IF
C------------------------------------------------------------
          CALL ANCMSG(MSGID=1631,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             PRMOD=MSG_PRINT)
C
          IF(IWPENE /= 0 .AND. IDDLEVEL == 1)THEN
            IF(INACTI==0)THEN
              CALL ANCMSG(MSGID=1632,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IWPENE)
            ELSEIF(INACTI==5)THEN
              CALL ANCMSG(MSGID=1633,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IWPENE)
            ELSEIF(INACTI==-1)THEN
              CALL ANCMSG(MSGID=1634,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IWPENE)
C           ELSE
C             CALL ANCMSG(MSGID=1165,
C     .                  MSGTYPE=MSGERROR,
C     .                  ANMODE=ANINFO_BLIND_1,
C     .                  I1=ID,
C     .                  C1=TITR,
C     .                  I2=INACTI)
            END IF
          END IF
        END IF !(NGROUS>0) THEN
C
       END IF ! IF(IDDLEVEL==1)THEN
       DEALLOCATE (IPARTSM)
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      IF(IWPENE/=0) THEN
         IF(INPENE/=0 .AND. IDDLEVEL == 1)THEN
          IF(.NOT.TYPE18)CALL ANCMSG(MSGID=343,
     .                               MSGTYPE=MSGWARNING,
     .                               ANMODE=ANINFO_BLIND_1,
     .                               I1=ID,
     .                               C1=TITR,
     .                               I2=IWPENE,
     .                               I3=INPENE)
         ELSEIF (IDDLEVEL == 1.AND.NTY/=24) THEN
          IF(.NOT.TYPE18)CALL ANCMSG(MSGID=499,
     .                               MSGTYPE=MSGWARNING,
     .                               ANMODE=ANINFO_BLIND_1,
     .                               I1=ID,
     .                               C1=TITR,I2=IWPENE)
         ENDIF
       IF(NTY==24.AND. IDDLEVEL == 1) THEN
         SELECT CASE (INACTI)
          CASE(0,1)
           CALL ANCMSG(MSGID=1184,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IWPENE,
     .                 I3=IWPENE0)
          CASE(-1)
           CALL ANCMSG(MSGID=1185,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IWPENE,
     .                 I3=IWPENE0)
          CASE(5)
           CALL ANCMSG(MSGID=1186,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IWPENE,
     .                 I3=IWPENE0)
         END SELECT
       IF(IPRI>=5.AND. IDDLEVEL == 1) CALL ANCMSG(MSGID=1164,
     .          MSGTYPE=MSGWARNING,
     .          ANMODE=ANINFO_BLIND_1,
     .          I1=ID,
     .          C1=TITR,
     .          PRMOD=MSG_PRINT)
       END IF
      ENDIF
C
      RETURN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 2001 FORMAT(//,1X,'INTERFACE NUMBER. . . . . . . . . . . . . .',I10/
     +         ,1X,'INTERFACE TYPE. . . . . . . . . . . . . . .',I6/)
 2002 FORMAT(//
     +'   SECONDARY NEAREST  NEAREST        MAIN NODES                       SECONDARY '/
     +'   NODE   MAIN  SEGMENT                                      S               T')
 2003 FORMAT(//
     +'  MAIN NEAREST  NEAREST        SECONDARY  NODES                       MAIN'/
     +'   NODE    SECONDARY  SEGMENT                                      S               T')
 2007 FORMAT(//'    IMPACT CANDIDATES',/,'   SECONDARY        MAIN SEGMENT NODES '/'   NODE ')
 2011 FORMAT(//'    IMPACT CANDIDATES',/,'  MAIN   NODES     SECONDARY  NODES ')
 2012 FORMAT(2X,'** WARNING ** THIS INTERFACE CONNECTS',' LAGRANGIAN MATERIALS')
 2181 FORMAT(//,1X,'INTERFACE NUMBER. . . . . . . . . . . . . .',I10/
     +         ,1X,'INTERFACE TYPE. . . . . . . . . . . . . . .    18')
 2200 FORMAT(2X,/,I8,' TINY INITIAL PENETRATIONS WILL BE TAKEN INTO
     + ACCOUNT')
 2300 FORMAT(2X,/,I8,' INITIAL PENETRATIONS WILL BE IGNORED ')
 2400 FORMAT(2X,/,'IPEN_MAX = ',1PG20.13,'HAS BEEN USED')
 2500 FORMAT(/,
     .    'FICTIVE NODES ADDED FOR EDGE . . . . . . . . =',I10/,
     .    'RATIO of Fictive SECONDARY nodes/SECONDARY nodes. .  =',1PG20.13/)
      END

